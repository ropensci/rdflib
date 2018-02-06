as_rdf <- function(df, base_uri = NULL) UseMethod("as_rdf")


## tidy data to rdf
as_rdf.data.frame <- function(df, base_uri = NULL){
  
  x <- tibble::rowid_to_column(df, "subject")
  suppressWarnings(
    x <- tidyr::gather(x, key = predicate, value = object, -subject)
  )
  
  ## gather looses col-classes
  col_classes <- data.frame(datatype = vapply(df, rdflib:::xs_class, character(1)))
  col_classes <- tibble::rownames_to_column(col_classes, "predicate")
  x <- dplyr::inner_join(x, col_classes, "predicate")
  
  rdf <- rdf()
  for(i in seq_along(x$subject)){
    rdf <- rdf_add(rdf, 
                   subject = paste0(base_uri, as.character(x$subject[[i]])),
                   predicate = paste0(base_uri, x$predicate[[i]]),
                   object = x$object[[i]],
                   datatype_uri = x$datatype[[i]])
  }
  rdf
}



as_rdf.list <- function(x){
}


## Testing: Digest some data.frames into RDF and extract back
# library(tidyverse)
# cars <- mtcars %>% rownames_to_column("Model")
# x1 <- as_rdf(iris, "iris:")
# x2 <- as_rdf(cars, "mtcars:")
# rdf <- c(x1,x2)

rdf_serialize(x1, "iris.json", "jsonld")
jsonld::jsonld_compact("iris.json", context = '{"@base": "iris:"}')


#' Tidy Schema
#' 
#' Construct a SPARQL query in which properties (predicates) become column headings
#' and values (objects) become entries of individual cells.  
#' 
#' @param ... names of the properties that should make up columns in the table
#' @param columns Alternatively, supply a vector of property names. 
#' @param prefix the URI string to prefix before the property names to give
#'  fully-resolved properties.  
#' @param na.rm logical, default TRUE. Will not return a row for any 
#'  subject that does not have a object value matching the schema.
#' @examples
#' 
#' sparql <- table_schema("Species",  "Sepal.Length", prefix = "iris")
#' rdf_query(rdf, sparql)
#' 
#' ## use columns arg for an existing vector of names.
#' columns <- names(iris)
#' sparql <- table_schema(columns = columns, prefix = "iris")
#' rdf_query(rdf, sparql)
#' 
#' ## use na.rm = FALSE to include NA if variable is not defined for some observations
#' sparql <- table_schema("Species",  "Sepal.Length", "Sepal.Color",
#'                        prefix = "iris", na.rm=FALSE)
#' rdf_query(rdf, sparql)
tidy_schema <- function(..., columns = NULL, prefix=NULL, na.rm = TRUE){
  if(is.null(columns)){
    columns <- c(...)
  }
  
  if(!is.null(prefix)){
    attributes <- paste0("<",prefix, ":", columns, ">")
  } else {
    attributes <- paste0("<", columns, ">")
  }
  ## Replace forbidden characters with "_", 
  ## See spec: https://www.w3.org/TR/rdf-sparql-query/#rVARNAME
  vars <- gsub("[^a-zA-Z1-9_]","_", basename(columns))
  select <- paste0("?", vars)
  
  where <- paste("?s", attributes, select)
  if(!na.rm)
    where <- paste("OPTIONAL {", where, "}")
  else
    where <- paste(where, ".")
  where <- paste(where, collapse = "\n")
  query <- paste("SELECT", paste0(select, collapse = " "),
                 "\nWHERE {\n", where, "\n}")
  query
}


#sparql <- table_schema("Sepal.Length", "Sepal.Width", 
#                       "Petal.Length", "Petal.Width",  
#                       "Species", prefix="iris")
#sparql <- table_schema(columns=names(iris), prefix="iris")
#rdf_query(rdf, sparql) %>% as_tibble()


