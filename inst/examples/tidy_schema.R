

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
#' sparql <- tidy_schema(columns = columns, prefix = "iris")
#' rdf_query(rdf, sparql)
#' 
#' ## use na.rm = FALSE to include NA if variable is not defined for some observations
#' sparql <- tidy_schema("Species",  "Sepal.Length", "Sepal.Color",
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
