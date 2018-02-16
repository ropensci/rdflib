as_rdf <- function(df, key = NULL, base_uri = NULL, loc = NULL) UseMethod("as_rdf")



## tidy data to rdf
as_rdf.data.frame <- function(df, key = NULL, base_uri = NULL, loc = tempfile()){
  
  x <- df
  if(is.null(key)){
    x <- tibble::rowid_to_column(x, "subject")
  } else {
    names(x)[names(x) == key] <- "subject"
  }
  
  ## FIXME consider taking an already-gathered table to avoid dependency?
  suppressWarnings(
    x <- tidyr::gather(x, key = predicate, value = object, -subject)
  )
  
  
  
  ## gather looses col-classes, so pre-compute them (with base R)
  col_classes <- data.frame(datatype = 
                              vapply(df, 
                                     rdflib:::xs_class, 
                                     character(1), 
                                     explicit_strings = TRUE),
                            stringsAsFactors = FALSE)
  col_classes$predicate <- rownames(col_classes)
  rownames(col_classes) <- NULL
  
  x <- merge(x, col_classes, by = "predicate")
  
  ## DROP NA triples -- fixme, these should be blank nodes
  x <- na.omit(x)
  
  ## NA needs to become a unique blank node number, could do uuid or _:r<rownum>
  #x$object[is.na(x$object)] <- ""
  #x$subject[is.na(x$subject)] <- ""
  
  
  
  ## A poor man's serializtion of a data.frame into nquads
  x$subject = paste0("<", base_uri, x$subject, ">")
  x$predicate = paste0("<", base_uri, x$predicate, ">")
  x$object = paste0('\"', x$object, '\"^^<', x$datatype, ">")
  x$graph = "."
  x <- x[c("subject", "predicate", "object", "graph")]       
  write.table(x, loc, col.names = FALSE, quote = FALSE, row.names = FALSE)     
  
  ## And parse text file.  Way faster than adding row by row!
  rdf <- rdf_parse(loc, "nquads")
  
  
  ## readr would probably be faster but has incompatible auto-quoting rules
  ##readr::write.delim(x, path = loc, col_names = FALSE)
  ## NA to blank string 
  #x$object[is.na(x$object)] <- ""
  #x$subject[is.na(x$subject)] <- ""
  #rdf <- rdf()
  #for(i in seq_along(x$subject)){
  #  rdf <- rdf_add(rdf, 
  #                 subject = paste0(base_uri, as.character(x$subject[[i]])),
  #                 predicate = paste0(base_uri, x$predicate[[i]]),
  #                 object = x$object[[i]],
  #                 datatype_uri = x$datatype[[i]])
  #}
  
  rdf
}


as_rdf.list <- function(x, context = '"@vocab": "x:"'){
  if(is(x, "list") && length(x) == 1) x <- x[[1]]
  json <- jsonlite::toJSON(x, pretty = TRUE, auto_unbox = TRUE, force = TRUE)
  json2 <- paste0('{\n"@context": {', context, '},\n',  '"@graph": ', json,  '}')
  rdf <- rdflib::rdf_parse(json2, "jsonld")
  rdf
}

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

quick_quads <- function(df, prefix = "a:"){
  #stopifnot(names(df) == c("subject", "predicate", "object"))
  mutate(subject = paste0(prefix, subject), predicate = )
}
