as_rdf <- function(df, key = NULL, base_uri = NULL, loc = NULL) UseMethod("as_rdf")

# test2 <- tibble(
#   age = 5L,
#   name = "bob",
#   height = 1.9,
#   address = "x:address",
#   knows = NA
# )
# as_rdf.data.frame(test2, NULL, "x:", "test2.nquads")

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
                                     character(1)),
                            stringsAsFactors = FALSE)
  col_classes$predicate <- rownames(col_classes)
  rownames(col_classes) <- NULL
  
  ## merge is Slow! ~ 5 seconds for 800K triples
  #x <- merge(x, col_classes, by = "predicate")
  x <- dplyr::left_join(x, col_classes, by = "predicate")
 
  ## paste0 is a little slow ~ 1 s on 800K triples
  
  ## A POOR MAN'S NQUAD writer
  
  ## NA needs to become a unique blank node number, could do uuid or _:r<rownum>
  x$object[is.na(x$object)] <- paste0("_:r", which(is.na(x$object)))
  x$subject[is.na(x$subject)] <- paste0("_:r", which(is.na(x$subject)))
  
  ## strings and URIs do not get a datatype
  needs_type <- !is.na(x$datatype)
  
  x$subject = paste0("<", base_uri, x$subject, ">")
  ## Predicate is always a URI
  x$predicate = paste0("<", base_uri, x$predicate, ">")
  
  ## Strings should be quoted
  is_string <- !grepl("\\w+:\\w.*", x$object) & !needs_type
  x$object[is_string] <- paste0('\"', x$object[is_string] , '\"')
  
  ## URIs should be <> instead
  x$object <- gsub("(^\\w+:\\w.*$)", "<\\1>", x$object)
  
  ## assumes datatype is not empty (e.g. string)
  x$object[needs_type] = paste0('\"', x$object[needs_type], 
                                '\"^^<', x$datatype[needs_type], ">")
  
  
  ## quads needs a graph column
  x$graph = "."
  
  ## write table is a little slow, ~ 1s on 800K triples
  
  ## drop datatype
  x <- x[c("subject", "predicate", "object", "graph")]       
  write.table(x, loc, col.names = FALSE, quote = FALSE, row.names = FALSE)     
  
  ## And parse text file.  Way faster than adding row by row!
  ## but still about 8 s on 800K triples, all in the C layer
  rdf <- rdf_parse(loc, "nquads")
  unlink(loc)
  
  
  rdf
}





## tidy data to rdf
add_df_to_rdf <- function(df, key = NULL, base_uri = NULL, loc = tempfile()){
  
  ## FIXME: Iterate over data.frame, but do not gather!  
  ## Just treat key as subject, column name as predicate, cell as object

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
