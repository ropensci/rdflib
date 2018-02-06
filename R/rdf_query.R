#' Perform a SPARQL Query
#'
#' @param rdf an rdf object (e.g. from \code{\link{rdf_parse}})
#' @param query a SPARQL query, as text string
#' @param data.frame logical, should the resutls be returned as a data.frame? 
#' @param ... additional arguments to a redland initialize-Query
#'
#' @return a data.frame of all query results (default.)  Columns will 
#' be named according to variable names in the SPARQL query. Returned
#' object values will be coerced to match the corresponding R type 
#' to any associated datatype URI, if provided. If a column would 
#' result in mixed classes (e.g. strings and numerics), all types
#' in the column will be coerced to character strings. If `data.frame`
#' is false, results will be returned as a list with each element 
#' typed by its data URI.   
#' 
#'     
#' @importClassesFrom redland Query
#' @importMethodsFrom redland executeQuery getNextResult
#' @importMethodsFrom redland freeQuery freeQueryResults
#' @export
#' @examples
#' doc <- system.file("extdata", "dc.rdf", package="redland")
#'
#' sparql <-
#' 'PREFIX dc: <http://purl.org/dc/elements/1.1/>
#'  SELECT ?a ?c
#'  WHERE { ?a dc:creator ?c . }'
#'
#' rdf <- rdf_parse(doc)
#' rdf_query(rdf, sparql)
#'
rdf_query <- function(rdf, query, data.frame = TRUE, ...){
  queryObj <- new("Query", rdf$world, query, ...)
  # ... defaults are: base_uri=NULL, query_language="sparql", query_uri=NULL)
  queryResult <- redland::executeQuery(queryObj, rdf$model)

  out <- list()
  result <- redland::getNextResult(queryResult)
  out <- c(out, result)
  while(!is.null(result)){
    result <- redland::getNextResult(queryResult)
    out <- c(out, result)
    
  }
  redland::freeQueryResults(queryResult)
  redland::freeQuery(queryObj)
  
  if(data.frame){
    out <- rectangularize_query_results(out)
  } else {
    ## group by query variable 
    vars <- unique(names(out))
    out <- lapply(vars, function(v){ 
      contents <- as.character(out[names(out) == v ])
      type_by_datauri(contents)  
      })
  names(out) <- vars
  }
  out
}


rectangularize_query_results <- function(out){
  vars <- unique(names(out))
  
  X <- lapply(vars, function(v){ 
    contents <- as.character(out[names(out) == v ])
    values <- type_by_datauri(contents)
    
    ## use "character" if mixed type column
    types <- vapply(values, function(x) class(x)[[1]], character(1))
    u <- unique(types)
    if(length(u) == 1){
      values <- unlist(values)
      if(u %in% c("Date", "POSIXct"))
      class(values) <- unique(types) # Restore date class
    } else {
      values <- vapply(values, as.character, character(1))
    }
    values
  })
  
  names(X) <- vars
  ## Or we could use tibble::as_data_frame for list columns w/ mixed type..
  as.data.frame(X, stringsAsFactors=FALSE)
}