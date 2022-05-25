#' Perform a SPARQL Query
#'
#' @param rdf an rdf object (e.g. from \code{\link{rdf_parse}})
#' @param query a SPARQL query, as text string
#' @param data.frame logical, should the results be returned as a data.frame? 
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
#' @importMethodsFrom redland executeQuery
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
  
  # ... defaults are: base_uri=NULL, query_language="sparql", query_uri=NULL
  
  queryResult <- redland::executeQuery(queryObj, rdf$model)
  out <- getResults(queryResult)
  redland::freeQueryResults(queryResult)
  redland::freeQuery(queryObj)
  
  out
}

## Notes
## readr does a pretty good job guessing types returned from sparql
## character, numeric, integer, Dates, POSIXct work fine
## logicals are denoted `true` and `false`, which readr mistakes for characters

## Redland only exports the getNextResult parser, which is extremely slow on large returns

#' @importFrom readr read_csv
#' @importFrom redland librdf_query_results_to_string2
getResults <- function(queryResult, format = "csv", ...){
  mimetype <- switch(format,
                     "csv" = "text/csv; charset=utf-8",
                     NULL)
  readr::read_csv(I(redland::librdf_query_results_to_string2(
                            queryResult@librdf_query_results, 
                            format, mimetype, NULL, NULL)),
                  progress = FALSE, show_col_types = FALSE,
                  ...)
}

