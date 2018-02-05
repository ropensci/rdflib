#' Perform a SPARQL Query
#'
#' @param rdf an rdf object (e.g. from \code{\link{rdf_parse}})
#' @param query a SPARQL query, as text string
#' @param ... additional arguments to a redland initialize-Query
#'
#' @return a data.frame of all query results
#' @importClassesFrom redland Query
#' @importMethodsFrom redland executeQuery getNextResult freeQuery freeQueryResults
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
rdf_query <- function(rdf, query, ...){
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
  rectangularize_query_results(out)
}


rectangularize_query_results <- function(out){
  vars <- unique(names(out))
  X <- lapply(vars, function(v){ 
    contents <- as.character(out[names(out) == v ])
    type_by_datauri(contents)
  })
  names(X) <- vars
  as.data.frame(X, stringsAsFactors=FALSE)
}