
#' SPARQL query
#'
#' @param rdf an rdf object (e.g. from \code{\link{parse}})
#' @param query a SPARQL query, as text string
#' @param ... additional arguments to a redland initialize-Query
#'
#' @return a list of all query results
#' @export
#' @import methods
#' @importClassesFrom redland Query
#' @importMethodsFrom redland executeQuery getNextResult
#'
#' @examples
#' file <- system.file("extdata", "dc.rdf", package="redland")
#'
#' sparql <-
#' 'PREFIX dc: <http://purl.org/dc/elements/1.1/>
#'  SELECT ?a ?c
#'  WHERE { ?a dc:creator ?c . }'
#'
#' rdf <- parse(file)
#' query.rdf(rdf, sparql)
#'
query.rdf <- function(rdf, query, ...){
  queryObj <- new("Query", rdf$world, query, base_uri=NULL,
                  query_language="sparql", query_uri=NULL)
  queryResult <- redland::executeQuery(queryObj, rdf$model)
  out <- list()
  result <- redland::getNextResult(queryResult)
  out <- c(out, result)
  # while(!is.null(result)){
  #   result <- redland::getNextResult(queryResult)
  #  out <- c(out, result)
  # }
  out
}

#' Parse RDF files
#'
#' @param file path to the rdf file to serialize
#' @param format rdf serialization format of the file,
#' one of "rdfxml", "nquads", "ntriples", "trig", or "turtle"
#' @param ... additional parameters (not implemented)
#'
#' @return an rdf S3 object, containing the redland world
#'  and model objects
#' @importClassesFrom redland World Storage Model Parser
#' @importMethodsFrom redland parseFileIntoModel
#' @export
#'
#' @examples
#' file <- system.file("extdata", "dc.rdf", package="redland")
#' rdf <- parse(file)
#'
parse <- function(file,
                  format = c("rdfxml",
                             "nquads",
                             "ntriples",
                             "trig",
                             "turtle"),
                  ...){
  format <- match.arg(format)
  world <- new("World")
  storage <- new("Storage", world, "hashes", name="", options="hash-type='memory'")
  model <- new("Model", world = world, storage, options="")

  mimetype <- rdf_mimetypes[format]

  parser <- new("Parser", world, name = format, mimeType = mimetype)
  redland::parseFileIntoModel(parser, world, file, model)

  structure(list(world = world, model = model),
            class = "rdf")

}




#' Serialize RDF files
#'
#' @inheritParams parse
#' @inheritParams query.rdf
#' @param namespace string giving the namespace to set
#' @param prefix string giving the prefix associated with the namespace
#'
#' @return an integer containing the return status where non
#'  zero indicates an error occured during serialization
#' @importClassesFrom redland Serializer
#' @importMethodsFrom redland setNameSpace serializeToFile
#' @export
#'
#' @examples
#' infile <- system.file("extdata", "dc.rdf", package="redland")
#' out <- tempfile("file", fileext = ".rdf")
#'
#' rdf <- parse(infile)
#' serialize.rdf(rdf, out)
#'
#' ## With a namespace
#' serialize.rdf(rdf,
#'           out,
#'           namespace = "http://purl.org/dc/elements/1.1/",
#'           prefix = "dc")
#'
serialize.rdf <- function(rdf,
                          file,
                          format = c("rdfxml",
                                     "nquads",
                                     "ntriples",
                                     "trig",
                                     "turtle"),
                          namespace = NULL,
                          prefix = NULL,
                          ...){

  format <- match.arg(format)
  mimetype <- rdf_mimetypes[format]

  serializer <-
    new("Serializer", rdf$world,
        name = format, mimeType = mimetype)

  if(!is.null(namespace)){
    redland::setNameSpace(serializer,
                 rdf$world,
                 namespace = namespace,
                 prefix = prefix)
  }
  redland::serializeToFile(serializer, rdf$world, rdf$model, file)
}


#serialize <- function(x, out, ...) UseMethod("serialize")
#query <- function(x, query, ...) UseMethod("query")

# Must match parser name & q 1.0 mimetype listed at:
# http://librdf.org/raptor/api/raptor-formats-types-by-parser.html
rdf_mimetypes <- c("nquads" = "text/x-nquads",
                   "ntriples" = "application/n-triples",
                   "rdfxml" = "application/rdf+xml",
                   "trig" = "application/x-trig",
                   "turtle" = "application/turtle")

# application/x-turtle & text/turtle also ok
