
#' rdf constructor
#'
#' @return an rdf graph object
#' @export
#'
#' @examples
#' x <- rdf()
#' 
rdf <- function(){
  world <- new("World")
  storage <- new("Storage", world, "hashes", name="", options="hash-type='memory'")
  model <- new("Model", world = world, storage, options="")
  structure(list(world = world, model = model),
            class = "rdf")
}

#' Parse RDF files
#'
#' @aliases parse_rdf parse.rdf
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
#' rdf <- parse_rdf(file)
#'
parse_rdf <- function(file,
                     format = c("rdfxml",
                                "nquads",
                                "ntriples",
                                "trig",
                                "turtle"),
                  ...){
  format <- match.arg(format)
  x <- rdf()
  mimetype <- rdf_mimetypes[format]
  parser <- new("Parser", x$world, name = format, mimeType = mimetype)
  redland::parseFileIntoModel(parser, x$world, file, x$model)

  x
}



#' Serialize RDF files
#'
#' @inheritParams parse_rdf
#' @inheritParams query
#' @param namespace string giving the namespace to set
#' @param prefix string giving the prefix associated with the namespace
#'
#' @return an integer containing the return status where non
#'  zero indicates an error occured during serialization
#' @importClassesFrom redland Serializer
#' @importMethodsFrom redland setNameSpace serializeToFile
#'
#' @export
#' @examples
#' infile <- system.file("extdata", "dc.rdf", package="redland")
#' out <- tempfile("file", fileext = ".rdf")
#'
#' rdf <- parse_rdf(infile)
#' serialize(rdf, out)
#'
#' ## With a namespace
#' serialize(rdf,
#'           out,
#'           namespace = "http://purl.org/dc/elements/1.1/",
#'           prefix = "dc")
#'
serialize <- function(x, file, format, namespace, prefix, ...) UseMethod("serialize")

#' @export
serialize.rdf <- function(x,
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
    new("Serializer", x$world,
        name = format, mimeType = mimetype)

  if(!is.null(namespace)){
    redland::setNameSpace(serializer,
                 x$world,
                 namespace = namespace,
                 prefix = prefix)
  }
  invisible(
  redland::serializeToFile(serializer, x$world, x$model, file))
}



#' SPARQL query
#'
#' @param x an rdf object (e.g. from \code{\link{parse}})
#' @param query a SPARQL query, as text string
#' @param ... additional arguments to a redland initialize-Query
#'
#' @return a list of all query results
#' @import methods
#' @importClassesFrom redland Query
#' @importMethodsFrom redland executeQuery getNextResult
#' @export
#' @examples
#' file <- system.file("extdata", "dc.rdf", package="redland")
#'
#' sparql <-
#' 'PREFIX dc: <http://purl.org/dc/elements/1.1/>
#'  SELECT ?a ?c
#'  WHERE { ?a dc:creator ?c . }'
#'
#' rdf <- parse_rdf(file)
#' query(rdf, sparql)
#'
query <- function(x, query, ...) UseMethod("query")

#' @export
query.rdf <- function(x, query, ...){
  queryObj <- new("Query", x$world, query, base_uri=NULL,
                  query_language="sparql", query_uri=NULL)
  queryResult <- redland::executeQuery(queryObj, x$model)
  out <- list()
  result <- redland::getNextResult(queryResult)
  out <- c(out, result)
  while(!is.null(result)){
     result <- redland::getNextResult(queryResult)
    out <- c(out, result)
  
    }
  out
}


#' add a triple (subject, predicate, object) to the rdf graph
#'
#' @param x an rdf graph object
#' @param subject character string containing the subject
#' @param predicate character string containing the predicate
#' @param object character string containing the object
#' @param subjectType the Node type of the subject, i.e. "blank", "uri"
#' @param objectType the Node type of the object, i.e. "blank", "uri"
#' @param datatype_uri the datatype URI to associate with a object literal value
#'
#' @return the rdf graph object.
#' @details Since the rdf graph object simply contains external pointers
#' to the model object in C code, note that the input object is modified
#' directly.  
#' @importClassesFrom redland Statement
#' @importMethodsFrom redland addStatement
#' @export
#'
#' @examples
#' x <- rdf()
#' add(x, 
#'     subject="http://www.dajobe.org/",
#'     predicate="http://purl.org/dc/elements/1.1/language",
#'     object="en")
#'
add <- function(x,
                subject,
                predicate,
                object,
                subjectType = as.character(NA),
                objectType = as.character(NA),
                datatype_uri = as.character(NA)) {
  UseMethod("add")
}

#' @export  
add.rdf <- function(x, subject, predicate, object, 
                    subjectType = as.character(NA), 
                    objectType = as.character(NA), 
                    datatype_uri = as.character(NA)){
  stmt <- new("Statement", world = x$world, 
              subject, predicate, object,
              subjectType, objectType, datatype_uri)
  addStatement(x$model, stmt)
  
  ## rdf object is a list of pointers, modified in pass-by-reference
  invisible(x)
}

# Must match parser name & q 1.0 mimetype listed at:
# http://librdf.org/raptor/api/raptor-formats-types-by-parser.html
rdf_mimetypes <- c("nquads" = "text/x-nquads",
                   "ntriples" = "application/n-triples",
                   "rdfxml" = "application/rdf+xml",
                   "trig" = "application/x-trig",
                   "turtle" = "application/turtle")

# application/x-turtle & text/turtle also ok
