#' Serialize an RDF Document
#'
#' @inheritParams rdf_parse
#' @inheritParams rdf_query
#' @param doc file path to write out to. If null, will write to character.
#' @param namespace a named character containing the prefix to namespace bindings. \code{names(namespace)} are the prefixes, whereas \code{namespace} are the namespaces
#' @param ... additional arguments to \code{redland::serializeToFile}
#' @return rdf_serialize returns the output file path `doc` invisibly.
#'   This makes it easier to use rdf_serialize in pipe chains with
#'   \code{\link{rdf_parse}}.
#' @importFrom methods new
#' @importClassesFrom redland Serializer
#' @importMethodsFrom redland setNameSpace serializeToFile freeSerializer
#' @importFrom jsonld jsonld_compact
#'
#' @export
#' @examples
#' infile <- system.file("extdata", "dc.rdf", package="redland")
#' out <- tempfile("file", fileext = ".rdf")
#'
#' some_rdf <- rdf_parse(infile)
#' rdf_add(some_rdf, subject = "http://www.dajobe.org/dave-beckett", predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", object = "http://xmlns.com/foaf/0.1/Person")
#' rdf_serialize(some_rdf, out)
#'
#' ## With a namespace
#' rdf_serialize(some_rdf,
#'           out,
#'           format = "turtle",
#'           namespace = c(dc = "http://purl.org/dc/elements/1.1/", foaf = "http://xmlns.com/foaf/0.1/")
#'           )
#'
#' readLines(out)
rdf_serialize <- function(rdf,
                          doc = NULL,
                          format = c("guess",
                                     "rdfxml",
                                     "nquads",
                                     "ntriples",
                                     "turtle",
                                     "jsonld"),
                          namespace = NULL,
                          base = getOption("rdf_base_uri", "localhost://"),
                          ...){
  
  format <- match.arg(format)
  if(format == "guess" & !is.null(doc)){
    format <- guess_format(doc)
  }
  
  
  ## redlands doesn't support jsonld. So write as nquads and then transform
  jsonld_output <- format == "jsonld"
  if(jsonld_output){
    format <- "nquads"
  }
  
  mimetype <- rdf_mimetypes[format]
  
  serializer <-
    new("Serializer", rdf$world,
        name = format, mimeType = mimetype)
  
  if(!is.null(namespace) && is.character(namespace) && length(namespace) >= 1){
    ix = 1:length(namespace)
    for (i in ix) {
      redland::setNameSpace(serializer,
                            rdf$world,
                            namespace = namespace[i],
                            prefix = names(namespace)[i]) 
    }
  }
  
  if(is.null(doc)){
    doc <- redland::serializeToCharacter(serializer, rdf$world, 
                                         rdf$model, baseUri = base, ...)
  } else {
    status <-
      redland::serializeToFile(serializer, rdf$world, 
                               rdf$model, doc, baseUri = base, ...)
  }
  
  if(jsonld_output){
    txt <- paste(readLines(doc), collapse = "\n")
    if(length(txt) > 0){ ## don't attempt to write empty file into json
      json <- jsonld::jsonld_from_rdf(txt,
                                      options = list(
                                        base = base,
                                        format = "application/nquads"))
      compact_json <- jsonld_compact(json, "{}")
      writeLines(compact_json, doc)
    }
  }
  
  redland::freeSerializer(serializer)
  invisible(doc)
}
