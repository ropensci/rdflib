#' Serialize an RDF Document
#'
#' @inheritParams rdf_parse
#' @inheritParams rdf_query
#' @param doc file path to write out to. If null, will write to character.
#' @param namespace string giving the namespace to set
#' @param prefix string giving the prefix associated with the namespace
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
#' rdf <- rdf_parse(infile)
#' rdf_serialize(rdf, out)
#'
#' ## With a namespace
#' rdf_serialize(rdf,
#'           out,
#'           namespace = "http://purl.org/dc/elements/1.1/",
#'           prefix = "dc")
#'
rdf_serialize <- function(rdf,
                          doc = NULL,
                          format = c("guess",
                                     "rdfxml",
                                     "nquads",
                                     "ntriples",
                                     "turtle",
                                     "jsonld"),
                          namespace = NULL,
                          prefix = NULL,
                          base = getOption("rdflib_base_uri", as.character(NA)),
                          ...){
  
  format <- match.arg(format)
  if(format == "guess"){
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
  
  if(!is.null(namespace)){
    redland::setNameSpace(serializer,
                          rdf$world,
                          namespace = namespace,
                          prefix = prefix)
  }
  
  if(is.null(doc)){
    doc <- redland::serializeToCharacter(serializer, rdf$world, rdf$model, baseUri = base, ...)
  } else {
    status <-
      redland::serializeToFile(serializer, rdf$world, rdf$model, doc, baseUri = base, ...)
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
