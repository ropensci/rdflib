#' Parse RDF Files
#'
#' @param doc path, URL, or literal string of the rdf document to parse
#' @param format rdf serialization format of the doc,
#' one of "rdfxml", "nquads", "ntriples", "turtle"
#' or "jsonld"
#' @param ... additional parameters (not implemented)
#'
#' @return an rdf object, containing the redland world
#'  and model objects
#' @importClassesFrom redland World Storage Model Parser
#' @importMethodsFrom redland parseFileIntoModel freeParser
#' @importFrom jsonld jsonld_to_rdf
#' @export
#'
#' @examples
#' doc <- system.file("extdata", "dc.rdf", package="redland")
#' rdf <- rdf_parse(doc)
#'
rdf_parse <- function(doc,
                      format = c("rdfxml",
                                 "nquads",
                                 "ntriples",
                                 "turtle",
                                 "jsonld"),
                      ...){
  format <- match.arg(format)
  
  # convert string input or url to local file
  doc <- text_or_url_to_doc(doc)
  
  ## redlands doesn't support jsonld. So rewrite as nquads using jsonld package
  ## We use tmp to avoid altering input doc, since parsing a local file should
  ## be a read-only task!
  if(format == "jsonld"){
    tmp <- tempfile()
    tmp <- add_base_uri(doc, tmp)
    rdf <- jsonld::jsonld_to_rdf(tmp)
    writeLines(rdf, tmp)
    format <- "nquads"
    doc <- tmp
  }
  
  rdf <- rdf()
  mimetype <- unname(rdf_mimetypes[format])
  parser <- new("Parser", rdf$world, name = format, mimeType = mimetype)
  redland::parseFileIntoModel(parser, rdf$world, doc, rdf$model)
  redland::freeParser(parser)
  
  rdf
}

# Whenever we convert JSON-LD to RDF we should set a @base if not set.
# https://json-ld.org/playground does this (with it's own url) 
# but jsonld R package does not.
# For details, see https://github.com/cboettig/rdflib/issues/5
#
#' @importFrom jsonld jsonld_expand jsonld_compact
add_base_uri <- function(doc, tmp = tempfile()){
  
  ## Cannot assume it has context, may already be expanded 
  ## (e.g. from rdf_serialize).  Expanding will also make 
  ## any preset @base context take precedence 
  expanded <- jsonld::jsonld_expand(doc)
  base <- getOption("rdflib_base_uri", "localhost://")
  context <- paste0('{"@base": "', base, '"}')
  compact <- jsonld::jsonld_compact(expanded, context)
  writeLines(compact, tmp)
  tmp
}


# rdf functions like working with local files
# this helper function allows us to also use URLs or strings
#' @importFrom utils download.file
text_or_url_to_doc <- function(x, tmp = tempfile()){
  if(file.exists(x)){
    return(x) 
  } else if(grepl("^https?://", x)) {
    utils::download.file(x, tmp, quiet = TRUE)
    return(tmp)
  } else {
    writeLines(x, tmp)
    return(tmp)
  }
}
