#' Parse RDF Files
#'
#' @param doc path, URL, or literal string of the rdf document to parse
#' @param format rdf serialization format of the doc,
#' one of "rdfxml", "nquads", "ntriples", "turtle"
#' or "jsonld". If not provided, will try to guess based
#' on file extension and fall back on rdfxml.
#' @param rdf an existing rdf triplestore to extend with triples from
#' the parsed file.  Default will create a new rdf object.
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
                      format = c("guess",
                                 "rdfxml",
                                 "nquads",
                                 "ntriples",
                                 "turtle",
                                 "jsonld"),
                      rdf = NULL,
                      ...){
  
  format <- match.arg(format)
  if(format == "guess"){
    format <- guess_format(doc)
  }
  
  ## if we get a string as input, we'll store it in tmp file here
  ## which we can later be sure to clean up.
  tmp_string <- tempfile()
  ## if we get json-ld, we'll need a temp location to serialize that too:
  tmp_json <- tempfile()
  
  # convert string input or url to local file
  doc <- text_or_url_to_doc(doc, tmp_string)
  
  ## redlands doesn't support jsonld. So rewrite as nquads using jsonld package
  ## We use tmp to avoid altering input doc, since parsing a local file should
  ## be a read-only task!
  if(format == "jsonld"){
    x <- jsonld::jsonld_to_rdf(doc, 
                               options = 
           list(base = getOption("rdflib_base_uri", "localhost://"),
                format = "application/nquads"))
    writeLines(x, tmp_json)
    format <- "nquads"
    doc <- tmp_json
  }
  
  if(is.null(rdf)){
    rdf <- rdf()
  }
    
  mimetype <- unname(rdf_mimetypes[format])
  parser <- new("Parser", rdf$world, name = format, mimeType = mimetype)
  redland::parseFileIntoModel(parser, rdf$world, doc, rdf$model)
  
  redland::freeParser(parser)
  unlink(tmp_string)
  unlink(tmp_json)  
  
  ## return rdf object (pointer)
  rdf
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
