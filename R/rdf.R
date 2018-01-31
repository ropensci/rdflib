
#' rdf constructor
#'
#' @return an rdf object
#' @export
#'
#' @examples
#' x <- rdf()
#' 
rdf <- function(){
  world <- new("World")
  storage <- new("Storage", world, "hashes", name = "", 
                 options = "hash-type='memory'")
  model <- new("Model", world = world, storage, options = "")
  structure(list(world = world, model = model),
            class = "rdf")
}

#' @export
format.rdf <- function(x,
                       format = getOption("rdf_print_format", "nquads"),
                       ...){
  tmp <- tempfile()
  rdf_serialize(x, 
                tmp,
                format = format)
  txt <- paste(readLines(tmp), collapse = "\n")
  unlink(tmp)
  txt
}

#' @export
print.rdf <- function(x, ...){
  cat(format.rdf(x), sep = "\n")
}


#' Parse RDF files
#'
#' @param doc path, URL, or literal string of the rdf document to parse
#' @param format rdf serialization format of the doc,
#' one of "rdfxml", "nquads", "ntriples", "trig", "turtle"
#' or "jsonld"
#' @param ... additional parameters (not implemented)
#'
#' @return an rdf object, containing the redland world
#'  and model objects
#' @importClassesFrom redland World Storage Model Parser
#' @importMethodsFrom redland parseFileIntoModel
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
                                "trig",
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
  
  x <- rdf()
  mimetype <- unname(rdf_mimetypes[format])
  parser <- new("Parser", x$world, name = format, mimeType = mimetype)
  redland::parseFileIntoModel(parser, x$world, doc, x$model)

  x
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


#' Serialize an RDF Document
#'
#' @inheritParams rdf_parse
#' @inheritParams rdf_query
#' @param doc file path to write out to
#' @param namespace string giving the namespace to set
#' @param prefix string giving the prefix associated with the namespace
#'
#' @return rdf_serialize returns the output file path `doc` invisibly.
#'   This makes it easier to use rdf_serialize in pipe chains with rdf_parse.
#' @importFrom methods new
#' @importClassesFrom redland Serializer
#' @importMethodsFrom redland setNameSpace serializeToFile
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
rdf_serialize <- function(x,
                          doc,
                          format = c("rdfxml",
                                     "nquads",
                                     "ntriples",
                                     "trig",
                                     "turtle",
                                     "jsonld"),
                          namespace = NULL,
                          prefix = NULL,
                          ...){

  format <- match.arg(format)
  
  
  ## redlands doesn't support jsonld. So write as nquads and then transform
  jsonld_output <- format == "jsonld"
  if(jsonld_output){
    format <- "nquads"
  }
  
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
 
  status <-
    redland::serializeToFile(serializer, x$world, x$model, doc)
  
  if(jsonld_output){
    txt <- paste(readLines(doc), collapse = "\n")
    if(length(txt) > 0){ ## don't attempt to write empty file into json
      json <- jsonld::jsonld_from_rdf(txt)
      writeLines(json, doc)
    }
  }
  
  invisible(x)
}



#' SPARQL query
#'
#' @param x an rdf object (e.g. from \code{\link{parse}})
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
rdf_query <- function(x, query, ...){
  queryObj <- new("Query", x$world, query, ...)
    # ... defaults are: base_uri=NULL, query_language="sparql", query_uri=NULL)
  queryResult <- redland::executeQuery(queryObj, x$model)
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

## FIXME: coerce data type based on Type-string?
rectangularize_query_results <- function(out){
  vars <- unique(names(out))
  X <- lapply(vars, function(v) 
    ## Strip ^^TYPE typing
    gsub('\"(([^\\^])+)\"\\^*.*', 
         "\\1", 
         as.character(out[names(out) == v ])))
  names(X) <- vars
  as.data.frame(X, stringsAsFactors=FALSE)
}


#' add a triple (subject, predicate, object) to the RDF graph
#'
#' @param x an rdf object
#' @param subject character string containing the subject
#' @param predicate character string containing the predicate
#' @param object character string containing the object
#' @param subjectType the Node type of the subject, i.e. "blank", "uri"
#' @param objectType the Node type of the object, i.e. "blank", "uri"
#' @param datatype_uri the datatype URI to associate with a object literal value
#'
#' @return the updated RDF graph (rdf object).
#' @details Since the rdf object simply contains external pointers
#' to the model object in C code, note that the input object is modified
#' directly.  
#' @importClassesFrom redland Statement
#' @importMethodsFrom redland addStatement
#' @export
#'
#' @examples
#' x <- rdf()
#' rdf_add(x, 
#'     subject="http://www.dajobe.org/",
#'     predicate="http://purl.org/dc/elements/1.1/language",
#'     object="en")
#'
rdf_add <- function(x, subject, predicate, object, 
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



# rdf functions like working with local files
# this helper function allows us to also use URLs or strings
#' @importFrom utils download.file
text_or_url_to_doc <- function(x, tmp = tempfile()){
  if(file.exists(x)){
   return(x) 
  } else if(grepl("^https?://", x)) {
    utils::download.file(x, tmp)
    return(tmp)
  } else {
    writeLines(x, tmp)
    return(tmp)
  }
}

#' rdflib: Tools to Manipulate and Query Semantic Data
#'
#' The Resource Description Framework, or RDF is a widely used
#' data representation model that forms the cornerstone of the 
#' Semantic Web. 'RDF' represents data as a graph rather than 
#' the familiar data table or rectangle of relational databases.
#'
#' 
#' It has three main goals:
#'
#' \itemize{
#' \item Easily read, write, and convert between all major RDF serialization formats
#' \item Support SPARQL queries to extract data from an RDF graph into a data.frame
#' \item Support JSON-LD format as a first-class citizen in RDF manipulations
#' }
#'
#' For more information, see the Wikipedia pages for RDF, SPARQL, and JSON-LD:
#' 
#' #' \itemize{
#' \item \url{https://en.wikipedia.org/wiki/Resource_Description_Framework}
#' \item \url{https://en.wikipedia.org/wiki/SPARQL}
#' \item \url{https://en.wikipedia.org/wiki/JSON-LD}
#' }
#'
#' To learn more about rdflib, start with the vignettes:
#' `browseVignettes(package = "rdflib")`
#'
#'
"_PACKAGE"


