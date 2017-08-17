
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

#' @export
format.rdf <- function(x,
                       format = getOption("rdf_print_format", "nquads"),
                       ...){
  tmp <- tempfile()
  rdf_serialize(x, 
                tmp,
                format = format)
  txt <- readLines(tmp)
  unlink(tmp)
  txt
}

#' @export
print.rdf <- function(x, ...){
  cat(format.rdf(x), sep = "\n")
}


#' Parse RDF files
#'
#' @param doc path to the rdf doc to parse
#' @param format rdf serialization format of the doc,
#' one of "rdfxml", "nquads", "ntriples", "trig", "turtle"
#' or "jsonld"
#' @param ... additional parameters (not implemented)
#'
#' @return an rdf S3 object, containing the redland world
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
  if(format == "jsonld"){
    tmp <- tempfile()
    writeLines(jsonld::jsonld_to_rdf(doc), tmp)
    doc <- tmp
    format <- "nquads"
  }
  
  x <- rdf()
  mimetype <- unname(rdf_mimetypes[format])
  parser <- new("Parser", x$world, name = format, mimeType = mimetype)
  redland::parseFileIntoModel(parser, x$world, doc, x$model)

  x
}



#' Serialize RDF docs
#'
#' @inheritParams rdf_parse
#' @inheritParams rdf_query
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
  
  
  ## redlands doesn't support jsonld. So write as nquads and then trnasform
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
    txt <- readLines(doc)
    if(length(txt) > 0){ ## don't attempt to write empty file into json
      json <- jsonld::jsonld_from_rdf(txt)
      writeLines(json, doc)
    }
  }
  
  invisible(status)
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