
#' Initialize an `rdf` Object
#'
#' @param path where should local database to store RDF triples be created.
#' Default NULL will store triples in memory and should be best for most use cases.
#' Large databases should give a path on disk. Requires redland package to be 
#' built with support for the Berkeley DB (libdb-dev on Ubuntu, berkeley-db on homebrew).
#'
#' @return an rdf object
#' @details an rdf Object is a list of class 'rdf', consisting of
#' three pointers to external C objects managed by the redland library.
#' These are the `world` object: basically a top-level pointer for
#' all RDF models, and a `model` object: a collection of RDF statements,
#' and a `storage` object, indicating how these statements are stored.
#' `rdflib` defaults to an in-memory hash-based 
#' storage structure at this time. The primary purpose of the `rdf`
#' object is to abstract these low-level details away from the user.
#' Typical use will be simply to initialize a container to which
#' the user would manually add triples using \code{\link{rdf_add}}.
#'
#'
#' @importClassesFrom redland World Model Storage
#' @importMethodsFrom redland freeWorld freeModel freeStorage
#' @importFrom utils capture.output
#' @export
#'
#' @examples
#' x <- rdf()
#' 
rdf <- function(path = NULL){
  world <- new("World")
  
  ## Handle storage type
  if(is.character(path)){
    if(has_bdb()){
      ## Store in Berkeley DB
      options <- paste0("new='yes',hash-type='bdb',dir='", path, "'") 
    } else {
      warning("BDB driver not found. Falling back on in-memory storage")
      options <- "hash-type='memory'"
    }
  } else { ## Store in memory
   options <- "hash-type='memory'"
  }
  storage <- new("Storage", world, "hashes", name = "rdflib", 
                 options = options)
  
  
  model <- new("Model", world = world, storage, options = "")
  structure(list(world = world, model = model, storage = storage),
            class = "rdf")
}

#' Free Memory Associated with RDF object
#' 
#' @param rdf an rdf object
#' @details Free all pointers associated with an rdf object. 
#' Frees memory associated with the storage, world, and model
#' objects. After this a user should remove the rdf object 
#' from the environment as well with `rm`, since attempting
#' to reference an object after it has been removed can crash
#' R!
#' @export
#' @examples 
#' rdf <- rdf()
#' rdf_free(rdf)
#' rm(rdf)
rdf_free <- function(rdf){
  redland::freeModel(rdf$model)
  redland::freeStorage(rdf$storage)
  redland::freeWorld(rdf$world)
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


#' Serialize an RDF Document
#'
#' @inheritParams rdf_parse
#' @inheritParams rdf_query
#' @param doc file path to write out to
#' @param namespace string giving the namespace to set
#' @param prefix string giving the prefix associated with the namespace
#'
#' @return rdf_serialize returns the output file path `doc` invisibly.
#'   This makes it easier to use rdf_serialize in pipe chains with
#'   \code{\link{rdf_parse}}.
#' @importFrom methods new
#' @importClassesFrom redland Serializer
#' @importMethodsFrom redland setNameSpace serializeToFile freeSerializer
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
                          doc,
                          format = c("rdfxml",
                                     "nquads",
                                     "ntriples",
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
    new("Serializer", rdf$world,
        name = format, mimeType = mimetype)

  if(!is.null(namespace)){
    redland::setNameSpace(serializer,
                 rdf$world,
                 namespace = namespace,
                 prefix = prefix)
  }
 
  status <-
    redland::serializeToFile(serializer, rdf$world, rdf$model, doc)
  
  if(jsonld_output){
    txt <- paste(readLines(doc), collapse = "\n")
    if(length(txt) > 0){ ## don't attempt to write empty file into json
      json <- jsonld::jsonld_from_rdf(txt)
      writeLines(json, doc)
    }
  }
  
  redland::freeSerializer(serializer)
  invisible(doc)
}



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


#' Add RDF Triples
#'
#' add a triple (subject, predicate, object) to the RDF graph
#'
#' @param rdf an rdf object
#' @param subject character string containing the subject
#' @param predicate character string containing the predicate
#' @param object character string containing the object
#' @param subjectType the Node type of the subject, i.e. "blank", "uri"
#' @param objectType the Node type of the object, i.e. "blank", "uri", "literal"
#' @param datatype_uri the datatype URI to associate with a object literal value
#'
#' @return the updated RDF graph (rdf object).
#' @details Since the rdf object simply contains external pointers
#' to the model object in C code, note that the input object is modified
#' directly.  
#' @importClassesFrom redland Statement
#' @importMethodsFrom redland addStatement freeStatement
#' @export
#'
#' @examples
#' rdf <- rdf()
#' rdf_add(rdf, 
#'     subject="http://www.dajobe.org/",
#'     predicate="http://purl.org/dc/elements/1.1/language",
#'     object="en")
#'     
#' ## blank nodes should be declared as such:
#' rdf_add(rdf, "", "http://schema.org/jobTitle", "Professor", 
#'         subjectType = "blank")   
#'
rdf_add <- function(rdf, subject, predicate, object, 
                    subjectType = as.character(NA), 
                    objectType = as.character(NA), 
                    datatype_uri = as.character(NA)){
  stmt <- new("Statement", world = rdf$world, 
              subject, predicate, as.character(object),
              subjectType, objectType, datatype_uri)
  redland::addStatement(rdf$model, stmt)
  
  redland::freeStatement(stmt)
  ## rdf object is a list of pointers, modified in pass-by-reference
  invisible(rdf)
}

#' Concatenate rdf Objects
#' Note: this assumes absolute URIs for subject and predicate
#' @method c rdf
#' @export
#' @param ... objects to be concatenated
c.rdf <- function(...){
  quads <- lapply(list(...), format)
  txt <- paste(quads, collapse = "\n")
  rdf_parse(txt, "nquads")
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


