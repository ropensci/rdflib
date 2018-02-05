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
  ## Fix encoding on nquads, ntriples 
  txt <- utf8me(paste(readLines(tmp), collapse = "\n"))
  unlink(tmp)
  txt
}

#' @export
print.rdf <- function(x, ...){
  cat(format.rdf(x), sep = "\n")
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


