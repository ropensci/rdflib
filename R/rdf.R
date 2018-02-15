#' Initialize an `rdf` Object
#'
#' @param path where should local database to store RDF triples be created, if
#' configured for disk-based storage; see details.
#'
#' @return an rdf object
#' @details an rdf Object is a list of class 'rdf', consisting of
#' three pointers to external C objects managed by the redland library.
#' These are the `world` object: basically a top-level pointer for
#' all RDF models, and a `model` object: a collection of RDF statements,
#' and a `storage` object, indicating how these statements are stored.
#' 
#' `rdflib` defaults to an in-memory hash-based storage structure. 
#' which should be best for most use cases. For very large triplestores,
#' disk-based storage will be necessary.  Enable this by setting the option
#' `options(rdflib_storage = "BDB")` before calling `rdf()` to use disk-based
#' storage. Specify a path with the optional `path` argument, default uses
#' the current working directory. Disk-based storage requires redland package
#' to be installed from source with support for the Berkeley DB 
#' (libdb-dev on Ubuntu, berkeley-db on homebrew), otherwise this will
#' fall back to in-memory storage with a warning. Check for working BDB
#' support with the function `rdf_has_bdb()`.  
#' 
#' 
#' Typical use will be simply to initialize a container to which
#' the user would manually add triples using \code{\link{rdf_add}}.
#'
#' Overview of configuration options
#' rdflib_storage:
#'   - NULL or "memory" for in memory storage. (default)
#'   - "BDB" for disk-based storage in Berkeley Database
#' rdflib_print_format: 
#'   - NULL or "nquads" (default)
#'   - any valid serializer name: e.g. "rdfxml", "jsonld", "turtle",  "ntriples"
#' rdflib_base_uri:
#'   - Default base URI to use (when serializing JSON-LD only at this time)
#'     default is "localhost://"
#'
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
rdf <- function(path = "."){
  world <- new("World")
  
  ## Handle storage type
  if(getOption("rdflib_storage", "memory") == "BDB"){
    if(rdf_has_bdb()){
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


