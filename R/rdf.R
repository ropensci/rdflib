#' Initialize an `rdf` Object
#'
#' @param storage Storage backend to use; see details
#' @param host host address for mysql, postgres, or viruoso storage
#' @param port port for mysql (mysql storage defaults to mysql standard port, 3306)
#' or postgres (postgres storage defaults to postgres standard port, 4321)
#' @param user user name for postgres, mysql, or virtuoso
#' @param password password for postgres, mysql, or vituoso
#' @param database name of the database to be created/used
#' @param charset charset for virtuoso database, if desired
#' @param dir directory of where to write sqlite or berkeley database.
#' @param dsn Virtuoso dsn, either "Local Virtuoso" or "Remote Virtuoso"
#' @param name name for the storage object created. Default is usually fine.  
#' @param new_db logical, default FALSE. Create new database or connect to existing?
#' @param fallback logical, default TRUE. If requested storage system cannot initialize,
#' should `rdf()` fall back on memory (default) or throw an error (fallback=FALSE)?
#' @return an rdf object
#' @details an rdf Object is a list of class 'rdf', consisting of
#' three pointers to external C objects managed by the redland library.
#' These are the `world` object: basically a top-level pointer for
#' all RDF models, and a `model` object: a collection of RDF statements,
#' and a `storage` object, indicating how these statements are stored.
#' 
#' `rdflib` defaults to an in-memory hash-based storage structure. 
#' which should be best for most use cases. For very large triplestores,
#' disk-based storage will be necessary. Enabling external storage devices
#' will require additional libraries and custom compiling. See the storage
#' vignette for details.  
#'
#' @importClassesFrom redland World Model Storage
#' @importMethodsFrom redland freeWorld freeModel freeStorage
#' @importFrom utils capture.output
#' @export
#'
#' @examples
#' x <- rdf()
#' 
rdf <- function(storage = c("memory", "BDB", "sqlite", 
                            "postgres", "mysql", "virtuoso"), 
                host = NULL,
                port = NULL,
                user = NULL,
                password = NULL,
                database = NULL,
                charset = NULL,
                dir = NULL,
                dsn = "Local Virtuoso",
                name = "rdflib",
                new_db = FALSE,
                fallback = TRUE){
  
  world <- new("World")
  store <- rdf_storage(storage, world, host, port, user, password, 
                       database, charset, dir, dsn, name, new_db, fallback)
  model <- new("Model", world = world, storage = store, options = "")
  structure(list(world = world, model = model, storage = store),
            class = "rdf")
}






rdf_storage <- function(storage = c("memory", 
                                    "BDB", 
                                    "sqlite", 
                                    "postgres", 
                                    "mysql", 
                                    "virtuoso"),
                        world = NULL,
                        host = NULL,
                        port = NULL,
                        user = NULL,
                        password = NULL,
                        database = NULL,
                        charset = NULL,
                        dir = NULL,
                        dsn = "Local Virtuoso",
                        name = "rdflib",
                        new_db = FALSE,
                        fallback = TRUE,
                        check_only = FALSE){
  if(is.null(world)){
    world <- new("World")
  }
  storage <- match.arg(storage)
  
  new <- NULL
  if(new_db){
    new <- "yes"
  }
  if(is.null(dir)){
    dir <- "."
  }
  
  options <- options_to_str(
    switch(storage,
      memory = list("hash-type" = "memory"),
         BDB = list(new = new, "hash-type" = "bdb", dir = dir),  
      sqlite = list(new = new, dir = dir),
    postgres = list(new = new, host = host, port = port,
                    database = database, user = user, password = password),
       mysql = list(new = new, host = host, port = port,
                    database = database, user = user, password = password),
    virtuoso = list(dsn = dsn, user = user, password = password, 
                    database = database, host = host, charset = charset),
    list()
    ))
  
  store <- switch(storage,
    memory = new("Storage", world, "hashes", name = name, options = options),
    BDB = new("Storage", world, "hashes", name = name, options = options),
    sqlite = new("Storage", world, "sqlite", name = name, options = options),
    postgres = new("Storage", world, "postgresql", name = name, options = options),
    mysql =  new("Storage", world, "mysql", name = name, options = options),
    virtuoso =  new("Storage", world, "virtuoso", name = name, options = options)
  ) 
  
  continue <- !is_null_pointer(store)
  
  if(check_only){
    freeStorage(store)
    freeWorld(world)
    return(continue)
  }
  
  if(!continue){
    if(fallback){
       warning(paste(storage, "driver not found. Falling back on in-memory storage"))
       redland::freeStorage(store)
       store <- new("Storage", world)
     } else {
       stop(paste(storage, "not found"))
     }
  }
 
  store
}


compact <- function(l){ Filter(Negate(is.null), l)}
options_to_str <- function(x){
  x <- compact(x)
  n <- names(x)
  out <- character(0)
  for(i in seq_along(x)){
    out <- paste0(c(out, paste0(n[[i]], "=", "'", x[[i]], "'")), collapse=",")
  }
  out
}

is_null_pointer <- function(x){
  utils::capture.output(
    base::print.default(
      x@librdf_storage@ref)) == 
  "<pointer: 0x0>"
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
#' \itemize{
#' \item \url{https://en.wikipedia.org/wiki/Resource_Description_Framework}
#' \item \url{https://en.wikipedia.org/wiki/SPARQL}
#' \item \url{https://en.wikipedia.org/wiki/JSON-LD}
#' }
#'
#' To learn more about rdflib, start with the vignettes:
#' `browseVignettes(package = "rdflib")`
#'
#'  Configurations via `options()`
#' 
#' `rdflib_print_format`:
#'  
#' - NULL or "nquads" (default)
#' - any valid serializer name: e.g. "rdfxml", "jsonld", "turtle",  "ntriples"
#'   
#' `rdflib_base_uri`:
#' 
#' - Default base URI to use (when serializing JSON-LD only at this time)
#'     default is "localhost://"
#'
#'
#'
"_PACKAGE"


