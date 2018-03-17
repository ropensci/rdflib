#' Check for BDB support
#' 
#' Detect whether Berkeley Database for disk-based storage of RDF graphs
#' is available.  Disk-based storage requires redland package
#' to be installed from source with support for the Berkeley DB 
#' (libdb-dev on Ubuntu, berkeley-db on homebrew), otherwise `rdf()` will
#' fall back to in-memory storage with a warning.
#' 
#' @return TRUE if BDB support is detected, false otherwise
#' @export
#' @examples 
#' rdf_has_bdb()
rdf_has_bdb <- function(){
 rdf_has_backend("BDB")
}



rdf_has_backend <- function(storage = c("memory", 
                                     "BDB", 
                                     "sqlite", 
                                     "postgres", 
                                     "mysql", 
                                     "virtuoso"),
                            host = NULL, 
                            user = NULL,
                            password = NULL,
                            port = NULL){
  storage <- match.arg(storage)
  if(storage == "memory"){
    return(TRUE)
  }
  
  ## Unfortunately convoluted way to check if we have Berkeley DB Support
  world <- new("World")
  
  store <- switch(storage,
    BDB      = new("Storage", world, "hashes", name = "rdflib", 
                   options = paste0("new='yes',hash-type='bdb',dir='",
                                  tempdir(), "'")),
    sqlite   = new("Storage", world, "sqlite", name = "sqlite1",
                   options = "new='yes'"),
    postgres = new("Storage", world, "postgresql", name = "db",
                   options = paste0("new='yes',host='", host, "',", 
                                    "user='", user, 
                                    "','password='", password, "'")),
    mysql = new("Storage", world, "mysql", name = "db",
                options = paste0("new='yes',host='", host, "',", 
                                 "user='", user,
                                 "','password='", password, "'")),
    virtuoso = new("Storage", world, "virtuoso", name = "db1",
                   options = "dsn='Local Virtuoso',user='", 
                   user, "','password='", password, "'")
  )
  
  out <- !(utils::capture.output(
    base::print.default(
      store@librdf_storage@ref)) == 
      "<pointer: 0x0>")
  
  redland::freeStorage(store)
  redland::freeWorld(world)
  
  out
}

