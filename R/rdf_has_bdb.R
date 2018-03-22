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
 rdf_storage("BDB", new_db = TRUE, check_only = TRUE)
}

rdf_has_virtuoso <- function(user="dba", 
                             password="dba", 
                             dsn="Local Virtuoso"){
#  has_driver <- rdf_storage("virtuoso", 
#                            user = user, 
#                            password = password, 
#                            dsn = dsn, 
#                            check_only=TRUE)
  r <- tryCatch(rdf("virtuoso", user = user, 
           password = password, dsn = dsn, fallback = FALSE),
           error = function(e) FALSE)
  if(is.logical(r)){
    has_connection <- r
  } else {
    rdf_add(r, "", "dc:name", "bob")
    if(length(r) >= 1){
      has_connection <- TRUE
    } else { 
      has_connection <- FALSE
    }
    rdf_free(r)
  }

  has_connection
  
}
