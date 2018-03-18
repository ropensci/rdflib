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
