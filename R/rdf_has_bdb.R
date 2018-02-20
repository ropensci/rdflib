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
  ## Unfortunately convoluted way to check if we have Berkeley DB Support
  world <- new("World")
  path <-tempdir()
  options <- paste0("new='yes',hash-type='bdb',dir='", path, "'")
  storage <- new("Storage", world, "hashes", name = "rdflib", 
                 options = options)
  
  out <- !(utils::capture.output(
    base::print.default(
      storage@librdf_storage@ref)) == 
      "<pointer: 0x0>")
  
  redland::freeStorage(storage)
  redland::freeWorld(world)
  
  out
}

