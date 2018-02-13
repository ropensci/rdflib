
#' Free Memory Associated with RDF object
#' 
#' @param rdf an rdf object
#' @param rm logical, default TRUE. Remove pointer from parent.frame()?
#' Usually a good idea since referring to a pointer after it has been
#' removed can crash R.
#' @details Free all pointers associated with an rdf object. 
#' Frees memory associated with the storage, world, and model
#' objects. 
#' @export
#' @examples 
#' rdf <- rdf()
#' rdf_free(rdf)
#' rm(rdf)
rdf_free <- function(rdf, env = parent.frame()){
  redland::freeModel(rdf$model)
  redland::freeStorage(rdf$storage)
  redland::freeWorld(rdf$world)

  ## Remove pointer if possible
  ## Since referring to this pointer after it has been
  ## freed would otherwise just crash R.
  rm(list = deparse(substitute(rdf)), envir = parent.frame())

}




