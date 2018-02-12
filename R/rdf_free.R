
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
