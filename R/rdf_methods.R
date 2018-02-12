
#' Concatenate rdf Objects.  
#' 
#' All subsequent rdf objects will be appended to the first rdf object
#' Note: this does not free memory from any of the individual rdf objects 
#' @method c rdf
#' @export
#' @param ... objects to be concatenated
c.rdf <- function(...){
  rdfs <- list(...)
  loc <- tempdir()
  rdf <- rdfs[[1]]
  for(i in seq_along(rdfs)){
    f <- file.path(loc,paste0(i, ".rdf"))
    rdf_serialize(rdfs[[i]],f) 
    rdf_parse(f, rdf = rdf)
  }
  rdf
}



#' @export
print.rdf <- function(x, ...){
  cat(format.rdf(x), sep = "\n")
}


#' @importFrom stringi stri_unescape_unicode
#' @export
format.rdf <- function(x,
                       format = getOption("rdf_print_format", "nquads"),
                       ...){
  tmp <- tempfile()
  rdf_serialize(x, 
                tmp,
                format = format)
  ## Fix encoding on nquads, ntriples 
  txt <- stringi::stri_unescape_unicode(
    paste(readLines(tmp), collapse = "\n"))
  unlink(tmp)
  txt
}
