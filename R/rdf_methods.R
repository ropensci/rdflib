
#' Concatenate rdf Objects.  
#' 
#' All subsequent rdf objects will be appended to the first rdf object
#' Note: this does not free memory from any of the individual rdf objects
#' Note: It is generally better to avoid the use of this function by passing
#' an existing rdf object to and rdf_parse or rdf_add objects. Multiple active
#' rdf objects can cause problems when using disk-based storage backends.   
#'   
#' @method c rdf
#' @export
#' @param ... objects to be concatenated
c.rdf <- function(...){
  rdfs <- list(...)
  loc <- tempdir()
  rdf <- rdfs[[1]]
  for(i in seq_along(rdfs)){
    f <- file.path(loc,paste0(i, ".rdf"))
    rdf_serialize(rdfs[[i]],f, format = "turtle") 
    rdf_parse(f, rdf = rdf, format = "turtle")
    file.remove(f)
  }
  unlink(loc)
  rdf
}



#' @importFrom redland librdf_model_size
#' @export
length.rdf <- function(x){
  redland::librdf_model_size(x$model@librdf_model)
}

#' @export
print.rdf <- function(x, ...){
  cat(format.rdf(x, ...), sep = "\n")
}


#' @importFrom stringi stri_unescape_unicode
#' @export
# @param max_print maximum number of lines to print of rdf preview
# @param max_preview if number of triples exceeds this, no preview
# will be displayed, since preview method must currently serialize
# entire triplestore.
format.rdf <- function(x,
                       format = getOption("rdf_print_format", "nquads"),
                       max_print = getOption("rdf_max_print", 10L),
                       max_preview = 1e5,
                       ...){
  n <- redland::librdf_model_size(x$model@librdf_model)
  header <- paste0("Total of ", n, " triples, stored in ", x$storage@type, "\n",
                  "-------------------------------\n")
  if(n < max_preview){
    tmp <- tempfile()
    rdf_serialize(x, 
                  tmp,
                  format = format,
                  ...)
    ## Fix encoding on nquads, ntriples 
    footer <- NULL
    if(n > max_print){
      footer <- paste0("\n\n... with ", n-max_print, " more triples")
    }
    txt <- paste0(header, stringi::stri_unescape_unicode(
      paste(readLines(tmp, n = max_print), collapse = "\n")),
      footer)
    unlink(tmp)
  } else {
    txt <- paste(header, "\n (preview supressed for performance)")
  }
  txt
}
