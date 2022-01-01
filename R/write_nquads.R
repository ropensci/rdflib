

#' write object out as nquads
#'
#' @param x an object that can be represented as nquads
#' @param file output filename
#' @param ... additional parameters, see examples
#'
#' @export
#'
#' @examples
#' tmp <- tempfile(fileext = ".nq")
#' library(datasets)
#' write_nquads(iris, tmp)
#' read_nquads(tmp)
write_nquads <- function(x, file, ...){
  UseMethod("write_nquads")
}


#' read an nquads file
#' @param file path to nquads file
#' @param ... additional arguments to [rdf_parse()]
#' @return an rdf object.  See [rdf_parse()]
#' 
#' @examples
#' tmp <- tempfile(fileext = ".nq")
#' library(datasets)
#' write_nquads(iris, tmp)
#' read_nquads(tmp)
#' 
#' @export
read_nquads <- function(file, ...){
  rdf_parse(file, "nquads", ...)
}

#' @export
write_nquads.rdf <- function(x, file, ...){
  rdf_serialize(x, file, "nquads", ...)
}
  
#' @export
write_nquads.data.frame <- function(x, 
                         file,
                         ...){
  
  
  
  df <- normalize_table(x, ...)
  poor_mans_nquads(df, file, ...)
}


#' @importFrom tidyr gather
#' @importFrom dplyr left_join
normalize_table <- function(df, key_column = NULL, ...){
  ## gather looses col-classes, so pre-compute them (with base R)
  col_classes <- data.frame(datatype = 
                              vapply(df, 
                                     xs_class, 
                                     character(1)))
  col_classes$predicate <- rownames(col_classes)
  rownames(col_classes) <- NULL
  
  ## Use row names as key (subject), unless a key column is specified
  ## Should we verify that requested key column is indeed a unique key first?
  out <- df
  if (is.null(key_column)) {
    out$subject <- as.character(1:dim(out)[[1]])
  } else {
    names(out)[names(out) == key_column] <- "subject"
  }
  
  ## FIXME consider taking an already-gathered table to avoid dependency?
  
  suppressWarnings(# Possible warnings about mixed types
    out <- tidyr::gather(out, 
                       key = "predicate", 
                       value = "object", 
                       -"subject"))
  
  ## merge is Slow! ~ 5 seconds for 800K triples 
  ## (almost as much time as rdf_parse)
  # merge(out, col_classes, by = "predicate")
  
  dplyr::left_join(out, col_classes, by = "predicate")
  
}



## x is a data.frame with columns: subject, predicate, object, & datatype
#' @importFrom utils write.table
poor_mans_nquads <- function(x, file, prefix = NULL, ...){
  
  if (is.null(prefix)) {
    prefix <- paste0(deparse(substitute(x)), ":")
    warning(paste("prefix not declared, using", prefix))
  }
  
  prefix <- uri_prefix(prefix)
  ## Currently written to be base-R compatible, 
  ## but a tidyverse implementation may speed serialization.  
  ## However, this seems to be fast enough that it is rarely the bottleneck
  
  ## NOTE: paste0 is a little slow ~ 1 s on 800K triples
  ## No datatype on blank (missing) nodes
  
  blank_object <-is.na(x$object)
  blank_subject <- is.na(x$subject)
  
  x$datatype[blank_object] <- as.character(NA)
  ## NA needs to become a unique blank node number, could do uuid or _:r<rownum>
  x$object[blank_object] <- paste0("_:r", which(blank_object))
  x$subject[blank_subject] <- paste0("_:r", which(blank_subject))
  
  ## strings and URIs do not get a datatype
  needs_type <- !is.na(x$datatype)
  
  ## URIs that are not blank nodes need <>
  x$subject[!blank_subject] <- paste0("<", prefix, x$subject[!blank_subject], ">")
  ## Predicate is always a URI
  x$predicate <- paste0("<", prefix, x$predicate, ">")
  
  ## Strings should be quoted
  is_string <- !grepl("\\w+:\\w.*", x$object) &
    !needs_type & !blank_object
  x$object[is_string] <- paste0('\"', x$object[is_string] , '\"')
  
  ## URIs should be <> instead, but not blanks!
  x$object[!blank_object] <- gsub("(^\\w+:\\w.*$)", "<\\1>", 
                                  x$object[!blank_object])
  
  ## assumes datatype is not empty (e.g. string)
  x$object[needs_type] <- paste0('\"', x$object[needs_type], 
                                 '\"^^<', x$datatype[needs_type], ">")
  
  ## quads needs a graph column
  x$graph <- "."
  
  ## write table is a little slow, ~ 1s on 800K triples, 
  ## but readr cannot write in nquads style
  
  ## drop datatype
  x <- x[c("subject", "predicate", "object", "graph")]       
  utils::write.table(x, file, col.names = FALSE, quote = FALSE, row.names = FALSE)
}


