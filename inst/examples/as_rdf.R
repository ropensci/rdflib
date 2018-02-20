
#' As RDF
#' 
#' Coerce an object into RDF
#' @param x an object to coerce into RDF (list, list-like, or data.frame)
#' @param rdf An existing rdf object, (by default a new object will be initialized)
#' @param vocab A default vocabulary (URI prefix) to assume for all predicates
#' @param base A base URI to assume for blank subject nodes
#' @param context: a named list mapping any string to a URI
#' @param ... additional arguments, specific to certain types
#' 
as_rdf <- function(x, 
                   rdf = rdf(),
                   vocab = NULL, 
                   base = getOption("rdflib_base_uri", "localhost://"), 
                   context = NULL, 
                   ...) UseMethod("as_rdf")

as_rdf.list <- function(x, 
                        rdf = NULL,
                        vocab = NULL, 
                        base = getOption("rdflib_base_uri", "localhost://"), 
                        context = NULL){
  
  if(is.null(rdf)){
    rdf <- rdf()  
  }
  ## unbox length-1 lists so we can apply a context successfully
  if(is(x, "list") && length(x) == 1) x <- x[[1]]
  
  json <- jsonlite::toJSON(x, pretty = TRUE, auto_unbox = TRUE, force = TRUE)
  jsonld_context <- json_context(vocab, base, context)
  json2 <- paste0('{\n"@context":', jsonld_context, 
                  ',\n',  '"@graph": ', json,  '}')
  rdflib::rdf_parse(json2, "jsonld", rdf = rdf)
  rdf
}


# helper function (identical to plyr::compact)
compact <- function (l) Filter(Negate(is.null), l)
json_context <- function(vocab = NULL, 
                         base = getOption("rdflib_base_uri", "localhost://"), 
                         context = NULL){
  jsonlite::toJSON(
    compact(c(list("@vocab" = vocab,
                   "@base" = base),
              context)), auto_unbox = TRUE)
}



# test2 <- tibble(
#   age = 5L,
#   name = "bob",
#   height = 1.9,
#   address = "x:address",
#   knows = NA
# )
# as_rdf.data.frame(test2, vocab = "x:", key = "nquads")

## tidy data to rdf
as_rdf.data.frame <- function(df,  
                              rdf = NULL, 
                              vocab = NULL, 
                              base = getOption("rdflib_base_uri",
                                               "localhost://"), 
                              context = NULL, 
                              key = NULL){
  if(is.null(rdf)){
    rdf <- rdf()  
  }
  
  ## gather looses col-classes, so pre-compute them (with base R)
  col_classes <- data.frame(datatype = 
                              vapply(df, 
                                     rdflib:::xs_class, 
                                     character(1)))
  col_classes$predicate <- rownames(col_classes)
  rownames(col_classes) <- NULL
  
  ## Use row names as key (subject), unless a key column is specified
  x <- df
  if(is.null(key)){
    x$subject <- 1:dim(x)[[1]]
  } else {
    names(x)[names(x) == key] <- "subject"
  }
  ## FIXME consider taking an already-gathered table to avoid dependency?
  suppressWarnings(
    x <- tidyr::gather(x, key = predicate, 
                       value = object, -subject))
  
  ## merge is Slow! ~ 5 seconds for 800K triples 
  ## (almost as much time as rdf_parse)
  #x <- merge(x, col_classes, by = "predicate")
  x <- dplyr::left_join(x, col_classes, by = "predicate")
  
  loc <- tempfile()
  poor_mans_nquads(x, loc, vocab)
  ## And parse text file.  Way faster than adding row by row!
  ## but still about 8 s on 800K triples, all in the C layer
  
  rdf <- rdf_parse(loc, rdf = rdf, format = "nquads")
  unlink(loc)
  
  rdf
}

## x is a data.frame with columns: subject, predicate, object, & datatype
poor_mans_nquads <- function(x, loc, vocab){
  
  ## FIXME: currently assumes
  
  
  ## PROFILE ME.  Currently written to be base-R compatible, 
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
  x$subject[!blank_subject] <- paste0("<", vocab, x$subject[!blank_subject], ">")
  ## Predicate is always a URI
  x$predicate <- paste0("<", vocab, x$predicate, ">")
  
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
  
  ## write table is a little slow, ~ 1s on 800K triples
  
  ## drop datatype
  x <- x[c("subject", "predicate", "object", "graph")]       
  write.table(x, loc, col.names = FALSE, quote = FALSE, row.names = FALSE)
}


## tidy data to rdf: use rownames as key column
## Note: this method is too slow to be practical on very large data frames
iterative_rdf_add <- function(df, vocab = "x:", base = vocab){
  rownames <- rownames(df)
  colnames <- colnames(df)
  for(i in 1:dim(df)[[1]]){
    for(j in 1:dim(df)[[2]]){
      rdf_add(rdf, 
              subject = paste0(base, as.character(rownames[[i]])),
              predicate = paste0(vocab, colnames[[j]]),
              object = x$object[[i]])
    }
  }
  rdf
}







