
#' Coerce an object into RDF
#' 
#' Coerce an object into RDF
#' @param x an object to coerce into RDF (list, list-like, or data.frame)
#' @param rdf An existing rdf object, (by default a new object will be initialized)
#' @param prefix A default vocabulary (URI prefix) to assume for all predicates
#' @param base A base URI to assume for blank subject nodes
#' @param context: a named list mapping any string to a URI
#' @param ... additional arguments, specific to certain types
#' @export
#' 
#' @examples 
#' as_rdf(mtcars)
#' as_rdf(list(repo = "rdflib", owner = list("id", "ropensci")))
as_rdf <- function(x, 
                   rdf = rdf(),
                   prefix = NULL, 
                   base = getOption("rdf_base_uri", "localhost://"), 
                   context = NULL, 
                   ...) UseMethod("as_rdf")


#' @export
as_rdf.list <- function(x, 
                        rdf = NULL,
                        prefix = NULL, 
                        base = getOption("rdf_base_uri", "localhost://"), 
                        context = NULL){
  
  if(is.null(rdf)){
    rdf <- rdf()  
  }
  ## unbox length-1 lists so we can apply a context successfully
  if(is(x, "list") && length(x) == 1) x <- x[[1]]
  
  json <- jsonlite::toJSON(x, pretty = TRUE, auto_unbox = TRUE, force = TRUE)
  jsonld_context <- json_context(prefix, base, context)
  json2 <- paste0('{\n"@context":', jsonld_context, 
                  ',\n',  '"@graph": ', json,  '}')
  rdflib::rdf_parse(json2, "jsonld", rdf = rdf)
  invisible(rdf)
}


# helper function (identical to plyr::compact)
compact <- function (l) Filter(Negate(is.null), l)
json_context <- function(prefix = NULL, 
                         base = getOption("rdf_base_uri", "localhost://"), 
                         context = NULL){
  jsonlite::toJSON(
    compact(c(list("@vocab" = prefix,
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
# as_rdf.data.frame(test2, prefix = "x:", key = "nquads")


## tidy data to rdf
#' @export
as_rdf.data.frame <- function(df,  
                              rdf = rdf(), 
                              prefix = NULL, 
                              base = getOption("rdf_base_uri",
                                               "localhost://"), 
                              context = NULL, 
                              key_column = NULL){
  
  file = tempfile()
  write_nquads(df, file, prefix, key_column)
  rdf <- rdf_parse(file, rdf = rdf, format = "nquads")

  unlink(file)
  invisible(rdf)
}


## Not used
## tidy data to rdf: use rownames as key column
## Note: this method is too slow to be practical on very large data frames
iterative_rdf_add <- function(df, prefix = "x:", base = prefix){
  rownames <- rownames(df)
  colnames <- colnames(df)
  for(i in 1:dim(df)[[1]]){
    for(j in 1:dim(df)[[2]]){
      rdf_add(rdf, 
              subject = paste0(base, as.character(rownames[[i]])),
              predicate = paste0(prefix, colnames[[j]]),
              object = x$object[[i]])
    }
  }
  invisible(rdf)
}

