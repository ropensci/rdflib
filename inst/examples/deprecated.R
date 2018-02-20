



## Deprecate this strategy in favor of direct result
## also means we can deprecate type_by_datauri and other utilities
iter_getNextResult <- function(queryResult){
  out <- list()
  result <- redland::getNextResult(queryResult)
  out <- c(out, result)
  while(!is.null(result)){
    result <- redland::getNextResult(queryResult)
    out <- c(out, result)
  }
  out
}

rectangularize_query_results <- function(out){
  vars <- unique(names(out))
  
  X <- lapply(vars, function(v){ 
    contents <- as.character(out[names(out) == v ])
    values <- type_by_datauri(contents)
    
    ## use "character" if mixed type column
    types <- vapply(values, function(x) class(x)[[1]], character(1))
    u <- unique(types)
    if(length(u) == 1){
      values <- unlist(values)
      if(u %in% c("Date", "POSIXct"))
        class(values) <- unique(types) # Restore date class
    } else {
      values <- vapply(values, as.character, character(1))
    }
    values
  })
  
  names(X) <- vars
  ## Or we could use tibble::as_data_frame for list columns w/ mixed type..
  as.data.frame(X, stringsAsFactors=FALSE)
}





#' @importFrom methods as
type_by_datauri <- function(x){
  types <- get_types(x)
  r_types <- vapply(types, r_class, character(length(1)))
  values <- get_values(x)
  
  ## Output must be a list since types can differ
  out <- vector("list", length = length(values))
  for(i in seq_along(values)){
    out[[i]] <- as(utf8me(values[i]), r_types[[i]])
    
  }
  out
}

## Utilities to coerce return type, if recognized
r_class <- function(x){
  switch(gsub("<http://www.w3.org/2001/XMLSchema#(.*)>", "xs:\\1", x),
         "xs:decimal" = "numeric",
         "xs:string" = "character",
         "xs:boolean" = "logical",
         "xs:integer" = "integer",
         "xs:date" = "Date",
         "xs:dateTime" = "POSIXct",
         "character"
  )
}

## Helper utilitiesfor parsing data URIs
get_values <- function(x) gsub('\"(([^\\^])+)\"\\^*(.*)',  "\\1", x)
get_types <- function(x) gsub('\"(([^\\^])+)\"\\^*(.*)',  "\\3", x)
#' @importFrom stringi stri_unescape_unicode
# https://stackoverflow.com/questions/48602294
utf8me <- function(x){ 
  removed_quotes <- gsub('\"', '', x)
  stringi::stri_unescape_unicode(removed_quotes)
}


## so that we can do as("2018-02-05", "Date") in type_by_datauri
setAs("character", "Date", function(from) as.Date(as.character(from)))
setAs("character", "POSIXct", function(from) as.POSIXct(as.character(from)))
