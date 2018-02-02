
## Utilities to coerce return type, if recognized
r_class <- function(x){
  switch(gsub("<http://www.w3.org/2001/XMLSchema#(.*)>", "xs:\\1", x),
         "xs:decimal" = "numeric",
         "xs:string" = "character",
         "xs:string" = "factor",
         "xs:boolean" = "logical",
         "xs:integer" = "integer",
         "xs:date" = "Date",
         "xs:dateTime" = "POSIXct",
         "character"
  )
}
get_values <- function(x) gsub('\"(([^\\^])+)\"\\^*(.*)',  "\\1", x)
get_types <- function(x) out <- gsub('\"(([^\\^])+)\"\\^*(.*)',  "\\3", x)


#' @importFrom methods as
type_by_datauri <- function(x){
  types <- get_types(x)
  r_types <- vapply(get_types(x), r_class, character(length(1)))
  df <- data.frame(value = get_values(x), class = r_types)
  apply(df, 1, function(x) as(x[1], x[2]))
}


rectangularize_query_results <- function(out){
  vars <- unique(names(out))
  X <- lapply(vars, function(v) 
    ## Strip ^^TYPE typing
    #gsub('\"(([^\\^])+)\"\\^*.*', 
    #     "\\1", 
     type_by_datauri(as.character(out[names(out) == v ])))
  names(X) <- vars
  as.data.frame(X, stringsAsFactors=FALSE)
}
