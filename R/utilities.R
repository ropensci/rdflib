
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





has_bdb <- function(){
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

# Must match parser name & q 1.0 mimetype listed at:
# http://librdf.org/raptor/api/raptor-formats-types-by-parser.html
# 3 turtle options listed but only text/turtle works. 
rdf_mimetypes <- c("nquads" = "text/x-nquads",
                   "ntriples" = "application/n-triples",
                   "rdfxml" = "application/rdf+xml",
                   "trig" = "application/x-trig",
                   "turtle" = "text/turtle")

# trig not working right now, not clear why
# Consider adding/testing: 
# - n3 (text/n3)
# - rdfa (application/xhtml+xml, or text/html)
# - rss (application/rss+xml or text/rss)


# rdf functions like working with local files
# this helper function allows us to also use URLs or strings
#' @importFrom utils download.file
text_or_url_to_doc <- function(x, tmp = tempfile()){
  if(file.exists(x)){
    return(x) 
  } else if(grepl("^https?://", x)) {
    utils::download.file(x, tmp, quiet = TRUE)
    return(tmp)
  } else {
    writeLines(x, tmp)
    return(tmp)
  }
}
