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

## My redland version does not find support for these, probably optional 
## additional dependency needed when compiling redland
# - trig  (application/x-trig)
# - n3 (text/n3) 
# - rdfa (application/xhtml+xml, or text/html)
# - rss (application/rss+xml or text/rss)

