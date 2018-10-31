

# Must match parser name & q 1.0 mimetype listed at:
# http://librdf.org/raptor/api/raptor-formats-types-by-parser.html
# 3 turtle options listed but only text/turtle works. 
# support may depend on libs avialable depending on when and how redland is compiled
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



guess_format <- function(doc){
  switch(gsub(".*\\.(\\w+)$", "\\1", basename(doc)),
         "xml" = "rdfxml",
         "rdf" = "rdfxml",
         "json" = "jsonld",
         "nq" = "nquads",
         "nt" = "ntriples",
         "ttl" = "turtle",
         "jsonld" = "jsonld",
         "quads" = "nquads",
         "turtle" = "turtle",
         "rdfxml")
}


## Don't explicitly type characters as strings, since this is default
xs_class <- function(x){
  
    type <- switch(class(x)[[1]],
                   "numeric" = "xs:decimal",
                   "factor" = "xs:string",
                   "logical" = "xs:boolean",
                   "integer" = "xs:integer",
                   "Date" = "xs:date",
                   "POSIXct" = "xs:dateTime",
                   NULL
    )
  
  
  string <- gsub("^xs:", 
                 "http://www.w3.org/2001/XMLSchema#",
                 type)
  ## consistent return length, character(1)
  if (length(string) == 0) {
    string <- as.character(NA)
  }
  string
}


uri_prefix <- function(x){
  abs_uri <- grepl("^\\w+://", x)
  if (abs_uri) {
    if (!grepl("[#/]$", x)) return(paste0(x, "#"))
    return(x)
  }
  if (!grepl(":$", x)) return(paste0(x, ":"))
  x
}



