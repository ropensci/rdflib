library(magrittr)

system.file("extdata", "dc.rdf", package="redland") %>%
  rdf_parse() %>%
  rdf_serialize("test.nquads", "nquads")

file <- system.file("extdata", "dc.rdf", package="redland")
sparql <-
'PREFIX dc: <http://purl.org/dc/elements/1.1/>
 SELECT ?a ?c
 WHERE { ?a dc:creator ?c . }'
rdf <- rdf_parse(file)
rdf %>% rdf_query(sparql)


x <- rdf()
x <- rdf_add(x, 
         subject="http://www.dajobe.org/",
         predicate="http://purl.org/dc/elements/1.1/language",
         object="en")



library(jsonld)
tmp <- tempfile()
rdf_serialize(x, tmp, "nquads")
str <- readLines(tmp) %>% jsonld_from_rdf() 


tmp <- tempfile()
str %>% jsonld_to_rdf() %>% writeLines(tmp)
rdf <- rdf_parse(tmp, format = "nquads")



unlink("test.nquads")

