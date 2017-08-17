

doc <- system.file("extdata", "dc.rdf", package="redland")
rdf <- rdf_parse(doc) 

out <- tempfile()
rdf_serialize(rdf, out, "nquads")

sparql <-
'PREFIX dc: <http://purl.org/dc/elements/1.1/>
 SELECT ?a ?c
 WHERE { ?a dc:creator ?c . }'

rdf <- rdf_parse(doc)
rdf_query(rdf, sparql)


x <- rdf()
x <- rdf_add(x, 
         subject="http://www.dajobe.org/",
         predicate="http://purl.org/dc/elements/1.1/language",
         object="en")


rdf_serialize(x, out, "jsonld")
rdf <- rdf_parse(out, format = "jsonld")



unlink(out)

