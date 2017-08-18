testthat::context("test-rdf.R")

doc <- system.file("extdata/example.rdf", package="redland")
out <- tempfile()

testthat::test_that("we can parse (in rdfxml)
                    and serialize (in nquads) a simple rdf graph", {
  rdf <- rdf_parse(doc) 
  rdf_serialize(rdf, out, "nquads")
})

testthat::test_that("we can make sparql queries", {
sparql <-
'PREFIX dc: <http://purl.org/dc/elements/1.1/>
 SELECT ?a ?c
 WHERE { ?a dc:creator ?c . }'

rdf <- rdf_parse(doc)
rdf_query(rdf, sparql)
})

testthat::test_that("we can initialize add triples to rdf graph", {
  x <- rdf()
  x <- rdf_add(x, 
           subject="http://www.dajobe.org/",
           predicate="http://purl.org/dc/elements/1.1/language",
           object="en")
  testthat::expect_is(x, "rdf")
})

testthat::test_that("we can parse and serialize json-ld", {
  #x <- rdf_parse(doc)
  x <- rdf()
  x <- rdf_add(x, 
               subject="http://www.dajobe.org/",
               predicate="http://purl.org/dc/elements/1.1/language",
               object="en")
  rdf_serialize(x, out, "jsonld")
  rdf <- rdf_parse(out, format = "jsonld")
  testthat::expect_is(rdf, "rdf")
})

testthat::test_that("print and format work", {
  txt <- format(rdf, format = "rdfxml")
  print(rdf)
  
  testthat::expect_is(txt, "character")
})

testthat::test_that("we can parse from a text string", {
  rdf <- rdf_parse(doc)
  txt <- format(rdf, format = "rdfxml")
  rdf_parse(txt, format="rdfxml")
})


unlink(out)

