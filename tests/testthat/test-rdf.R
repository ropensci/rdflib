testthat::context("test-rdf.R")

doc <- system.file("extdata/example.rdf", package="redland")
out <- "testing.rdf"

testthat::test_that("we can parse (in rdfxml)
                    and serialize (in nquads) a simple rdf graph", {
  rdf <- rdf_parse(doc) 
  rdf_serialize(rdf, out, "nquads")
  roundtrip <- rdf_parse(out, "nquads")
  testthat::expect_is(roundtrip, "rdf")
})

testthat::test_that("we can make sparql queries", {
  sparql <-
  'PREFIX dc: <http://purl.org/dc/elements/1.1/>
   SELECT ?a ?c
   WHERE { ?a dc:creator ?c . }'
  
  rdf <- rdf_parse(doc)
  match <- rdf_query(rdf, sparql)
  testthat::expect_length(match, 2)
})

testthat::test_that("we can initialize add triples to rdf graph", {
  x <- rdf()
  x <- rdf_add(x, 
           subject="http://www.dajobe.org/",
           predicate="http://purl.org/dc/elements/1.1/language",
           object="en")
  testthat::expect_is(x, "rdf")
})

testthat::test_that("we can add, parse and serialize json-ld", {
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
  rdf <- rdf_parse(doc)
  txt <- format(rdf, format = "rdfxml")
  testthat::expect_output(print(rdf), ".*johnsmith.*")
  
  testthat::expect_is(txt, "character")
})

testthat::test_that("we can parse from a text string", {
  rdf <- rdf_parse(doc)
  txt <- format(rdf, format = "rdfxml")
  testthat::expect_is(txt, "character")
  roundtrip <- rdf_parse(txt, format="rdfxml")
  testthat::expect_is(roundtrip, "rdf")
})

testthat::test_that("we can add a namespace on serializing", {
  rdf <- rdf_parse(doc)
  rdf_serialize(rdf,
                out,
                namespace = "http://purl.org/dc/elements/1.1/",
                prefix = "dc")
  roundtrip <- rdf_parse(doc)
  testthat::expect_is(roundtrip, "rdf")
  
})


testthat::test_that("we can parse and serialize json-ld", {
  x <- rdf_parse(doc)
  rdf_serialize(x, out, "jsonld")
  roundtrip <- rdf_parse(out, "jsonld")
  testthat::expect_is(roundtrip, "rdf")

})

testthat::test_that("we can parse and serialize nquads", {
  x <- rdf_parse(doc)
  rdf_serialize(x, out, "nquads")
  roundtrip <- rdf_parse(out, "nquads")
  testthat::expect_is(roundtrip, "rdf")
})
testthat::test_that("we can parse and serialize ntriples", {
  x <- rdf_parse(doc)
  rdf_serialize(x, out, "ntriples")
  roundtrip <- rdf_parse(out, "ntriples")
  testthat::expect_is(roundtrip, "rdf")
})
testthat::test_that("we can parse and serialize tutle", {
  x <- rdf_parse(doc)
  rdf_serialize(x, out, "turtle")
  roundtrip <- rdf_parse(out, "turtle")
  testthat::expect_is(roundtrip, "rdf")
})
testthat::test_that("we can parse and serialize rdfxml", {
  x <- rdf_parse(doc)
  rdf_serialize(x, out, "rdfxml")
  roundtrip <- rdf_parse(out, "rdfxml")
  testthat::expect_is(roundtrip, "rdf")
})


testthat::test_that("we can parse from a url", {
  # CRAN seems okay with tests requiring an internet connection
  #testthat::skip_on_cran()
  rdf <- rdf_parse("https://tinyurl.com/ycf95c9h")
  testthat::expect_is(rdf, "rdf")
  
})


testthat::test_that("we can parse from a string", {
  string <- 
    '
  _:b0 <http://schema.org/jobTitle> "Professor" .
  _:b0 <http://schema.org/name> "Jane Doe" .
  _:b0 <http://schema.org/telephone> "(425) 123-4567" .
  _:b0 <http://schema.org/url> <http://www.janedoe.com> .
  _:b0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
  '
  rdf <- rdf_parse(string, "nquads")
  testthat::expect_is(rdf, "rdf")
  
})


unlink(out)

