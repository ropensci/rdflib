testthat::context("test-rdf.R")

doc <- system.file("extdata/example.rdf", package="redland")
out <- "testing.rdf"


testthat::test_that("We can initialize and free rdf objects", {
  rdf <- rdf()
  
  testthat::expect_is(rdf, "rdf")
  testthat::expect_is(rdf$world, "World")
  testthat::expect_is(rdf$model, "Model")
  testthat::expect_is(rdf$storage, "Storage")
  
  rdf_free(rdf)
})

testthat::test_that("We warn if we cannot use disk-based storage", {
  testthat::skip_if(has_bdb())
  path <- tempdir()
  testthat::expect_warning(rdf <- rdf(path), "BDB driver not found")
  
  ## Falls back on memory-based storage, still creates rdf
  testthat::expect_is(rdf, "rdf")
  rdf_free(rdf)
  
})



testthat::test_that("We can use BDB storage", {
  
  testthat::skip_if_not(has_bdb())
  path <- tempdir()
  testthat::expect_silent(rdf <- rdf(path))
  testthat::expect_is(rdf, "rdf")

  rdf_free(rdf)
  
})


testthat::test_that("we can parse (in rdfxml)
                    and serialize (in nquads) a simple rdf graph", {
  rdf <- rdf_parse(doc) 
  rdf_serialize(rdf, out, "nquads")
  roundtrip <- rdf_parse(out, "nquads")
  testthat::expect_is(roundtrip, "rdf")
  
  rdf_free(rdf)
})

## FIXME check format, check return types
testthat::test_that("we can make sparql queries", {
  sparql <-
  'PREFIX dc: <http://purl.org/dc/elements/1.1/>
   SELECT ?a ?c
   WHERE { ?a dc:creator ?c . }'
  
  rdf <- rdf_parse(doc)
  match <- rdf_query(rdf, sparql)
  testthat::expect_length(match, 2)
  
  rdf_free(rdf)
  
})

testthat::test_that("we can initialize add triples to rdf graph", {
  rdf <- rdf()
  rdf <- rdf_add(rdf, 
           subject="http://www.dajobe.org/",
           predicate="http://purl.org/dc/elements/1.1/language",
           object="en")
  testthat::expect_is(rdf, "rdf")
  rdf_free(rdf)
  
})


testthat::test_that("we can concatenate rdfs", {
  rdf1 <- rdf_parse(system.file("extdata/ex2.xml", package = "rdflib"))
  rdf2 <- rdf_parse(doc)
  rdf <- c(rdf1, rdf2)
  testthat::expect_is(rdf, "rdf")
  
  rdf_free(rdf1)
  rdf_free(rdf2)
  rdf_free(rdf)
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
  rdf_free(x)
  rdf_free(rdf)
  
})




testthat::test_that("print and format work", {
  rdf <- rdf_parse(doc)
  txt <- format(rdf, format = "rdfxml")
  testthat::expect_output(print(rdf), ".*johnsmith.*")
  
  testthat::expect_is(txt, "character")
  rdf_free(rdf)
})


testthat::test_that("we can add a namespace on serializing", {
  rdf <- rdf_parse(doc)
  rdf_serialize(rdf,
                out,
                namespace = "http://purl.org/dc/elements/1.1/",
                prefix = "dc")
  roundtrip <- rdf_parse(doc)
  testthat::expect_is(roundtrip, "rdf")

  rdf_free(rdf)
  rdf_free(roundtrip)
  
})

################################################################

testthat::context("Test each serialization format")

testthat::test_that("we can parse and serialize json-ld", {
  rdf <- rdf_parse(doc)
  rdf_serialize(rdf, out, "jsonld")
  roundtrip <- rdf_parse(out, "jsonld")
  testthat::expect_is(roundtrip, "rdf")
  rdf_free(rdf)
  
})

testthat::test_that("we can parse and serialize nquads", {
  rdf <- rdf_parse(doc)
  rdf_serialize(rdf, out, "nquads")
  roundtrip <- rdf_parse(out, "nquads")
  testthat::expect_is(roundtrip, "rdf")
  rdf_free(rdf)
  
})
testthat::test_that("we can parse and serialize ntriples", {
  rdf <- rdf_parse(doc)
  rdf_serialize(rdf, out, "ntriples")
  roundtrip <- rdf_parse(out, "ntriples")
  testthat::expect_is(roundtrip, "rdf")
  rdf_free(rdf)
  
})
testthat::test_that("we can parse and serialize tutle", {
  rdf <- rdf_parse(doc)
  rdf_serialize(rdf, out, "turtle")
  roundtrip <- rdf_parse(out, "turtle")
  testthat::expect_is(roundtrip, "rdf")
  rdf_free(rdf)
})
testthat::test_that("we can parse and serialize rdfxml", {
  rdf <- rdf_parse(doc)
  rdf_serialize(rdf, out, "rdfxml")
  roundtrip <- rdf_parse(out, "rdfxml")
  testthat::expect_is(roundtrip, "rdf")
  rdf_free(rdf)
})

################################################################

testthat::context("Parsing different sources (URL, string)")

testthat::test_that("we can parse from a url", {
  # CRAN seems okay with tests requiring an internet connection
  #testthat::skip_on_cran()
  rdf <- rdf_parse("https://tinyurl.com/ycf95c9h")
  testthat::expect_is(rdf, "rdf")
  
  rdf_free(rdf)
})

testthat::test_that("we can parse from a text string", {
  rdf <- rdf_parse(doc)
  txt <- format(rdf, format = "rdfxml")
  testthat::expect_is(txt, "character")
  roundtrip <- rdf_parse(txt, format="rdfxml")
  testthat::expect_is(roundtrip, "rdf")
  rdf_free(rdf)

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
  
  rdf_free(rdf)
})




unlink(out)

