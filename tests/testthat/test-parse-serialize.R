testthat::context("Parsers and Serializers")

doc <- system.file("extdata/example.rdf", package="redland")
out <- "testing.rdf"



testthat::test_that("we can serialize to character", {
                      rdf <- rdf_parse(doc) 
                      txt <- rdf_serialize(rdf, format = "nquads")
                      testthat::expect_is(txt, "character")
                      testthat::expect_match(txt, "John Smith")
                      rdf_free(rdf)
                      })


testthat::test_that("we can parse (in rdfxml)
                    and serialize (in nquads) a simple rdf graph", {
                      rdf <- rdf_parse(doc) 
                      rdf_serialize(rdf, out, "nquads")
                      roundtrip <- rdf_parse(out, "nquads")
                      testthat::expect_is(roundtrip, "rdf")
                      
                      rdf_free(roundtrip)
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


testthat::test_that("we can parse and serialize json-ld", {
  rdf <- rdf_parse(doc)
  rdf_serialize(rdf, "out.json")
  roundtrip <- rdf_parse("out.json")
  testthat::expect_is(roundtrip, "rdf")
  rdf_serialize(rdf, "out.jsonld")
  unlink("out.json")
  unlink("out.jsonld")
  rdf_free(roundtrip)
  rdf_free(rdf)
  
})

testthat::test_that("we can parse and serialize nquads", {
  rdf <- rdf_parse(doc)
  rdf_serialize(rdf, "out.nquads")
  roundtrip <- rdf_parse("out.nquads")
  testthat::expect_is(roundtrip, "rdf")
  unlink("nquads")
  rdf_free(roundtrip)
  rdf_free(rdf)
  
})
testthat::test_that("we can parse and serialize ntriples", {
  rdf <- rdf_parse(doc)
  rdf_serialize(rdf, "out.nt")
  roundtrip <- rdf_parse("out.nt")
  testthat::expect_is(roundtrip, "rdf")
  unlink("out.nt")
  rdf_serialize(rdf, "out.ntriples")
  unlink("out.ntriples")

  rdf_free(roundtrip)
  rdf_free(rdf)
})
testthat::test_that("we can parse and serialize tutle", {
  rdf <- rdf_parse(doc)
  rdf_serialize(rdf, "out.ttl")
  roundtrip <- rdf_parse("out.ttl")
  testthat::expect_is(roundtrip, "rdf")
  unlink("out.ttl")
  rdf_serialize(rdf, "out.turtle")
  unlink("out.turtle")
  
  rdf_free(roundtrip)
  rdf_free(rdf)
})
testthat::test_that("we can parse and serialize rdfxml", {
  rdf <- rdf_parse(doc)
  rdf_serialize(rdf, "out.rdf")
  roundtrip <- rdf_parse("out.rdf")
  testthat::expect_is(roundtrip, "rdf")

  unlink("out.rdf")
  rdf_serialize(rdf, "out.xml")
  unlink("out.xml")
  
  rdf_free(roundtrip)
  rdf_free(rdf)
})

################################################################


testthat::test_that("we can parse by guessing on the file extension", {
  ex <- system.file("extdata/person.nq", package="rdflib")
  rdf <- rdf_parse(ex)
  rdf_serialize(rdf, "tmp.nq", base = "http://schema.org/")
  roundtrip <- rdf_parse("tmp.nq", "turtle")
  testthat::expect_is(roundtrip, "rdf")
  unlink("tmp.nq")
  rdf_free(rdf)
})


testthat::test_that("we can serialize turtle with a baseUri", {
  ex <- system.file("extdata/person.nq", package="rdflib")
  rdf <- rdf_parse(ex, "nquads")
  rdf_serialize(rdf, out, "turtle", base = "http://schema.org/")
  roundtrip <- rdf_parse(out, "turtle")
  testthat::expect_is(roundtrip, "rdf")
  rdf_free(rdf)
})


## JSON-LD tests with default base uri

  testthat::test_that("@id is not a URI, we should get localhost", {
    ex <- '{
      "@context": "http://schema.org/",
      "@id": "person_id",
      "name": "Jane Doe"
    }'
    
    rdf <- rdf_parse(ex, "jsonld")
    testthat::expect_output(cat(format(rdf, "nquads")), "localhost")
    rdf_free(rdf)
  })

  testthat::test_that("@id is a URI, we should not get localhost", {
    ex <- '{
      "@context": "http://schema.org/",
      "@id": "uri:person_id",
      "name": "Jane Doe"
    }'
    rdf <- rdf_parse(ex, "jsonld")
    testthat::expect_false(grepl("localhost", format(rdf, "nquads")))
    rdf_free(rdf)
  })  
  
  testthat::test_that("we can alter the base URI", {
    ex <- '{
      "@id": "person_id",
      "schema:name": "Jane Doe"
    }'
    options(rdf_base_uri = "http://example.com/")
    rdf <- rdf_parse(ex, "jsonld")
    testthat::expect_output(cat(format(rdf, "nquads")), "http://example.com")
    rdf_free(rdf)
    
    
    options(rdf_base_uri = "")
    rdf <- rdf_parse(ex, "jsonld")
    testthat::expect_length(rdf, 0)
    rdf_free(rdf)
    
    options(rdf_base_uri = NULL)
  })
  
  
  
  

  
  
testthat::test_that("we can parse into an existing rdf model", {
    rdf1 <- rdf_parse(system.file("extdata/ex.xml", package = "rdflib"))
    rdf2 <- rdf_parse(system.file("extdata/ex2.xml", package = "rdflib"),
                      rdf = rdf1)
    
    testthat::expect_is(rdf1, "rdf")
    testthat::expect_is(rdf2, "rdf")
    testthat::expect_identical(rdf1, rdf2)
    rdf_free(rdf1)
    
    ## NOTE: rdf is same pointer as rdf1, not a new pointer.  cannot free twice
})
  
  

testthat::test_that("we can parse from a url", {
  # CRAN seems okay with tests requiring an internet connection
  #testthat::skip_on_cran()
  rdf <- rdf_parse("https://tinyurl.com/ycf95c9h")
  testthat::expect_is(rdf, "rdf")
  
  rdf_free(rdf)
})

testthat::test_that("we can parse from a text string", {
  
  
  rdf <- rdf_parse(doc)
  txt <- rdf_serialize(rdf, format = "rdfxml")
  testthat::expect_is(txt, "character")
  roundtrip <- rdf_parse(txt, format="rdfxml")
  testthat::expect_is(roundtrip, "rdf")
  rdf_free(rdf)
  
  string <-
    '
  _:b0 <http://schema.org/jobTitle> "Professor" .
  _:b0 <http://schema.org/name> "Jane Doe" .
  _:b0 <http://schema.org/age> "35" .
  _:b0 <http://schema.org/url> <http://www.janedoe.com> .
  '
  rdf <- rdf_parse(string, "nquads")
  testthat::expect_is(rdf, "rdf")
  
  rdf_free(rdf)
})

unlink(out)

