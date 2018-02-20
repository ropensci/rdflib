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
  
  ## Confirm `rdf` has been removed from the workspace
  testthat::expect_false("rdf" %in% ls())
  
  RDF_graph <- rdf()
  testthat::expect_true("RDF_graph" %in% ls())
  rdf_free(RDF_graph)
  testthat::expect_false("RDF_graph" %in% ls())
})

testthat::test_that("We warn if we cannot use disk-based storage", {
  testthat::skip_if(rdf_has_bdb())
  testthat::expect_warning(rdf <- rdf(storage = "BDB"), "BDB driver not found")
  ## Falls back on memory-based storage, still creates rdf
  testthat::expect_is(rdf, "rdf")
  rdf_free(rdf)
  
})

testthat::test_that("We can use BDB storage", {
  testthat::skip_if_not(rdf_has_bdb())
  testthat::expect_silent(rdf <- rdf(storage="BDB", new_db = TRUE))
  
  rdf_add(rdf, "", "dc:name", "bob")
  expect_match(format(rdf, "nquads"), "bob")
  testthat::expect_is(rdf, "rdf")
  rdf_free(rdf)
  
  ## We can reconnect to disk based storage after freeing
  rdf2 <- rdf(storage = "BDB", new_db = FALSE)
  expect_match(format(rdf2, "nquads"), "bob")
  rdf_free(rdf2)
  
  unlink("rdflib-po2s.db")
  unlink("rdflib-so2p.db")
  unlink("rdflib-sp2o.db")
})


testthat::test_that("we can concatenate rdfs", {
  rdf1 <- rdf_parse(system.file("extdata/ex.xml", package = "rdflib"))
  rdf2 <- rdf_parse(system.file("extdata/ex2.xml", package = "rdflib"))
  rdf <- c(rdf1, rdf2)
  testthat::expect_is(rdf, "rdf")
  
  rdf_free(rdf1)
  rdf_free(rdf2)
  ## NOTE: rdf is same pointer as rdf1, not a new pointer.  cannot free twice
})



testthat::test_that("we can add, parse and serialize json-ld", {
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






unlink(out)

