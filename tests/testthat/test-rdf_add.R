

testthat::test_that("we can print.rdf UTF-8", {
  
  r <- rdf()
  rdf_add(r,
          subject="", 
          predicate="http://schema.org/name", 
          object="Maëlle Salmon")
  
  testthat::expect_output(print(r), "Maëlle")
})



testthat::test_that("we can initialize add triples to rdf graph", {
  rdf <- rdf()
  rdf <- rdf_add(rdf, 
                 subject="http://www.dajobe.org/",
                 predicate="http://schema.org/dateCreated",
                 object=as.Date("2015-01-01"))
  testthat::expect_is(rdf, "rdf")
  rdf_free(rdf)
  
})

