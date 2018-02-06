doc <- system.file("extdata/example.rdf", package="redland")
out <- "testing.rdf"


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

testthat::test_that("SPARQL handles data types", {
  
  rdf <- rdf()
  rdf_add(rdf, "", "ex:integer", 33L)
  rdf_add(rdf, "", "ex:decimal", 3.141)
  rdf_add(rdf, "", "ex:decimal", 2.718)
  rdf_add(rdf, "", "ex:logical", TRUE)
  rdf_add(rdf, "", "ex:Date", Sys.Date())
  rdf_add(rdf, "", "ex:POSIXct", Sys.time())
  rdf_add(rdf, "", "ex:factor", as.factor("text"))
  rdf_add(rdf, "", "ex:string", "text")
  
  testthat::expect_is(rdf, "rdf")
  match <- rdf_query(rdf, 'SELECT ?o WHERE { ?s <ex:Date> ?o }')
  testthat::expect_type(match$o[[1]], "double")
  testthat::expect_is(match$o[[1]], "Date")
  
  match <- rdf_query(rdf, 'SELECT ?o WHERE { ?s <ex:POSIXct> ?o }')
  testthat::expect_is(match$o[[1]], "POSIXct")
  testthat::expect_type(match$o[[1]], "double")
  
  match <- rdf_query(rdf, 'SELECT ?o WHERE { ?s <ex:decimal> ?o }')
  testthat::expect_is(match$o[[1]], "numeric")
  testthat::expect_type(match$o[[1]], "double")
  
  match <- rdf_query(rdf, 'SELECT ?o WHERE { ?s <ex:logical> ?o }')
  testthat::expect_is(match$o[[1]], "logical")
  testthat::expect_type(match$o[[1]], "logical")
  
  
  match <- rdf_query(rdf, 'SELECT ?o WHERE { ?s <ex:integer> ?o }')
  testthat::expect_is(match$o[[1]], "integer")
  testthat::expect_type(match$o[[1]], "integer")
  
  
  match <- rdf_query(rdf, 'SELECT ?o WHERE { ?s <ex:string> ?o }')
  testthat::expect_is(match$o[[1]], "character")
  testthat::expect_type(match$o[[1]], "character")
  
  

  
  
  ## Matching mixed type results in all types treated as character
  # vector, since o is a single column....
  match <- rdf_query(rdf, 'SELECT ?p ?o WHERE { ?s ?p ?o }')
  testthat::expect_is(match$o, "character")
  
  testthat::expect_is(match, "data.frame")
  
  match <- rdf_query(rdf, 'SELECT ?p ?o WHERE { ?s ?p ?o }', data.frame=FALSE)
  testthat::expect_is(match, "list")
  
  rdf_free(rdf)
  
})

unlink(out)


