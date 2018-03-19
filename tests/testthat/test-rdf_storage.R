testthat::context("RDF Storage")

testthat::test_that("SQLite Backend", {
  testthat::skip_if_not(rdf_storage("sqlite", new_db = TRUE, 
                                    check_only = TRUE, name = "rdflib.sqlite"))
  testthat::expect_silent(r <- rdf(storage="sqlite", 
                                   new_db = TRUE,
                                   name="rdflib.sqlite"))
  rdf_add(r, "", "dc:name", "bob")
  testthat::expect_match(format(r, "nquads"), "bob")
  testthat::expect_is(r, "rdf")
  rdf_free(r)
  unlink("rdflib.sqlite")
  
  
})

testthat::test_that("Postgres Backend", {
  testthat::skip_on_travis()
  testthat::skip_if_not(rdf_storage("postgres", 
                                    host="postgres", user="postgres",
                                    password="rdflib", new_db = TRUE,
                                    check_only = TRUE))
  testthat::expect_silent(
    rdf <- rdf(storage="postgres", host = "postgres", 
               user="postgres", password="rdflib", new_db = TRUE)
  )
  
  rdf_add(rdf, "", "dc:name", "bob")
  testthat::expect_match(format(rdf, "nquads"), "bob")
  testthat::expect_is(rdf, "rdf")
  rdf_free(rdf)
  
})


## Note: `mysql` is the name default database created by mariadb
testthat::test_that("MySQL Backend", {
  testthat::skip_on_travis()
  testthat::skip_if_not(rdf_storage("mysql", host = "mariadb", 
                                    user="root", password="rdflib",
                                    database = "mysql",
                                    new_db=TRUE, check_only = TRUE ))
  testthat::expect_silent(
    rdf <- rdf(storage="mysql", host = "mariadb", 
               user="root", password="rdflib", 
               database = "mysql",
               new_db = TRUE)
  )
  rdf_add(rdf, "", "dc:name", "bob")
  expect_match(format(rdf, "nquads"), "bob")
  testthat::expect_is(rdf, "rdf")
  rdf_free(rdf)
  
})


testthat::test_that("Virtuoso Backend", {
  testthat::skip("Virtuoso not tested")
  testthat::skip_on_travis()
  ## FIXME Can pass even when database not present
  testthat::skip_if_not(rdf_storage("virtuoso", 
                                    user="demo", 
                                    password="demo",
                                    new_db=TRUE,
                                    check_only = TRUE))
  testthat::expect_silent(
    r <- rdf(storage="virtuoso", user="demo", 
             password="demo",new_db = TRUE)
    )
  rdf_add(r, "", "dc:name", "bob")
  expect_match(format(r, "nquads"), "bob")
  testthat::expect_is(r, "rdf")
  rdf_free(r)
  
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
  
  # not sure why this is now failing on appveyor
  testthat::skip_on_os("windows")
  
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

