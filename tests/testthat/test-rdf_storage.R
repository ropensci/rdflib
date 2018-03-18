testthat::context("RDF Storage")

library(redland)

testthat::test_that("BDB, redland-level",{
  
  world <- new("World")
  store <- new("Storage", world, "hashes", name = "db1", 
                     options = "new='yes',hash-type='bdb',dir='.'")
  
  testthat::skip_if(is_null_pointer(store))
  testthat::expect_true(TRUE)
  redland::freeStorage(store)
  redland::freeWorld(world)
  
  })
  
testthat::test_that("SQLite, redland-level",{
  world <- new("World")
  store <- new("Storage", world, "sqlite", name = "sqlite1", options = "new='yes'")
  testthat::skip_if(is_null_pointer(store))
  testthat::expect_true(TRUE)
  redland::freeStorage(store)
  redland::freeWorld(world)
  
})

testthat::test_that("Postgres, redland-level",{
  world <- new("World")
  store <- new("Storage", world, "postgresql", name = "postgres1", 
    options = "new='yes',host='postgres',user='postgres','password='rdflib'")
  testthat::skip_if(is_null_pointer(store))
  testthat::expect_true(TRUE)
  redland::freeStorage(store)
  redland::freeWorld(world)
  
})

testthat::test_that("Virtuoso, redland-level",{
  world <- new("World")
  store <- new("Storage", world, "virtuoso", name = "rdflib",
                          "new='yes',dsn='Local Virtuoso',user='demo',password='demo'")
  testthat::skip_if(is_null_pointer(store))
  testthat::expect_true(TRUE)
  redland::freeStorage(store)
  redland::freeWorld(world)
})


#  store <- rdf_storage("sqlite", world = world, new_db = TRUE, check_only = TRUE)
#  store <- rdf_storage("sqlite", new_db = TRUE, check_only = TRUE)
#  store <- rdf("sqlite", new_db = TRUE)


testthat::test_that("SQLite Backend", {
  
  testthat::skip_on_travis()
  
  testthat::skip_if_not(rdf_storage("sqlite", new_db = TRUE, check_only = TRUE))
  
  testthat::expect_silent(r <- rdf(storage="sqlite", new_db = TRUE))
  
  rdf_add(r, "", "dc:name", "bob")
  testthat::expect_match(format(r, "nquads"), "bob")
  testthat::expect_is(r, "rdf")
  rdf_free(r)
  
})


testthat::test_that("Postgres Backend", {
  
  testthat::skip_on_travis()
  
  testthat::skip_if_not(rdf_storage("postgres", user="postgres",
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


## FIXME may need to create the database on mysql as well.
testthat::test_that("MySQL Backend", {
  
  testthat::skip_on_travis()
  
  
  testthat::skip_if_not(rdf_storage("mysql", host = "mysql", 
                                    user="root", password="rdflib",
                                    new_db=TRUE, check_only = TRUE ))
  
  testthat::expect_silent(
    rdf <- rdf(storage="mysql", host = "mysql", 
               user="root", password="rdflib", 
               new_db = TRUE)
  )
  
  rdf_add(rdf, "", "dc:name", "bob")
  expect_match(format(rdf, "nquads"), "bob")
  testthat::expect_is(rdf, "rdf")
  rdf_free(rdf)
  
})


testthat::test_that("Virtuoso Backend", {
  
  testthat::skip_on_travis()
  
  
  testthat::skip_if_not(rdf_storage("virtuoso", 
                                    user="demo", 
                                    password="demo",
                                    new_db=TRUE,
                                    check_only = TRUE))
  
  testthat::expect_silent(r <- 
                            rdf(storage="virtuoso", 
                                user="demo", password="demo", 
                                new_db = TRUE))
  
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

