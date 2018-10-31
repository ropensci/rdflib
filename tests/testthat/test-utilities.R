context("utilities")

test_that("we can correctly set prefix formats", {

  uri_prefix(  "http://namespace.com/" )
  uri_prefix(  "http://namespace.com/terms#" )
  
  expect_match( uri_prefix( "schema" ), "schema:")
  expect_match( uri_prefix( "schema:" ), "schema:") 
})