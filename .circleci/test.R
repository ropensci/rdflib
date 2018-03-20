
system("virtuoso-t -c /etc/virtuoso-opensource-6.1/virtuoso.ini")
devtools::load_all()
x <- testthat::test_file("tests/testthat/test-rdf_storage.R")
testthat::expect_length(x$errors,0)
testthat::expect_length(x$warnings,0)

