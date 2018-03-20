
Sys.sleep(5)
system("virtuoso-t -c /etc/virtuoso-opensource-6.1/virtuoso.ini")
Sys.sleep(5)
devtools::load_all()
library("testthat")
## cannot catch errors with test_file()
source("tests/testthat/test-rdf_storage.R")

