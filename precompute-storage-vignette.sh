#!/bin/bash
echo '\n
Sys.sleep(5)
system("virtuoso-t -c /etc/virtuoso-opensource-6.1/virtuoso.ini")
Sys.sleep(5)
knitr::knit("vignettes/articles/storage.Rmd.orig", output = "vignettes/articles/storage.Rmd")' > precompute.R


docker-compose run rdflib R -f precompute.R 
rm precompute.R
