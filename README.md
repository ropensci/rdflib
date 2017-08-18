
[![Travis-CI Build Status](https://travis-ci.org/cboettig/rdflib.svg?branch=master)](https://travis-ci.org/cboettig/rdflib) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/cboettig/rdflib?branch=master&svg=true)](https://ci.appveyor.com/project/cboettig/rdflib) [![Coverage Status](https://img.shields.io/codecov/c/github/cboettig/rdflib/master.svg)](https://codecov.io/github/cboettig/rdflib?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rdflib)](https://cran.r-project.org/package=rdflib)

<!-- README.md is generated from README.Rmd. Please edit that file -->
rdflib
======

A friendly and consise user interface for performing common tasks on rdf data, such as parsing and converting between formats including rdfxml, turtle, nquads, ntriples, and trig, creating rdf graphs, and performing SPARQL queries. This package wraps the redland R package which provides direct bindings to the redland C library. Additionally, the package supports parsing and serialization of rdf into json-ld through the json-ld package, which binds the official json-ld javascript API. The package interface takes inspiration from the Python rdflib library.

Installation
------------

You can install rdflib from github with:

``` r
# install.packages("devtools")
devtools::install_github("cboettig/rdflib")
```

Basic use
---------

While not required, `rdflib` is designed to play nicely with `%>%` pipes, so we will load the `magrittr` package as well:

``` r
library(magrittr)
library(rdflib)
```

Parse a file and serialize into a different format:

``` r
doc <- system.file("extdata", "dc.rdf", package="redland")

doc %>%
  rdf_parse() %>%
  rdf_serialize("test.nquads", "nquads")
```

Perform SPARQL queries:

``` r
sparql <-
 'PREFIX dc: <http://purl.org/dc/elements/1.1/>
  SELECT ?a ?c
  WHERE { ?a dc:creator ?c . }'

rdf <- rdf_parse(doc)
rdf
#> <http://www.dajobe.org/> <http://purl.org/dc/elements/1.1/description> "The generic home page of Dave Beckett." .
#> <http://www.dajobe.org/> <http://purl.org/dc/elements/1.1/title> "Dave Beckett's Home Page" .
#> <http://www.dajobe.org/> <http://purl.org/dc/elements/1.1/creator> "Dave Beckett" .


rdf %>% rdf_query(sparql)
#> $a
#> [1] "<http://www.dajobe.org/>"
#> 
#> $c
#> [1] "\"Dave Beckett\""
```

Initialize graph a new object or add triples statements to an existing graph:

``` r
x <- rdf()
x <- rdf_add(x, 
    subject="http://www.dajobe.org/",
    predicate="http://purl.org/dc/elements/1.1/language",
    object="en")
x
#> <http://www.dajobe.org/> <http://purl.org/dc/elements/1.1/language> "en" .
```

Change the default display format (`nquads`) for graph objects:

``` r
options(rdf_print_format = "jsonld")
x
#> [
#>   {
#>     "@id": "http://www.dajobe.org/",
#>     "http://purl.org/dc/elements/1.1/language": [
#>       {
#>         "@value": "en"
#>       }
#>     ]
#>   }
#> ]
```

JSON-LD
-------

We can also work with the JSON-LD format through additional functions provided in the R package, `jsonld`.

``` r
out <- tempfile()
rdf_serialize(x, out, "jsonld")
rdf_parse(out, format = "jsonld")
#> [
#>   {
#>     "@id": "http://www.dajobe.org/",
#>     "http://purl.org/dc/elements/1.1/language": [
#>       {
#>         "@value": "en"
#>       }
#>     ]
#>   }
#> ]
```

For more information on the JSON-LD RDF API, see <https://json-ld.org/spec/latest/json-ld-rdf/>.
