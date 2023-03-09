
# rdflib <img src="man/figures/logo.svg" align="right" alt="" width="120" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/ropensci/rdflib/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci/rdflib/actions)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/rdflib)](https://cran.r-project.org/package=rdflib)
[![](http://badges.ropensci.org/169_status.svg)](https://github.com/ropensci/software-review/issues/169)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/rdflib)](https://CRAN.R-project.org/package=rdflib)
[![DOI](https://zenodo.org/badge/100521776.svg)](https://zenodo.org/badge/latestdoi/100521776)
<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

A friendly and consise user interface for performing common tasks on rdf
data, such as parsing and converting between formats including rdfxml,
turtle, nquads, ntriples, and trig, creating rdf graphs, and performing
SPARQL queries. This package wraps the redland R package which provides
direct bindings to the redland C library. Additionally, the package
supports parsing and serialization of rdf into json-ld through the
json-ld package, which binds the official json-ld javascript API. The
package interface takes inspiration from the Python rdflib library.

## Installation

You can install rdflib from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("ropensci/rdflib")
```

## Basic use

While not required, `rdflib` is designed to play nicely with `%>%`
pipes, so we will load the `magrittr` package as well:

``` r
library(magrittr)
library(rdflib)
```

Parse a file and serialize into a different format:

``` r
system.file("extdata/dc.rdf", package="redland") %>%
  rdf_parse() %>%
  rdf_serialize("test.nquads", "nquads")
```

Perform SPARQL queries:

``` r
sparql <-
 'PREFIX dc: <http://purl.org/dc/elements/1.1/>
  SELECT ?a ?c
  WHERE { ?a dc:creator ?c . }'

system.file("extdata/dc.rdf", package="redland") %>%
rdf_parse() %>%
rdf_query(sparql)
#> # A tibble: 1 × 2
#>   a                      c           
#>   <chr>                  <chr>       
#> 1 http://www.dajobe.org/ Dave Beckett
```

Initialize graph a new object or add triples statements to an existing
graph:

``` r
x <- rdf()
x <- rdf_add(x, 
    subject="http://www.dajobe.org/",
    predicate="http://purl.org/dc/elements/1.1/language",
    object="en")
x
#> Total of 1 triples, stored in hashes
#> -------------------------------
#> <http://www.dajobe.org/> <http://purl.org/dc/elements/1.1/language> "en" .
```

Change the default display format (`nquads`) for graph objects:

``` r
options(rdf_print_format = "jsonld")
x
#> Total of 1 triples, stored in hashes
#> -------------------------------
#> {
#>   "@id": "http://www.dajobe.org/",
#>   "http://purl.org/dc/elements/1.1/language": "en"
#> }
```

## JSON-LD

We can also work with the JSON-LD format through additional functions
provided in the R package, `jsonld`.

``` r
out <- tempfile()
rdf_serialize(x, out, "jsonld")
rdf_parse(out, format = "jsonld")
#> Total of 1 triples, stored in hashes
#> -------------------------------
#> {
#>   "@id": "http://www.dajobe.org/",
#>   "http://purl.org/dc/elements/1.1/language": "en"
#> }
```

For more information on the JSON-LD RDF API, see
<https://json-ld.org/spec/latest/json-ld-rdf/>.

## Advanced Use

See [articles](https://docs.ropensci.org/rdflib/articles/) from the
documentation for advanced use including applications to large
triplestores, example SPARQL queries, and information about additional
database backends.

------------------------------------------------------------------------

## Citing rdflib

Please also cite the underlying `redland` library when citing `rdflib`

Carl Boettiger. (2018). rdflib: A high level wrapper around the redland
package for common rdf applications (Version 0.1.0). Zenodo.
<https://doi.org/10.5281/zenodo.1098478>

Jones M, Slaughter P, Ooms J, Boettiger C, Chamberlain S (2022).
*redland: RDF Library Bindings in R*. <doi:10.5063/F1VM496B>
<https://doi.org/10.5063/F1VM496B>, R package version 1.0.17-16,
<https://github.com/ropensci/redland-bindings/tree/master/R/redland>.

[![rofooter](https://ropensci.org//public_images/github_footer.png)](https://ropensci.org/)
