# rdflib 0.1.0

* add `c()` method to concatenate `rdf` objects
* `rdf_query` now coerces data into appropriate type 
   if it recognizes the data URI and can match that 
   to an R type (a few XMLSchema types are recognized,
   otherwise still defaults to character string)
* All methods free memory from any temporary objects they initialize
  (e.g. parsers, serializers, query, statement)
* rdf includes explicit pointer to storage object
* rdf constructor supports BDB backend for disk-based triplestore [#6](https://github.com/cboettig/rdflib/issues/6)
* tests free rdf objects
* extend unit tests for some of new functionality
* Add `rdf_free` to free rdf (ideally would be done by GC in redland...)

# rdflib 0.0.3 (2018-01-02)

## New Features

* add paper.md
* add package level documentation

## Bug Fixes

* set base uri when serializing json-ld to rdf ([#5](https://github.com/cboettig/rdflib/issues/5))


# rdflib 0.0.2 (2018-01-02)

## New Features

* Added a `NEWS.md` file to track changes to the package.
* sparql query returns a data.frame format
* added a vignette
* added pkgdown website for vignette

# rdflib 0.0.1 (2017-12-09)

* Initial prototype


