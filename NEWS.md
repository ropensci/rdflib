# rdflib 0.1.0

## New Features

* add `c()` method to concatenate `rdf` objects
* `rdf()` supports BDB backend for disk-based storage for large triplestores [#6](https://github.com/cboettig/rdflib/issues/6)

## Minor Improvements

* Numerous improvements to documentation, see 
  [#9](https://github.com/cboettig/rdflib/issues/9) and 
  [#10](https://github.com/cboettig/rdflib/issues/10) 

## Bug Fixes 

* fix encoding with UTF-8 characters (coming from nquads & ntriples)
* `rdf_query` now coerces data into appropriate type 
   if it recognizes the data URI and can match that 
   to an R type (a few XMLSchema types are recognized,
   otherwise still defaults to character string)
* Memory management: All methods free memory from any 
  temporary objects they initialize, tests free memory.
  (e.g. parsers, serializers, query, statement)
* extend unit tests to cover new features
* `turtle` parser/serializer fixed

## Deprecated

* `trig` support removed (was never working in redland)


# rdflib 0.0.3 (2018-01-02)

## Bug Fixes

* add paper.md
* add package level documentation
* set base uri when serializing json-ld to rdf ([#5](https://github.com/cboettig/rdflib/issues/5))


# rdflib 0.0.2 (2018-01-02)

## New Features

* Added a `NEWS.md` file to track changes to the package.
* sparql query returns a data.frame format
* added a vignette
* added pkgdown website for vignette

# rdflib 0.0.1 (2017-12-09)

* Initial prototype


