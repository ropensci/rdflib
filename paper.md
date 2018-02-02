---
title: 'Tools to Manipulate and Query Semantic Data'
tags:
 - linked data
 - rdf
 - sparql
 - semantic
 - json-ld
authors:
 - name: Carl Boettiger
   orcid: 0000-0002-1642-628X
   affiliation: 1
affiliations:
 - name: University of California, Berkeley
   index: 1
date: 2017-12-11
bibliography: paper.bib
---

# Summary

The Resource Description Framework, or RDF [@RDF; @W3C_RDF] is a widely used
data representation model that forms the cornerstone of the 
Semantic Web. RDF represents data as a graph rather than 
the familiar data table or rectangle of relational databases.
The `rdflib` package provides a friendly and concise user interface
for performing common tasks on RDF data, such as reading, writing
and converting between the various serializations of RDF data,
including `rdfxml`, `turtle`, `nquads`, `ntriples`, and `json-ld`;
creating new `rdf` graphs, and performing graph queries using SPARQL [@SPARQL; @W3C_SPARQL].
This package wraps the low level `redland` R package [@redland] which
provides direct bindings to the redland C library.  Additionally,
the package supports the newer and more developer friendly
JSON-LD format through the `jsonld` package [@jsonld; @W3C_jsonld].

# References
