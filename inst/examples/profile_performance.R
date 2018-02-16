
library(nycflights13)
library(tidyverse)
library(rdflib)
source(system.file("examples/as_rdf.R", package="rdflib"))


## Must use BDB to successfully import flights 
options(rdflib_storage = "BDB")

## Tidyverse Style
df <- flights %>% 
  left_join(airlines) %>%
  left_join(planes, by="tailnum") %>% 
  select(carrier, manufacturer, model)

## JSON-LD approach -- < 1 sec, 
## ~ all rdf_parse
## ~ all of which is inside jsonld_to_rdf()
system.time(
  x3 <- as_rdf.list(planes)
)


## rdf_add approach 33 sec
## ~ all of which is rdf_add
## ~ all of which is calls to S4 new() constructor
#3 ~ none of which is calls to the low level librdf_* C calls


## New method, poor-mans-quads, < 0.5s, mostly gather
system.time(
x3 <- as_rdf(planes,  "tailnum", "x:")
)

x1 <- as_rdf(airlines, "carrier", "x:")


x2 <- as_rdf(airports, "faa", "x:") ## a few funny chars, UTF8 issues?



sparql <-
  'SELECT  ?seats ?model ?tailnum
WHERE {
 ?tailnum <x:seats>  ?seats .
 ?tailnum <x:model>  ?model .
}'

profvis::profvis(
  out <- rdf_query(x3, sparql)
)



# 165 seconds
system.time(
    x4 <- as_rdf(flights, NULL, "x:")
)

## Nope, appears to crash R...
#system.time(
#  x4 <- as_rdf.list(na.omit(flights))
#)

rdf <- c(x4,x1,x2,x3)

## FIXME: foreign keys must be URIs




sparql <-
'SELECT  ?carrier ?tailnum ?dep_delay
WHERE {
 ?s <x:tailnum>  ?tailnum .
 ?s <x:dep_delay>  ?dep_delay .
 ?s <x:carrier>  ?carrier 

}'

system.time(
#out <- rdf_query(x4, sparql)
)
dim(out)