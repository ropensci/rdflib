library(nycflights13)
library(tidyverse)
library(rdflib)
source(system.file("examples/as_rdf.R", package="rdflib"))

## Must use BDB to successfully import all of flights 
options(rdflib_storage = "BDB")

## Tidyverse Style
df <- flights %>% 
  left_join(airlines) %>%
  left_join(planes, by="tailnum") %>% 
  select(carrier, manufacturer, model)


## generic list-based conversion via JSON-LD 
rdf_planes_from_list <- as_rdf.list(planes)

as_uri <- function(x, base_uri = "x:") paste0("<", base_uri, x, ">")

if(!rdf_has_bdb()){
flights <- flights %>% 
  filter(distance > 2500) # try smaller dataset
}

flights <- flights %>% 
  mutate(tailnum = as_uri(tailnum),
         carrier = as_uri(carrier)) -> flights

## New method, poor-mans-quads, < 0.5s, mostly gather
x1 <- as_rdf(airlines, "carrier", "x:")
x2 <- as_rdf(airports, "faa", "x:") ## a few funny chars, UTF8 issues?
x3 <- as_rdf(planes,  "tailnum", "x:")


sparql <-
  'SELECT  ?seats ?model ?tailnum
WHERE {
 ?tailnum <x:seats>  ?seats .
 ?tailnum <x:model>  ?model .
}'

out <- rdf_query(x3, sparql)


# 165 seconds if this is the full table
system.time(
    x4 <- as_rdf(flights, NULL, "x:")
)

## nope, the jsonld method appears to crash R...
#system.time(
#  x4 <- as_rdf.list(na.omit(flights))
#)

## we can join all of these:  
rdf <- c(x4,x1,x2,x3)

#}



## separate queries:
sparql <-
'SELECT  ?tailnum ?dep_delay ?carrier
WHERE { 
  ?flight <x:tailnum>  ?tailnum .
  ?flight <x:dep_delay>  ?dep_delay .
  ?flight <x:carrier>  ?carrier 
}'
f1 <- rdf_query(rdf, sparql)

sparql <-
  'SELECT  ?tailnum ?model ?manufacturer
WHERE {
?tailnum <x:manufacturer> ?manufacturer .
?tailnum <x:model> ?model
}'
f2 <- rdf_query(rdf, sparql)

tmp <- inner_join(f1,f2)

## Can we get this to work in one single sparql Query?
## Why not working?

s <- 
  'SELECT  ?dep_delay ?tailnum ?manufacturer
WHERE {
?flight <x:tailnum>  ?tailnum .
?tailnum <x:manufacturer> ?manufacturer .
?tailnum <x:model> ?model 
?flight <x:dep_delay>  ?dep_delay .
}'

out2 <- rdf_query(rdf, s)
out2

### DEBUG



s <- 
'SELECT ?tailnum
WHERE {
  ?flight <x:tailnum>  ?tailnum .
  ?flight <x:dep_delay>  11 
}'

rdf_query(rdf, s)


