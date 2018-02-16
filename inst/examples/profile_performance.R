
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


## New method, poor-mans-quads, much faster
system.time(
x3 <- as_rdf(planes,  "tailnum", "x:")
)



x1 <- as_rdf(airlines, "carrier", "x:")
x2 <- as_rdf(airports, "faa", "x:", loc = "quickquads.nq")

system.time(
    x4 <- as_rdf(flights, NULL, "x:", loc = "quickquads.nq")
)

## Nope, appears to crash R...
#system.time(
#  x4 <- as_rdf.list(na.omit(flights))
#)

rdf <- c(x1,x2,x3)
