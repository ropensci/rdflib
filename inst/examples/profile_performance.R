
library(nycflights13)
library(tidyverse)
library(rdflib)
source(system.file("examples/as_rdf.R", package="rdflib"))


## Tidyverse Style
df <- flights %>% 
  left_join(airlines) %>%
  left_join(planes, by="tailnum") %>% 
  select(carrier, manufacturer, model)

## JSON-LD approach -- 5 sec, 
## ~ all rdf_parse
## ~ all of which is jsonld_expand()
profvis::profvis(
  x2 <- as_rdf.list(airports)
)


## rdf_add approach 12 sec
## ~ all of which is rdf_add
## ~ all of which is calls to new / initialize S4 method
#3 ~ none of which is calls to the low level librdf_* C calls
profvis::profvis(
x2 <- as_rdf(airports,  "faa", "x:")
)



x1 <- as_rdf(airlines, "carrier", "x:")
x3 <- as_rdf(planes, "tailnum", "x:")

system.time(
  x4 <- as_rdf(flights, NULL, "x:")
)

rdf <- c(x1,x2,x3)
