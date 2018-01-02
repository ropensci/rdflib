## Take any rdf and map into a table

sparql <-'
  SELECT ?subject ?predicate ?object
  WHERE { ?subject ?predicate ?object . }
'
doc <- system.file("extdata", "dc.rdf", package="redland")
rdf <- rdf_parse(doc)
df <- rdf_query(rdf, sparql)
df



## Take a table of triples (columns: id, property, value)
## and create a table with ids as rows and properties as columns (values as cell values)

spread_triples <- function(df){
  df %>%
    select(subject, predicate, object) %>%
    group_by(subject, predicate) %>%
    mutate(key = row_number()) %>%
    spread(predicate, object) %>%
    select(-key) %>%
    fill(-subject) %>%  ## Fill in NA based on known properties for id, needs group_by id
    ungroup()
}

df %>%
  mutate(predicate = gsub("<\\w+://.*/(\\w+)>$", "\\1", predicate)) %>%
spread_triples()




## a method based on parsing nquads, may not be the easiest serialization to tabularize.
## but works with just jsonld_to_rdf without any use of redland
library(tidyverse)
tabularize <- function(file){
  ## Express type string inside "" so we can parse
  read_lines(file) %>% str_replace("\"\\^\\^(.*)>", "\\^\\^\\1\"") %>% write_lines(file)
  df <- readr::read_delim(file, delim = " ", col_names = FALSE, col_types = "cccc")
  names(df) <- c("id", "property", "value", "about")
  df %>%
    mutate(property = gsub("<http://ecoinformatics.org/eml-2.1.1/(.*)>", "\\1", property)) %>%
    separate(value, c("value", "type"), sep="\\^\\^<", fill="right")
}
