library(DBI)
library(odbc)

virtuoso <- dbConnect(odbc::odbc(),
                      driver = "Virtuoso",
                      uid = "dba",
                      pwd = "dba",
                      host = "virtuoso",
                      port = "1111")

## It's alive!
virtuoso

## Bulk import -- fast!
dbGetQuery(virtuoso, "ld_dir('/var/data/', '*.nq', 'rdflib')" )
dbGetQuery(virtuoso, "rdf_loader_run()" )


## List all graphs -- look, we have one called 'rdflib' now!
dbGetQuery(virtuoso,
           "SPARQL SELECT  DISTINCT ?g WHERE { GRAPH ?g {?s ?p ?o} } ORDER BY ?g"
)


## Select FROM GRAPH
dbGetQuery(virtuoso, "SPARQL SELECT * FROM <rdflib> WHERE { {?s ?p ?o} } LIMIT 10")

dbGetQuery(virtuoso, "SPARQL SELECT * FROM <rdflib> WHERE { {?s <flights:dep_delay> ?o} } LIMIT 10")

## Wow, we can write standard tables into Virtuoso too...
#dbWriteTable(virtuoso, "iris", iris) # Dots not allowed in column names
dbWriteTable(virtuoso, "mtcars", mtcars)
dbListTables(virtuoso, table_name = "mt%")
dbReadTable(virtuoso, "mtcars")


## Clear graph <rdflib>
dbGetQuery(virtuoso, "SPARQL CLEAR GRAPH <rdflib>")


## Counting triples in each different graph...
q <- "SPARQL SELECT ?g ?s ?p ?o  WHERE { GRAPH ?g {?s ?p ?o} }"
df <- dbGetQuery(virtuoso, q)
library(dplyr)
df %>% count(g)



#remotes::install_github("ropensci/rdflib")
library(rdflib)
library(nycflights13)
library(dplyr)
## prefix foreign keys
uri_flights <- flights %>% 
  mutate(tailnum = paste0("planes:", tailnum),
         carrier = paste0("airlines:", carrier))

h <- here::here("data/rdflib/inst/extdata")
write_nquads(airlines, file.path(h, "airlines.nq"),  key = "carrier", prefix = "airlines:")
write_nquads(planes, file.path(h, "planes.nq"),   key = "tailnum", prefix = "planes:")
write_nquads(uri_flights,  file.path(h, "uri_flights.nq"), prefix = "flights:")

## Bulk import -- fast!
dbGetQuery(virtuoso, "ld_dir('/var/data/', '*.nq', 'rdflib')" )
dbGetQuery(virtuoso, "rdf_loader_run()" )





remotes::install_github("ropensci/rdflib")
library(rdflib)
triplestore <- rdf(storage = "virtuoso", 
                   user = "dba", 
                   password = "dba",
                   host = "virtuoso:1111"
)
triplestore
#write_nquads(iris, "iris.nq", prefix = "iris")
#read_nquads("iris.nq", rdf = triplestore)
## Works!
triplestore

## We can query with rdflib too!
query <- "SELECT ?s ?p ?o WHERE {?s ?p ?o } LIMIT 10"
out <- rdf_query(triplestore, query)
rdf_free(triplestore)




