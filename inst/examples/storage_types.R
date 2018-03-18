## Experimental testing of different storage backends in redland:

library(redland)
world <- new("World")

## BDB
bdb_storage <- new("Storage", world, "hashes", name = "db1", 
                      options = "new='yes',hash-type='bdb',dir='.'")
model <- new("Model", world = world, storage = bdb_storage, options = "")

## SQLITE 
sqlite_storage <- new("Storage", world, "sqlite", name = "sqlite1", options = "new='yes'")


## POSTGRES
## Needs postgres backend running
postgres_storage <- new("Storage", world, "postgresql", name = "postgres1", 
    options = "new='yes',host='postgres',user='postgres','password='rflib'")

## MYSQL
## Needs mysql backend running
mysql_store <- new("Storage", world, "mysql", name = "mysql1", 
                        options = "new='yes',host='mariadb',user='root',password='rdflib',database='db'")

## VIRTUOSO
## Needs virtuoso backend running
virtuoso_storage <- new("Storage", world, "virtuoso", name = "db1",
      "dsn='Local Virtuoso',user='demo',password='demo'")


## Works, in memory, serializes to an rdf/xml file called thing.rdf when freed.
## Not indexed, so will be slow. Suitable for small models.
file_storage <- new("Storage", world, "file", "thing.rdf", "")
storage <- file_storage
model <- new("Model", world = world, storage = storage, options = "")

## Works, fast write, not indexed, good for only small models, 
## no reason to use this instead of hash-based memory (which is indexed)
memory_storage <- new("Storage", world, "memory", "", "")
storage <- memory_storage
model <- new("Model", world = world, storage = storage, options = "")


## Testing
library(rdflib)
rdf <- structure(list(world = world, model = model, storage = storage),
          class = "rdf")
rdf_add(rdf, 
        subject="http://www.dajobe.org/",
        predicate="http://purl.org/dc/elements/1.1/language",
        object="en")
rdf
