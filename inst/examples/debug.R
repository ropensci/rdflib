
library(redland)
world <- new("World")
storage <- new("Storage", world, "hashes", name="", 
               options="hash-type='memory'")
model <- new("Model", world, storage, options="")

stmt <- new("Statement", 
            world = world,  
            subject="", 
            predicate="http://schema.org/name", 
            object="Maëlle Salmon")
addStatement(model, stmt)

stmt <- new("Statement", 
            world = world,  
            subject="", 
            predicate="http://schema.org/name", 
            object="Matt Jones")
addStatement(model, stmt)

query <-'SELECT ?o WHERE { ?s ?p ?o}'
queryObj <- new("Query", world, query)
queryResult <- executeQuery(queryObj, model)
r <-getNextResult(queryResult)
r

## These two fail to encode UTF-8, I get "Ma\u00EBlle" not Maëlle
serializer <- new("Serializer", world, name = "nquads", 
                  mimeType = "text/x-nquads")
redland::serializeToFile(serializer, world, model, "test.rdf")
cat(readLines("test.rdf"))

serializer <- new("Serializer", world, name = "ntriples", 
                  mimeType = "application/n-triples")
redland::serializeToFile(serializer, world, model, "test.rdf")
cat(readLines("test.rdf"))

## As expected here
serializer <- new("Serializer", world, name = "turtle", 
                  mimeType = "text/turtle")
redland::serializeToFile(serializer, world, model, "test.rdf")
cat(readLines("test.rdf"))

serializer <- new("Serializer", world)
redland::serializeToFile(serializer, world, model, "test.rdf")
cat(readLines("test.rdf"))


libary(rdflib)
r <- rdf()
rdf_add(r,
subject="", 
predicate="http://schema.org/name", 
object="Maëlle Salmon")

r


x <- "\"2018-02-05 11:54:10\"^^<http://www.w3.org/2001/XMLSchema#dateTime>"
type_by_datauri(x)

reprex::reprex({
  tbl <- tibble:::as_data_frame(list(col1  = list(Sys.Date(), "stuff")))
  df <- as.data.frame(tbl)
  df
  
  df[1,1]
})


get_values(x)
get_types(x)





a <- '\"some text\"'
b <- '"some text"'
identical(a,b) #TRUE

txt <- "\"Ma\\u00EBlle\""
stringi::stri_unescape_unicode(txt)

# match only interior quotes:
a <- '"some text with \"interior quotes\" and more text"'
gsub("(^.+(\").+$)", "\\2", a)
stringr::str_replace(a, )


