as_rdf <- function(df, base_uri = NULL) UseMethod("as_rdf")

as_rdf.data.frame <- function(df, base_uri = NULL){

  subjectType <- "blank"
  if(is.null(base_uri)){
    base_uri <- paste0(gsub("[^a-zA-Z]","",deparse(substitute(df))), ":")
    subjectType <- "uri"
  }
  
  x <- tibble::rowid_to_column(df, "subject")
  suppressWarnings(
  x <- tidyr::gather(x, key = predicate, value = object, -subject)
  )
  ## Add a column for the data type
  col_classes <- data.frame(datatype = vapply(df, xs_class, character(1)))
  col_classes <- tibble::rownames_to_column(col_classes, "predicate")
  x <- dplyr::inner_join(x, col_classes, "predicate")
  
  rdf <- rdf()
  for(i in seq_along(x$subject)){
    rdf <- rdf_add(rdf, 
                   subject = paste0(base_uri,as.character(x$subject[[i]])),
                   predicate = paste0(base_uri, x$predicate[[i]]),
                   object = as.character(x$object[[i]]),
                   subjectType = subjectType,
                   datatype_uri = x$datatype[[i]])
  }
  rdf
}

xs_class <- function(x){
  gsub("^xs:", 
       "http://www.w3.org/2001/XMLSchema#",
    switch(class(x),
         "numeric" = "xs:decimal",
         "character" = "xs:string",
         "factor" = "xs:string",
         "logical" = "xs:boolean",
         "integer" = "xs:integer",
         "Date" = "xs:date",
         "POSIXct" = "xs:dateTime",
         NULL
  ))
}


as_rdf.list <- function(x){
  
}

cars <- mtcars[1:4, 1:4] %>% rownames_to_column("Model")


x1 <- as_rdf(iris)
x2 <- as_rdf(cars)
rdf <- c(x1,x2)


sparql <-
  'SELECT ?Sepal_Length ?Sepal_Width ?Petal_Length ?Petal_Width ?Species
   WHERE {
      ?s <iris:Sepal.Width>  ?Sepal_Width . 
      ?s <iris:Sepal.Length>  ?Sepal_Length . 
      ?s <iris:Petal.Width>  ?Petal_Width . 
      ?s <iris:Petal.Length>  ?Petal_Length . 
      ?s <iris:Species>  ?Species . 
  }'
tmp <- rdf_query(rdf, sparql)
