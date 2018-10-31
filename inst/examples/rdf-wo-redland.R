library(nycflights13)
library(dplyr)

dim(flights) # 336,776 x 19 

as_uri <- function(x, base_uri = "x:") paste0(base_uri, x)
uri_flights <- flights %>% 
  mutate(tailnum = as_uri(tailnum),
         carrier = as_uri(carrier))

  
df1 <- rdflib:::normalize_table(airlines, key = "carrier", vocab = "x:")
df2 <- rdflib:::normalize_table(planes,   key = "tailnum", vocab = "x:")
df3 <- rdflib:::normalize_table(uri_flights, key = NULL, vocab = "x:")

df <- bind_rows(df1,df2,df3)

dim(df3) # 6,398,744 x 4
  
df %>% 
  filter(predicate %in% c("carrier", "name", "manufacturer", "model", "dep_delay")) %>% 
  count(predicate)

## We can recover an individual table
df %>% 
  filter(predicate %in% c("manufacturer", "model")) %>% 
  select(subject, predicate, object) %>% 
  tidyr::spread(predicate, object) %>% 
  rename(tailnum = subject) ## Also need to apply datatype...
## ... and join manually....


## What about recovering the "joined" table?
df %>% 
  filter(predicate %in% c("carrier", "name", "manufacturer", "model", "dep_delay")) %>%
  select(subject, predicate, object) %>% tidyr::spread(predicate, object) %>% 
  filter(!is.na(carrier))


## Compare to pure approach on original tables
flights %>% 
  left_join(airlines, by = "carrier") %>%
  left_join(planes, by = "tailnum") %>% 
  select(carrier, name, manufacturer, model, dep_delay) %>% 
  distinct()

#### All fits in memory anyway
#library(MonetDBLite)
#library(DBI)
#triplestore <- rappdirs::user_data_dir("rdflib")
#con <- dbConnect(MonetDBLite::MonetDBLite(), triplestore)

# Could use append=TRUE instead to extend triplestore later
#DBI::dbWriteTable(con, "flights", df, overwrite = TRUE)  


## size as flat files on disk:
readr::write_tsv(flights, "flights.tsv")    # 29.6 MB
readr::write_tsv(df, "triplestore.tsv")     # 319 MB
readr::write_tsv(df, "triplestore.tsv.bz2") # 17 MB


