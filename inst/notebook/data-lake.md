Data Lake RDF
================

``` r
library(nycflights13)
library(tidyverse)
library(rdflib)

## experimental methods
source(system.file("examples/as_rdf.R", package="rdflib"))
```

``` r
#options(rdflib_storage = "BDB") ## may also be much slower...
options(rdflib_storage = "memory") 
```

``` r
# Use a smaller dataset if we do not have a BDB backend: 
if(getOption("rdflib_storage") != "BDB"){
flights <- flights %>% 
  filter(distance > 2600) # try smaller dataset
}
```

Tidyverse Style
---------------

Operations in `dplyr` on the `nyflights13` dataset are easy to write and fast to execute

``` r
df <- flights %>% 
  left_join(airlines) %>%
  left_join(planes, by="tailnum") %>% 
  select(carrier, name, manufacturer, model) %>% 
  distinct()
head(df)
```

    ## # A tibble: 4 x 4
    ##   carrier name                   manufacturer model    
    ##   <chr>   <chr>                  <chr>        <chr>    
    ## 1 HA      Hawaiian Airlines Inc. AIRBUS       A330-243 
    ## 2 UA      United Air Lines Inc.  BOEING       767-424ER
    ## 3 UA      United Air Lines Inc.  <NA>         <NA>     
    ## 4 UA      United Air Lines Inc.  BOEING       757-222

RDF Data Lake
-------------

In RDF, we simply toss all of our data into the triplestore, or to use a more evocative metaphor, the "Data Lake." We can then extract whatever tabular structure we need by querying the data lake using SPARQL, something sometimes referred to as "schema-on-read," since we are specifying the desired format of the data when we pull it out of the lake.

This can serve as a very effective means of data integration (provided a reasonably conistent and dilgent use of URIs in identifying subjects and properties (predicates)), since just about any data can be added to the lake without worrying about whether it comes in a schema that matches the existing architecture of the database. It is this flexibility not to have to define your database schema at the start that is the primary strength of the RDF approach.

Okay, let's dump the `nyflights13` into the data lake. First, the foreign keys in any table must be represented as URIs and not literal strings:

``` r
as_uri <- function(x, base_uri = "x:") paste0(base_uri, x)
uri_flights <- flights %>% 
  mutate(tailnum = as_uri(tailnum),
         carrier = as_uri(carrier))
```

Similiarly, when reading into RDF we have to declare the key column for the table, and again establish a `base_uri` which will allow RDF methods to distinguish between URIs (subjects, predicates, and foreign keys) and literal strings.

``` r
system.time(
  
rdf <- c(
  as_rdf(airlines, "carrier", "x:"),
  as_rdf(planes,  "tailnum", "x:"),
  as_rdf(uri_flights, NULL, "x:"))

)
```

Note that flights does not have a natural key (somewhat surprisingly, `flight` number is not a unique key for this table, as the same flight number is reused on the same route at different times.) So, we will treat each row as a unique anonymous key by setting the key to `NULL`.

Schema on read
--------------

We simply define the columns we want and we immediately get back the desired `data.frame`:

``` r
s <- 
  'SELECT  ?carrier ?name ?manufacturer ?model ?dep_delay
WHERE {
?flight <x:tailnum>  ?tailnum .
?flight <x:carrier>  ?carrier .
?flight <x:dep_delay>  ?dep_delay .
?tailnum <x:manufacturer> ?manufacturer .
?tailnum <x:model> ?model .
?carrier <x:name> ?name
}'

system.time(
df <- rdf_query(rdf, s)
)
```

    ##    user  system elapsed 
    ##   0.263   0.015   0.310

``` r
head(df)
```

    ## # A tibble: 6 x 5
    ##   carrier name                   manufacturer model     dep_delay
    ##   <chr>   <chr>                  <chr>        <chr>         <int>
    ## 1 x:HA    Hawaiian Airlines Inc. AIRBUS       A330-243          6
    ## 2 x:HA    Hawaiian Airlines Inc. AIRBUS       A330-243         14
    ## 3 x:UA    United Air Lines Inc.  BOEING       767-424ER        18
    ## 4 x:UA    United Air Lines Inc.  BOEING       767-424ER         1
    ## 5 x:HA    Hawaiian Airlines Inc. AIRBUS       A330-243        - 8
    ## 6 x:UA    United Air Lines Inc.  BOEING       767-424ER         2

Note that in place of joins, we give more semantically meaningful statements about the data: e.g. `manufacturer` is a property of a `tailnum` (corresponding to a particular physical aircraft), not of a `flight` number. Departure delay `dep_delay` is a property of a flight, not of an aircraft (`tailnum`).

This is reminiscent of the way in which these data are organized in the relational database tables to begin with: we find `deb_delay` in the `flights` table and `manufacturer` in the `planes` table. Good relational design encourages this, but to work with the data the user is often left having to do the required joins, which also creates tables where these semantics are less clear.

Tabular formats can often be sloppy about what is a key and what is a literal value, and also whether a column with the same name in different tables means the same thing in both. Both of these things pose challenges for later use when joining data. RDF representation encourages greater discipline through the use of URIs (though we've run a bit roughshod over that with the cavilier use of `x:` here.)

Non-tabular data
----------------

``` r
f <- system.file("extdata/gh_repos.json", package="repurrrsive")
gh_data <- jsonlite::read_json(f)
```

Tidyverse style
---------------

``` r
gh_flat <- gh_data %>% flatten()  # abandon nested structure and hope we didn't need it

gh_tibble <- tibble(
  name =     gh_flat %>% map_chr("name"),
  issues =   gh_flat %>% map_int("open_issues_count"),
  wiki =     gh_flat %>% map_lgl("has_wiki"),
  homepage = gh_flat %>% map_chr("homepage", .default = ""),
  owner =    gh_flat %>% map_chr(c("owner", "login"))
)

gh_tibble %>% arrange(name) %>% head()
```

    ## # A tibble: 6 x 5
    ##   name                         issues wiki  homepage owner  
    ##   <chr>                         <int> <lgl> <chr>    <chr>  
    ## 1 2013-11_sfu                       1 T     ""       jennybc
    ## 2 2014-01-27-miami                  4 T     ""       jennybc
    ## 3 2014-05-12-ubc                    1 T     ""       jennybc
    ## 4 2015_Coartic                      0 F     ""       jennybc
    ## 5 2015-02-23_bryan-fields-talk      0 T     ""       jennybc
    ## 6 2015-06-28_r-summit-talk          0 T     ""       jennybc

RDF on non-tabular data
-----------------------

``` r
gh_rdf <- as_rdf.list(gh_data, '"@vocab": "gh:"')
```

We can add it to the lake just for kicks:

``` r
system.time(
rdf <- c(rdf, gh_rdf)
)
```

    ##    user  system elapsed 
    ##   2.954   0.060   3.217

And we can query it back out of the lake

``` r
s <- 
  'SELECT ?name ?issues ?wiki ?homepage ?owner
WHERE {
?repo <gh:homepage>  ?homepage .
?repo <gh:has_wiki> ?wiki .
?repo <gh:open_issues_count> ?issues .
?repo <gh:name> ?name .
?repo <gh:owner> ?owner_id .
?owner_id <gh:login>  ?owner 
}'

system.time(
rdf_tibble <- rdf_query(rdf, s)
)
```

    ##    user  system elapsed 
    ##   0.355   0.006   0.367

``` r
head(rdf_tibble)
```

    ## # A tibble: 6 x 5
    ##   name            issues wiki  homepage                             owner 
    ##   <chr>            <int> <chr> <chr>                                <chr> 
    ## 1 dvn                  0 true  http://cran.r-project.org/web/packa… leeper
    ## 2 catan_card_game      0 true  _:r1518895207r16721r436              masal…
    ## 3 EML                  0 true  <NA>                                 masal…
    ## 4 batman               0 true  <NA>                                 leeper
    ## 5 2015_Coartic         0 false <NA>                                 jenny…
    ## 6 CoffeeCoop           0 true  _:r1518895207r16721r413              jenny…
