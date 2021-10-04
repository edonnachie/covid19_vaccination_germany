library(tidyverse)
library(httr)
baseurl <-  "https://www-genesis.destatis.de/genesisWS/rest/2020/data/table"

query_params <- list(
  username="DEYE6IDDMH",
  password="djej3Jabf924Fa2g5$", 
  name = "12411-0017"
)
res <- GET(baseurl, query = query_params)

readr::write_file(content(res)$Object$Content, file = "tmp.csv")
readr::read_csv2("tmp.csv", skip = 5,
                 n_max = as.numeric(content(res)$Object$Structure$Rows[[1]]$Values)) %>% 
  rename(Stichtag = ...1,
         LK_ID = ...2,
         Kreis = ...3) %>% 
  pivot_longer(names_to = "Altersgruppe", values_to = "Bevoelkerung",
               !one_of(c("Stichtag", "LK_ID", "Kreis")))

