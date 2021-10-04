library(tidyverse)

## Bevölkerung
bev <- readr::read_csv2("data/metadata/12411-0018_flat.csv",
                       locale = locale(encoding = "latin1")) %>% 
  rename(Stichtag = Zeit,
         AGS = `1_Auspraegung_Code`,
         Kreis = `1_Auspraegung_Label`,
         Altersgruppe = `3_Auspraegung_Label`,
         Bevoelkerung = BEVSTD__Bevoelkerungsstand__Anzahl,
         Geschlecht = `2_Auspraegung_Label`
         ) %>% 
  mutate(Bevoelkerung = as.numeric(Bevoelkerung)) %>% 
  select(Stichtag, AGS, Kreis, Altersgruppe, Geschlecht, Bevoelkerung) %>% 
  filter(nchar(AGS) == 5) %>% 
  filter(Geschlecht != "Insgesamt") %>% 
  filter(Altersgruppe != "Insgesamt") %>% 
  mutate(Alter = map(stringi::stri_match_all_regex(Altersgruppe, "\\d{1,2}"),
                     as.numeric)) %>% 
  mutate(
    Alter_min = map_dbl(Alter,  function(x) {
      # x[1] bis unter x[2]
      if (length(x) == 2)
        return(x[1])
      # unter 1 Jahr
      if (isTRUE(x == 3))
        return(0)
      # ab X Jahre
      if (!is.na(x))
        return(x[1])
      return(NA_real_)
    }),
    Alter_max = map_dbl(Alter, function(x) {
      # x[1] bis unter x[2]
      if (length(x) == 2)
        return(x[2] - 1)
      # unter 1 Jahr
      if (isTRUE(x == 3))
        return(3)
      # ab X Jahre
      if(!is.na(x))
        return(120)
      return(NA_real_)
    })
  ) %>% 
  select(-Alter)

saveRDS(bev, file = "data/metadata/BEVOELKERUNG_KREIS.rds")

bev %>% 
  mutate(Altersgruppe = case_when(
    between(Alter_min, 10, 17) ~ "12-17",
    between(Alter_min, 18, 59) ~ "18-59",
    between(Alter_min, 60, 120) ~ "60+",
    Alter_min < 10 ~ "0-11"
  )) %>% 
  left_join(readRDS("data/metadata/BBSR_ROR.rds"), by = "AGS") %>% 
  group_by(ROR_ID, ROR, Altersgruppe) %>% 
  summarise(Bevoelkerung = sum(Bevoelkerung)) %>% 
  ungroup() %>% 
  saveRDS("data/metadata/BEVOELKERUNG_ROR.rds")
