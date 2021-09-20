library(tidyverse)

# Read in the latest vaccination data by Bundesland
# form the RKI GitHub repository
impf_kreis <- readr::read_csv("https://github.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/raw/master/Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.csv")

bundesland <- readr::read_csv("https://raw.githubusercontent.com/sumtxt/ags/master/data-raw/bundeslaender.csv") %>% 
  mutate(Bundesland_ID = formatC(id, width = 2, flag = "0")) %>% 
  rename(Bundesland_en = name_eng, Bundesland_de = name_de) %>% 
  select(Bundesland_ID, Bundesland_en, Bundesland_de)

# Aggregate over the Kreise to get Bundesland data 
impf_bundesland <- impf_kreis %>% 
  arrange(Impfdatum) %>% 
  mutate(Bundesland_ID = substr(LandkreisId_Impfort, 1, 2)) %>% 
  inner_join(bundesland, by = "Bundesland_ID") %>% 
  group_by(Bundesland_en, Bundesland_de, Impfdatum, Altersgruppe, Impfschutz) %>% 
  summarise(Anzahl = sum(Anzahl)) %>% 
  ungroup() %>% 
  group_by(Bundesland_en, Bundesland_de, Impfschutz, Altersgruppe) %>% 
  mutate(Anzahl7 = slider::slide_dbl(Anzahl, mean, .before = 6)) %>% 
  ungroup()

plotdat <- impf_bundesland %>% 
  filter(Impfdatum > Sys.Date() - 90) %>%
  filter(Altersgruppe == "18-59") %>% 
  filter(Impfschutz < 3) %>% 
  mutate(Impfschutz = factor(Impfschutz, levels = 1:2,
                             labels = c("First dose", "Final dose"))) %>% 
  mutate(Bundesland_label = case_when(
    Bundesland_en == "Bavaria" ~ "Bavaria",
    Bundesland_en == "North Rhine-Westphalia" ~ "North Rhine-Westphalia",
    TRUE ~ "Other Bundesländer"
  ))

ggplot(plotdat, aes(x = Impfdatum, y = Anzahl7/1000,
                    group = Bundesland_en,
                    colour = Bundesland_label)) +
  geom_vline(xintercept = as.Date("2021-09-14"),
             colour = "grey80") +
  geom_path(alpha = 0.6, guide = "none") +
  facet_wrap(vars(Impfschutz), scales = "free_y") +
  labs(
    x = "Date of vaccination",
    y = "Weekly doses (x1000)",
    colour = ""
  ) +
  theme(legend.position = "bottom")

ggsave(filename = "figures/vaccination_by_bundesland.png",
       width = 18, height = 9, units = "cm", dpi = 300)
