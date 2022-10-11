library(tidyverse)
library(janitor)



parks <- read_csv(here::here("Week4", "data", "parks.csv")) %>% clean_names()
species <- read_csv(here::here("Week4", "data", "species.csv")) %>% clean_names()


head(parks)
head(species)
species %>%
  group_by(park_name) %>%
  count() %>%
  arrange(desc(n))

# Tab 1 - species diversity / state
n_spec <- species %>%
  group_by(park_name) %>%
  count() %>%
  arrange(desc(n))

species %>% filter(park_name == "Acadia National Park")
species %>%
  select(conservation_status) %>%
  unique()

species %>%
  select(species_id, category) %>%
  group_by(category) %>%
  table() %>%
  as.data.frame() %>%
  group_by(category) %>%
  summarise(count = sum(Freq))

species %>%
  select(park_name, category) %>%
  unique() %>%
  group_by(park_name) %>%
  tally(sort = TRUE)
