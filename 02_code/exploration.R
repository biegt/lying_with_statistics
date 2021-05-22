library(tidyverse)
library(here)
library(janitor)
library(lubridate)

dog_races <- read_csv2(
  here("01_data", "Hunde-vie.csv"),
  skip = 1,
  locale = locale(encoding = "latin1")
) %>%
  clean_names() %>% 
  mutate(ref_date = ydm(ref_date))

dog_races %>% count(ref_date, wt = anzahl)

dog_races %>% count(postal_code, dog_breed, sort = TRUE, wt = anzahl)

listdogs_vec <-
  c(
    "Bullterrier",
    "Staffordshire Bullterrier",
    "American Staffordshire Terrier",
    "Mastino Napoletano",
    "Mastin Espanol",
    "Fila Brasileiro",
    "Mastiff",
    "Bullmastiff",
    "Tosa Inu",
    "Pit Bull Terrier",
    "Rottweiler",
    "Dogo Argentino"
  )

listdogs_regex_all <- paste0(".*", listdogs_vec, ".*", collapse = "|")
listdogs_regex <- paste0(listdogs_vec, collapse = "|")

listdogs_df <- dog_races %>% 
  filter(str_detect(dog_breed, listdogs_regex_all)) %>% 
  mutate(listdog_breed = str_extract(dog_breed, listdogs_regex))

listdogs_df %>%
  group_by(ref_date) %>%
  summarize(sum = sum(anzahl))

listdogs_df %>% 
  group_by(postal_code, ref_date) %>% 
  summarize(anzahl = sum(anzahl)) %>% 
  ggplot(aes(x = ref_date, y = anzahl)) +
  facet_wrap(~ postal_code, scales = "free_y") +
  geom_line()

listdogs_df %>% 
  filter(listdog_breed == "Rottweiler") %>% 
  group_by(postal_code, ref_date) %>% 
  summarize(anzahl = sum(anzahl)) %>% 
  ggplot(aes(x = ref_date, y = anzahl)) +
  facet_wrap(~ postal_code, scales = "free_y") +
  geom_line()

listdogs_df %>% 
  filter(listdog_breed == "Bullterrier") %>% 
  group_by(postal_code, ref_date) %>% 
  summarize(anzahl = sum(anzahl)) %>% 
  ggplot(aes(x = ref_date, y = anzahl)) +
  facet_wrap(~ postal_code, scales = "free_y") +
  geom_line()

listdogs_df %>% 
  group_by(listdog_breed, ref_date) %>% 
  summarize(anzahl = sum(anzahl)) %>% 
  ggplot(aes(x = ref_date, y = anzahl)) +
  geom_line() +
  facet_wrap(~ listdog_breed, scale = "free_y")
