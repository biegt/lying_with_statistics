library(tidyverse)
library(here)
library(janitor)
library(scales)
library(patchwork)
library(ggalt)
library(ggforce)
library(ggtext)
library(lubridate)
library(cowplot)
library(magick)

# Datenimport -------------------------------------------------------------

dog_races <- read_csv2(
  here("01_data", "Hunde-vie.csv"),
  skip = 1,
  locale = locale(encoding = "latin1")
) %>%
  clean_names() %>% 
  mutate(ref_date = lubridate::ydm(ref_date))

# Preprocessing ------------------------------------------------------

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

ast_data <-  listdogs_df %>%
  filter(listdog_breed == "American Staffordshire Terrier")  %>%
  group_by(ref_date) %>%
  summarize(anzahl = sum(anzahl)) %>%
  mutate(prop = (anzahl / 2) - 1)

pbt_data <-
  listdogs_df %>%
  filter(listdog_breed == "Pit Bull Terrier")  %>%
  group_by(ref_date) %>%
  summarize(anzahl = sum(anzahl)) %>%
  mutate(prop = (anzahl / 2) - 1)

# # Individuelle Plots ----------------------------------------------------

ast_data %>%
  ggplot(aes(x = ref_date, y = prop)) +
  geom_line(size = 1, color = "#10a39e") +
  geom_point(size = 3, color = "#10a39e") +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = c(1, 3, 5)) +
  scale_x_date() +
  theme_minimal() +
  labs(x = "Jahr", y = "Proz. Anstieg", title = "<span style = 'color:#10a39e; font-family:Blacksword;'>American Staffordshire Terrier </span>") +
  theme(
    panel.grid.major.x =  element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_markdown(size = 20)
  ) +
  expand_limits(y = c(0, 6)) +
  annotate(
    geom = "curve",
    curvature = 0.2,
    color = "#4d4c4c",
    linetype = "dashed",
    x = ymd("2013-07-31"),
    y = 3,
    xend = ymd("2017-12-01"),
    yend = 5
  ) +
  annotate(
    geom = "label",
    size = 3,
    x = ymd("2013-07-31"),
    color = "#4d4c4c",
    y = 3,
    label = "Ende 2017 hat sich die Anzahl \nim Vergleich zu 2012 versechsfacht",
    hjust = "left"
  ) -> ast_plot

pbt_data %>%
  ggplot(aes(x = ref_date, y = prop)) +
  geom_line(size = 1, color = "#cf0287") +
  geom_point(size = 3, color = "#cf0287") +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = c(0.5, 1, 1.5, 2.5)) +
  scale_x_date() +
  theme_minimal() +
  labs(x = "Jahr", y = "Proz. Anstieg", title = "<span style = 'color:#cf0287;font-family:Blacksword;'>Pitbull Terrier</span>") +
  theme(
    panel.grid.major.x =  element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_markdown(size = 20),
    plot.caption.position = "plot"
  ) +
  expand_limits(y = c(0, 1.75)) +
  annotate(
    geom = "curve",
    curvature = 0.2,
    color = "#4d4c4c",
    linetype = "dashed",
    x = ymd("2013-07-31"),
    y = 1.25,
    xend = ymd("2017-12-01"),
    yend = 1.5
  ) +
  annotate(
    geom = "label",
    size = 3,
    x = ymd("2013-07-31"),
    color = "#4d4c4c",
    y = 1.25,
    label = "Ende 2017 hat sich die Anzahl \nim Vergleich zu 2012 \nmehr als verdoppelt",
    hjust = "left"
  ) -> pbt_plot


# Plots mittels Patchwork kombinieren -------------------------------------

(pbt_plot + ggdraw() + draw_image(here("04_misc", "img", "pbt_sil.png"))) /
(ast_plot + ggdraw() + draw_image(here("04_misc", "img", "ast_sil.png"))) +
  plot_annotation(
    title = "Gefährliche Hunderassen in Wien \nnehmen stark zu.",
    subtitle = "Prozentueller Anstieg seit 31.12.2012 \n",
    caption = "Quelle: Open Data Österreich",
    theme = theme(
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.caption = element_text(hjust = 0.9),
      plot.subtitle = element_text(color = "darkgrey"),
      text = element_text(size = 16, family = "Roboto Lt")
    )
  ) -> final_plot

# Finalen Plot speichern --------------------------------------------------

ggsave(
  here("03_output", "plot.svg"),
  final_plot,
  scale = 0.95,
  width = 6.5,
  height = 8
)
