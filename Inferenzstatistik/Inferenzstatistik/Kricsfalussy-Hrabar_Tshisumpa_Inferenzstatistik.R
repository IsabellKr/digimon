#### Kricsfalussy-Hrabar_Inferenzstatistik.R

# source("install_libraries.R")
library(tidyverse)
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(plotrix)
library(jmv)


## Farben verfügbar machen
rwthcolor <- hcictools::rwth.colorpalette()

## Daten laden
data_robot <- readRDS("data/robo_pflege.rds")

## Unverbundener T-Test. Hypothese: Männer und Frauen unterscheiden sich im tv.
t.test( filter(data_robot, gender -- "weiblich")$tv,
        filter(data_robot, gender -- "männlich")$tv) +
  library(ggplot2)

data_robot %>%
  filter(gender != "keine Angabe") %>% 
  group_by(gender) %>%
  summarise(mean_tv = mean(tv) - 1, sem_tv = std.error(tv)) %>%
ggplot() +
  aes(x = gender, weight = mean_tv, fill = gender, ymin = mean_tv - sem_tv, ymax = mean_tv + sem_tv) +
  scale_fill_manual(values = c(rwthcolor$blue, rwthcolor$red)) +
  geom_bar( width = 0.5) +
  geom_errorbar( width = 0.2) +
  ylim(0,5) +
  theme_gray() +
  labs(title = "Männer sehen tv eher als Frauen",
       subtitle = "Balkendiagramm: tv im Vergleich zwischen Männern und Frauen",
       x = "Geschlecht",
       y = "tv [0 - 5]",
       fill = "Geschlecht",
       caption = "Fehlerbalken zeigen Standardfehler des Mittelwertes.") 
NULL

data_robot %>%
  filter(gender != "keine Angabe") %>% 
  group_by(gender) %>%
  summarise(mean_tv = mean(tv), sem_tv = std.error(tv)) %>%
  ggplot() +
  aes(x = gender, y = mean_tv, colour = gender, ymin = mean_tv - sem_tv, ymax = mean_tv + sem_tv) +
  scale_colour_manual(values = c(rwthcolor$blue, rwthcolor$red)) +
  geom_errorbar( width = 0.2, colour = rwthcolor$black) +
  geom_point( size = 3) +
  ylim(3.5, 5) +
  theme_gray() +
  labs(title = "Männer sehen tv eher als Frauen",
       subtitle = "Punktdiagramm: tv im Vergleich zwischen Männern und Frauen",
       x = "Geschlecht",
       y = "tv [1 - 6]",
       colour = "Geschlecht",
       caption = "Fehlerbalken zeigen Standardfehler des Mittelwertes. Y-Achse wurde aus Gründen der Übersichtlichkeit verschoben.") 
NULL







