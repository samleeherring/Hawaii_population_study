library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggtext)

raw_data <- read_csv('DBEDT_Data.csv')

just_pop <- raw_data %>%
  pivot_longer(cols= 4:87, names_to = 'year', values_to = 'population') %>%
  select(1, 5, 6) %>%
  pivot_wider(names_from = 'Indicator', values_from = 'population') %>%
  mutate(year = as.numeric(year)) %>%
  rename(census_decade = 'Population (Census)- Total Resident',
         census = 'Total Resident Population',
         de_facto = 'De Facto Population',
         births = 'Resident births',
         deaths= 'Resident deaths') %>%
  select(1, 3:6) %>%
  filter(year > 1957)

just_pop %>%
  select(de_facto) %>%
  drop_na() %>%
  max()

pop_graph <- just_pop %>%
  pivot_longer(cols = 2:3, names_to = 'source', values_to = 'population') %>%
  ggplot(aes(x=year, y=population, group = source, color = source)) +
  geom_line() +
  geom_point(size = 1) +
  geom_vline(xintercept = 2020, color = 'black', linetype = 'dashed', linewidth = 0.4) +
  #geom_smooth(se=FALSE, linewidth = 0.5) +
  scale_y_continuous(breaks = seq(500000, 1700000, 200000), labels = seq(500, 1700, 200),
                     limits = c(500000, 1750000)) +
  scale_x_continuous(breaks = seq(1958, 2023, 15), expand = c(0.01,0.01), limits = c(1957, 2024)) +
  scale_color_manual(name = 'Population type:',
                     values=c("red", "dodgerblue"),
                     labels=c("US Census", "HI DEBDT")) +
  labs(
    title = "Population of the state of Hawai'i (1958-2023)",
    subtitle = "Based on data from the US Census & HI State DBEDT",
    x = NULL,
    y = 'Population (millions)'
  ) +
  theme(
    plot.title = element_textbox_simple(margin = margin(b=5)),
    plot.subtitle = element_text(color = 'darkgrey', size = 10),
    panel.background = element_rect(fill = '#f6f6f6'),
    #panel.grid = element_blank(),
    legend.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = '#f6f6f6'),
    legend.position = "inside",
    legend.position.inside = c(0.19, 0.78),
    #legend.text = element_text(color="black"),
    axis.line = element_line(),
    plot.margin = margin(10,5,10,5),
    panel.spacing = unit(0.3, 'in')
  )

lbl_txt <- data.frame(year = (2020 - 7), population = 1100000, label = '(COVID-19)',
                      group = source)
pop_graph +  
  geom_text(data = lbl_txt,label = lbl_txt$label, color = 'black', size = 3,
            group = source)

ggsave('figures/hawaii_population.png', width = 5, height = 4, units = 'in')


