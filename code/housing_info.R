## Here we will be looking at the living arrangements available in Hawai'i
## and any trends there might be therein

source('code/initial_glimpse.R')

library(ggplot2)
library(tidyverse)
library(stringr)

price_data <- read_csv('data/housing_prices.csv')

just_price <- price_data %>%
  pivot_longer(cols= 4:72, names_to = 'quarters', values_to = 'prices') %>%
  select(1, 5, 6) %>%
  pivot_wider(names_from = 'Indicator', values_from = 'prices') %>%
  mutate(year = str_sub(quarters, 1,4),
         quarters = str_sub(quarters, -2)) %>%
  mutate(year = as.numeric(year)) %>%
  rename(homes = 'Single Family Home- Median Price',
         condos = 'Condos - Median price') %>%
  select(year, quarters, homes, condos) %>%
  
  slice_min(condos)
  
  pivot_longer(cols = 3:4, names_to = 'types', values_to = 'prices') %>%
  filter(quarters == 'Q2',
         year > 2007) %>%
  select(year, types, prices)

just_price %>% print(n=32)
  select(prices) %>%
  max() 

price_labels <- c('$200k', '$400k', '$600k', '$800k', '$1m', '$1.2m')
sequ <- seq(200000,1200000, 250000)

housing_data <- raw_data %>%
  pivot_longer(cols= 4:87, names_to = 'year', values_to = 'value') %>%
  select(1,5,6) %>%
  pivot_wider(names_from = 'Indicator', values_from = 'value') %>%
  mutate(year = as.numeric(year)) %>%
  select(1,15,16) %>%
  filter(year > 2007) %>%
  rename(homes_sold = `Single Family Home- Number of units sold`,
         condos_sold = `Condos - Number of units sold`)

# p1<- 
  
housing_data %>%
  pivot_longer(cols = 2:3, names_to = 'type', values_to = 'sells') %>%
  ggplot(aes(x=year, y=sells, fill=type)) +
  geom_col(position = 'dodge') +
  geom_path(data = just_price, aes(x=year, y=prices/100, color=types),
            inherit.aes = FALSE, linewidth=1, lineend = 'round', linejoin = 'round' ) +
  scale_y_continuous(breaks = c(4000, 6000, 8000, 10000, 12000), expand = c(4000,NA),
                     labels = c('4k', '6k', '8k', '10k', '12k'),
                     sec.axis = sec_axis(name = 'Median price',
                                         labels = price_labels,
                                         breaks = seq(200000,1200000, 200000),
                                         transform = ~.*100)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(name = 'Housing units sold', breaks = c("homes_sold", "condos_sold"),
                    values = c('darkseagreen', 'lightpink1'),
                    labels = c( 'Condominiums', 'Single-family homes'),
                    guide = guide_legend(reverse= TRUE) ) +
  scale_color_manual(name= 'Median prices', values = c('seagreen', 'orchid'),
                     labels = c('Condos', 'Homes')) +
  # scale_fill_gradient(name = 'Housing units sold', low = c('darkseagreen', 'pink'),
  #                      high = c('seagreen1', 'orchid1')) +
  coord_cartesian(expand = TRUE, ylim = c(2800, 12200)) +
  
  labs(
    title = "Looking at housing options available in Hawai'i",
    subtitle = 'Overall trends show that housing prices have gone up 50-57% in the last 17 years.',
    y = 'Units sold',
    x = NULL,
    caption = 'Data provided by HI Board of Realtors'
  ) +
  
  theme(
    plot.title = element_textbox_simple(family='patua-one', face='bold', size=28,
                                        margin = margin(0,0,5,0)),
    plot.title.position = 'plot',
    plot.subtitle = element_text(size=16),
    panel.background = element_rect(fill = '#f6f6f6'),
    legend.background = element_rect(color='darkgrey', fill = "white"),
    legend.key = element_rect(fill = '#f6f6f6'),
    legend.position = "inside",
    legend.position.inside = c(0.2, 0.8),
    legend.text = element_text(size=12),
    legend.title = element_text(size=15),
    axis.line = element_line(color = 'darkgrey'),
    axis.text = element_text(size = 12),
    axis.title = element_text(size=15),
    plot.margin = margin(10,5,10,5),
    panel.spacing = unit(0.3, 'in')
  )
  
# p1 +
#   geom_point(data = just_price, aes(x=year, y=prices, color=types),
#              inherit.aes = FALSE, show.legend = FALSE,
#              size = 10) +
#   geom_path(data = just_price, aes(x=year, y=prices, color=types),
#             inherit.aes = FALSE, show.legend = FALSE,
#             size = 10) +
#   scale_y_continuous(sec.axis = sec_axis(name = 'Median price',
#                                          labels = price_labels,
#                                          breaks = seq(400000,1200000, 200000),
#                                          transform = ~.*100))

  # scale_y_continuous(breaks = seq(4000, 14000, 2000), limits = c(3000,14000), expand = c(4000,NA),
  #                    labels = c('4k', '6k', '8k', '10k', '12k', '14k'))
  



housing_data %>%
  filter(condos_sold > 12000)
