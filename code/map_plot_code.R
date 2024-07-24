## Creating a map visual to show highest rates of relocation from Hawai'i

source('code/emigration_tracking.R')

library(showtext)
library(ggtext)
library(usmap)
library(maps)
library(glue)
library(stringr)

## joining map data with my data frames
state_map_1 <- map_data('state') %>%
  select(long, lat, region) %>%
  mutate(region = str_to_title(region))

## Checking for outliers
anti_join(destinations, state_map_1, 
          by = c('state' = 'region'))
# I forgot this package doesn't include Alaska or Hawai'i

state_map_2 <- us_map('states') %>%
  select(fips, abbr, full, geom)
# This one does include all the states, but has mulipolygonal coordinates which
# I will try to convert to lat lon

map2 <- state_map_2 %>%
  sf::st_coordinates() %>%
  as_tibble() %>%
  select("lon" = X, "lat" = Y)
# That didn't work
usmap_crs(state_map_2)

## Trying with functions from the us_map package itself
m1 <- map_with_data(destinations, values = 'emigrants') %>%
  select(abbr, full, geometry, year, emigrants, fips)
# not quite

## Following parameter of other us_map functions more closely
m2 <- inner_join(destinations, state_map_2, 
           by = c('state' = 'full', 'abb' = 'abbr')) %>%
  select(fips, emigrants, year)
  
plot_usmap(data = m2, values = 'emigrants')
## LET'S GOOOO

m3 <- map_with_data(m2, values = 'emigrants')
over_time%>%
  arrange(total)
## Same with the combined decade data frame
dec1 <- inner_join(over_time, state_map_2, 
                 by = c('state' = 'full', 'abb' = 'abbr')) %>%
  select(fips, total)

plot_usmap(data = dec1, values = 'total') +
  scale_fill_gradient2(low = '#FFFFFF', mid = '#FF0000', high = 'darkred',
                       limits=c(0,NA), midpoint = 130000,
                       breaks = c(0, 125000, 250000),
                       labels=c('500', '125k', '250k'),
                       name = "New residents\nfrom Hawai'i:",
                       guide = guide_colorbar(direction='horizontal',reverse=T)) +
  labs(
    title = "Distribution of emigrants from Hawai'i across the United States (2009-2019)",
    subtitle = "This decade saw the highest spike in migration away from Hawai'i since the US Census\nbegan recording there.",
    caption = "Data source: US Census"
  ) +
  theme_classic() +
  theme(
    plot.title = element_textbox_simple(size=30, family='patua-one', face='bold',
                              margin = margin(t=5,0,b=7,0), lineheight = 0.75),
    plot.title.position = 'plot',
    plot.subtitle = element_text(size=15),
    plot.caption.position = 'panel',
    plot.caption = element_text(face='italic'),
    plot.margin = margin(t=0,r=5,b=0,l=10),
    legend.position = 'inside',
    legend.position.inside = c(0.77, 0.08),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
## LET'S GO AGAINNNN

d1 <- map_with_data(dec1, values = 'total')

m3 <- map_with_data(m2, values = 'emigrants')
diaspora_map_data <- as_tibble(m3)

diaspora_map_data %>%
  ggplot(aes(xy=geom, fill=emigrants)) + 
  geom_polygon(color='black')


