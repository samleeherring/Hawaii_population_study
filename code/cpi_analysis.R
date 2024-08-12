## Looking at how consumer price index has changed in Hawai'i over time
library(ggtext)
library(ggplot2)
library(tidyverse)

cpi_data <- read_csv('data/HI_CPI_data.csv') 

cpi <- cpi_data %>%
  pivot_longer(cols= 4:19, names_to = 'year', values_to = 'CPI') %>%
  select(1, 5, 6) %>%
  pivot_wider(names_from = 'Indicator', values_from = 'CPI') %>%
  mutate(year = as.numeric(year)) %>%
  rename(food_bev = 'CPI- Food & beverages',
         housing = 'CPI- Housing',
         apparel = 'CPI- Apparel',
         trnsprt = 'CPI- Transportation',
         med_care = 'CPI- Medical care',
         education = 'CPI- Educ. & communication',
         rec = 'CPI- Recreation',
         energy = 'CPI- Energy',
         other = 'CPI- Other good & services')

cpi_plot <- cpi %>%
  select(-apparel, -rec, -education, -med_care) %>%
  pivot_longer(cols = 2:6, names_to = 'cpi', values_to = 'value') %>%
  
  ggplot(aes(x=year, y=value, color=cpi)) +
  # geom_smooth(data = subset(cpi2, cpi=='avgs'), show.legend = FALSE) +
  geom_point(size = 2) +
  geom_path(linewidth = 1, linejoin = 'round', show.legend = FALSE) +
  
  scale_x_continuous(expand = c(0.01,0.01))+
  scale_color_manual(name = 'CPI category',
                     values = c('palegreen3', 'mediumpurple', 'indianred1',
                                'dodgerblue', 'goldenrod1'),
                     labels = c('Energy', 'Food/bev', 'Housing', 'Other', 'Transport'),
                     guide = guide_legend(override.aes = list(shape=15, size = 4))) +
  
  labs(
    title = "Changes in the cost of living in Hawai'i (2008-2023)",
    x = NULL,
    y = 'Consumer Price Index (CPI)'
  ) +
  
  theme(
    plot.title = element_textbox_simple(family='patua-one', face='bold', size=26,
                                        margin = margin(0,0,5,0)),
    plot.title.position = 'plot',
    panel.background = element_rect(fill = '#f6f6f6'),
    legend.background = element_rect(color='darkgrey', fill = "white"),
    legend.key = element_rect(fill = '#f6f6f6'),
    legend.position = "inside",
    legend.position.inside = c(0.2, 0.8),
    axis.line = element_line(color = 'darkgrey'),
    axis.text = element_text(size = 12),
    axis.title = element_text(size=15),
    plot.margin = margin(10,5,10,5),
    panel.spacing = unit(0.3, 'in')
  )
 
cost_of_living <- cpi %>%
  mutate(avgs = rowMeans(cpi[2:6], na.rm = TRUE)) %>%
  select(year, avgs) +
  pivot_longer(cols = 2, names_to = 'cpi', values_to = 'value')
  
## Researched funtions for inseting layers beneath existing plot, krassowski
`-.gg` <- function(plot, layer) {
  if (missing(layer)) {
    stop("Cannot use `-.gg()` with a single argument. Did you accidentally put 
         - on a new line?")
  }
  if (!is.ggplot(plot)) {
    stop('Need a plot on the left side')
  }
  plot$layers = c(layer, plot$layers)
  plot
}

cpi_plot -
  geom_smooth(data = cost_of_living, name = NULL, aes(x=year, y=avgs, color=NA, linewidth=NA),
              show.legend = FALSE, inherit.aes = FALSE, position=position_dodge2())
