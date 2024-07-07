## This will be my file for formatting the new emigration data set from the
## US Census on Hawaiian population domestic migration

source('code/initial_glimpse.R')
library(showtext)
library(ggtext)
library(usmap)
library(maps)
## Codes for google sheets work: 

## =SUM(Z10:Z22,Z24:Z44,Z50:Z75)
## =SUM(AI10:AI22,AI24:AI44,AI50:AI75)

## =AVERAGE(AA10:AA44,AA50:AA75)
## =AVERAGE(AJ10:AJ44,AJ50:AJ75)

early_sets <- '=SUM(B23, D23, F23, H23, J23, M23, O23, Q23, S23, U23, X23, AB23, 
AD23, AF23, AI23, AK23, AM23, AO23, AQ23, AT23, AV23, AX23, AZ23, BB23, BE23, BG23, 
BI23, BK23, BM23, BP23, BR23, BT23, BV23, BX23, CA23, CC23, CE23, CG23, CI23, CL23, 
CN23, CP23, CR23, CT23, CW23, CY23, DA23, DC23, DE23, DH23, DJ23)'

raw_data2 <- read_csv('data/HI_population_movement.csv')
diaspora_data <- HI_diaspora


emigration_data <- raw_data2 %>%
  select_all() %>%
  rename(year = Hawaii,
         resident_emigration = Emigration,
         MOE = 'Margin of Error',
         MOE_prcnt = 'MOE Percentage',
         immigration = Immigration) %>%
  mutate(resident_emigration = as.numeric(resident_emigration),
         MOE = as.numeric(MOE),
         MOE_prcnt = as.numeric(MOE_prcnt),
         MOE_2 = as.numeric(MOE_2),
         MOE_prcnt_2 = as.numeric(MOE_prcnt_2)) 

library(showtext)
library(ggtext)
font_add_google(family = 'patua-one', 'Patua One')
showtext.auto()

## Plotting data to find patterns in population movement
emigration_data %>%
  mutate(labels = factor(sprintf('%02d', year %% 100))) %>%
  group_by(year) %>%
  ggplot(aes(x=year, y=resident_emigration, label = labels)) +
    geom_smooth(method = 'loess', span = 0.41, color = 'purple') +
    geom_label(size = 3.2, nudge_y = -3800) +
    #geom_path(linetype = 'dashed') +
    geom_point(shape = 1, size = 2, stroke = 0.8, fill = 'white') +
    
    scale_y_continuous(breaks = seq(20000,160000, 20000), limits = c(0,160000),
                       expand = c(0,1000), labels = seq(20, 160, 20)) +
    scale_x_continuous(expand = c(0.02, 0.02)) +
    
    labs(
      title = "Tracking Hawai'i resident emigration (2004-2022)",
      subtitle = "Extrapolated from emigration data of residents who spent the previous year living in HI.
(missing data from 2007 & 2020)",
      tag = "Data from US Census",
      x = NULL,
      y = 'Resident  emigration (x1,000)'
    ) +
    
    theme(
      plot.title = element_textbox_simple(margin = margin(b=5)),
      plot.subtitle = element_text(color = 'darkgrey', size = 10),
      panel.background = element_rect(fill = '#f6f6f6'),
      axis.line = element_line(),
      plot.margin = margin(10,5,10,5),
      panel.spacing = unit(0.3, 'in'),
      plot.tag = element_text(size = 9, color = 'darkgrey'),
      plot.tag.position = 'bottomright',
      plot.tag.location = 'panel'
    )

ggsave('figures/resident_emigration.png', width = 6, height = 5, units = 'in')


## Tracking both emigration & immigration data in Hawai'i to look for population trends
emigration_data %>%
  select(year, resident_emigration, immigration) %>%
  pivot_longer(cols = 2:3, names_to = 'movement_type', values_to = 'amount') %>%
  mutate(labels = factor(sprintf('%02d', year %% 100))) %>%
  
  ggplot(aes(x=year, y=amount, color=movement_type, label = labels)) +
  geom_smooth(method = 'loess', span = 0.41, level = 0.9, show.legend = FALSE) +
  #geom_label(size = 3.2, nudge_y = -3800) +
  geom_path(linetype = 'dashed', show.legend = FALSE) +
  geom_point(shape = 1, size = 2, stroke = 0.8) +
  geom_point(size = 1.2, color = 'white') +
  
  scale_y_continuous(breaks = seq(20000,160000, 20000), limits = c(0,160000),
                     expand = c(0,1000), labels = seq(20, 160, 20)) +
  scale_x_continuous(expand = c(0.005, 0.005)) +
  scale_color_manual(name = NULL,
                     values = c('red', 'dodgerblue'),
                     labels = c('Immigration', 'Emigration'),
                     guide = guide_legend(override.aes = list(shape=15, size = 4))) +
  
  labs(
    title = "Tracking Hawai'i resident migration trends (2004-2022)",
    subtitle = "Extrapolated from emigration & domestic immigration data of residents who spent the previous year living in HI.",
    tag = "Data from US Census",
    caption = "The term 'Resident' applies to anyone who has lived at least one year in said location. (missing data from 2007 & 2020)",
    x = NULL,
    y = 'Resident  migration (x1,000)'
  ) +
  
  theme(
    plot.title = element_textbox_simple(family='patua-one', size = 40, margin = margin(b=5)),
    plot.subtitle = element_textbox_simple(color = 'darkgrey', size = 25, margin = margin(b=3, t=3)),
    panel.background = element_rect(fill = '#f6f6f6'),
    axis.line = element_line(),
    axis.title.y = element_text(size = 25),
    axis.text = element_text(size = 22),
    plot.margin = margin(10,5,10,5),
    panel.spacing = unit(0.3, 'in'),
    legend.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = '#f6f6f6'),
    legend.position = "inside",
    legend.position.inside = c(0.2, 0.82),
    legend.title = element_blank(),
    legend.text = element_text(size = 22),
    plot.tag = element_text(size = 18, color = 'darkgrey'),
    plot.tag.position = 'bottomright',
    plot.tag.location = 'panel',
    plot.caption = element_text(color='darkgrey', hjust = 0, size = 22)
  )

ggsave('figures/resident_migration.png', width = 6, height = 5, units = 'in')


library(maps)
map('usmap')
map_data("state")
# doesn't include AK & HI... which is kind of paramount to this whole thing

library(usmap)
us_map(regions='states')

#state_total <- 
  diaspora_data %>%
  mutate(total = select(., '2009':'2019') %>% rowSums(na.rm = TRUE)) %>%
  rename(state = `State:`) %>%
  # pivot_longer(cols = 2:12, names_to = 'year', values_to = 'emigrants') %>%
  # mutate(year = as.numeric(year)) %>%
  drop_na() %>%
  
  ggplot(aes(x=total, y=reorder(state, total), label = total)) +
  geom_col(position = 'dodge', show.legend = FALSE, fill = 'skyblue') +
  # geom_text(nudge_x = 5000) +
  
  scale_x_continuous(breaks = seq(0,135000, 15000), limits = c(0,140000),
                     expand = c(0.1, NA)) +
  scale_y_discrete(drop=TRUE) +
  # scale_fill_manual(name=NULL,
  #                   breaks=c("a", "b"),
  #                   values=c("#F8F8F8", "#FFFFFF"),
  #                   labels=c("a", "b")) +
  
  labs(
    title = 'Finding the most frequent domestic destinations for HI residents',
    subtitle = 'The decade of 2009-2019 saw the highest rates of emigration from the state',
    #caption = 'CAPTION',
    #tag = 'TAG',
    y = NULL,
    x = "People leaving Hawai'i"
  ) +
  
  theme(
    plot.title = element_text(family='patua-one', size = 35, margin = margin(b=2)),
    plot.title.position = 'plot',
    plot.subtitle = element_textbox_simple(color = 'darkgrey', size = 25,
                                           margin = margin(b=5, t=3)),
    axis.title = element_text(size = 25),
    axis.text = element_text(size=20),
    panel.background = element_rect(color = 'lightgrey'),
    #panel.background = element_blank(),
    #panel.grid = element_blank(),
    plot.margin = margin(10,5,10,5),
    panel.spacing = unit(0.3, 'in')
  )

ggsave('figures/HI_diaspora_destinations.png', width = 5, height = 6, units = 'in')


# strip_data <- diaspora_data %>%
#   select(state, y_position) %>%
#   mutate(xmin = 50, xmax=100,
#          ymin = y_position - 0.5,
#          ymax = y_position + 0.5,
#          fill = c(rep(c("a", "b"), length.out=nrow(.)))) %>%
#   pivot_longer(cols=c(xmin, xmax), values_to="x", names_to="xmin_xmax") %>%
#   select(-xmin_xmax)

## Formatting long data fro future plots
diaspora_data %>%
  rename(state = `State:`) %>%
  pivot_longer(cols = 2:12, names_to = 'year', values_to = 'emigrants') %>%
  mutate(year = as.numeric(year)) %>%
  drop_na()