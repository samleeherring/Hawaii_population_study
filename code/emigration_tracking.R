## This will be my file for formatting the new emigration data set from the
## US Census on Hawaiian population domestic migration

source('code/initial_glimpse.R')

## Codes for google sheets work: 

## =SUM(Z10:Z22,Z24:Z44,Z50:Z75)
## =SUM(AI10:AI22,AI24:AI44,AI50:AI75)

## =AVERAGE(AA10:AA44,AA50:AA75)
## =AVERAGE(AJ10:AJ44,AJ50:AJ75)

raw_data2 <- read_csv('data/HI_population_movement.csv')

emigration_data <- raw_data2 %>%
  select_all() %>%
  rename(year = Hawaii,
         resident_emigration = Estimate,
         MOE = 'Margin of Error',
         MOE_prcnt = 'MOE Percentage') %>%
  mutate(resident_emigration = as.numeric(resident_emigration),
         MOE = as.numeric(MOE),
         MOE_prcnt = as.numeric(MOE_prcnt)) 

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

