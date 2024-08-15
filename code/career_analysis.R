# Looking at the prevalance of accomodation jobs to everything else

library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggtext)

career_data <- read_csv('data/job_data.csv')

career_df <- career_data %>%
  pivot_longer(cols= 4:37, names_to = 'year', values_to = 'jobs') %>%
  select(1, 5, 6) %>%
  pivot_wider(names_from = 'Indicator', values_from = 'jobs') %>%
  mutate(year = as.numeric(year)) %>%
  rename(employed = 'Employed, Civilian',
         unemployment = 'Unemployment rate, Civilian',
         total_jobs = 'Total wage and salary jobs',
         non_ag_jobs = 'Total non agriculture wage & salary jobs',
         mining_constr = 'Natural resources, mining & construction jobs',
         manufacturing = 'Manufacturing jobs',
         trade = 'Wholesale trade jobs',
         retail = 'Retail trade jobs',
         trasnit_utils = 'Transportation, warehousing & utilities jobs',
         info = 'Information jobs',
         finance = 'Financial activities jobs',
         pro_service = 'Professional & business services jobs',
         education = 'Educational services jobs',
         healthcare = 'Health care & social assistance jobs',
         entertainment = 'Arts, entertainment & recreation jobs',
         accomodation = 'Accommodation jobs',
         food_service = 'Food services & drinking places jobs',
         other_service = 'Other services jobs',
         govt = 'Government jobs',
         agriculture = 'Agriculture wage and salary jobs') %>%
  select(-total_jobs) %>%
  mutate(unemployment2 = unemployment/100,
         unemployment2 = scales::percent(unemployment2))

## Rough bar plot of career types in HI
years <- c('1993', '2003', '2013', '2023')
xlabels <- c('Accomodation', 'Entertainment', 'Food service', 'Retail', 'Agriculture',
             'Education', 'Finance', 'Government', 'Healthcare', 'Information', 
             'Manufacturing', 'Mining/Construction', 'Other services',
             'Professional service', 'Wholesale trade', 'Transit & utils')

career_df %>%
  select(-employed, -unemployment, -unemployment2, -non_ag_jobs) %>%
  mutate(a_accomodation = accomodation,
         a_entertainment = entertainment,
         a_food_service = food_service,
         a_retail = retail) %>%
  select(-accomodation, -entertainment, -food_service, -retail) %>%
  pivot_longer(cols = 2:17, names_to = 'job_types', values_to = 'workers') %>%
  filter(year == 1993 | year == 2003 | year == 2013 | year == 2023) %>%
  mutate(name = factor(year, levels=c(1993, 2003, 2013, 2023))) %>%
  
  ggplot(aes(x=job_types, y=workers, fill = job_types)) +
  geom_col(position = 'dodge', show.legend = FALSE) +
  geom_vline(xintercept = 4.5, linetype = 'dashed') +
  facet_wrap(~name, ncol = 1, scales = 'free_y', strip.position = 'right',
             shrink = TRUE, labeller = labeller(name = years)) +
  scale_x_discrete(breaks = waiver(), labels = xlabels) +
  #scale_y_continuous(breaks = waiver(), labels = breaks/1000)
  
  labs(title = "What are employment prospects like in Hawai'i?",
       y = 'Amount of people employed',
       x = NULL,
       caption = 'Data from U.S. Bureau of Labor Statistics & HI State Dep. of Business, Economic Development & Tourism'
       ) +
  theme(
    plot.title = element_textbox_simple(family='patua-one', face='bold', size=25,
                                        margin = margin(0,0,3,0)),
    plot.title.position = 'plot',
    plot.subtitle = element_text(size=16),
    plot.margin = margin(10,15,10,5),
    plot.caption.position = 'plot',
    plot.caption = element_text(face = 'italic', size = 8),
    panel.background = element_rect(fill = '#f6f6f6'),
    #panel.grid.major.x = element_line(color='darkgrey', hjust = 1),
    axis.line = element_line(color = 'darkgrey'),
    axis.text = element_text(size = 8),
    axis.text.x = element_text(angle = -45, vjust = 0.5, hjust = 0),
    axis.title = element_text(size=12),
    panel.spacing = unit(0.3, 'in')
  )
  


## Creating a boxplot showing the top careers categories in HI
xlabels2 <- c('Hospitality', 'Government', 'Healthcare', 'Professional\nServices',
              'Mining &\nConstruction')

career_df %>%
  select(-employed, -unemployment, -unemployment2) %>%
  mutate(hospitality = accomodation+entertainment+food_service+retail) %>%
  select(-accomodation, -entertainment, -food_service, -retail) %>%
  select(year, hospitality, govt, healthcare, pro_service, mining_constr) %>%

  pivot_longer(cols = 2:6, names_to = 'job_types', values_to = 'workers') %>%
  mutate(career = factor(job_types, levels=c('hospitality', 'govt', 'healthcare',
                                             'pro_service', 'mining_constr')))%>%

  ggplot(aes(x=career, y=workers, fill=job_types)) +
  geom_boxplot(show.legend = FALSE, alpha=0.6, outlier.shape=NA, width=0.8) +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5), linetype='dotted') +
  geom_jitter(show.legend = FALSE, shape=21, width = 0.25, alpha = 0.6) +
  # stat_summary(fun.data = median_hilow, fun.args=0.5, show.legend = FALSE,
  #              geom='crossbar', color='black', width=0.8, size=0.5, alpha=0.6) +
  scale_y_continuous(breaks = seq(50000, 200000, 50000), labels=c('50k', '100k',
                                                                  '150k','200k'),
                     expand = c(0,5000)) +
  scale_x_discrete(breaks = waiver(), labels = xlabels2) +

  labs(
    x=NULL,
    y="Amount of people employed",
    title=" Top career options in Hawai'i",
    subtitle='Distribution of employment by top 5 most numerous sectors (1990-2023)',
    caption = "Data from U.S. Bureau of Labor Statistics & HI State \nDep. of Business, Economic Development & Tourism"
  ) +
  
  #theme_classic() +
  theme(
    plot.title = element_textbox_simple(size=26, family='patua-one', face='bold',
                                        margin = margin(t=5,0,b=7,0), lineheight = 0.75),
    plot.title.position = 'plot',
    plot.subtitle = element_text(size=12),
    plot.caption.position = 'plot',
    plot.caption = element_text(face='italic', size=8),
    plot.margin = margin(t=0,r=5,b=0,l=10),
    panel.background = element_rect(fill = '#f6f6f6'),
    axis.title = element_text(size = 12),
    axis.text = element_text(size=10)
  )


## Same boxplot graph but without hospitality industries for analysis
xlabels3 <- c('Government', 'Healthcare', 'Professional\nServices',
              'Mining &\nConstruction', 'Transit &\nUtilities')

career_df %>%
  select(-employed, -unemployment, -unemployment2) %>%
  mutate(hospitality = accomodation+entertainment+food_service+retail) %>%
  select(-accomodation, -entertainment, -food_service, -retail) %>%
  select(year, govt, healthcare, pro_service, mining_constr, trasnit_utils) %>%
  
  pivot_longer(cols = 2:6, names_to = 'job_types', values_to = 'workers') %>%
  mutate(career = factor(job_types, levels=c('govt', 'healthcare', 'pro_service',
                                             'mining_constr', 'trasnit_utils'))) %>%
  
  ggplot(aes(x=career, y=workers, fill=job_types)) +
  geom_boxplot(show.legend = FALSE, alpha=0.6, outlier.shape=NA, width=0.8) +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5), linetype='dotted') +
  geom_jitter(show.legend = FALSE, shape=21, width = 0.25, alpha = 0.6) +
  # stat_summary(fun.data = median_hilow, fun.args=0.5, show.legend = FALSE,
  #              geom='crossbar', color='black', width=0.8, size=0.5, alpha=0.6) +
  scale_y_continuous(breaks = seq(25000, 125000, 25000), labels=c('25k', '50k', '75k',
                                                                  '100k', '125k')) +
  scale_x_discrete(breaks = waiver(), labels = xlabels3) +
  
  labs(
    x=NULL,
    y="Amount of people employed",
    title=" Top career options in Hawai'i",
    subtitle='Distribution of employment by top 5 most numerous sectors excluding\nHospitality related careers (1990-2023)',
    caption = "Data from U.S. Bureau of Labor Statistics & HI State \nDep. of Business, Economic Development & Tourism"
  ) +
  
  #theme_classic() +
  theme(
    plot.title = element_textbox_simple(size=26, family='patua-one', face='bold',
                                        margin = margin(t=5,0,b=7,0), lineheight = 0.75),
    plot.title.position = 'plot',
    plot.subtitle = element_text(size=12),
    plot.caption.position = 'plot',
    plot.caption = element_text(face='italic', size=8),
    plot.margin = margin(t=0,r=5,b=0,l=10),
    panel.background = element_rect(fill = '#f6f6f6'),
    axis.title = element_text(size = 12),
    axis.text = element_text(size=10)
  )

## Looking at what percentage of employment is underhospitality
percent_careers <- career_df %>%
  select(-non_ag_jobs, -unemployment, -unemployment2) %>%
  mutate(hospitality = accomodation+entertainment+food_service+retail) %>%
  select(-accomodation, -entertainment, -food_service, -retail)  %>%
  mutate(mining_constr = (mining_constr/employed)*100,
         manufacturing = (manufacturing/employed)*100,
         trade = (trade/employed)*100,
         transit_utils = (trasnit_utils/employed)*100,
         info = (info/employed)*100,
         finance = (finance/employed)*100,
         pro_service = (pro_service/employed)*100,
         education = (education/employed)*100,
         healthcare = (healthcare/employed)*100,
         other_service = (other_service/employed)*100,
         govt = (govt/employed)*100,
         agriculture = (agriculture/employed)*100,
         hospitality = (hospitality/employed)*100
         ) %>%
  select(-trasnit_utils) %>%
  mutate(totals = rowSums(across(c(3:15)))) %>%
  select(year, hospitality) %>% print(n=34)
  # about 30% typically
  # and that doesn't include large parts of the transit/utilities & professional
  # services industries, which include many of the airline employees & maintenance
  
  
  