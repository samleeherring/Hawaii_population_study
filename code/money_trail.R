## Here we'll be taking a look at correlations between resident populations,
## amounts of tourism, and overall economic advancement 

source('code/initial_glimpse.R')

## Initializing data frame for ease of use

money_trail <- raw_data %>%
  pivot_longer(cols= 4:87, names_to = 'year', values_to = 'amounts') %>%
  select(1, 5, 6) %>%
  pivot_wider(names_from = 'Indicator', values_from = 'amounts') %>%
  mutate(year = as.numeric(year)) %>%
  select(1, 7:13) %>%
  ## 'acc' will be short for 'accommodation' henceforth
  rename('acc_GDP' = 2,
         'personal_income' = 3,
         'total_wages' = 4,
         'acc_jobs' = 5,
         'income_tax' = 6,
         'corporate_tax' = 7,
         'arrivals' = 8) %>%
  filter(year > 1949)


## First, we'll look at the relation of tourism to GDP

# Pearson's product-moment correlation
cor.test(money_trail$arrivals, money_trail$acc_GDP)
## t = 6.0387, df = 22, p-value = 4.453e-06
## correlation: 0.789756

## Next, GDP to jobs
cor.test(money_trail$acc_GDP, money_trail$acc_jobs)
## t = 2.0238, df = 24, p-value = 0.05426
## correlation: 0.3818166

## arrivals to jobs
cor.test(money_trail$arrivals, money_trail$acc_jobs)
## t = 6.8333, df = 23, p-value = 5.732e-07
## correlation: 0.8185251

## wages to arrivals
cor.test(money_trail$arrivals, money_trail$total_wages)
## t = 2.9372, df = 11, p-value = 0.01352
## correlation: 0.6629866

## Calculating percent change by year for specific columns
money_trail %>%
  select(year, acc_GDP, acc_jobs, arrivals) %>%
  filter(year >= 1997) %>%
  #group_by(acc_GDP) %>%
  mutate(acc_GDP = as.integer(acc_GDP)) %>%
  mutate(gdp_change = (acc_GDP/lag(acc_GDP) - 1) * 100,
         jobs_change = (acc_jobs/lag(acc_jobs) - 1) * 100,
         arrival_change = (arrivals/lag(arrivals) - 1) * 100) %>%
  select(year, acc_GDP, gdp_change, acc_jobs, jobs_change, arrivals,
         arrival_change) %>%
  print(n=27)
  

## Personal income/wages went from $510 million in 1950 to $45.6 billion in 2023
money_trail %>%
  ggplot(aes(x=year,y=personal_income)) +
  geom_point() +
  geom_line()

## Looking at taxes in the corporate and individual sectors
tax_labels <- c('income_tax' = 'Individual  income  tax  x10,000',
                'corporate_tax' = 'Corporate  tax  x1,000')

money_trail %>%
  select(year, income_tax, corporate_tax) %>%
  filter(year >= 1990) %>% 
  mutate(income_tax = income_tax/10000,
         corporate_tax = corporate_tax/1000) %>%
  pivot_longer(cols = c('income_tax', 'corporate_tax')) %>%
  mutate(name = factor(name, levels=c('income_tax', 'corporate_tax'))) %>%
  ggplot(aes(x=year,y=value, color=name)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~name, ncol = 1, scales = 'free_y', strip.position = 'left',
             labeller = labeller(name = tax_labels)) +
  scale_y_continuous(expand = c(0,NA), limits = c(NA,NA)) +
  coord_cartesian(clip = 'off') +
  
  labs(
    title = "Tracking trends in personal income and corporate tax rates \nin Hawai'i",
    subtitle = 'Data from Hawaii State Department of Taxation',
    x = NULL,
    y = NULL
  ) +
  
  theme(
    plot.title.position = 'plot',
    strip.placement = 'outside',
    plot.subtitle = element_text(color = 'darkgrey')
  )

ggsave('figures/individual_v_corporate_tax.png', width = 5, height = 6, units = 'in')
  
  
  
## Labels for the plot graphic below
acc_labels <- c('acc_jobs' = 'Accomodation jobs',
                'acc_GDP' = 'Accomodation sector GDP',
                'arrivals' = 'Total annual arrivals')

## Accomodation jobs have gone up and down quite a bit, but there arent' much more
## today than there were in 1990
money_trail%>% filter(year>=2018)
  select(year, acc_GDP, acc_jobs, arrivals) %>%
  filter(year > 1996) %>%
  pivot_longer(cols = c(acc_GDP, acc_jobs, arrivals)) %>%
  mutate(name = factor(name, levels = c('acc_jobs', 'acc_GDP', 'arrivals'))) %>%
  ggplot(aes(x=year, y=value, color=name)) +
  geom_smooth(se=FALSE, linewidth = 0.7, color='blue') +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~name, ncol = 1, scales = 'free_y', strip.position = 'left', 
             labeller = labeller(name = acc_labels)) +
  
  coord_cartesian(clip = 'off') +
  
  labs(
    title = "Correlations between Hawaii's total accomodation \n jobs, GDP, and annual arrivals",
    x = NULL,
    y = NULL
  ) +
  
  theme(
    strip.placement = 'outside',
    strip.background = element_rect(fill='#f6f6f6'),
    panel.background = element_rect(fill='#f6f6f6'),
    #panel.grid = element_blank(),
    axis.line = element_line(),
    plot.margin = margin(10,5,10,5),
    panel.spacing = unit(0.3, 'in')
    
  )

ggsave('figures/money_trail_correlations.png', width = 5, height = 6, units = 'in')
