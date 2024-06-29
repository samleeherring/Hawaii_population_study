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

## Personal income/wages went from $510 million in 1950 to $45.6 billion in 2023
money_trail %>%
  ggplot(aes(x=year,y=personal_income)) +
  geom_point()

## Labels for the plot graphic below
acc_labels <- c('acc_jobs' = 'Accomodation jobs',
                'acc_GDP' = 'Accomodation sector GDP',
                'arrivals' = 'Total annual arrivals')

## Accomodation jobs have gone up and down quite a bit, but there arent' much more
## today than there were in 1990
money_trail%>%
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
