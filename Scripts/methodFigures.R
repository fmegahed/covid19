# scripts to create Some Covid19 visualizations

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # automatically set working dir

pacman::p_load(tidyverse, COVID19, magrittr, lubridate, fpp2, zoo) # loading required packages

# National Data
us = covid19(country = 'US', level = 1, start = '2020-03-01', end = '2020-10-03')
us %<>% select(id, key_google_mobility, date, confirmed) %>% 
  mutate(newCases = c(NA, diff(confirmed)),
         newMA7 = rollmeanr(newCases, k = 7, fill = NA), # 7-day ma of new (adjusted) cases
         maxMA7 = max(newMA7, na.rm = T), # obtaining the max per county to scale data
         scaledNewMA7 = pmax(0, newMA7/maxMA7, na.rm = TRUE))

# Counties Data
counties = covid19(country = 'US', level = 3, start = '2020-03-01', end = '2020-10-03')

counties %<>% select(id, key_google_mobility, date, confirmed) %>% 
  mutate(newCases = c(NA, diff(confirmed)),
         newMA7 = rollmeanr(newCases, k = 7, fill = NA), # 7-day ma of new (adjusted) cases
         maxMA7 = max(newMA7, na.rm = T), # obtaining the max per county to scale data
         scaledNewMA7 = pmax(0, newMA7/maxMA7, na.rm = TRUE))

indices = c('Alabama, Lee County', 'Illinois, Madison County', 'Michigan, Oscoda County',
            'New York, Queens County', 'Ohio, Butler County') # some counties

# filtering some counties and combining both data frames
df = counties %>% filter(key_google_mobility %in% indices)
df = rbind(us, df)

df$key_google_mobility %<>% recode(US = 'Aggregate for the Entire US') 


# Facet Wrap Plot
df %>% ggplot(aes(x = date, y = scaledNewMA7, group = id, color = key_google_mobility)) +
  geom_line(size = 1.5) +
  facet_wrap(~ key_google_mobility, scales = 'free_y', ncol = 2) + 
  theme_bw(base_size = 24) + 
  theme(legend.position = 'none') + 
  labs(color = '', x = 'Month', y = 'New Cases By County') +
  scale_color_brewer(type = 'qual', palette = 'Paired')



# Single New Cases Plot
counties %>% filter(key_google_mobility == 'California, San Bernardino County') %>% 
  ggplot(aes(x = date, y = newCases)) +
  geom_line(size =1.5) + theme_bw(base_size = 30) + 
  labs(x = 'Month', y = 'MA(7) of New Cases') +
  scale_color_brewer(type = 'qual', palette = 'Set2')


mapData = counties %>% ungroup() %>% 
  select(key_numeric, administrative_area_level_2, administrative_area_level_3,
         countyType, popDensity, percRepVotes, governorsParty, regions,
         PercentSeniors, icuBedsPer10000Residents, povertyPercent) %>% 
  unique()
write.csv(mapData, file = 'mapData.csv', row.names = F)
