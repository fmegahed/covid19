# scripts to create Some Covid19 visualizations

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # automatically set working dir

pacman::p_load(tidyverse, COVID19, magrittr, lubridate, fpp2, zoo) # loading required packages

# National Data
us = covid19(country = 'US', level = 1, start = '2020-03-01', end = '2021-01-02')
us %<>% select(id, key_google_mobility, date, confirmed) %>% 
  mutate(newCases = c(NA, diff(confirmed)),
         newMA7 = rollmeanr(newCases, k = 7, fill = NA), # 7-day ma of new (adjusted) cases
         maxMA7 = max(newMA7, na.rm = T), # obtaining the max per county to scale data
         scaledNewMA7 = pmax(0, newMA7/maxMA7, na.rm = TRUE))

# Counties Data
counties = covid19(country = 'US', level = 3, start = '2020-03-01', end = '2021-01-02',
                   raw = FALSE)

counties %<>% select(id, key_google_mobility, date, confirmed) %>% 
  mutate(newCases = c(NA, diff(confirmed)),
         newMA7 = rollmeanr(newCases, k = 7, fill = NA), # 7-day ma of new (adjusted) cases
         maxMA7 = max(newMA7, na.rm = T), # obtaining the max per county to scale data
         scaledNewMA7 = pmax(0, newMA7/maxMA7, na.rm = TRUE))

indices = c('Alabama, Lee County', 'Arizona, Navajo County', 'California, San Francisco County',
            'Illinois, Madison County', 'New York, New York County','Ohio, Butler County',
            'Virginia, Charlottesville', 'Virginia, Falls Church') # some counties

# filtering some counties and combining both data frames
df = counties %>% filter(key_google_mobility %in% indices)
df = rbind(us, df)

df$key_google_mobility %<>% recode(US = 'Aggregate for the Entire US') 


# Facet Wrap Plot: nonScaledMotivationPlot
png(filename = '../Figures/nonScaledMotivationPlot.png',
     width = 1366, height =768, pointsize = 16)
df %>% ggplot(aes(x = date, y = newMA7, group = id)) +
  geom_line(size = 1.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(~ key_google_mobility, scales = 'free_y', ncol = 3) + 
  theme_bw(base_size = 24) + 
  theme(legend.position = 'none') + 
  labs(color = '', x = 'Month', y = 'New Cases By County',
       caption = 'Based on data from 2020-03-01 to 2021-01-02')
invisible( dev.off() ) # to suppress the unwanted output from dev.off


# Facet Wrap Plot: motivationPlot
png(filename = '../Figures/motivationPlot.png',
     width = 1366, height =768, pointsize = 16)
df %>% ggplot(aes(x = date, y = scaledNewMA7, group = id)) +
  geom_line(size = 1.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(~ key_google_mobility, scales = 'free_y', ncol = 3) + 
  theme_bw(base_size = 24) + 
  theme(legend.position = 'none') +
  labs(color = '', x = 'Month', y = 'New Cases By County',
       caption = 'Based on data from 2020-03-01 to 2021-01-02')
invisible( dev.off() ) # to suppress the unwanted output from dev.off

# SampleCountiesOct17: Plot
indexesForPlot = unique(counties$key_google_mobility) %>% sample(9)

counties %>% filter(key_google_mobility %in% indexesForPlot) %>% 
  ggplot(aes(x = date, y = confirmed, group = id, color = key_google_mobility)) +
  geom_line(size = 1.5) +
  facet_wrap(~ key_google_mobility, scales = 'free_y', ncol = 3) + 
  theme_bw(base_size = 24) + 
  theme(legend.position = 'none') + 
  labs(color = '', x = 'Month', y = 'Cumulative Confirmed Cases By County') +
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


# CDC Plot in R
# Counties
pacman::p_load(maps, tidyverse, r2d3maps, magrittr, albersusa, tigris, leaflet)

state_sf = usa_sf("longlat") %>% filter(!name %in% c('Alaska', 'Hawaii')) # from albersua

counties %>% ungroup() %>%  select(administrative_area_level_2, regions) %>% unique() -> dfCDC
dfCDC$regions = gsub('\\.', ' ', dfCDC$regions) %>% as.factor()

state_sf %<>% geo_join(dfCDC, by_sp= 'name', by_df= 'administrative_area_level_2')

d3map = d3_map(shape = state_sf, projection = "Albers", height = 768) %>%
  add_discrete_scale(var = "regions", palette = "Paired") %>%
  add_legend(title = "")

d3map

r2d3::save_d3_png(d3map, file = 'cdcRegionsV1.png', 
                  width = 1366, height = 768, zoom = 1, delay = 3)



# Improving the Resolution of the 4 Clusters Map
cty_sf = counties_sf("longlat") %>% # from albersusa package
  filter(!state %in% c('Alaska', 'Hawaii'))

# ungrouping clusterCounties and selecting just the two variables needed for the clustering
LeafletCounties = clusterCounties %>% ungroup() %>%
  dplyr::select(key_numeric, administrative_area_level_2, administrative_area_level_3, cluster_group, popDensity, povertyPercent)
colnames(LeafletCounties) = c('key_numeric', 'NAME_1', 'NAME_2', 'value', 'popDensity', 'povertyPercent')

# Converting the key_numeric to a proper FIPS_Code and then to a factor variable
LeafletCounties$key_numeric %<>% str_pad(width = 5, side = 'left', pad = '0') %>% as.factor()

LeafletCounties = cty_sf %>% geo_join(LeafletCounties, by_sp = "fips", by_df = "key_numeric")
LeafletCounties %<>%  mutate( value = paste0('C', value) )
LeafletCounties %<>% mutate_at(vars(value), na_if, 'CNA') %>% 
  mutate(value = as.factor(value))


# using same color scheme as the static one
myPal = colorFactor('Set2', domain = LeafletCounties$value, na.color = "white")

d3map = d3_map(shape = LeafletCounties, projection = "Albers", height = 768) %>%
  add_discrete_scale(var = "value", palette = "Set2") %>%
  add_legend(title = "")

d3map

r2d3::save_d3_png(d3map, file = 'CountiesClustered.png', 
                  width = 1366, height = 768, zoom = 1, delay = 3)
