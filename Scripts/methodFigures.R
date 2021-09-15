### scripts to create Some Covid19 visualizations

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # automatically set working dir

pacman::p_load(tidyverse, COVID19, magrittr, lubridate, fpp2,
               zoo, tmap, tigris, RColorBrewer, ggpubr, sf) # loading required packages

if(require(urbnmapr)==FALSE) devtools::install_github('UrbanInstitute/urbnmapr')
library(urbnmapr)

### National Data ----
us = covid19(country = 'US', level = 1, start = '2020-03-01', end = '2021-01-02')
us %<>% select(id, key_google_mobility, date, confirmed) %>% 
  mutate(newCases = c(NA, diff(confirmed)),
         newMM7 = rollmedianr(newCases, k = 7, fill = NA), # 7-day mm of new (adjusted) cases
         maxMM7 = max(newMM7, na.rm = T), # obtaining the max per county to scale data
         scaledNewMM7 = pmax(0, newMM7/maxMM7, na.rm = TRUE))

### * US Plot: usPlot ----
tiff(filename = '../Figures/usPlot.tiff',
     width = 1920, height =1080, pointsize = 16)
us %>% ggplot(aes(x = date, y = newCases)) +
  geom_line(aes(color = "New Cases")) +
  geom_line(aes(y = newMM7, color = "7-Day Moving Median of New Cases"), size = 3 ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw(base_size = 24) + 
  theme(legend.position = 'top') + 
  scale_colour_manual("Lines", values=c("New Cases"="black", "7-Day Moving Median of New Cases"="red")) +
  labs(color = '', x = 'Month', y = 'New Cases Aggregated for the Entire US',
       caption = 'Based on data from 2020-03-01 to 2021-01-02')
invisible( dev.off() ) # to suppress the unwanted output from dev.off


### * US Plot: EPS usPlot ----
postscript(file = '../Figures/usPlot.eps',
     width = 1920, height =1080, pointsize = 16)
us %>% ggplot(aes(x = date, y = newCases)) +
  geom_line(aes(color = "New Cases")) +
  geom_line(aes(y = newMM7, color = "7-Day Moving Median of New Cases"), size = 3 ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw(base_size = 24) + 
  theme(legend.position = 'top') + 
  scale_colour_manual("Lines", values=c("New Cases"="black", "7-Day Moving Median of New Cases"="red")) +
  labs(color = '', x = 'Month', y = 'New Cases Aggregated for the Entire US',
       caption = 'Based on data from 2020-03-01 to 2021-01-02')
invisible( dev.off() ) # to suppress the unwanted output from dev.off



### Counties Data ----
counties = covid19(country = 'US', level = 3, start = '2020-03-01', end = '2021-01-02',
                   raw = FALSE)

counties %<>% select(id, key_google_mobility, date, confirmed) %>% 
  mutate(newCases = c(NA, diff(confirmed)),
         newMM7 = rollmedianr(newCases, k = 7, fill = NA), # 7-day mm of new (adjusted) cases
         maxMM7 = max(newMM7, na.rm = T), # obtaining the max per county to scale data
         scaledNewMM7 = pmax(0, newMM7/maxMM7, na.rm = TRUE))

indices = c('Alabama, Lee County', 'Arizona, Navajo County', 'Arkansas, Washington County', 'California, San Francisco County',
            'Illinois, Madison County', 'New York, New York County','Ohio, Butler County',
            'Virginia, Charlottesville', 'Virginia, Falls Church') # some counties

# filtering some counties and combining both data frames
df = counties %>% filter(key_google_mobility %in% indices)



# * Motivation Plot: Parts A and B ----

df %>% ggplot(aes(x = date, y = newMM7, group = id)) +
  geom_line(size = 1.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(~ key_google_mobility, scales = 'free_y', ncol = 3) + 
  theme_bw(base_size = 30) + 
  theme(legend.position = 'none') + 
  labs(color = '', x = 'Month', y = 'New Cases By County',
       caption = 'Based on data from 2020-03-01 to 2021-01-02') -> p1


df %>% ggplot(aes(x = date, y = scaledNewMM7, group = id)) +
  geom_line(size = 1.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(~ key_google_mobility, scales = 'free_y', ncol = 3) + 
  theme_bw(base_size = 30) + 
  theme(legend.position = 'none') + 
  labs(color = '', x = 'Month', y = 'Scaled New Cases By County',
       caption = 'Based on data from 2020-03-01 to 2021-01-02') -> p2

tiff(filename = '../Figures/motivationPlot.tiff',
     width = 1800, height =1800, pointsize = 28)
ggpubr::ggarrange(p1, p2, ncol = 1, labels = c('(a)', '(b)'),
                  font.label =  list(size = 45, face = "bold", color ="black"))
invisible( dev.off() ) # to suppress the unwanted output from dev.off



png(filename = '../Figures/motivationPlot.png',
           width = 1800, height =1800, pointsize = 28)
ggpubr::ggarrange(p1, p2, ncol = 1, labels = c('(a)', '(b)'),
                  font.label =  list(size = 45, face = "bold", color ="black"))
invisible( dev.off() ) # to suppress the unwanted output from dev.off





# CDC Plot
crossSectionalData <- readRDS("../Data/crossSectionalData.rds")
crossSectionalData$fips = str_pad(crossSectionalData$key_numeric,
                                  width = 5, side = 'left', pad = '0')
# Retrieving the U.S. state composite map as a simplefeature
state_sf = usa_sf("aeqd") %>% filter(!name %in% c('Alaska', 'Hawaii')) # from albersua
state_sf %<>% geo_join(crossSectionalData, by_sp= 'name', by_df= 'administrative_area_level_2')

postscript(file = '../Figures/cdcRegions.eps', width = 1920, height = 1080, pointsize = 16)
tm_shape(state_sf) + tm_polygons('regions', title = 'Region', palette = "Paired")
invisible( dev.off() ) # to suppress the unwanted output from dev.off


# Cluster Counties
endDate = '2021-01-02'
endDatePrintV = format(ymd(endDate), format = "%b %d, %Y")

cty_sf = get_urbn_map(map = "counties", sf = TRUE) %>% 
  filter(!state_name %in% c('Alaska', 'Hawaii') )
colnames(cty_sf)[1] = 'fips'

# Getting the states map from the urbnmapr package and excluding non-continental US
states_sf = get_urbn_map(map = "states", sf = TRUE) %>% 
  filter(!state_name %in% c('Alaska', 'Hawaii') )

clusterCounties <- readRDS("../Data/clusterCounties.rds")
clusterCounties$fips = str_pad(clusterCounties$key_numeric, width = 5, side = 'left', pad = '0')
clusterCounties %<>% ungroup()
clusterCounties$cluster_group %<>% as.factor()
  
colorPal =  brewer.pal(n= levels(clusterCounties$cluster_group) %>% length(), 'Set2')
names(colorPal) = levels(clusterCounties$cluster_group)


cty_sf %<>% left_join(clusterCounties[, c('fips', 'cluster_group')], by = 'fips') # adding cluster_group to cty_sf

bbox_new = st_bbox(cty_sf)

xrange = bbox_new$xmax - bbox_new$xmin # range of x values
yrange = bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[1] = bbox_new[1] - (0.05 * xrange) # xmin - left
bbox_new[2] = bbox_new[2] - (0.1 * yrange) # ymin - bottom

bbox_new = bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon



tiff(filename = '../Figures/clusterMap.tiff', width = 9, height =6, units = "cm", res = 300)
tm_shape(cty_sf, bbox = bbox_new) +
  tm_borders(col = "gray40", lwd = 0.1) +
  tm_fill('cluster_group', palette = colorPal, colorNA = "gray50",
          title = "Cluster #") +
  tm_shape(states_sf) + tm_borders(col = "black", lwd = 0.5) +
  tm_credits(paste0('Based on Data from March 01, 2020 - ', endDatePrintV, " "),
             position=c("right", "bottom"))
invisible( dev.off() ) # to suppress the unwanted output from dev.off



# Cluster Match
finalModel <- readRDS("../Data/finalModel.rds")
multiClassDF <- readRDS("../Data/multiClassDF.rds")
predictedProbs = fitted(finalModel) # computing predicted probabilities for each of the outcome levels
mapResults = cbind(multiClassDF, predictedProbs) # col binding predProbs for each cluster with multiClassDF

# Finding indices to subset the data
numberOfClusters = unique(mapResults$cluster_group) %>% as.character() %>% length() 
startCol = ncol(mapResults) - numberOfClusters + 1
endCol = ncol(mapResults)

# Finding whether the predicted and actual clusters matched for each county
mapResults$LargestProbCluster = colnames(mapResults[, startCol:endCol])[apply(mapResults[, startCol:endCol], 1, which.max)] 
mapResults$match = ifelse(mapResults$cluster_group == mapResults$LargestProbCluster, 'Yes', 'No') %>% as.factor()


cty_sf = get_urbn_map(map = "counties", sf = TRUE) %>% 
  filter(!state_name %in% c('Alaska', 'Hawaii') )
colnames(cty_sf)[1] = 'fips'


cty_sf %<>% geo_join(mapResults, by_sp= 'fips', by_df= 'fips')

colorPal2 = c("Yes" = "#CAB2D6", "No" = "#6A3D9A")
# Creating a static visual for use in the paper

png(filename = '../Figures/clusterMatchMap.png',width = 9, height =6, units = "cm", res = 300)
tm_shape(cty_sf, bbox = bbox_new) +
  tm_borders(col = "gray40", lwd = 0.1) +
  tm_fill('match', palette = colorPal2, colorNA = "gray50",
          title = "Cluster Match") +
  tm_shape(states_sf) + tm_borders(col = "black", lwd = 0.5) +
  tm_credits(paste0('Based on Data from March 01, 2020 - ', endDatePrintV, " "),
             position=c("right", "bottom"))
invisible( dev.off() ) # to suppress the unwanted output from dev.off



# Creating the policy figure to explain why we chose the median Government Response Index
endDate = '2021-01-02'
policy = read_csv('https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv')
policy = filter(policy, !is.na(RegionName) | !RegionName %in% c('Alaska', 'Hawaii'))
policy$state = toupper(policy$RegionName) # a state variable = an upper case of existing RegionName
policy$Date %<>% ymd() # converting the Date data to a date format

policySummary = policy %>% # calculating a summary table of median value for the GovernmentResponseIndex per state
  filter(Date >= '2020-03-01' & Date <= endDate) %>% # to match our COVID Data timeSeries
  group_by(state) %>% # perform computations using the median value, per state, for each index
  summarise(GovernmentResponseIndexMedian = median(GovernmentResponseIndex, na.rm = TRUE))
policySummary$state %<>%  str_replace('WASHINGTON DC', 'DISTRICT OF COLUMBIA') %>% str_to_title()


policy %<>% group_by(RegionName) %>% filter(Date >= '2020-03-01' & Date <= endDate) %>% 
  mutate(med = median(GovernmentResponseIndex))

# * Policy Plot ----
tiff(filename = '../Figures/medianPlot.tiff', width = 1200, height = 1200)
policy %>%
  filter(RegionName %in% c('Alabama', 'Ohio', 'New York', 'Texas', 
                           'South Dakota',  'California', 'Florida', 'North Carolina',
                           'Washington') ) %>% 
  ggplot(aes(x = Date, y = GovernmentResponseIndex, group = RegionName)) +
  geom_line(size = 2) +
  geom_hline(aes(yintercept = med), color = 'red', size = 1.25) + 
  facet_wrap(~RegionName) + theme_bw(base_size = 30) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  labs(y = 'Government Response Index', x = 'Month')
invisible( dev.off() ) # to suppress the unwanted output from dev.off

postscript(file = '../Figures/medianPlot.eps', width = 1200, height = 1200)
policy %>%
  filter(RegionName %in% c('Alabama', 'Ohio', 'New York', 'Texas', 
                           'South Dakota',  'California', 'Florida', 'North Carolina',
                           'Washington') ) %>% 
  ggplot(aes(x = Date, y = GovernmentResponseIndex, group = RegionName)) +
  geom_line(size = 2) +
  geom_hline(aes(yintercept = med), color = 'red', size = 1.25) + 
  facet_wrap(~RegionName) + theme_bw(base_size = 20) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  labs(y = 'Government Response Index', x = 'Month')
invisible( dev.off() ) # to suppress the unwanted output from dev.off

