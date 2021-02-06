# scripts to create Some Covid19 visualizations

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # automatically set working dir

pacman::p_load(tidyverse, magrittr) # loading required packages

df = readRDS("../Data/df.rds")

df %>% mutate(popDensity = log(popDensity),
              clustReLeveled = as.character(clustReLeveled) %>% as.factor) %>% 
  group_by(clustReLeveled) %>% 
  select_if(is.numeric) %>% 
  summarise_all(
    list(mean = mean, 
         stdDev = sd,
         firstQuartile = ~ quantile(., probs = 0.25),
         median = median,
         thirdQuartile = ~ quantile(., probs = 0.75))
  ) -> numericSummaries
table(df$clustReLeveled)

numericSummaries %>% select(contains('poverty')) %>% select(-contains('median')) -> temp

df %>% mutate(clustReLeveled = as.character(clustReLeveled) %>% as.factor) %>% 
  group_by(clustReLeveled) %>% 
  select(countyType) %>% table() # counting countyTypes

# Counting the distribution of each region among each cluster
df %>% mutate(clustReLeveled = as.character(clustReLeveled) %>% as.factor) %>% 
  group_by(clustReLeveled, regions) %>% select(clustReLeveled, regions) %>% count() -> temp

temp %>% filter(regions == 'J') %>% ungroup() %>% mutate(pct = round(100*n/sum(n), digits = 1))
