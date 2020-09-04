pacman::p_load(COVID19, tidyverse, dataPreparation,
               lubridate, recipes, magrittr)

df = covid19(country = "US", level = 3, start = "2020-03-01", end = Sys.Date() - 2 )

covidCounties = df %>% group_by(key_google_mobility) %>% 
  mutate(newCasesFromYesterday = c(NA, diff(confirmed)),
         newDeathsFromYesterday = c(NA, diff(deaths)), 
         newCasesFromLastWeek = c(rep(NA,7), diff(confirmed, 7)),
         newDeathsFromLastWeek = c(rep(NA,7), diff(deaths, 7))) %>% 
  fastFilterVariables() # removing constant and bijection columns in R

covidCounties %>% 
  filter(key_google_mobility == "Ohio, Butler County") %>% 
  ggplot(aes(x=date, y = newCasesFromLastWeek, group = key_google_mobility)) +
  geom_line()
