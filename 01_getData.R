# This R file is used to scrape and get covid19-related data on the county level
# from a GitHub repository mantained by a JHU researcher
# https://github.com/JieYingWu/COVID-19_US_County-level_Summaries/tree/master/data


# [1] Setting working directory, initilization and reading the required packages:
# -------------------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
cat("\014")

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, devtools, rvest)

if(!require(tidycovid19)) install_github("joachim-gassen/tidycovid19")
countryLevelData = download_merged_data(silent = TRUE, cached = TRUE)


# [2] Extracting the needed data from GitHub:
# -------------------------------------------
gitURL = "https://github.com/JieYingWu/COVID-19_US_County-level_Summaries/tree/master/data"

repoFiles = read_html(gitURL) %>%
  html_node("div > div.Box.mb-3.Box--condensed > table") %>% 
  html_table() %>% 
  slice(-c(1,2)) %>% 
  select(-Type)

repoFileURLS = repoFiles$Name %>% 
  str_detect(".csv") %>%  
  {subset(repoFiles, subset = ., select = Name)} %>% 
  pull() %>% 
  paste0("https://raw.githubusercontent.com/JieYingWu/COVID-19_US_County-level_Summaries/master/data/", .)
  
covidCountyList = lapply(repoFileURLS, read_csv)
names(covidCountyList) = str_match(repoFileURLS, "data/(\\w+?).csv")[,2]

# Note all dates have to be converted using as.Date(Some Value, origin = '0001-01-01')