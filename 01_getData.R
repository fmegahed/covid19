# This R file is used to scrape and get covid19-related data on the county level
# from a GitHub repository mantained by a JHU researcher
# https://github.com/JieYingWu/COVID-19_US_County-level_Summaries/tree/master/data


# [1] Setting working directory, initilization and reading the required packages:
# -------------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to file location
rm(list = ls()) # clear global environment
cat("\014") # clear console

if(!require(pacman)) install.packages("pacman") # install pacman package if not installed
pacman::p_load(tidyverse, devtools, rvest) # load three packages (and install them if not found on machine)

if(!require(tidycovid19)) install_github("joachim-gassen/tidycovid19") # install tidycovid19 from GitHub if not installed


# [2] Extracting the needed data from GitHub:
# -------------------------------------------

# [a] Country/ Nation's level data:
# ---------------------------------
countryLevelData = download_merged_data(silent = TRUE, cached = TRUE) # download all the country level data

# [b] U.S. county level data:
# ---------------------------
gitURL = "https://github.com/JieYingWu/COVID-19_US_County-level_Summaries/tree/master/data"

repoTable = 
  read_html(gitURL) %>% # reading the webpage contents 
  html_node("div > div.Box.mb-3.Box--condensed > table") %>% # extracting contents of the table
  html_table() # cleaning the extracted contents by removing unnecessary HTML tags

repoFileURLS = 
  str_detect(repoTable$Name, ".csv") %>%  # detecting all csv file locations in repoTable's Name column
  {subset(repoTable, subset = ., select = Name)} %>% # passing the locations to the subset argument of subset function
  pull() %>% # converting it into a character vector
  paste0("https://raw.githubusercontent.com/JieYingWu/COVID-19_US_County-level_Summaries/master/data/", .)
  

countyLevelData = lapply(repoFileURLS, read_csv) # extracting all csv data and saving it into a list
names(countyLevelData) = str_match(repoFileURLS, "data/(\\w+?).csv")[,2] # naming the sublists

for (counter in vector) {
  
}

# [3] Tidying the county level data:
# ----------------------------------
# Note all dates have to be converted using as.Date(Some Value, origin = '0001-01-01')
