# Clearing the workspace and setting the working directory
rm(list = ls()) # clear workspace
cat("\014") # clear consolde
if(!is.null(dev.list())) dev.off() # clear plots
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set directory

# Loading the required packages
if(require(h2o)==FALSE) install.packages("h2o", type = "source", repos = (c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))
pacman::p_load(tidyverse, tidyquant, magrittr, timetk, lubridate, dataPreparation, janitor, 
               COVID19,
               NbClust, caret, 
               h2o, bit64, 
               DT, pander)

# ---------------------------------------------------------------------------------------------------------------
## Step 1: Getting the Data

# We will set the end date to the previous Saturday. To do this, we first set the date to the previous day
# to ensure that we have the data loaded. Once that is done, we floor the date to Saturday
endDate = (Sys.Date() - 1) %>% ymd() %>% # Initial value for end date is the day before today
  floor_date(unit = 'week', week_start = 6) %>% as_date() # we then get the previous Saturday to have a full week
endDatePrintV = format(ymd(endDate), format = "%b %d, %Y")

# Extracting the COVID19 Data
counties = covid19(country = "US", 
                   level = 3, # for county
                   start = "2020-03-01", # First Sunday in March
                   end = endDate, # end Date 
                   raw = FALSE, # to ensure that all counties have the same grid of dates
                   amr = NULL, # we are not using the apple mobility data for our analysis
                   gmr = NULL, # we are not using the Google mobility data for our analysis
                   wb = NULL, # world bank data not helpful for county level analysis
                   verbose = FALSE)

# Focusing on the Contiguous US
counties %<>% # next line removes non-contiguous US states/territories
  filter(!administrative_area_level_2 %in% c('Alaska', 'Hawaii', 'Puerto Rico', 'Northern Mariana Islands', 'Virgin Islands')) %>% 
  fast_filter_variables(verbose = FALSE) %>% #dropping invariant columns or bijections
  filter(!is.na(key_numeric)) %>%  # these are not counties
  group_by(id) %>% # grouping the data by the id column to make computations correct
  arrange(id, date) %>% # to ensure correct calculations
  mutate(fips = str_pad(key_numeric, width = 5, side = 'left', pad = 0), # creating a fips variable from key_numeric
         day = wday(date, label = TRUE) %>% factor(ordered = F), # day of week
         newCases = confirmed - lag(confirmed), # computing new daily cases per county
         newDeaths = deaths - lag(deaths))  # computing new daily deaths per county

# manually identifying factor variables
factorVars = c("school_closing", "workplace_closing", "cancel_events",
               "gatherings_restrictions", "transport_closing", "stay_home_restrictions",
               "internal_movement_restrictions", "international_movement_restrictions",
               "information_campaigns", "testing_policy", "contact_tracing")

counties %<>% # converting those variables into character and then factor
  mutate_at(.vars = vars(any_of(factorVars)), .funs = as.character) %>% 
  mutate_at(.vars = vars(any_of(factorVars)), .funs = as.factor) %>% 
  mutate()


# ----------------------------------------------------------------------------------------------------------------

clusteringPrep = counties %>% # from the counties
  select(id, fips, key_google_mobility, population, date, newCases, newDeaths) %>% # selecting minimal amount of cols for visual inspection
  arrange(id, date) %>% # arranged to ensure correct calculations
  mutate(scaledCasesMM7 = (newCases/population) %>% rollmedianr(k=7, fill = NA),
         scaledDeathsMM7 = (newDeaths/population) %>% rollmedianr(k=7, fill = NA) ) %>% 
  select(-c(population, newCases, newDeaths))

casesDF = clusteringPrep %>% select(-scaledDeathsMM7) %>% 
  pivot_wider(names_from = date, values_from = scaledCasesMM7)

deathsDF = clusteringPrep %>% select(-scaledCasesMM7) %>% 
  pivot_wider(names_from = date, values_from = scaledDeathsMM7)

# Creating a Matrix from Each DF following the formatting requirements of dtwclust package
casesMatrix = casesDF %>% ungroup() %>% select(-c(id, fips, key_google_mobility)) %>%
  remove_empty(which = "cols") %>% data.matrix() %>% t() # dropping all NA columns
deathsMatrix = deathsDF %>% ungroup() %>% select(-c(id, fips, key_google_mobility)) %>%
  remove_empty(which = "cols") %>% data.matrix() %>% t()# dropping all NA columns

colnames(casesMatrix) = casesDF$key_google_mobility
colnames(deathsMatrix) = deathsDF$key_google_mobility

caseDeathList = as.vector(x = dim(casesMatrix)[2], mode = 'list')
for (counter in 1:dim(casesMatrix)[2]) {
  caseDeathList[[counter]] = data.frame(scaledCases = casesMatrix[, counter],
                                        scaledDeaths = deathsMatrix[, counter]) %>% data.matrix()
}

names(caseDeathList) = casesDF$key_google_mobility

mvc = tsclust(caseDeathList, type = "hierarchical", distance = "Manhattan",
              k = 2L, seed = 2021)
hcd <- as.dendrogram(mvc)
windows()
ggdendrogram(hcd, rotate = TRUE, theme_dendro = TRUE) + theme_bw()
