pacman::p_load(tidyverse, magrittr, lubridate, scales, stargazer, nnet)

df = readRDS('../Data/df.rds') # loading the data
df %<>% select(-fips) # removed since it was only used in the spatial model 
df$popDensity = log(df$popDensity) # since it is highly skewed and we are using a linear model 
finalModel =  multinom(clustReLeveled ~ ., data = df) # building the multinomial model

stargazer(finalModel, type = 'latex', p.auto = FALSE, out="../Data/multi.tex", 
          single.row = TRUE, header = FALSE, 
          ci.level = 0.95, ci = TRUE)



