setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # automatically set working dir to file location

pacman::p_load(tidyverse) # needed package

regionsCDC = data.frame(region = c('Connecticut', 'Maine', 'Massachusetts', 'New Hampshire', 'Rhode Island' , 
                                   'Vermont', 'New York', # End of Region A
                                   'Delaware', 'District of Columbia', 'Maryland', 'Pennsylvania',
                                   'Virginia', 'West Virginia', 'New Jersey', # End of Region B
                                   'North Carolina', 'South Carolina', 'Georgia', 'Florida', # Region C
                                   'Kentucky', 'Tennessee', 'Alabama', 'Mississippi', # Region D
                                   'Illinois', 'Indiana', 'Michigan', 'Minnesota', 'Ohio',
                                   'Wisconsin', # End of Region E
                                   'Arkansas', 'Louisiana', 'New Mexico', 'Oklahoma', 'Texas', # Region F
                                   'Iowa', 'Kansas', 'Missouri', 'Nebraska', # Region G
                                   'Colorado', 'Montana', 'North Dakota', 'South Dakota',
                                   'Utah', 'Wyoming', # End of Region H
                                   'Arizona', 'California', 'Hawaii', 'Nevada', # Region I
                                   'Alaska', 'Idaho', 'Oregon', 'Washington' # Region J 
                                   ),
                        value = c(rep('A', 7), rep('B', 7), rep('C', 4), 
                                  rep('D', 4), rep('E', 6), rep('F', 5), 
                                  rep('G', 4), rep('H', 6), rep('I', 4), 
                                  rep('J', 4) ) )

write.csv(regionsCDC, file = 'cdcRegions.csv', row.names = F)
