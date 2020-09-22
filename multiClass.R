

# [0] Initialization:
# -------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # automatically set the working dir to where the file is saved


pacman::p_load(tidyverse, magrittr, doParallel,
               caret, caretEnsemble, AUC, caTools, DMwR, MLmetrics,
               rpart, nnet, #rpart and multinom packages
               randomForest, e1071, foreach, import, #parRF packages
               naivebayes, nnet, # for naive bayes and nnet
               kernlab, # svmRadial package
               xgboost, plyr, # needed packages for xgboost
               conflicted) # Loading required packages and data

set.seed(2020)

conflict_prefer("select", "dplyr") #Prefering dplyr::select over any other package
conflict_prefer("summarize", "dplyr") # similar to above but with dplyr::summarize
conflict_prefer("filter", "dplyr") # similar to above but with dplyr::summarize

startTime = Sys.time()


# [1] ETL:
# --------

df = readRDS("Data/clusterCounties.rds")

df = df %>% filter(!administrative_area_level_2 %in% c('Alaska', 'Hawaii', 'Puerto Rico', 'Northern Mariana Islands',
                                            'Virgin Islands'))
df = na.omit(df) %>% ungroup() # removing NAs
df$countyType %<>% as.factor() # converting countyType to factor instead of character
df$povertyPercent %<>% as.numeric() # converting povertyPercent to numeric -- it was read as character from raw data
levels(df$cluster_group) = paste0('Cluster', levels(df$cluster_group) ) # making each factor level start with characters


# [2] Training the Classifiers
# ----------------------------
trainRowNums = createDataPartition(df$cluster_group, p = 0.8, list = F) %>% 
  as.vector() # index rows such that approximately 80% of each class are selected
trainData = df[trainRowNums, 5:10] # training set using the above indices
testData = df[-trainRowNums, 5:10] # testing set excluding the above indices


fitControl = trainControl(method = "cv", # bootstrap sampling
                          number = 5, # 5 folds
                          summaryFunction = multiClassSummary, # using multiclass metrics
                          classProbs = TRUE, # saving each of the class probabilities
                          savePredictions = "final", # saving final predictions
                          selectionFunction = "best", # picking best model
                          sampling = "down", # to handle class imbalance in training
                          index = createResample(trainData$cluster_group, times = 5)) 

numCores = detectCores() -2 # to allow the use of other programs on machine


cl = makePSOCKcluster(numCores, outfile ="Data/mulitClasslLog.txt")
registerDoParallel(cl)


# to search: effect of tunelength for each method? How to tune for each method
models = caretList(y = trainData$cluster_group,
                   x = trainData[,-6],
                   tuneList = list( # defining each algorithm
                     cart = caretModelSpec(method = "rpart", tuneLength = 10),
                     knn = caretModelSpec(method = "knn", tuneGrid = expand.grid(k = seq(1, 19, by = 2)) ),
                     multi = caretModelSpec(method = "multinom", tuneLength = 10),
                     nb = caretModelSpec(method="naive_bayes", tuneLength=30),
                     nnet = caretModelSpec(method = 'avNNet', tuneLength = 30)), # tuneLength = 10* number of tunable params
                   trControl = fitControl, # see fitControl
                   continue_on_fail = T, # skip to next model if current did not converge
                   preProcess = c("scale", "center") # preprocessing parameters
)

stopCluster(cl)
cvResults = resamples(models) # summarizing training results for each bootstrap sample * ML method

pdf("Data/pilotMultiClassTrainBoxPlots.pdf")
bwplot(cvResults, 
       scales = list(x=list(relation="free"), y=list(relation="free")) )
dev.off()


# [3] Evaluation Results:
# -----------------------
xTest = subset(testData, select = -c(cluster_group))
fullResultsList = lapply(models, predict, xTest, type = "raw") %>% 
  lapply(confusionMatrix, testData$cluster_group)

fullResultsList %>% lapply("[[", "table") -> listConfMatrices # for each method
fullResultsList %>% lapply("[[", "byClass") -> listByClassMetrics # for each method
fullResultsList %>% lapply("[[", "overall") %>% 
  as.data.frame() %>% dplyr::slice(c(1,2)) -> dfOverallResults # acc + Kappa for each method
row.names(dfOverallResults) = c("Accuracy", "Kappa")

print(dfOverallResults) # printing overall results for inspecting the Rout file


# [4] Saving Results:
# -------------------
runTime = Sys.time() - startTime # completion time
sInfo = sessionInfo()

# models saved separately since it can get quite large
saveRDS(models, file="Data/pilotMultiClassModels.RData")

# remaining of objects of interest differenced with global environment
rm(list = setdiff(ls(), c("dfOverallResults", "listByClassMetrics", "listConfMatrices", 
                          "fullResultsList", "cvResults", "runTime", "sInfo")))
save.image(file = "Data/pilotMultiClassResults.RData")