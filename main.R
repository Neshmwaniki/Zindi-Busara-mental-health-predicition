
# load libraries
library(tidyverse)
library(timeDate)
library(zoo)
library(data.table)
#####################
library(xgboost)
library(caret)
library(MLmetrics)


path.dir = getwd()
#dir.create(paste0(path.dir,"/subm"))
#dir.create(paste0(path.dir,"/tmp"))
subm.dir = paste0(path.dir,"/subm")
save.files.dir = paste0(path.dir,"/tmp")


##########
## Sourcing Utility files
source("./utils.R")

### details of the data , data cleaning and little feature engineering
source("./data details.R")


###########################
## Model Training
###########################
cat("Model Training and Predictions....... ","\n")
###
start.time = Sys.time()
set.seed(1235)
model = train(x = df, y = as.factor(label2), method ="xgbTree",
                        trControl = trainControl(
                          method = "cv",
                          number = 5,
                          verboseIter = T,
                          classProbs = T,
                          savePredictions = "final"
                        ))

total.time = Sys.time() - start.time
total.time 

### MODEL SUMMARY
cat("Model Summary....... ","\n")
summary(model)
### FEATURE IMPORTANCE
cat("Model Top 30 Importance....... ","\n")
plot(varImp(model),30)
###
cat("Model Predictions Ready!!!!!....... ","\n")
pred = predict(model, newdata = test, type = "prob")$Yes
#### REDUCING THRESHOLD DUE TO CLASS IMBALANCE
pred = ifelse(pred > 0.3,1,0)
submission = data.frame(test.id, pred)
colnames(submission) = c("surveyid","depressed")
submission$depressed[submission$surveyid == 248] = 0
cat("Predictions ready for Submission!!....... ","\n")
write.csv(submission, file =paste0(subm.dir,"/", Sys.Date(),"_Submission.csv"), row.names = FALSE)
cat("Done!!!!!!!!!!!!!", "\n")

