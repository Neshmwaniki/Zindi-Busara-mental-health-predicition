#### Setting working directory
#setwd("~/R/course work/Zindi/depressed")


####
#source("misc.R")
#source("skaggle.R")

# load libraries
library(tidyverse)
library(caret)
library(SOAR)
library(timeDate)
library(zoo)
library(data.table)
#####################
library(xgboost)
library(caret)
library(MLmetrics)
library(caTools)
library(caretEnsemble)


#####
fstoreconnect=function(subdir){
  ######## Define subdirectory to store R objects (subdir) 
  oldLC <- Sys.getenv("R_LOCAL_CACHE", unset = ".R_Cache")
  Sys.setenv(R_LOCAL_CACHE=subdir) }


fstoreconnect("rstore")
tmp=Objects()

# Encode a feature value with its frequency in the entire dataset
freq.encode = function(x, xnew = x) {
  if (is.factor(x) || is.character(x)) {
    return (as.numeric(factor(xnew, levels = names(sort(table(x))))))
  } else {
    return (approxfun(density(x[!is.na(x)], n = length(x) / 100))(xnew))
  }
}

path.dir = getwd()
#dir.create(paste0(path.dir,"/subm"))
#dir.create(paste0(path.dir,"/tmp"))
subm.dir = paste0(path.dir,"/subm")
save.files.dir = paste0(path.dir,"/tmp")


######### data
df = read.csv(paste0(path.dir,"/train.csv"))
test = read.csv(paste0(path.dir,"/test.csv"))
label = df$depressed

test.id = test$surveyid
Store(test.id)
# Look at the properties of the features
ftrs = data.frame(
  type      = unlist(lapply(df[1:1143,], class)),
  n.unique  = unlist(lapply(df[1:1143,], function(x) length(unique(x)))),
  f.missing = unlist(lapply(df[1:1143,], function(x) mean(is.na(x)))),
  spear.cor  = unlist(lapply(df[1:1143,], function(x) { idx = !is.na(x); 
  if (is.factor(x)) x = as.numeric(x);
  if (is.integer(x)) x = as.numeric(x);
  if (is.character(x)) x = as.numeric(x);
  cor(x[idx], y = as.numeric(label)[idx], method = 'spearman') }))
)

ftrs$name = rownames(ftrs)
ftrs = ftrs %>% drop_na()
df = df[,names(df) %in% ftrs$name]


##### Age
df$age = round(df$age)
test$age[test$age == ".d"] = 17
test$age[test$age == ""] = 17
test$age = round(as.numeric(test$age))

group_age <- function(age){
  if (age >= 0 & age <= 25){
    return('students')
  }else if(age > 25 & age <= 40){
    return('strong working class')
  }else if (age > 40 & age <= 55){
    return('weak working class')
  }else if (age > 55){
    return('grand parents')
  }
}

df$age2 <- sapply(df$age, group_age)
df$age2 <- as.factor(df$age2) %>% as.numeric()
test$age2 = sapply(test$age, group_age)
test$age2 = as.factor(test$age2) 
test$age2 = as.numeric(test$age2)

##### freq Encode
df$age_encode = freq.encode(df$age)
test$age_encode = freq.encode(test$age)

### hh size
group_size <- function(x){
  if (x > 0 & x <= 3){
    return('small family')
  }else if(x > 3 & x <= 6){
    return('average family')
  }else if (x > 6 & x <= 9){
    return('large family')
  }else{
    return('exlarge family')
  }
}

df$hhsize2 = sapply(df$hhsize, group_size)
df$hhsize2 = as.factor(df$hhsize2) %>% as.numeric()
test$hhsize2 = sapply(test$hhsize, group_size)
test$hhsize2 = as.factor(test$hhsize2) %>% as.numeric()

#### Education
group_edu <- function(x){
  if (x > 0 & x <= 4){
    return('bin1')
  }else if(x > 4 & x <= 8){
    return('bin2')
  }else if (x > 8 & x <= 12){
    return('bin3')
  }else{
    return('bin4')
  }
}
df$edu2 = sapply(df$edu, group_edu)
df$edu2 = as.factor(df$edu2) %>% as.numeric()
test$edu2 = sapply(test$edu, group_edu)
test$edu2 = as.factor(test$edu2) %>% as.numeric()
## freq encode
# df$edu_encode = freq.encode(df$edu)
# test$edu_encode = freq.encode(test$edu)

#### fsadwholed_often
df$fs_adwholed_often = round(df$fs_adwholed_often)
test$fs_adwholed_often = round(test$fs_adwholed_often)
df$fs_has0 = ifelse(df$fs_adwholed_often == 0,1,0)
test$fs_has0 = ifelse(test$fs_adwholed_often == 0,1,0)

##### med_u4_death
df$med_u5_deaths = round(df$med_u5_deaths)
test$med_u5_deaths = round(test$med_u5_deaths)
df$med_hasNA = ifelse(is.na(df$med_u5_deaths),1,0)
test$med_hasNA = ifelse(is.na(test$med_u5_deaths),1,0)
### school attend
df$ed_schoolattend = round(df$ed_schoolattend)
df$ed_schoolattend = ifelse(df$ed_schoolattend >=1, 1,0)
df$has_Na = ifelse(is.na(df$ed_schoolattend),1,0)
test$ed_schoolattend = round(test$ed_schoolattend)
test$has_Na = ifelse(is.na(test$ed_schoolattend),1,0)

# ##### ed expenses
# df$ed_expenses = round(df$ed_expenses)
# test$ed_expenses = round(test$ed_expenses)
# df$ed_expenses[df$ed_expenses>100] = 100
# test$ed_expenses[test$ed_expenses>100] = 100
# group_ex = function(x){
#   if(is.na(x)){
#    return("None") 
#   }else if(x >= 0 & x <= 20){
#     return('bin2')
#   }else if (x > 20 & x <= 44){
#     return('bin3')
#   }else{
#     return('bin4')
#   }
# }
# df$ed_expenses2 = sapply(df$ed_expenses,group_ex)
# df$ed_expenses2 = as.factor(df$ed_expenses2) %>% as.numeric()
# test$ed_expenses2 = sapply(test$ed_expenses, group_ex)
# test$ed_expenses2 = as.factor(test$ed_expenses) %>% as.numeric()

# ### durable investment
# df$durable_investment = round(df$durable_investment)
# df$durable_investment[df$durable_investment > 400] = 400
# df$durable_investment[df$durable_investment > 0 & df$durable_investment <=100] = 100
# df$durable_investment[df$durable_investment > 100 & df$durable_investment <=300] = 200
# df$durable_investment[df$durable_investment > 200 & df$durable_investment <=399] = 300
# test$durable_investment = round(test$durable_investment)
# test$durable_investment[test$durable_investment > 400] = 400
# test$durable_investment[test$durable_investment > 0 & test$durable_investment <=100] = 100
# test$durable_investment[test$durable_investment > 100 & test$durable_investment <=300] = 200
# test$durable_investment[test$durable_investment > 200 & test$durable_investment <=399] = 300

# ## ed work act
# df$ed_work_act_pc = round(df$ed_work_act_pc)
# df$ed_work_act_pc = ifelse(df$ed_work_act_pc >0,1,0)
# test$ed_work_act_pc = round(test$ed_work_act_pc)
# test$ed_work_act_pc = ifelse(test$ed_work_act_pc>0,1,0)
# 
# ###m med health consilt
# df$med_healthconsult = round(df$med_healthconsult)
# test$med_healthconsult = round(test$med_healthconsult)
# #### med sick days
# df$med_sickdays_hhave = round(df$med_sickdays_hhave)
# test$med_sickdays_hhave = round(test$med_sickdays_hhave)
# 
# ### cons social
# df$cons_social = round(df$cons_social)
# df$cons_social[df$cons_social>4] = 4
# test$cons_social = round(test$cons_social)
# test$cons_social[test$cons_social>4] = 4
# 
# df$cons_social_is0 = ifelse(df$cons_social == 0,1,0)
# test$cons_social_is0 = ifelse(test$cons_social == 0,1,0)
# 
# ### asset savings
# df$asset_savings = round(df$asset_savings)
# df$asset_savings[df$asset_savings > 16] = 16
# test$asset_savings = round(test$asset_savings)
# test$asset_savings[test$asset_savings>16] = 16
# 
# df$asset_savings_has0 = ifelse(df$asset_savings == 0,1,0)
# test$asset_savings_has0 = ifelse(test$asset_savings == 0,1,0)
# 
# ### asset livestock
# df$asset_livestock = round(df$asset_livestock)
# df$asset_livestock[df$asset_livestock>384] = 384
# test$asset_livestock = round(test$asset_livestock)
# test$asset_livestock[test$asset_livestock>384] = 384
# 
# df$lhas0 = ifelse(df$asset_livestock == 0,1,0)
# test$lhas0 = ifelse(test$asset_livestock == 0,1,0)
# 
# # asset phone
# df$asset_phone = round(df$asset_phone)
# df$asset_phone = ifelse(df$asset_phone>0,1,0)
# test$asset_phone = round(test$asset_phone)
# test$asset_phone = ifelse(test$asset_phone>0,1,0)
# 
# ## cons nondurable
# df$cons_nondurable =round(df$cons_nondurable)
# group_con <- function(x){
#   if (x > 0 & x <= 200){
#     return('bin1')
#   }else if(x > 200 & x <= 400){
#     return('bin2')
#   }else{
#     return('bin4')
#   }
# }
# df$con = sapply(df$cons_nondurable,group_con)
# df$con = as.factor(df$con) %>% as.numeric()
# test$con = sapply(test$cons_nondurable, group_con)
# test$con = as.factor(test$con) %>% as.numeric()
# 

# Separate date elements
df$survey_date= as.character(df$survey_date)
date.format <- as.Date(as.character(df$survey_date), format="%d-%b-%Y")
df$survey_date = as.Date(df$survey_date, format="%d-%b-%Y")
df$survey_date = df$survey_date %>% gsub(pattern = "00", replacement = "19")
df$year = year(df$survey_date) %>% as.factor() %>% as.numeric()
df$month = as.factor(months(date.format)) %>% as.numeric()
df$weekdays = as.factor(weekdays(date.format)) %>% as.numeric()
df$quarters = quarters(date.format) %>% as.factor() %>% as.numeric()
df$Date_week <- as.integer(strftime(df$survey_date, format="%W"))
#####
test$survey_date= as.character(test$survey_date)
date.format <- as.Date(test$survey_date, format="%d-%b-%Y")
test$survey_date = as.Date(test$survey_date, format="%d-%b-%Y")
test$survey_date = test$survey_date %>% gsub(pattern = "00", replacement = "19")
test$year = year(test$survey_date) %>% as.factor() %>% as.numeric()
test$month = as.factor(months(date.format)) %>% as.numeric()
test$weekdays = as.factor(weekdays(date.format)) %>% as.numeric()
test$quarters = quarters(date.format) %>% as.factor() %>% as.numeric()
test$Date_week <- as.integer(strftime(test$survey_date, format="%W"))


df = df[,-c(1:4)]
df = df %>% select(-depressed)
v = names(df)
df[is.na(df)] = -1
#### MISSING VALUES
#check for columns with missing values
# na.cols <- which(colSums(is.na(df)) > 0)
# na.cols <- sort(colSums(sapply(df[na.cols], is.na)), decreasing = TRUE)
# paste('There are', length(na.cols), 'columns with missing values')
# 
# ## hh total member
# summary(as.factor(df$hh_totalmembers))
# ## can be drop as it's the same as hhsize

##

test = test[, names(test) %in% v]
test[is.na(test)] = -1





tr = cbind(df,y = as.factor(label))
#tr = df
# df2 = tr %>% filter(y == 0) %>%
#   sample_n(size = 193, replace = F)
# df3 = tr %>% filter(y == 1)
# tr = rbind(df2,df3)
# df2 = tr %>% select(-y)
Store(tr)
set.seed(1235)
samples = sample.split(tr$y,SplitRatio = 0.75)
trainset = subset(tr, samples == TRUE)
testset = subset(tr,samples == FALSE)
rm(samples)

# ## Feature eng
trlabel = trainset$y
trainset = trainset %>% within(rm("y"))
testlab = testset$y %>% as.factor()
testset = testset %>% within(rm("y"))
Store(trainset);Store(testset);Store(trlabel); Store(testlab)


label2 = ifelse(tr$y == 0, "No","Yes")
# GBM
# model to use
# treebag, gcvEarth,pcr,glmnet,bsyesglm,pls,glm.nb
start.time = Sys.time()
h2o::h2o.init(nthreads = -1)
#grid = expand.grid(ncomp = c(40,50,60,70))
grid <- expand.grid(eta = 0.03,#2^seq(-7,-5), 
                    colsample_bytree = c(0.6,0.75,0.8,1),
                    max_depth = c(1:3), 
                    min_child_weight = c(1,2,3),
                    nrounds = 50, gamma = 0,
                    subsample = 0.9)
#gamma = c(0.01))
set.seed(1235)
gbm.mod <- caret::train(x = df, y = as.factor(label2), method ="xgbTree",
                        trControl = trainControl(
                          method = "cv",
                          number = 5,
                          verboseIter = T,
                          classProbs = T,
                          savePredictions = "final"
                        ))#, tuneGrid = grid)
gbm.mod

total.time = Sys.time() - start.time
total.time 

plot(gbm.mod)
plot(varImp(gbm.mod),30)

# Predict
pred.cub <- predict(gbm.mod, newdata = testset, type = "prob")$Yes
pred.cub = ifelse(pred.cub >0.3,1,0)
Accuracy(pred.cub,testlab)
# Evaluation
#library(pROC)
auc1 <- pROC::roc(as.numeric(testlab), as.numeric(pred.cub),plot = T)
plot(auc1, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc1$auc[[1]],3)),col = 'blue')

# Predict
pred.cub <- predict(gbm.mod, newdata = test, type = "prob")$Yes
pred.cub = ifelse(pred.cub>0.3,1,0)

###
sub <- data.frame(test.id, pred.cub)
colnames(sub) <-c("surveyid","depressed")
write.csv(sub, file =paste0(subm.dir,"/", Sys.Date(),"_sub_final2.csv"), row.names = FALSE)



j = read.csv(paste0(subm.dir,"/",Sys.Date(),"sub_gbm.csv"))


pred.cub = (0.8*xgbpred+rfpred*0.2)



#####
library(lightgbm)
param = list(objective = "binary",
            #eval = "Accuracy",
             boosting = "gbdt",
             learning_rate = 0.03,
            boost_from_average = FALSE)
dd = lgb.Dataset(as.matrix(df), label = label2)
d2 = lgb.Dataset(as.matrix(testset))
dtest = lgb.Dataset(test)
set.seed(1235)
gbm.mod = lightgbm(params = param, data = dd, nrounds = 582,
                   verbose = 1)
pred.cub <- predict(gbm.mod, data = as.matrix(testset))
MAE(pred.cub, testlab)

pred.cub <- predict(gbm.mod, data = as.matrix(df_test))


set.seed(1235)
cv = lgb.cv(params = param,dd, 
            nrounds = 500,nfold = 5, 
            verbose = 100,early_stopping_rounds = 20)




ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = T,
  classProbs = T,
  savePredictions = "final" 
)
library(caretEnsemble)

models <- c("gbm","xgbTree","glmnet","lda","rf")
model2 = c("glmboost","lda2","evtree","cforest","rpart")
model3 = c("glmnet","xgbTree")
set.seed(1235)
mul.mod = caretList(x = df, y = as.factor(label2), 
                    trControl = ctrl,
                    methodList = models,
                    continue_on_fail = TRUE)

res = resamples(mul.mod)
summary(res)
modelCor(res)

#####
simple.ens = caretEnsemble(mul.mod,
                           trControl = ctrl,
                           metric = "Accuracy")
summary(simple.ens)
## make predictions
pred.cub = predict(simple.ens, newdata = test,type = "prob")



###### 
set.seed(1235)
stack.mod = caretStack(mul.mod,
                       method = "glmboost",
                       trControl = trainControl(
                         method = "boot",
                         number = 5,
                         verboseIter = T,
                         classProbs = T,
                         savePredictions = "final"
                       ))
summary(stack.mod)

## make predictions
pred.cub = predict(stack.mod, newdata = test, type = "prob")




library(catboost)
grid <- expand.grid(depth = c(2, 4, 6, 8),
                    learning_rate = 0.01,
                    iterations = 50,
                    l2_leaf_reg = 0.001,
                    rsm = 0.95,
                    border_count = c(10,20))
set.seed(1235)
gbm.mod <- train(df, as.factor(label2),
                method = catboost.caret,
                logging_level = 'Verbose', preProc = NULL,
                tuneGrid = grid, trControl = ctrl)

print(gbm.mod)

importance = varImp(gbm.mod, scale = FALSE)
print(importance)
