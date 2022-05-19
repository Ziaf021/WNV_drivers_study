require(xgboost)
library(caTools)
library(caret)
library(ROCR)
library(pROC)
library(SHAPforxgboost)
library(data.table)
library(here)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidymodels)
library(Metrics) 
library(imputeTS)
library(DMwR) #SMOTE function
library(ROSE) #ROSE function
library(reticulate)
library(SHAPforxgboost)
library(DALEX)
library(shapper)
library(iBreakDown)
library(breakDown)
library(caret)
library(viridis)
library(ggrepel)
library(compare)
library(cowplot)
library(ggpubr)


#upload necessary functions to be used () 
source(paste(getwd(),'/','scripts', '/', 'functionsFile_WNV.R', sep=''))

#upload the data file , 
df_wnv <- 
  readxl::read_xlsx("WNV_Data_Q2_21June.xlsx")%>% 
  mutate(wnf_case= fct_rev(as.factor(wnf_case))) %>%
  mutate(CNTR_CODE= str_sub(NUTS_ID,1,2))


df_wnv <- df_wnv%>%
  dplyr::select(-wnf_case, everything())

cols_to_drop <- c(1:2)
train_wnv <- df_wnv%>% filter(year!=2018 & year !=2019)%>%
  mutate_if(is.character,as.numeric)%>%
  dplyr::select(-cols_to_drop)


test_2018 <- df_wnv%>% filter (year== 2018)%>% 
  mutate_if(is.character,as.numeric)%>%
  dplyr::select(-cols_to_drop)
test_2019 <- df_wnv%>% filter (year== 2019)%>%
  mutate_if(is.character,as.numeric)%>%
  dplyr::select(-cols_to_drop)


label_training <- as.numeric(levels(train_wnv$wnf_case))[train_wnv$wnf_case]
label_test <- as.numeric(levels(test_2018$wnf_case))[test_2018$wnf_case]

dMtrxTrain = xgb.DMatrix(as.matrix(sapply(train_wnv[,-dim(train_wnv)[2]], as.numeric)), label=label_training)
dMtrxTest = xgb.DMatrix(as.matrix(sapply(test_2018[,-dim(test_2018)[2]], as.numeric)), label=label_test)

label_test19 <- as.numeric(levels(test_2019$wnf_case))[test_2019$wnf_case]

dMtrxTest19 = xgb.DMatrix(as.matrix(sapply(test_2019[,-dim(test_2019)[2]], as.numeric)), label=label_test19)

# Calculate scale_pos_weight: an xgb parameter to handle imbalanced data, as in our case.

class_count <- table(train_wnv$wnf_case)
scale_pos_weight <- class_count[2] / class_count[1]

# XGBoost parameters: This hyper-parameters set is tuned one
params.xgb <- list(booster = "gbtree", 
                   objective = "binary:logistic", 
                   eta =0.1, 
                   gamma=1, 
                   min_child_weight = 5,# ,
                   scale_pos_weight = scale_pos_weight ,
                   lambda=1 )
# xgb corss-validation train
xgbcv <- xgb.cv(params =  params.xgb, 
                data = dMtrxTrain, 
                nrounds = 1000, 
                nfold = 5, 
                showsd = F,
                prediction = T,
                stratified = T, 
                print_every_n = 1, 
                early_stopping_rounds = 30, 
                maximize = F
)

#' Final training and metrics evaluation on test sets

set.seed(1)

watchlist = list( test2018 = dMtrxTest,
                  test2019 = dMtrxTest19,
                  train = dMtrxTrain) 

model.xgb <- xgb.train(params = params.xgb, 
                       data = dMtrxTrain, 
                       nrounds =  1000,   
                       watchlist = watchlist, 
                       print_every_n = 1, 
                       early_stopping_rounds =30, 
                       maximize = F
)


#plot training and test error curves 
xgbCV_plot(xgbcv)
#set classification threshold for training 
trn_class_thrshold <- 0.5
trn_vld <- fct_rev(as.factor(ifelse(xgbcv$pred>trn_class_thrshold,1,0)))

#calculate confusion matrix for training set
tv_cm <- confusionMatrix(trn_vld, 
                         fct_rev((as.factor(label_training))), positive = "1")

title_train<- paste("Train-Threshold = ", 
                    trn_class_thrshold, sep='' )
draw_confusion_matrix(tv_cm,title_train )


#' Model Prediction:
## predict results for test_2018 data
xgb.pred <- predict(model.xgb, dMtrxTest)

tst2018_thrshold  <-  0.1

#calculate confusion matrix
cm_xgb  <- ConfMatrix(xgb.pred, label_test,tst2018_thrshold) 

#draw confusion matrix with different evaluation metrics
roc_2018 <- roc(label_test, xgb.pred)

title_2018 <- paste("Test2018-threshold = ", 
                    tst2018_thrshold,", ROC_AUC = ", 
                    round(roc_2018$auc,2), sep='' )

draw_confusion_matrix(cm_xgb, title_2018)

df_metrics2018 <- fn_chooseThreshold(model.xgb,dMtrxTest,label_test, c(0.1,0.2,0.5))%>%
  mutate(`Test.Year` = c("2018"))%>%
  mutate(Model=rep("Model Q2"))%>%
  mutate(ROC.AUC=rep(round(as.numeric(roc_2018$auc),2), dim(df_metrics2018)[1]))
  
## predict results for test_2019 data
xgb.pred19 <- predict(model.xgb, dMtrxTest19)

#calculate confusion matrix
tst2019_thrshold = 0.1
cm_xgb19 <- ConfMatrix(xgb.pred19, label_test19,tst2019_thrshold)

#draw confusion matrix with different evaluation metrics
roc_2019 <- roc(label_test19, xgb.pred19)
title_2019 <- paste("Test2019:threshold = ", 
                    tst2019_thrshold,", ROC_AUC = ", round(roc_2019$auc,2), sep='' )
draw_confusion_matrix(cm_xgb19,title_2019)

## estimate classification metrics for different thresholds for test_2019 data

df_metrics2018_2019 <-  fn_chooseThreshold(model.xgb,dMtrxTest,label_test, c(0.1,0.2,0.5))%>%
  mutate(`Test.Year` = c("2018"))%>%
  mutate(Model=rep(Model_name))%>%
  mutate(ROC.AUC=rep(round(as.numeric(roc_2019$auc),2), dim(df_metrics2019)[1]))%>%
  rbind(
  fn_chooseThreshold(model.xgb,dMtrxTest19,label_test19, c(0.1,0.2,0.5))%>%
  mutate(`Test.Year` = c("2019"))%>%
  mutate(Model=rep("Model Q2"))%>%
  mutate(ROC.AUC=rep(round(as.numeric(roc_2019$auc),2), dim(df_metrics2019)[1])))


# To return the SHAP values and ranked features by mean|SHAP|
shap_values <- shap.values(xgb_model = model.xgb, X_train = dMtrxTrain)

