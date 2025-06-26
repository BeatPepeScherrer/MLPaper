#############################################################################################################
#  
#  Description: Model long Benin
#
#  Authors:     Kathrin Durizzo               
#  Date:        30.07.2024
#
#############################################################################################################

#------------------------------------------------------------------------------------------------------------
#
#                                         Setup
#
#------------------------------------------------------------------------------------------------------------
options(warn = -1)

library(Hmisc)
library(stargazer)
library(ROSE)
library(caret)
library(ROCR)
library(glmnet)
library(xgboost)
library(randomForest)
library(dplyr)
library(haven)

getwd()
setwd("L:/research_assistants/Pepe/MLPaper")
load("merged_data.RData")

#------------------------------------------------------------------------------------------------------------
#
#                   I. Create model for insurance coverage
#
#------------------------------------------------------------------------------------------------------------
# summary(df)

## prepare insurance sample
# merged_data <- df[ , -68:-71] # excludes Other outcomes
# merged_data <- merged_data[ which(!is.na(merged_data$insurance)),] # excludes missing values
merged_data <- merged_data[!names(merged_data) %in% c("female", "education_NA", "HHID", "age", "hv001", "hv022", "hv021", "NAME_0", "region", "country", "LATNUM", "LONGNUM", "hv_wt", "year")] # exclude variables we do not need for the model
# merged_data <- merged_data[!names(merged_data) %in% c("slept","water_interrupted","share_toilet","soap", "child_mosquitonet","female_spend_health_care","female_spend_earnings", "female_spend_health_car", "tested_HIV")] # exclude variables we do not have any variation after dropping missing values in next step
# merged_data <- na.omit(merged_data) # exclude all missing values
# merged_data <- merged_data %>% mutate(across(-c(mosquitonets_nr, age_HH_head, member_child_under_5, member_men_HH, member_women_HH, member_all_HH), as.factor)) # Recode all variables in the data frame to factors
# summary(merged_data)
str(merged_data)

#splitting
smp_size_insurance <- floor(0.66 * nrow(merged_data)) # use 1/3 and 2/3 split
set.seed(1)   # set the seed to make it reproducible
train_ind_insurance <- sample(seq_len(nrow(merged_data)), size = smp_size_insurance)
train_insurance <- merged_data[train_ind_insurance, ]
test_insurance <- merged_data[-train_ind_insurance, ]

#imbalanced sample option II: increase share of positives to 20%
train_insurance <- ovun.sample(insurance~., data = train_insurance, method = "over", p = 0.2)$data
table(train_insurance$insurance)

#extra split for Lasso
shrinkage_test_insurance <- test_insurance[ ,-53] # Removes insurance
x_test <- model.matrix(~ ., data=shrinkage_test_insurance)
shrinkage_train_insurance <- train_insurance[ ,-53] # Removes insurance
x_train <- model.matrix(~ ., data=shrinkage_train_insurance)
train_dummy_x._insurance <- data.matrix(x_train) 
train_dummy_y._insurance <- as.double(as.matrix(train_insurance[, 53])) # Only insurance
test_dummy_x._insurance <- data.matrix(x_test) # Removes insurance

#define repeated CV
train_control <- trainControl(method="repeatedcv", number=5, repeats=5, savePredictions="final")

#===============================================================================
#
# I. Logit
#
#===============================================================================

logit_general_insurance  <- glm(as.factor(insurance)~ ., data=merged_data, family="binomial")

#Train-Test
predict_general_insurance <- predict(logit_general_insurance, merged_data)
Perf_general_insurance <- prediction(as.numeric(predict_general_insurance), as.numeric(merged_data$insurance))
AUC_general_insurance <- performance(Perf_general_insurance , "auc") 
ROC_general_insurance <- performance(Perf_general_insurance, "tpr", "fpr")
RP_general_insurance <- performance(Perf_general_insurance, "prec","rec")

pred_dummy <-  ifelse(predict_general_insurance> 0.5 , 1,0) 
cm_general_insurance <- confusionMatrix(as.factor(pred_dummy),as.factor(merged_data$insurance), positive = "1") 

cm_general_insurance
AUC_general_insurance@y.values
summary(logit_general_insurance)


#===============================================================================
# I.I In sample
#===============================================================================
set.seed(1)
logit_insurance_cv  <- train(as.factor(insurance)~.,
                             data=train_insurance, trControl=train_control, method="glm", family="binomial")

#Train-Test
predict_logit_insurance_cv <- predict(logit_insurance_cv, test_insurance, type="prob")
Perf_logit_insurance_cv <- prediction(as.numeric(predict_logit_insurance_cv[[2]]), as.numeric(test_insurance$insurance))
AUC_logit_insurance_cv  <- performance(Perf_logit_insurance_cv , "auc") 
ROC_logit_insurance_cv  <- performance(Perf_logit_insurance_cv , "tpr", "fpr")
RP_logit_insurance_cv  <- performance(Perf_logit_insurance_cv , "prec","rec")

pred_dummy <-  ifelse(predict_logit_insurance_cv[[2]]> 0.5 , 1,0) 
cm_logit_insurance_cv <- confusionMatrix(as.factor(pred_dummy),as.factor(test_insurance$insurance), positive = "1") 

cm_logit_insurance_cv
AUC_logit_insurance_cv@y.values
summary(logit_insurance_cv)
varImp(logit_insurance_cv)$importance


#===============================================================================
#
# II. Lasso
#
#===============================================================================

#===============================================================================
# II.I In sample
#===============================================================================
set.seed(1)
lambda_dummy_TZ <- cv.glmnet(train_dummy_x._insurance, train_dummy_y._insurance, family='binomial', alpha=1,  standardize=TRUE, nfolds=5, type.measure = "mse")
lambda_dummy_TZ$lambda.1se
lambda_dummy_TZ$lambda.min

set.seed(1)
lasso_insurance_cv  <- train(as.factor(insurance) ~., 
                             data=train_insurance, trControl=train_control,  method="glmnet", 
                             tuneGrid= expand.grid(.alpha=1, .lambda= lambda_dummy_TZ$lambda.1se))


#Train-Test
predict_lasso_insurance_cv <- predict(lasso_insurance_cv, test_insurance,type="prob")
Perf_lasso_insurance_cv <- prediction(as.numeric(predict_lasso_insurance_cv[[2]]), as.numeric(test_insurance$insurance))
AUC_lasso_insurance_cv  <- performance(Perf_lasso_insurance_cv , "auc") 
ROC_lasso_insurance_cv  <- performance(Perf_lasso_insurance_cv , "tpr", "fpr")
RP_lasso_insurance_cv  <- performance(Perf_lasso_insurance_cv , "prec","rec")

pred_dummy <-  ifelse(predict_lasso_insurance_cv[[2]]> 0.5 , 1,0) 
cm_lasso_insurance_cv <- confusionMatrix(as.factor(pred_dummy),as.factor(test_insurance$insurance), positive = "1") 

cm_lasso_insurance_cv 
AUC_lasso_insurance_cv
varImp(lasso_insurance_cv)$importance


# Coefficients from cv.glmnet object
coef_lasso_lambda_1se <- coef(lambda_dummy_TZ, s = "lambda.1se")
print(coef_lasso_lambda_1se)

# Optionally, coefficients at lambda.min
coef_lasso_lambda_min <- coef(lambda_dummy_TZ, s = "lambda.min")
print(coef_lasso_lambda_min)

# Extract coefficients from the caret trained model
lasso_best_lambda <- lasso_insurance_cv$bestTune$.lambda
final_model <- lasso_insurance_cv$finalModel
coef_final_model <- coef(final_model, s = lasso_best_lambda)
print(coef_final_model)





#===============================================================================
#
# III. Random forest
#
#===============================================================================

#===============================================================================
# III.I In sample
#===============================================================================
tuneRF(train_dummy_x._insurance, as.factor(train_dummy_y._insurance), stepFactor=1.5)  # 13 optimal
set.seed(1)
rf_insurance_cv  <- train(as.factor(insurance)~. ,
                          data=train_insurance, trControl=train_control, method="rf",
                          tuneGrid =expand.grid(mtry =13))

#Train-Test
predict_rf_insurance_cv <- predict(rf_insurance_cv, test_insurance, type="prob")
Perf_rf_insurance_cv <- prediction(as.numeric(predict_rf_insurance_cv[[2]]), as.numeric(test_insurance$insurance))
AUC_rf_insurance_cv  <- performance(Perf_rf_insurance_cv , "auc") 
ROC_rf_insurance_cv  <- performance(Perf_rf_insurance_cv , "tpr", "fpr")
RP_rf_insurance_cv  <- performance(Perf_rf_insurance_cv , "prec","rec")

pred_dummy <-  ifelse(predict_rf_insurance_cv[[2]]> 0.5 , 1,0) 
cm_rf_insurance_cv <- confusionMatrix(as.factor(pred_dummy),as.factor(test_insurance$insurance), positive = "1") 
cm_rf_insurance_cv
AUC_rf_insurance_cv 
varImp(rf_insurance_cv)$importance






#===============================================================================
#
# IV. Neural Network
#
#===============================================================================

#===============================================================================
# IV.I In sample
#===============================================================================
set.seed(1)
nnetGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1), decay = seq(from = 0.1, to = 0.5, by = 0.1)) #best tune is size 1 and decay = 0.5
#nnet_insurance_cv <-train(as.factor(insurance)~.,
#                         data=train_insurance, method='nnet', trControl = train_control, tuneGrid=expand.grid(size=5, decay=0.1)) ##nnet or neuralnet
nnet_insurance_cv <-train(as.factor(insurance)~. ,
                          data=train_insurance, method='nnet', 
                          trControl = train_control, tuneGrid=nnetGrid) ##nnet or neuralnet

#Train-Test
predict_nnet_insurance_cv <- predict(nnet_insurance_cv,test_insurance, type="prob") 
Perf_nnet_insurance_cv <- prediction(as.numeric(predict_nnet_insurance_cv[[2]]), as.numeric(test_insurance$insurance))
AUC_nnet_insurance_cv  <- performance(Perf_nnet_insurance_cv, "auc") 
ROC_nnet_insurance_cv  <- performance(Perf_nnet_insurance_cv, "tpr", "fpr")
RP_nnet_insurance_cv  <- performance(Perf_nnet_insurance_cv, "prec","rec")

pred_dummy <-  ifelse(predict_nnet_insurance_cv[[2]]> 0.5 , 1,0)
cm_nnet_insurance_cv <- confusionMatrix(as.factor(pred_dummy),as.factor(test_insurance$insurance), positive = "1") 

cm_nnet_insurance_cv
AUC_nnet_insurance_cv
varImp(nnet_insurance_cv)$importance




#===============================================================================
#
# V. XGBoost
#
#===============================================================================

#===============================================================================
# V.I In sample
#===============================================================================
set.seed(1)
tune_grid <- expand.grid(nrounds=c(100,200,300,400), max_depth = c(3:7),
                         eta = c(0.05, 1),
                         gamma = c(0.01),
                         colsample_bytree = c(0.75),
                         subsample = c(0.50),
                         min_child_weight = c(0))

xgboost_insurance_cv <- train(as.factor(insurance) ~., 
                              data = train_insurance, method = "xgbTree", 
                              trControl=train_control, tuneGrid = tune_grid, tuneLength = 10)

#Train-Test
predict_xgboost_insurance_cv <- predict(xgboost_insurance_cv,test_insurance, type="prob") 
Perf_xgboost_insurance_cv <- prediction(as.numeric(predict_xgboost_insurance_cv[[2]]), as.numeric(test_insurance$insurance))
AUC_xgboost_insurance_cv  <- performance(Perf_xgboost_insurance_cv, "auc") 
ROC_xgboost_insurance_cv  <- performance(Perf_xgboost_insurance_cv, "tpr", "fpr")
RP_xgboost_insurance_cv  <- performance(Perf_xgboost_insurance_cv, "prec","rec")

pred_dummy <-  ifelse(predict_xgboost_insurance_cv[[2]]> 0.5 , 1,0)
cm_xgboost_insurance_cv <- confusionMatrix(as.factor(pred_dummy),as.factor(test_insurance$insurance), positive = "1") 

cm_xgboost_insurance_cv
AUC_xgboost_insurance_cv
varImp(xgboost_insurance_cv)$importance






#===============================================================================
#
# V. Output
#
#===============================================================================
Performance_logit_general_insurance_1 <- as.data.frame(cm_general_insurance$overall)
Performance_logit_general_insurance_1 <- rename(Performance_logit_general_insurance_1 , c("logit_general_insurance" = "cm_general_insurance$overall"))
Performance_logit_general_insurance_2 <- as.data.frame(cm_general_insurance$byClass)
Performance_logit_general_insurance_2 <- rename(Performance_logit_general_insurance_2 , c("logit_general_insurance" = "cm_general_insurance$byClass"))
merged_Performance_logit_general_insurance<- rbind(Performance_logit_general_insurance_1, Performance_logit_general_insurance_2)
rm(Performance_logit_general_insurance_1, Performance_logit_general_insurance_2)

Performance_logit_insurance_1 <- as.data.frame(cm_logit_insurance_cv$overall)
Performance_logit_insurance_1 <- rename(Performance_logit_insurance_1 , c("logit_insurance_cv" = "cm_logit_insurance_cv$overall"))
Performance_logit_insurance_2 <- as.data.frame(cm_logit_insurance_cv$byClass)
Performance_logit_insurance_2 <- rename(Performance_logit_insurance_2 , c("logit_insurance_cv" = "cm_logit_insurance_cv$byClass"))
merged_Performance_logit_insurance<- rbind(Performance_logit_insurance_1, Performance_logit_insurance_2)
rm(Performance_logit_insurance_1, Performance_logit_insurance_2)

Performance_lasso_insurance_1 <- as.data.frame(cm_lasso_insurance_cv$overall)
Performance_lasso_insurance_1 <- rename(Performance_lasso_insurance_1 , c("lasso_insurance_cv" = "cm_lasso_insurance_cv$overall"))
Performance_lasso_insurance_2 <- as.data.frame(cm_lasso_insurance_cv$byClass)
Performance_lasso_insurance_2 <- rename(Performance_lasso_insurance_2 , c("lasso_insurance_cv" = "cm_lasso_insurance_cv$byClass"))
merged_Performance_lasso_insurance<- rbind(Performance_lasso_insurance_1, Performance_lasso_insurance_2)
rm(Performance_lasso_insurance_1, Performance_lasso_insurance_2)

Performance_rf_insurance_1 <- as.data.frame(cm_rf_insurance_cv$overall)
Performance_rf_insurance_1 <- rename(Performance_rf_insurance_1 , c("rf_insurance_cv" = "cm_rf_insurance_cv$overall"))
Performance_rf_insurance_2 <- as.data.frame(cm_rf_insurance_cv$byClass)
Performance_rf_insurance_2 <- rename(Performance_rf_insurance_2 , c("rf_insurance_cv" = "cm_rf_insurance_cv$byClass"))
merged_Performance_rf_insurance<- rbind(Performance_rf_insurance_1, Performance_rf_insurance_2)
rm(Performance_rf_insurance_1, Performance_rf_insurance_2)

Performance_nnet_insurance_1 <- as.data.frame(cm_nnet_insurance_cv$overall)
Performance_nnet_insurance_1 <- rename(Performance_nnet_insurance_1 , c("nnet_insurance_cv" = "cm_nnet_insurance_cv$overall"))
Performance_nnet_insurance_2 <- as.data.frame(cm_nnet_insurance_cv$byClass)
Performance_nnet_insurance_2 <- rename(Performance_nnet_insurance_2 , c("nnet_insurance_cv" = "cm_nnet_insurance_cv$byClass"))
merged_Performance_nnet_insurance<- rbind(Performance_nnet_insurance_1, Performance_nnet_insurance_2)
rm(Performance_nnet_insurance_1, Performance_nnet_insurance_2)

Performance_xgboost_insurance_1 <- as.data.frame(cm_xgboost_insurance_cv$overall)
Performance_xgboost_insurance_1 <- rename(Performance_xgboost_insurance_1 , c("xgboost_insurance_cv" = "cm_xgboost_insurance_cv$overall"))
Performance_xgboost_insurance_2 <- as.data.frame(cm_xgboost_insurance_cv$byClass)
Performance_xgboost_insurance_2 <- rename(Performance_xgboost_insurance_2 , c("xgboost_insurance_cv" = "cm_xgboost_insurance_cv$byClass"))
merged_Performance_xgboost_insurance<- rbind(Performance_xgboost_insurance_1, Performance_xgboost_insurance_2)
rm(Performance_xgboost_insurance_1, Performance_xgboost_insurance_2)

Output_Performance <- cbind(merged_Performance_logit_general_insurance,
                            merged_Performance_logit_insurance,
                            merged_Performance_lasso_insurance,
                            merged_Performance_rf_insurance, 
                            merged_Performance_nnet_insurance, 
                            merged_Performance_xgboost_insurance)
write.csv(Output_Performance, file = "Tables_Figures/Performance_Masterfile_insurance_20240722_Nigeria.csv")   


AUC_general_insurance@y.values
AUC_logit_insurance_cv@y.values
AUC_lasso_insurance_cv@y.values
AUC_rf_insurance_cv@y.values
AUC_nnet_insurance_cv@y.values
AUC_xgboost_insurance_cv@y.values
AUC_general_insurance <- as.data.frame(AUC_general_insurance@y.values)
AUC_logit_insurance <- as.data.frame(AUC_logit_insurance_cv@y.values)
AUC_lasso_insurance <- as.data.frame(AUC_lasso_insurance_cv@y.values)
AUC_rf_insurance <- as.data.frame(AUC_rf_insurance_cv@y.values)
AUC_nnet_insurance <- as.data.frame(AUC_nnet_insurance_cv@y.values)
AUC_xgboost_insurance <- as.data.frame(AUC_xgboost_insurance_cv@y.values)
Output_AUC <- cbind(     AUC_general_insurance,
                         AUC_logit_insurance,
                         AUC_lasso_insurance,
                         AUC_rf_insurance, 
                         AUC_nnet_insurance, 
                         AUC_xgboost_insurance)
write.csv(Output_AUC, file = "Tables_Figures/Performance_AUC_insurance_20240722_Nigeria.csv")   


#varImp(logit_insurance_cv, scale = FALSE)$importance
varImp(logit_insurance_cv, scale = T)$importance
#varImp(lasso_insurance_cv, scale = FALSE)$importance
varImp(lasso_insurance_cv, scale = T)$importance
varImp(rf_insurance_cv)$importance
varImp(nnet_insurance_cv)$importance
varImp(xgboost_insurance_cv)$importance
summary(logit_insurance_cv)
varImp_logit_insurance <- as.data.frame(varImp(logit_insurance_cv, scale = T)$importance)
varImp_lasso_insurance <- as.data.frame(varImp(lasso_insurance_cv, scale = T)$importance)
varImp_rf_insurance <- as.data.frame(varImp(rf_insurance_cv, scale = T)$importance)
varImp_nnet_insurance <- as.data.frame(varImp(nnet_insurance_cv, scale = T)$importance)
varImp_xgboost_insurance <- as.data.frame(varImp(xgboost_insurance_cv, scale = T)$importance)
Output_varImp <- cbind(     varImp_lasso_insurance,
                            varImp_rf_insurance, 
                            varImp_nnet_insurance, 
                            varImp_xgboost_insurance)
write.csv(varImp_logit_insurance, file = "Tables_Figures/Performance_varImp_logit_insurance_20240722_Nigeria.csv")   
write.csv(Output_varImp, file = "Tables_Figures/Performance_varImp_insurance_20240722_Nigeria.csv")   

test_insurance$insurance <- as.numeric(test_insurance$insurance) -1
test_insurance$average_insurance <- mean(test_insurance$insurance)
test_insurance$average_insurance_base <- mean(test_insurance$insurance_logit)
test_insurance$insurance_logit <- predict_logit_insurance_cv$`1`
test_insurance$average_insurance_logit <- mean(test_insurance$insurance_logit)
test_insurance$insurance_lasso <- predict_lasso_insurance_cv$`1`
test_insurance$average_insurance_lasso <- mean(test_insurance$insurance_lasso)
test_insurance$insurance_rf <- predict_rf_insurance_cv$`1`
test_insurance$average_insurance_rf <- mean(test_insurance$insurance_rf)
test_insurance$insurance_nnet <- predict_nnet_insurance_cv$`1`
test_insurance$average_insurance_nnet <- mean(test_insurance$insurance_nnet)
test_insurance$insurance_xgboost <- predict_xgboost_insurance_cv$`1`
test_insurance$average_insurance_xgboost <- mean(test_insurance$insurance_xgboost)
test_insurance$insurance_base <- cm_general_insurance$byClass["Detection Prevalence"]
save(test_insurance, file="Data/test_insurance_with_prediciton_20240722_Nigeria.RData")

test_insurance_extract <- test_insurance[ , c(85, 88, 90, 92, 94, 96, 97)] 
test_insurance_extract <- test_insurance_extract[!duplicated(test_insurance_extract$average_insurance), ]
write.csv(test_insurance_extract, file = "Tables_Figures/test_insurance_20240722_Nigeria.csv")   