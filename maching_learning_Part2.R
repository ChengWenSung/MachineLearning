##

setwd("D:/wen_sung/psychology/Machine_learning/final_report")

##library packages

pacman::p_load(intsvy,tidyverse,plyr,ltm,gridExtra,coefplot,randomForest,gbm,e1071,pROC,glmnet,kknn)

##

dta <- read.csv("TIMSS_2011_8th.csv")

dta <- dta%>%mutate(Math = (BSMMAT01+BSMMAT02+BSMMAT03+BSMMAT04+BSMMAT05)/5)%>%dplyr::select(-BSMMAT01,-BSMMAT02,-BSMMAT03,-BSMMAT04,-BSMMAT05)
dta_group_c <- dta%>%dplyr::group_by(IDCLASS)%>%dplyr::summarise(mean_math_class = mean(Math))
dta <- dplyr::left_join(dta,dta_group_c,by = c("IDCLASS","IDCLASS"))
dta_group_s <- dta%>%dplyr::group_by(IDSCHOOL)%>%dplyr::summarise(mean_math_school = mean(Math))
dta <- dplyr::left_join(dta,dta_group_s,by = c("IDSCHOOL","IDSCHOOL"))

##

set.seed(20160619) 
idc_train <- sample(c(1,2,3,4), nrow(dta), replace = T, prob = c(0.25,0.25,0.25,0.25))

#use training data for model training
#testing 1 for checking model prediction
#testing 2 for ensemble learning
training_data <- dta[idc_train %in% c(1,2),]
testing1 <- dta[idc_train == 3,]
testing2 <- dta[idc_train == 4,]


##random forest

randomForest_fit <- randomForest(Math ~ . -IDSCHOOL-IDCLASS-IDSTUD, data = dta, subset = idc_train %in% c(1,2), mtry = 80, sampsize = 3000, ntree = 2500, importance = T) 
y_hat_train_forest <- predict(randomForest_fit, newdata = dta[idc_train %in% c(1,2),]) 
y_hat_test_forest <- predict(randomForest_fit, newdata = dta[idc_train == 3,])

r2_train_forest <- 1 - mean((y_hat_train_forest - dta$Math[idc_train %in% c(1,2)])^2) / var(dta$Math[idc_train %in% c(1,2)]) 
r2_test_forest <- 1 - mean((y_hat_test_forest - dta$Math[idc_train == 3])^2) / var(dta$Math[idc_train == 3]) 

load("randomForest_fit.RData")

##gradient boosting

train <- training_data%>%dplyr::select(-c(IDSCHOOL,IDCLASS,IDSTUD))
x <- train %>% model.matrix(Math ~ (.)-1, data = .)
y <- train$Math

test <- testing1%>%dplyr::select(-c(IDSCHOOL,IDCLASS,IDSTUD))
x_test <- test%>%model.matrix(Math ~ (.)-1, data = .)

gbm_fit <- gbm.fit(x,y,distribution = "gaussian",n.trees = 3000,nTrain = 17649,
                   n.minobsinnode = 30,shrinkage = 0.05,interaction.depth = 5)

y_hat_train_gbm <- predict(gbm_fit, newdata = x, n.trees = 3000) 
y_hat_test_gbm <- predict(gbm_fit, newdata = x_test, n.trees = 3000)

r2_train_gbm <- 1 - mean((y_hat_train_gbm - train$Math)^2) / var(train$Math) 
r2_test_gbm <- 1 - mean((y_hat_test_gbm - test$Math)^2) / var(test$Math)

save(gbm_fit,file = "gbm_fit.RData")
load("gbm_fit.RData")

##support vector regression

svm_fit <- svm(Math ~., data = dta, subset = idc_train %in% c(1,2),
               type = "eps-regression", kernel = "linear", cost = 0.1)

y_hat_train_svm <- predict(svm_fit, newdata = training_data) 
y_hat_test_svm <- predict(svm_fit, newdata = testing1)

r2_train_svm <- 1 - mean((y_hat_train_svm - training_data$Math)^2) / var(training_data$Math) 
r2_test_svm <- 1 - mean((y_hat_test_svm - testing1$Math)^2) / var(testing1$Math)


save(svm_fit,file = "svm_fit.RData")
load("svm_fit.RData")

##load fit result

load("knn.RData")
load("knn_train.RData")
load("lasso.RData")
load("lasso_cv.RData")
load("lm.RData")
load("r2_all.RData")
load("y_hat.RData")
load("rg_fit.RData")


##knn predict

r2_train_knn <- 1 - mean((training_data$Math - knn_fit_train$fitted.values)^2) / var(training_data$Math)

r2_test_knn <- 1 - mean((testing1$Math - knn_fit$fitted.values)^2) / var(testing1$Math)


##lm fit result

y_hat_train_lm <- predict(lm_fit, newdata = training_data)
r2_train_lm <- 1 - mean((y_hat_train_lm - dta$Math[idc_train %in% c(1,2)])^2) / var(dta$Math[idc_train %in% c(1,2)]) 
y_hat_test_lm <- predict(lm_fit, newdata = testing1)
r2_test_lm <- 1 - mean((y_hat_test_lm - dta$Math[idc_train == 3])^2) / var(dta$Math[idc_train == 3]) 


#lasso
lambda_all <- exp(seq(-5, 15, length.out = 100))
x <- dta %>% sparse.model.matrix(Math ~ . - IDSCHOOL - IDCLASS - IDSTUD -1, data = .)
y <- dta$Math
gf <- glmnet(x[idc_train %in% c(1, 2), ], y[idc_train %in% c(1, 2)], alpha = 1, lambda = lambda_all, family = "gaussian", standardize = T)
cf <- cv.glmnet(x[idc_train %in% c(1, 2), ], y[idc_train %in% c(1, 2)], alpha = 1, lambda = lambda_all, family = "gaussian", nfolds = 10)

y_hat_train_lasso <- gf %>% predict(s = cf$lambda.min, newx = x[idc_train %in% c(1, 2),], type = "link")
r2_train_lasso <- 1 - mean((y_hat_train_lasso - dta$Math[idc_train %in% c(1,2)])^2) / var(dta$Math[idc_train %in% c(1,2)]) 

y_hat_test_lasso <- gf %>% predict(s = cf$lambda.min, newx = x[idc_train == 3, ], type = "link")
r2_test_lasso <- 1 - mean((y_hat_test_lasso - dta$Math[idc_train == 3])^2) / var(dta$Math[idc_train == 3]) 

##regression tree

y_hat_train_rg <- predict(rpart_fit, newdata = dta[idc_train %in% c(1, 2), ])
y_hat_test_rg <- predict(rpart_fit, newdata = dta[idc_train ==3 , ])
r2_train_rg <- 1 - mean((y_hat_train_rg - dta$Math[idc_train %in% c(1,2)])^2) / var(dta$Math[idc_train %in% c(1,2)]) 
r2_test_rg <- 1 - mean((y_hat_test_rg - dta$Math[idc_train == 3])^2) / var(dta$Math[idc_train == 3])








