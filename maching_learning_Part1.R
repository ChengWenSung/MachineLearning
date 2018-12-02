##


setwd("/Users/guanying/dataM/machine_learning")
##library packages

pacman::p_load(intsvy,tidyverse,plyr,ltm,gridExtra,coefplot,kknn, Matrix, glmnet, rpart, leaps, rpart.plot, partykit)

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

#knn

knn_fit_train <- kknn(Math ~ . - IDSCHOOL - IDCLASS - IDSTUD, train = training_data, test = training_data, k = 7, kernel = "rectangular")
r2_knn_train <- 1 - mean((knn_fit_train$fitted.values - training_data$Math)^2) / var(training_data$Math) 

r2_knn_test_all <- sapply(3:10, FUN = function(k){
  knn_test_fit <- kknn(Math ~ . - IDSCHOOL - IDCLASS - IDSTUD, train = training_data, test = testing1, k = k, kernel = "rectangular")
  r2_knn_test <- 1 - mean((knn_test_fit$fitted.values - dta$Math[idc_train == 3])^2) / var(dta$Math[idc_train == 3]) 
  return(r2_knn_test)
})
knn_fit <- kknn(Math ~ . - IDSCHOOL - IDCLASS - IDSTUD, train = training_data, test = testing1, k = 7, kernel = "rectangular")
r2_knn_test <- 1 - mean((knn_fit$fitted.values - dta$Math[idc_train == 3])^2) / var(dta$Math[idc_train == 3]) 
save(knn_fit, file = "knn.RData")
save(knn_fit_train, file = "knn_train.RData")
save(r2_knn_test_all, file = "cv_knn_train.RData")


#linear reagression
lm_fit <- lm(Math ~ . - IDSCHOOL - IDCLASS - IDSTUD, data = dta[idc_train %in% c(1, 2), ])
lm_y_hat_train <- predict(lm_fit, newdata = training_data)
r2_lm_train <- 1 - mean((lm_y_hat_train - dta$Math[idc_train %in% c(1,2)])^2) / var(dta$Math[idc_train %in% c(1,2)]) 
lm_y_hat_test <- predict(lm_fit, newdata = testing1)
r2_lm_test <- 1 - mean((lm_y_hat_test - dta$Math[idc_train == 3])^2) / var(dta$Math[idc_train == 3]) 
save(lm_fit, file = "lm.RData")

#ridge
gf <- glmnet(x[idc_train %in% c(1, 2), ], y[idc_train %in% c(1, 2)], alpha = 0, lambda = lambda_all, family = "gaussian", standardize = T)
cf <- cv.glmnet(x[idc_train %in% c(1, 2), ], y[idc_train %in% c(1, 2)], alpha = 0, lambda = lambda_all, family = "gaussian", nfolds = 10)

ridge_y_hat_train <- gf %>% predict(s = cf$lambda.min, newx = x[idc_train %in% c(1, 2),], type = "link")
r2_ridge_train <- 1 - mean((ridge_y_hat_train - dta$Math[idc_train %in% c(1,2)])^2) / var(dta$Math[idc_train %in% c(1,2)]) 

ridge_y_hat_test <- gf %>% predict(s = cf$lambda.min, newx = x[idc_train == 3, ], type = "link")
r2_ridge_test <- 1 - mean((ridge_y_hat_test - dta$Math[idc_train == 3])^2) / var(dta$Math[idc_train == 3]) 

#lasso
lambda_all <- exp(seq(-5, 15, length.out = 100))
x <- dta %>% sparse.model.matrix(Math ~ . - IDSCHOOL - IDCLASS - IDSTUD -1, data = .)
y <- dta$Math
gf <- glmnet(x[idc_train %in% c(1, 2), ], y[idc_train %in% c(1, 2)], alpha = 1, lambda = lambda_all, family = "gaussian", standardize = T)
cf <- cv.glmnet(x[idc_train %in% c(1, 2), ], y[idc_train %in% c(1, 2)], alpha = 1, lambda = lambda_all, family = "gaussian", nfolds = 10)

lasso_y_hat_train <- gf %>% predict(s = cf$lambda.min, newx = x[idc_train %in% c(1, 2),], type = "link")
r2_lasso_train <- 1 - mean((lasso_y_hat_train - dta$Math[idc_train %in% c(1,2)])^2) / var(dta$Math[idc_train %in% c(1,2)]) 

lasso_y_hat_test <- gf %>% predict(s = cf$lambda.min, newx = x[idc_train == 3, ], type = "link")
r2_lasso_test <- 1 - mean((lasso_y_hat_test - dta$Math[idc_train == 3])^2) / var(dta$Math[idc_train == 3]) 


#regression tree

rpart_fit <- rpart(Math ~ . - IDSCHOOL - IDCLASS - IDSTUD, dta, subset = idc_train %in% c(1, 2), method = "anova", minsplit = 2, cp = 0.001, maxdepth = 8)
rg_y_hat_train <- predict(rpart_fit, newdata = dta[idc_train %in% c(1, 2), ])
rg_y_hat_test <- predict(rpart_fit, newdata = dta[idc_train ==3 , ])
r2_rg_train <- 1 - mean((rg_y_hat_train - dta$Math[idc_train %in% c(1,2)])^2) / var(dta$Math[idc_train %in% c(1,2)]) 
r2_rg_test <- 1 - mean((rg_y_hat_test - dta$Math[idc_train == 3])^2) / var(dta$Math[idc_train == 3])


rpart_fit2 <- rpart(Math ~ . - IDSCHOOL - IDCLASS - IDSTUD , dta, subset = idc_train %in% c(1, 2), method = "anova", minsplit = 2, cp = 0.01, maxdepth = 8)
rg_y_hat_test2 <- predict(rpart_fit2, newdata = dta[idc_train ==3 , ])
r2_rg_test2 <- 1 - mean((rg_y_hat_test2 - dta$Math[idc_train == 3])^2) / var(dta$Math[idc_train == 3])


#all
r2_train <- data.frame(knn = r2_knn_train, lm = r2_lm_train, lasso = r2_lasso_train, ridge = r2_ridge_train, rg = r2_rg_train)
r2_test <- data.frame(knn = r2_knn_test, lm = r2_lm_test, lasso = r2_lasso_test, ridge = r2_ridge_test, rg = r2_rg_test)
r2_all <- rbind(r2_train, r2_test)
save(r2_all, file = "r2_all.RData")

result <- data.frame(knn = knn_fit$fitted.values, lm = lm_y_hat_test, lassp = lasso_y_hat_test, ridge = ridge_y_hat_test, rg = rg_y_hat_test)
save(result, file = "y_hat.RData")
