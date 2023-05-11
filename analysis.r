library(tidyverse)
library(MASS)
library(caret)
library(corrplot)

#install.packages("evalITR")

library(evalITR)


#install.packages("xgboost")
#install.packages("SuperLearner")
#install.packages("gbm")
#install.packages("glmnet")
#install.packages("KernelKnn")
#install.packages("arm")
#install.packages("bartMachine")

#library(bartMachine)


setwd("C:/school/stat 232/project")

# d is treatment indicator

expert <- read.table("./expert.csv", sep=",", header=TRUE)
expert$d <- 0

avg_expert <- mean(expert$post.bock.OSCE.TP) 

ai <- read.table("./ai.csv", sep=",", header=TRUE)
ai$d <- 1

avg_ai <- mean(ai$post.bock.OSCE.TP) 

df <- rbind(expert, ai)

# score on exam is y

df <- df %>% rename("y" = "post.bock.OSCE.TP")

df$Gender.M.F. <- as.numeric(factor(df$Gender.M.F.))-1
df$Pre..Ob.suture.real.pt.yes.no. <- as.numeric(factor(df$Pre..Ob.suture.real.pt.yes.no.))-1


pape <- c(avg_expert, avg_ai)
lin <- seq(avg_expert, avg_ai, length.out=length(df$y))

hund_lin <- seq(0, 100, length.out=length(df$y))


avg_score <- mean(df$y)


# these are the only covariates that can be measured before treatment would be assigned

covars <- c("age.year.", "Gender.M.F.", "Pre..Ob.suture.real.pt.yes.no.", "baseline.confidence.real.pt", "basic.interest.for.surgery")



# load in some required libraries

vec.pac= c("SuperLearner", "gbm", "glmnet","caret", "xgboost", "randomForest", "KernelKnn", "bartMachine", "e1071")

lapply(vec.pac, require, character.only = TRUE) 


#CV Control for the SuperLearner
control <- SuperLearner.CV.control(V=5)


S_learner <- function(data,covariates,learners){
  
  data$ID <- c(1:nrow(data))
  
  score_S <- matrix(0,nrow(data),1)
  
  set.seed(1234)
  folds <- createFolds(data$d,k=5)
  
  
  for(f in 1:(length(folds))){
    
    if(f == 1){
      data1 <- data[c(folds[[5]],folds[[2]],folds[[3]],folds[[4]]),]
      df_main <- data[folds[[1]],]
    } 
    if(f == 2){
      data1 <- data[c(folds[[1]],folds[[5]],folds[[3]],folds[[4]]),]
      df_main <- data[folds[[2]],]
    } 
    
    if(f == 3){
      data1 <- data[c(folds[[1]],folds[[2]],folds[[5]],folds[[4]]),]
      df_main <- data[folds[[3]],]
    } 
    
    if(f == 4){
      data1 <- data[c(folds[[1]],folds[[2]],folds[[3]],folds[[5]]),]
      df_main <- data[folds[[4]],]
    } 
    
    if(f == 5){
      data1 <- data[c(folds[[1]],folds[[2]],folds[[3]],folds[[4]]),]
      df_main <- data[folds[[5]],]
    } 
    
    df_aux <- data1
    
    X_train <- (df_aux[,c(covariates,"d")])
    
    
    # Train a regression model using the covariates and the treatment variable
    m_mod <- SuperLearner(Y = df_aux$y, X = X_train, SL.library = learners,
                          verbose = FALSE, method = "method.NNLS",cvControl = control)
    
    # Set treatment variable to 0
    X_test_0 <- (df_main[,c(covariates,"d")])
    X_test_0$d <- 0
    
    # Set treatment variable to 1
    X_test_1 <- (df_main[,c(covariates,"d")])
    X_test_1$d <- 1
    
    # Estimate the CATE as the difference between the model with different treatment status
    score_S[,1][df_main$ID] = predict(m_mod,X_test_1)$pred - predict(m_mod,X_test_0)$pred
    
  }
  
  return(score_S)
}

p = 20


prop = p/47


# learners indicates which ml alg will be used by the S learner


# lasso

learners <- c("SL.glmnet")
cate_lasso <- c(S_learner(df,covars,learners = learners))

cate_lasso <- -(cate_lasso - mean(cate_lasso) )

# 20 highest cate get treatment
itr_lasso <- order(cate_lasso) > length(cate_lasso)-p

pape_lasso <- PAPE(df$d, itr_lasso, df$y, plim=prop)
aupec_lasso <- AUPEC(df$d, cate_lasso, df$y)

lasso_df <- data.frame(x=hund_lin, y=lin+aupec_lasso$vec)

ggplot(data=lasso_df, aes(x=x, y=y, group=1)) +
  geom_line(color="red")+ 
  geom_line(aes(y = seq(avg_expert, avg_ai, length.out=length(df$y) ) ),color = "black") +
  ggtitle("Lasso") +
  xlab("Max Proportion Treated") + ylab("Average OSCE Score")





# random forest

learners <- c("SL.randomForest")
cate_rf <- c(S_learner(df,covars,learners = learners))

cate_rf <- -(cate_rf - mean(cate_rf))

# 20 highest cate get treatment
itr_rf <- order(cate_rf) > length(cate_rf)-p

pape_rf <- PAPE(df$d, itr_rf, df$y,plim=prop)
aupec_rf <- AUPEC(df$d, cate_rf, df$y)

rf_df <- data.frame(x=hund_lin, y=lin+aupec_rf$vec)

ggplot(data=rf_df, aes(x=x, y=y, group=1)) +
  geom_line(color="red")+ 
  geom_line(aes(y = seq(avg_expert, avg_ai, length.out=length(df$y) ) ),color = "black") +
  ggtitle("Random Forest") +
  xlab("Max Proportion Treated") + ylab("Average OSCE Score")


# xgboost

learners <- c("SL.xgboost")
cate_xgboost <- c(S_learner(df,covars,learners = learners))

cate_xgboost <- -(cate_xgboost-mean(cate_xgboost))

# 20 highest cate get treatment
itr_xgb <- order(cate_xgboost) > length(cate_xgboost)-p

pape_xgb <- PAPE(df$d, itr_xgb, df$y, plim=prop)
aupec_xgb <- AUPEC(df$d, cate_xgboost, df$y)

xgb_df <- data.frame(x=hund_lin, y=lin+aupec_xgb$vec)

ggplot(data=xgb_df, aes(x=x, y=y, group=1)) +
  geom_line(color="red")+ 
  geom_line(aes(y = seq(avg_expert, avg_ai, length.out=length(df$y) ) ),color = "black") +
  ggtitle("XGBoost") +
  xlab("Max Proportion Treated") + ylab("Average OSCE Score")


# kernel Knn

learners <- c("SL.kernelKnn")
cate_kknn <- c(S_learner(df,covars,learners = learners))

cate_kknn <- -(cate_kknn-mean(cate_kknn))

# 20 highest cate get treatment
itr_kknn <- order(cate_kknn) > length(cate_kknn)-p

pape_kknn <- PAPE(df$d, itr_kknn, df$y, plim=prop)
aupec_kknn <- AUPEC(df$d, cate_kknn, df$y)

kknn_df <- data.frame(x=hund_lin, y=lin+aupec_kknn$vec)

ggplot(data=kknn_df, aes(x=x, y=y, group=1)) +
  geom_line(color="red")+ 
  geom_line(aes(y = seq(avg_expert, avg_ai, length.out=length(df$y) ) ),color = "black") +
  ggtitle("Kernal KNN") +
  xlab("Max Proportion Treated") + ylab("Average OSCE Score")


# SVM

learners <- c("SL.svm")
cate_svm <- c(S_learner(df,covars,learners = learners))

cate_svm <- -(cate_svm-mean(cate_svm))

# 20 highest cate get treatment
itr_svm <- order(cate_svm) > length(cate_svm)-p

pape_svm <- PAPE(df$d, itr_svm, df$y, plim=prop)
aupec_svm <- AUPEC(df$d, cate_svm, df$y)

svm_df <- data.frame(x=hund_lin, y=lin+aupec_svm$vec)

ggplot(data=svm_df, aes(x=x, y=y, group=1)) +
  geom_line(color="red")+ 
  geom_line(aes(y = seq(avg_expert, avg_ai, length.out=length(df$y) ) ),color = "black") +
  ggtitle("SVM") +
  xlab("Max Proportion Treated") + ylab("Average OSCE Score")





# bart

learners <- c("SL.bartMachine")
cate_bart <- c(S_learner(df,covars,learners = learners))

cate_bart <- -(cate_bart-mean(cate_bart))

# 20 highest cate get treatment
itr_bart <- order(cate_bart) > length(cate_bart)-p

pape_bart <- PAPE(df$d, itr_bart, df$y, plim=prop)
aupec_bart <- AUPEC(df$d, cate_bart, df$y)

bart_df <- data.frame(x=hund_lin, y=lin+aupec_bart$vec)

ggplot(data=bart_df, aes(x=x, y=y, group=1)) +
  geom_line(color="red")+
  geom_line(aes(y = seq(avg_expert, avg_ai, length.out=length(df$y) ) ),color = "black") +
  ggtitle("BART") +
  xlab("Max Proportion Treated") + ylab("Average OSCE Score")











