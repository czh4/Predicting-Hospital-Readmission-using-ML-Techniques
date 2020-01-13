# Read training data set into R
setwd('~/Desktop/UMD/Spring 2019/Data Mining and Predictive Analytics (BUDT758T)/Project/R code')
library(readr)
hospitals<-read.csv('hospitals_train.csv')
#hospitals <- read.csv('~/Desktop/758T/hospitals.csv')

# Briefly view the data
#View(hospitals)
summary(hospitals)


### Data Preprocessing
## 1. Drop missing rows
# Drop rows with missing 'INDEX'
hospitals<-hospitals[!is.na(hospitals$INDEX),]

## 2. Manage variables
library(plyr)
library(ggplot2)
# 'HOSPITAL'
chisq.test(hospitals$HOSPITAL,hospitals$RETURN)
ggplot(hospitals,aes(x=HOSPITAL,group=RETURN,fill=RETURN))+geom_bar(position = 'fill')+labs(y='Proportion')
# 'GENDER': Drop 2 rows with 'GENDER' missing
count(hospitals,'GENDER')
hospitals<-hospitals[hospitals$GENDER!='',]
ggplot(hospitals,aes(x=GENDER,group=RETURN,fill=RETURN))+geom_bar(position = 'fill')+labs(y='Proportion')
# scale age between [0,1]
min_age<-min(hospitals$AGE)
max_age<-max(hospitals$AGE)
hospitals$AGE<-(hospitals$AGE-min_age)/(max_age-min_age)
# 'RACE': Put missing value to "Unknown" and put categories that have less than 50 obs to "Other"
count(hospitals,'RACE')
levels(hospitals$RACE)<-c("Unknown","American Indian or Alaskan Native","Asian","Black or African American","Other","Other","Other","Other","Other","Unknown","White")
# 'ETHNICITY': Put (missing value, Declined to Answer) to "Unknown"
count(hospitals,'ETHNICITY')
levels(hospitals$ETHNICITY)<-c("Unknown","Unknown","Hispanic or Latino","Not Hispanic or Latino","Unknown")
# 'FINANCIAL_CLASS': categories apart from "Other", "Self-pay" to "Insurance"
count(hospitals,'FINANCIAL_CLASS')
levels(hospitals$FINANCIAL_CLASS)<-c("Private Insurance","Private Insurance","Private Insurance","Private Insurance","Government Insurance","Medicaid","Medicaid","Medicare","Medicare","Government Insurance","Other","Medicaid","Self-pay","Other")
ggplot(hospitals,aes(x=FINANCIAL_CLASS,group=RETURN,fill=RETURN))+geom_bar(position = 'fill')+labs(y='Proportion')+theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 'HOUR_ARR': factorize
hospitals$HOUR_ARR=as.factor(hospitals$HOUR_ARR)
# 'ED_RESULT': re-classification
count(hospitals,'ED_RESULT')
levels(hospitals$ED_RESULT)<-c("Unknown","Admit","Psychiatry","Psychiatry","AMA","Arrived in Error","Deceased","Discharge","Elopement","Left prior to completing treatment","Left without signing discharge instructions","LWBS after Triage","LWBS before Triage","Admit","L&D","L&D","Transfer")
# 'ACUITY_ARR': Drop 1 row with value "5 Purple", define missing value as "Unknown"
count(hospitals,'ACUITY_ARR')
hospitals<-hospitals[hospitals$ACUITY_ARR!='5 Purple',]
levels(hospitals$ACUITY_ARR)[1]<-"Unknown"
# Missing value in 'CONSULT_IN_ED' column should be 0
count(hospitals,'CONSULT_IN_ED')
hospitals$CONSULT_IN_ED[is.na(hospitals$CONSULT_IN_ED)]<-"0"
ggplot(hospitals,aes(x=CONSULT_ORDER,group=CONSULT_CHARGE,fill=CONSULT_CHARGE))+geom_bar(position = 'fill')+labs(y='Proportion')
ggplot(hospitals,aes(x=CONSULT_CHARGE,group=CONSULT_IN_ED,fill=CONSULT_IN_ED))+geom_bar(position = 'fill')+labs(y='Proportion')
#'ADMIT_RESULT' level name 'blank' change to 'unknown'
count(hospitals,'ADMIT_RESULT')
levels(hospitals$ADMIT_RESULT)=c('Unknown','Inpatient','Observation','Psych Inpatient','Trauma Inpatient')
#split 'Diagonal details' into two groups 0 and 1~14
count(hospitals,'DIAG_DETAILS')
hospitals$DIAG_DETAILS=cut(hospitals$DIAG_DETAILS,breaks=c(-1,0,14))
levels(hospitals$DIAG_DETAILS)=c('0','1')
hospitals$DIAG_DETAILS=as.numeric(hospitals$DIAG_DETAILS)-1
hospitals$DIAG_DETAILS=as.factor(hospitals$DIAG_DETAILS)
# 'RISK': missing value to "None"
count(hospitals,'RISK')
levels(hospitals$RISK)[1]<-"None"
#hospitals$RISK<-factor(hospitals$RISK,levels = c('None','Minor','Moderate','Major','Extreme'))
# 'SEVERITY': missing value to "None"
count(hospitals,'SEVERITY')
levels(hospitals$SEVERITY)[1]<-"None"
hospitals$SEVERITY<-factor(hospitals$SEVERITY,levels = c('None','Minor','Moderate','Major','Extreme'))
chisq.test(hospitals$RISK,hospitals$SEVERITY)
ggplot(hospitals,aes(x=RISK,group=SEVERITY,fill=SEVERITY))+geom_bar(position = 'fill')+labs(y='Proportion')
# 'CHARGES'
# Treat #VALUE! in 'CHARGES' column as 0, rescale charges into [0,1]
charges_null<-hospitals[hospitals$CHARGES=='#VALUE!',][,c(26,27)]
charges_0<-hospitals[hospitals$CHARGES=='0',][,c(26,27)]
charges_other<-hospitals[(hospitals$CHARGES!='#VALUE!' & hospitals$CHARGES!='0'),][,c(26,27)]
charges_level<-c('null','0','Other')
charges_return<-c(sum(charges_null$RETURN=='Yes')/length(charges_null$RETURN),
                  sum(charges_0$RETURN=='Yes')/length(charges_0$RETURN),
                  sum(charges_other$RETURN=='Yes')/length(charges_other$RETURN))
charges_df<-data.frame(charges_level,charges_return)
charges_df
table(hospitals_charges$CHARGES,hospitals_charges$RETURN)
hospitals$CHARGES<-as.numeric(as.character(hospitals$CHARGES))
hospitals$CHARGES[is.na(hospitals$CHARGES)]<-1
hospitals$CHARGES[hospitals$CHARGES==0]<-1
hospitals$CHARGES<-log10(hospitals$CHARGES)
min_charges<-min(hospitals$CHARGES)
max_charges<-max(hospitals$CHARGES)
hospitals$CHARGES<-(hospitals$CHARGES-min_charges)/(max_charges-min_charges)
# Drop 141 rows with 'RETURN' missing
count(hospitals,'RETURN')
hospitals<-hospitals[hospitals$RETURN!='#N/A',]
hospitals$RETURN<-droplevels(hospitals$RETURN)

## 3. Change data type
# convert some categorical variables to factor
for (i in c(8:14,19:21))
{
  hospitals[,i]<-as.factor(hospitals[,i])
}
# View variables' type
sapply(hospitals,class)
#drop null levels
hospitals$DIAGNOSIS=factor(hospitals$DIAGNOSIS)
hospitals$ACUITY_ARR=factor(hospitals$ACUITY_ARR)
hospitals$GENDER=factor(hospitals$GENDER)

## 4. Drop columns
hospitals<-hospitals[,-c(1,2,8,10:13,17,25)]

## 5. Summary of variables
summary(hospitals)

## 6. Data partitioning
set.seed(12345)
test_obs=sample(nrow(hospitals),0.25*nrow(hospitals))
hospitals_test=hospitals[test_obs,]
hospitals_rest=hospitals[-test_obs,]
valid_obs=sample(nrow(hospitals_rest),0.25*nrow(hospitals_rest))
hospitals_valid=hospitals_rest[valid_obs,]
hospitals_train=hospitals_rest[-valid_obs,]
# Baseline accuracy
sum(hospitals_test$RETURN=='No')/nrow(hospitals_test)
sum(hospitals_valid$RETURN=='No')/nrow(hospitals_valid)

## 7. SMOTE (use random forest to compare performance)
library(DMwR)
library(randomForest)
acc=rep(0:8)
for (i in (2:10)){
  hospitals_smote=SMOTE(RETURN ~., hospitals_train, perc.over=200, perc.under=150, k=i, learner=NULL)
  rf_model=randomForest(RETURN~.-CHARGES,data=hospitals_smote,ntree=100,cutoff=c(0.5,0.5),importance=TRUE)
  rf_preds=predict(rf_model,newdata=hospitals_valid,type="prob")
  rf_probs=rf_preds[,2]
  rf_class=ifelse(rf_probs>0.5,'Yes','No')
  acc[i-1]=sum(ifelse(rf_class==hospitals_valid$RETURN,1,0))/nrow(hospitals_valid)
}
acc
# Prediction
hospitals_smote=SMOTE(RETURN ~., hospitals_rest, perc.over=200, perc.under=150, k=6, learner=NULL)
rf_ntree=300
rf_mtry=4
rf_model=randomForest(RETURN~.,data=hospitals_smote,ntree=rf_ntree,mtry=rf_mtry,importance=FALSE)
rf_preds=predict(rf_model,newdata=hospitals_test,type="prob")
rf_probs=rf_preds[,2]
rf_class=ifelse(rf_probs>0.5,'Yes','No')
table_smote=table(hospitals_test$RETURN,rf_class)
table_smote
acc_smote=sum(ifelse(rf_class==hospitals_test$RETURN,1,0))/nrow(hospitals_test)
acc_smote


### Modeling
## 1. Logistic regression
hospital_log=glm(RETURN~.,family='binomial',data=hospitals_rest)
summary(hospital_log)
pred_log=predict(hospital_log,newdata=hospitals_test,type='response')
class_log=ifelse(pred_log>0.5,1,0)
table_log=table(hospitals_test$RETURN,class_log)
table_log
acc_log=sum(table_log[1,1]+table_log[2,2])/sum(table_log)
acc_log

## 2. Stepwise regression
backward_log_model = step(hospital_log, direction="both")
pred_back_log=predict(backward_log_model,newdata=hospitals_test,type='response')
class_back_log=ifelse(pred_back_log>0.5,1,0)
table_back_log=table(hospitals_test$RETURN,class_back_log)
table_back_log
acc_back_log=sum(table_back_log[1,1]+table_back_log[2,2])/sum(table_back_log)
acc_back_log

## 3. LDA
library(MASS)
lda_model=lda(RETURN~.-ACUITY_ARR,data=hospitals_rest)
lda_predict=predict(lda_model,newdata = hospitals_test)
lda_preds=lda_predict$posterior[,2]
lda_class=ifelse(lda_preds>0.5,1,0)
table_lda=table(hospitals_test$RETURN,lda_class,dnn=c("Actual","Predicted"))
table_lda
acc_lda=(table_lda[1,1]+table_lda[2,2])/sum(table_lda)
acc_lda

## 4. Tree
library(tree)
hospitals_tree=tree(RETURN~.-ED_RESULT-CHARGES,hospitals_rest)
summary(hospitals_tree)
plot(hospitals_tree)
text(hospitals_tree)
tree_preds=predict(hospitals_tree,newdata=hospitals_test)
tree_probs=tree_preds[,2]
tree_class=ifelse(tree_probs>0.5,'Yes','No')
table_tree=table(hospitals_test$RETURN,tree_class,dnn=c("Actual","Predicted"))
table_tree
acc_tree=sum(ifelse(tree_class==hospitals_test$RETURN,1,0))/nrow(hospitals_test)
acc_tree

## 5. kNN
library(class)
train.X<-model.matrix(~.,hospitals_train[,c(1:17)])
valid.X<-model.matrix(~.,hospitals_valid[,c(1:17)])
test.X<-model.matrix(~.,hospitals_test[,c(1:17)])
rest.X<-model.matrix(~.,hospitals_rest[,c(1:17)])
train.return=hospitals_train$RETURN
valid.return=hospitals_valid$RETURN
test.return=hospitals_test$RETURN
rest.return=hospitals_rest$RETURN
# Find the best k
value=seq(1,60,2)
rep=rep(0,30)
for (i in 1:30){
  knn.pred=knn(train.X,valid.X,train.return,k=value[i])
  table=table(valid.return,knn.pred)
  rep[i]=(table[1,1]+table[2,2])/sum(table)}
plot(x=value, y=rep[1:30],xlab='K',ylab='Accuracy')
# kNN for test
knn.pred=knn(rest.X,test.X,rest.return,k=45)
table_knn=table(test.return,knn.pred)
table_knn
acc_knn=(table_knn[1,1]+table_knn[2,2])/sum(table_knn)
acc_knn

## 6. RIDGE
library(glmnet)
rest_X<-model.matrix( ~ .-1, hospitals_rest[,c(1:17)])
test_X<-model.matrix( ~ .-1, hospitals_test[,c(1:17)])
# Find the best lambda
ridge_model=glmnet(rest_X,hospitals_rest$RETURN,family="binomial",alpha=0)
ridge_model.cv=cv.glmnet(rest_X,hospitals_rest$RETURN,family="binomial",alpha=0)
best.lambda.ridge=ridge_model.cv$lambda.min
# Prediction
ridge_probs=predict(ridge_model,s=best.lambda.ridge,newx=test_X,type="response")
ridge_class=ifelse(ridge_probs>0.5,'Yes','No')
table_ridge=table(hospitals_test$RETURN,ridge_class)
table_ridge
acc_ridge=sum(ifelse(ridge_class==hospitals_test$RETURN,1,0))/nrow(hospitals_test)
acc_ridge

## 7. LASSO
library(glmnet)
rest_X<-model.matrix( ~ ., hospitals_rest[,c(1:17)])
test_X<-model.matrix( ~ ., hospitals_test[,c(1:17)])
# Find the best lambda
lasso_model=glmnet(rest_X,hospitals_rest$RETURN,family="binomial",alpha=1)
lasso_model.cv=cv.glmnet(rest_X,hospitals_rest$RETURN,family="binomial",alpha=1)
best.lambda.lasso=lasso_model.cv$lambda.min
# Prediction
lasso_probs = predict(lasso_model,s=best.lambda.lasso,newx=test_X,type="response")
lasso_class = ifelse(lasso_probs>0.5,'Yes','No')
table_lasso=table(hospitals_test$RETURN,lasso_class)
table_lasso
acc_lasso=sum(ifelse(lasso_class==hospitals_test$RETURN,1,0))/nrow(hospitals_test)
acc_lasso

## 8. Feedforward
library(kerasR)
rest_X<-model.matrix( ~ ., hospitals_rest[,c(1:17)])
test_X<-model.matrix( ~ .-1, hospitals_test[,c(1:17)])
label=to_categorical(ifelse(hospitals_rest$RETURN=='Yes',1,0))
# Structure
feedforward_model=Sequential()
feedforward_model$add(Dense(units = 512,input_shape = ncol(train_X)))
feedforward_model$add(Activation('sigmoid'))
feedforward_model$add(Dense(units = 256))
feedforward_model$add(Activation('sigmoid'))
feedforward_model$add(Dense(units = 64))
feedforward_model$add(Activation('sigmoid'))
feedforward_model$add(Dense(units = 32))
feedforward_model$add(Activation('sigmoid'))
feedforward_model$add(Dense(units = 2))
feedforward_model$add(Activation('softmax'))
# Fitting
keras_compile(feedforward_model,loss = 'categorical_crossentropy',optimizer = 'adam',metrics = 'accuracy')
keras_fit(feedforward_model,train_X,label,batch_size = 32,epochs = 20,verbose = 2,validation_split = 0.1)
# Prediction
feedforward_preds=keras_predict(feedforward_model,test_X)
feedforward_class=ifelse(feedforward_preds[,1]<=feedforward_preds[,2],'Yes','No')
table_dl=table(hospitals_test$RETURN,feedforward_class)
table_dl
acc_dl=sum(ifelse(feedforward_class==hospitals_test$RETURN,1,0))/nrow(hospitals_test)
acc_dl

## 9. Naive Bayes classification
library(e1071)
nb_model<-naiveBayes(RETURN~.,data=hospitals_rest)
nb_model
nb_preds<-predict(nb_model,hospitals_test,type = 'raw')
nb_class<-ifelse(nb_preds[,1]<0.5,'Yes','No')
table_nb=table(hospitals_test$RETURN,nb_class)
table_nb
acc_nb=sum(ifelse(nb_class==hospitals_test$RETURN,1,0))/nrow(hospitals_test)
acc_nb

## 10. Bagging
library(randomForest)
bag_model=randomForest(RETURN~.,data=hospitals_rest,ntree=200,mtry=18,importance=TRUE)
bag_model
importance(bag_model)
varImpPlot(bag_model)
# Prediction
bag_preds=predict(bag_model,newdata=hospitals_test,type="prob")
bag_probs=bag_preds[,2]
bag_class=ifelse(bag_probs>0.5,'Yes','No')
table_bagging=table(hospitals_test$RETURN,bag_class)
table_bagging
acc_bagging=sum(ifelse(bag_class==hospitals_test$RETURN,1,0))/nrow(hospitals_test)
acc_bagging

## 11. Boosting
library(gbm)
train_x=hospitals_train[,c(1:17)]
train_y=as.numeric(hospitals_train[,c(18)])-1
valid_x=hospitals_valid[,c(1:17)]
gbm_ntrees=300
gbm_minobs=50
# Tuning
boosting_result=data.frame()
for (i in seq(1,10)){
  for (j in seq(4,21)){
    gbm_shrinkage=i/100
    gbm_depth=j
    boost_model = gbm.fit(x = train_x,
                          y = train_y,
                          distribution = 'bernoulli',
                          n.trees = gbm_ntrees,
                          shrinkage = gbm_shrinkage,
                          interaction.depth = gbm_depth,
                          n.minobsinnode = gbm_minobs,
                          verbose = FALSE)
    boosting_preds=predict.gbm(boost_model,newdata=valid_x,gbm_ntrees,type='response')
    boosting_class=ifelse(boosting_preds>0.5,1,0)
    result=c(gbm_shrinkage,gbm_depth,sum(ifelse(boosting_class==as.numeric(hospitals_valid$RETURN)-1,1,0))/nrow(hospitals_valid))
    boosting_result=rbind(boosting_result,result)
  }
}
boosting_result
rr=ggplot(boosting_result,aes(X0.01,X4))
rr+geom_point(aes(colour=X0.762369996848408,size=X0.762369996848408))+scale_color_gradient(low = 'Red',high = 'Blue')+labs(x='Shrinkage',y='Depth',colour='Accuracy',size='Accuracy')
# Best set of parameters
gbm_ntrees=300
gbm_shrinkage=0.04
gbm_depth=22
gbm_minobs=50
boost_model = gbm.fit(x = hospitals_rest[,c(1:17)],
                      y = as.numeric(hospitals_rest[,c(18)])-1,
                      distribution = 'bernoulli',
                      n.trees = gbm_ntrees,
                      shrinkage = gbm_shrinkage,
                      interaction.depth = gbm_depth,
                      100 = gbm_minobs,
                      verbose = FALSE)
summary(boost_model)
# Prediction
boosting_preds=predict.gbm(boost_model,newdata=hospitals_test[,c(1:17)],gbm_ntrees,type = 'response')
boosting_class=ifelse(boosting_preds>0.5,1,0)
table_boosting=table(hospitals_test$RETURN,boosting_class)
table_boosting
acc_boosting=sum(ifelse(boosting_class==as.numeric(hospitals_test$RETURN)-1,1,0))/nrow(hospitals_test)
acc_boosting

## 12. Random Forest
library(randomForest)
# Basic model
rf_model=randomForest(RETURN~.,data=hospitals_rest,ntree=200,cutoff=c(0.5,0.5),importance=TRUE)
rf_model
importance(rf_model)
varImpPlot(rf_model)
rf_preds=predict(rf_model,newdata=hospitals_test,type="prob")
rf_probs=rf_preds[,2]
rf_class=ifelse(rf_probs>0.5,'Yes','No')
table(hospitals_test$RETURN,rf_class)
sum(ifelse(rf_class==hospitals_test$RETURN,1,0))/nrow(hospitals_test)
# Tuning
rf_result=data.frame()
for (i in seq(1,5)){
  for (j in seq(5,10)){
    rf_ntree=i*100
    rf_mtry=j
    rf_model=randomForest(RETURN~.,data=hospitals_train,ntree=rf_ntree,importance=FALSE)
    rf_preds=predict(rf_model,newdata=hospitals_valid,type="prob")
    rf_probs=rf_preds[,2]
    rf_class=ifelse(rf_probs>0.5,'Yes','No')
    result=c(rf_ntree,rf_mtry,sum(ifelse(rf_class==hospitals_valid$RETURN,1,0))/nrow(hospitals_valid))
    rf_result=rbind(rf_result,result)
  }
}
rf_result
rr=ggplot(rf_result,aes(X100,X5))
rr+geom_point(aes(colour=X0.767412543334384,size=X0.767412543334384))+scale_color_gradient(low = 'Red',high = 'Blue')+labs(x='Shrinkage',y='Depth',colour='Accuracy',size='Accuracy')
# Try different cutoff, finally choose 0.5
reg_acc=c()
reg_TPR=c()
reg_TNR=c()
cutoffs<-seq(0.05,0.95,0.05)
rf_preds=predict(rf_model,newdata=hospitals_test,type="prob")
rf_probs=rf_preds[,2]
for (i in 1:length(cutoffs)){
  rf_class<-ifelse(rf_probs>cutoffs[i],1,0)
  confuse_test<-table(hospitals_test$RETURN,rf_class)
  reg_acc[i]<-(confuse_test[1,1]+confuse_test[2,2])/sum(confuse_test)
  reg_TPR[i]<-confuse_test[2,2]/(confuse_test[2,1]+confuse_test[2,2])
  reg_TNR[i]<-confuse_test[1,1]/(confuse_test[1,1]+confuse_test[1,2])
}
result<-data.frame(cutoffs,reg_acc,reg_TPR,reg_TNR)
result
# Plot
plot(cutoffs,reg_TPR,col='green',type='l',ylab = 'Value',xlim = 0:1,ylim = 0:1)
lines(cutoffs,reg_TNR,col='red')
lines(cutoffs,reg_acc,col='blue')
# Prediction
rf_ntree=300
rf_mtry=4
rf_model=randomForest(RETURN~.,data=hospitals_rest,ntree=rf_ntree,mtry=rf_mtry,importance=FALSE)
rf_preds=predict(rf_model,newdata=hospitals_test,type="prob")
rf_probs=rf_preds[,2]
rf_class=ifelse(rf_probs>0.5,'Yes','No')
table_rf=table(hospitals_test$RETURN,rf_class)
table_rf
acc_rf=sum(ifelse(rf_class==hospitals_test$RETURN,1,0))/nrow(hospitals_test)
acc_rf

## 13. XGBoost
library(xgboost)
library(mlr)
library(parallel)
library(parallelMap)
# Tuning
# Create tasks
traintask<-makeClassifTask(data = hospitals_rest,target = 'RETURN')
testtask<-makeClassifTask(data = hospitals_test,target = 'RETURN')
traintask<-createDummyFeatures(obj = traintask)
testtask<-createDummyFeatures(obj = testtask)
# Create learner
learner<-makeLearner('classif.xgboost',predict.type = 'response')
learner$par.vals<-list(objective="binary:logistic", eval_metric="error")
# Set parameters
params<-makeParamSet(makeDiscreteParam("booster",values = c("gbtree","gblinear")),
                       makeIntegerParam("max_depth",lower = 3L,upper = 15L),
                       makeIntegerParam("nrounds",lower = 20L,upper = 100L),
                       makeNumericParam("eta",lower = 0.1,upper = 0.5),
                       makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
                       makeNumericParam("subsample",lower = 0.5,upper = 1),
                       makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))
rdesc<-makeResampleDesc("CV",stratify=T,iters=5L)
# Build 1000 models with different parameters
ctrl<-makeTuneControlRandom(maxit=1000L)
# Parallel computation
parallelStartSocket(cpus=detectCores())
xgbtune<-tuneParams(learner=learner,task=traintask,resampling=rdesc,measures=acc,par.set=params,control=ctrl,show.info=T)
# Save parameters
learner_tune<-setHyperPars(learner,par.vals=xgbtune$x)
xgb_model<-train(learner=learner_tune,task=traintask)
xgb_preds<-predict(xgb_model,testtask)
table_xgb=table(xgb_preds$data$truth,xgb_preds$data$response)
table_xgb
acc_xgb=sum(ifelse(xgb_preds$data$response==hospitals_test$RETURN,1,0))/nrow(hospitals_test)
acc_xgb