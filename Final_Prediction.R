setwd('~/Desktop/UMD/Spring 2019/Data Mining and Predictive Analytics (BUDT758T)/Project/R code')
library(readr)

# Training data
hospitals<-read.csv('hospitals_train.csv')
#hospitals <- read.csv('~/Desktop/758T/hospitals.csv')

hospitals<-hospitals[!is.na(hospitals$INDEX),]
hospitals<-hospitals[hospitals$GENDER!='',]
min_age<-min(hospitals$AGE)
max_age<-max(hospitals$AGE)
hospitals$AGE<-(hospitals$AGE-min_age)/(max_age-min_age)
levels(hospitals$RACE)<-c("Unknown","American Indian or Alaskan Native","Asian","Black or African American","Other","Other","Other","Other","Other","Unknown","White")
levels(hospitals$ETHNICITY)<-c("Unknown","Unknown","Hispanic or Latino","Not Hispanic or Latino","Unknown")
levels(hospitals$FINANCIAL_CLASS)<-c("Private Insurance","Private Insurance","Private Insurance","Private Insurance","Government Insurance","Medicaid","Medicaid","Medicare","Medicare","Government Insurance","Other","Medicaid","Self-pay","Other")
hospitals$HOUR_ARR=as.factor(hospitals$HOUR_ARR)
levels(hospitals$ED_RESULT)<-c("Unknown","Admit","Psychiatry","Psychiatry","AMA","Arrived in Error","Deceased","Discharge","Elopement","Left prior to completing treatment","Left without signing discharge instructions","LWBS after Triage","LWBS before Triage","Admit","L&D","L&D","Transfer")
hospitals<-hospitals[hospitals$ACUITY_ARR!='5 Purple',]
levels(hospitals$ACUITY_ARR)[1]<-"Unknown"
hospitals$CONSULT_IN_ED[is.na(hospitals$CONSULT_IN_ED)]<-"0"
levels(hospitals$ADMIT_RESULT)=c('Unknown','Inpatient','Observation','Psych Inpatient','Trauma Inpatient')
hospitals$DIAG_DETAILS=cut(hospitals$DIAG_DETAILS,breaks=c(-1,0,14))
levels(hospitals$DIAG_DETAILS)=c('0','1')
hospitals$DIAG_DETAILS=as.numeric(hospitals$DIAG_DETAILS)-1
hospitals$DIAG_DETAILS=as.factor(hospitals$DIAG_DETAILS)
levels(hospitals$RISK)[1]<-"None"
hospitals$CHARGES<-as.numeric(as.character(hospitals$CHARGES))
hospitals$CHARGES[is.na(hospitals$CHARGES)]<-1
hospitals$CHARGES[hospitals$CHARGES==0]<-1
hospitals$CHARGES<-log10(hospitals$CHARGES)
min_charges<-min(hospitals$CHARGES)
max_charges<-max(hospitals$CHARGES)
hospitals$CHARGES<-(hospitals$CHARGES-min_charges)/(max_charges-min_charges)
hospitals<-hospitals[hospitals$RETURN!='#N/A',]
hospitals$RETURN<-droplevels(hospitals$RETURN)
for (i in c(8:14,19:21))
{
  hospitals[,i]<-as.factor(hospitals[,i])
}
hospitals$DIAGNOSIS=factor(hospitals$DIAGNOSIS)
hospitals$ACUITY_ARR=factor(hospitals$ACUITY_ARR)
hospitals$GENDER=factor(hospitals$GENDER)
hospitals<-hospitals[,-c(1,2,8,10:13,17,25)]

# Test data
test_hospitals<-read.csv('Hospitals_Test_X.csv')

test_hospitals<-test_hospitals[!is.na(test_hospitals$INDEX),]
test_hospitals$AGE<-(test_hospitals$AGE-min_age)/(max_age-min_age)
test_hospitals$AGE[test_hospitals$AGE<0]=0
levels(test_hospitals$RACE)<-c("Unknown","American Indian or Alaskan Native","Asian","Black or African American","Other","Other","Other","Other","Other","Unknown","White")
levels(test_hospitals$ETHNICITY)<-c("Unknown","Unknown","Hispanic or Latino","Not Hispanic or Latino","Unknown")
levels(test_hospitals$FINANCIAL_CLASS)<-c("Private Insurance","Private Insurance","Private Insurance","Government Insurance","Medicaid","Medicaid","Medicare","Medicare","Government Insurance","Other","Medicaid","Self-pay","Other")
test_hospitals$HOUR_ARR=as.factor(test_hospitals$HOUR_ARR)
levels(test_hospitals$ED_RESULT)<-c("Unknown","Admit","Psychiatry","AMA","Arrived in Error","Deceased","Discharge","Elopement","Left prior to completing treatment","Left without signing discharge instructions","LWBS after Triage","LWBS before Triage","Admit","L&D","L&D","Transfer")
levels(test_hospitals$ACUITY_ARR)[1]<-"Unknown"
test_hospitals$CONSULT_IN_ED[is.na(test_hospitals$CONSULT_IN_ED)]<-"0"
levels(test_hospitals$ADMIT_RESULT)=c('Unknown','Inpatient','Observation','Psych Inpatient','Trauma Inpatient')
test_hospitals$DIAG_DETAILS=cut(test_hospitals$DIAG_DETAILS,breaks=c(-1,0,14))
levels(test_hospitals$DIAG_DETAILS)=c('0','1')
test_hospitals$DIAG_DETAILS=as.numeric(test_hospitals$DIAG_DETAILS)-1
test_hospitals$DIAG_DETAILS=as.factor(test_hospitals$DIAG_DETAILS)
levels(test_hospitals$RISK)[1]<-"None"
test_hospitals$CHARGES<-as.numeric(as.character(test_hospitals$CHARGES))
test_hospitals$CHARGES[is.na(test_hospitals$CHARGES)]<-1
test_hospitals$CHARGES[test_hospitals$CHARGES==0]<-1
test_hospitals$CHARGES<-log10(test_hospitals$CHARGES)
test_hospitals$CHARGES<-(test_hospitals$CHARGES-min_charges)/(max_charges-min_charges)
for (i in c(8:14,19:21))
{
  test_hospitals[,i]<-as.factor(test_hospitals[,i])
}
test_hospitals<-test_hospitals[,-c(1,2,8,10:13,17,25)]

## Prediction
# 1. Stepwise regression
logistic_all<-glm(RETURN ~ ,family='binomial',data = hospitals)
stepwise_model=step(logistic_all, direction="both")
test_preds=predict(stepwise_model,newdata=test_hospitals,type='response')
test_class=ifelse(test_preds>0.5,'Yes','No')

# 2. LASSO
hospitals_X<-model.matrix(~.-1,hospitals[,c(1:17)])
test_hospitals_X<-model.matrix(~.-1,test_hospitals)
lasso_all=glmnet(hospitals_X,hospitals$RETURN,family="binomial",alpha=1)
lasso_all.cv=cv.glmnet(hospitals_X,hospitals$RETURN,family="binomial",alpha=1)
best.lambda.lasso=lasso_all.cv$lambda.min
test_probs=predict(lasso_all,s=best.lambda.lasso,newx=test_hospitals_X,type="response")
test_class=ifelse(test_probs>0.5,'Yes','No')

# 3. Feedforward
train_X<-model.matrix( ~ .-1, hospitals[,c(1:18)])
test_X<-model.matrix( ~ .-1, test_hospitals[,c(1:18)])
label=to_categorical(ifelse(hospitals$RETURN=='Yes',1,0))

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
keras_compile(feedforward_model,loss = 'categorical_crossentropy',optimizer = 'adam',metrics = 'accuracy')
keras_fit(feedforward_model,train_X,label,batch_size = 32,epochs = 20,verbose = 2,validation_split = 0.1)
test_preds=keras_predict(feedforward_model,test_X)
test_class=ifelse(test_preds[,1]<=test_preds[,2],'Yes','No')

# 4. Random Forests
rf_ntree=300
rf_mtry=4
a<-data.frame('index'=c(1:12032))
for (i in 1:50){
  rf_all=randomForest(RETURN~.,data=hospitals,ntree=rf_ntree,mtry=rf_mtry,importance=FALSE)
  test_hospitals<-rbind(hospitals[1,c(1:17)],test_hospitals)
  test_hospitals<-test_hospitals[-1,]
  test_preds=predict(rf_all,newdata=test_hospitals,type="prob")
  test_probs=test_preds[,2]
  a=data.frame(a,ifelse(test_probs>0.5,'Yes','No'))
}

# 5. Boosting
library(gbm)
a<-data.frame('index'=c(1:12032))
train_x=hospitals[,c(1:17)]
train_y=as.numeric(hospitals[,c(18)])-1
test_x=test_hospitals

gbm_ntrees=300
gbm_shrinkage=0.04
gbm_depth=22
gbm_minobs=50
for (i in 1:20){
  boost_model = gbm.fit(x = train_x,
                        y = train_y,
                        distribution = 'bernoulli',
                        n.trees = gbm_ntrees,
                        shrinkage = gbm_shrinkage,
                        interaction.depth = gbm_depth,
                        n.minobsinnode = gbm_minobs,
                        verbose = FALSE)
  boosting_preds=predict.gbm(boost_model,newdata = test_x,gbm_ntrees,type = 'response')
  a=data.frame(a,ifelse(boosting_preds>0.5,'Yes','No'))
}

# 6. XGBoost
library(xgboost)
library(mlr)
library(parallel)
library(parallelMap)
traintask<-makeClassifTask(data = hospitals,target = 'RETURN')
traintask<-createDummyFeatures(obj = traintask)
RETURN<-rep('No',nrow(test_hospitals))
test_hospitals<-data.frame(test_hospitals,RETURN)
testtask<-makeClassifTask(data = test_hospitals,target = 'RETURN')
testtask<-createDummyFeatures(obj = testtask)

learner<-makeLearner('classif.xgboost',predict.type = 'response')
learner$par.vals<-list(objective="binary:logistic", eval_metric="error")
params<-makeParamSet(makeDiscreteParam("booster",values = c("gbtree")),
                     makeIntegerParam("max_depth",lower = 3L,upper = 15L),
                     makeIntegerParam("nrounds",lower = 20L,upper = 100L),
                     makeNumericParam("eta",lower = 0.01,upper = 0.5),
                     makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
                     makeNumericParam("subsample",lower = 0.5,upper = 1),
                     makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))
rdesc<-makeResampleDesc("CV",stratify = T,iters=5L)
ctrl<-makeTuneControlRandom(maxit = 1000L)
parallelStartSocket(cpus = detectCores())

start_time<-Sys.time()
xgbtune<-tuneParams(learner = learner, task = traintask, resampling = rdesc, measures = acc, par.set = params, control = ctrl, show.info = T)
end_time<-Sys.time()
end_time-start_time

learner_tune<-setHyperPars(learner,par.vals = xgbtune$x)
a<-data.frame('index'=c(1:12032))
n_model=100
for (i in seq(1,n_model)){
  xgb_model<-train(learner = learner_tune,task = traintask)
  xgb_preds<-predict(xgb_model,testtask)
  test_class<-xgb_preds$data$response
  a=data.frame(a,test_class)
}

# Most common class
test_class<-data.frame('INDEX'=c(1:12032))
RETURN<-vector('character')
for (s in 1:12032){
  b<-ifelse(sum(unlist(as.numeric(as.character(a[s,seq(2,n_model+1)]))-1))<n_model/2,'No','Yes')
  RETURN<-c(RETURN,b)
}
test_class<-data.frame(test_class,RETURN)

# Submission file
write.csv(a,'models.csv')
write.csv(test_class, 'submission_TEAM_7.csv',row.names = FALSE)