#VIMP
library(randomForestSRC)
require(xgboost)
library(ggRandomForests)
# Veteran's Administration Lung Cancer Trial. Randomized 
# trial of two treatment regimens for lung cancer.
data(veteran, package = "randomForestSRC")
head(veteran)
dim(veteran)
vet.rsf <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100,importance = TRUE)
print(vet.rsf)
gg_dta <- gg_vimp(vet.rsf)
plot(gg_dta)
         
#xgboost
dtrain <- xgb.DMatrix(data = data.matrix(veteran[,c(1,2,5,6,7,8)]), label=veteran$time,censor=veteran$status)
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  censor = getinfo(dtrain, "censor")
  permissible=0
  concordance=0
  k=length(labels)-1
  for (i in 1:k){
    m=i+1
    for(j in m:length(labels)){
      #cat(i,j,labels[i], labels[j],censor[i],censor[j],preds[i],preds[j],"\n")
      if((labels[i]==labels[j]) & (censor[i]==0) & (censor[j]==0)){ next } 
      if((labels[i]<labels[j]) & (censor[i]==0) ){ next } 
      if((labels[j]<labels[i]) & (censor[j]==0) ){ next } 
      
      permissible<-permissible+1 
      if((labels[i]<labels[j]) & (preds[i]<preds[j])) {concordance<-concordance+1} 
      if((labels[i]>labels[j]) & (preds[i]>preds[j]))
      {concordance<-concordance+1}
      if((labels[i]!=labels[j]) & (preds[i]==preds[j])) {concordance<-concordance+0.5} 
      if((labels[i]==labels[j]) & (censor[i]==1) & (censor[j]==1) & (preds[i]==preds[j]))  
      {concordance<-concordance+1} 
      if((labels[i]==labels[j]) & (censor[i]==1) & (censor[j]==1) & (preds[i]!=preds[j]))   
      {concordance<-concordance+0.5} 
      if((labels[i]==labels[j]) & (censor[i]==1) & (censor[j]==0) &  (preds[i]<preds[j]))    
      {concordance<-concordance+1} 
      if((labels[i]==labels[j]) & (censor[i]==0) & (censor[j]==1) & (preds[i]>preds[j]))    
      {concordance<-concordance+1} 
      if((labels[i]==labels[j]) & (censor[i]==1) & (censor[j]==0) & (preds[i]>=preds[j]))    
      {concordance<-concordance+0.5} 
      if((labels[i]==labels[j]) & (censor[i]==0) & (censor[j]==1) & (preds[i]<=preds[j])) 
      {concordance<-concordance+0.5}
      
    }
  }
  err <- as.numeric(1.0-concordance/permissible)
  return(list(metric = "cindex", value = err))
}
bst<-xgb.train(dtrain,
               nrounds     = 500,nthread = 2,
               param = list(eta         = 0.01,
                            max_depth   = 6,
                            colsample_bytree=0.7,
                            min_child_weight=50,
                            subsample=0.7,
                            obj = "coxph",  eval_metric=evalerror))

importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

#classifications

dtrain <- xgb.DMatrix(data = data.matrix(veteran[,c(1,2,5,6,7,8)]),
                      label=veteran$status)

bst<-xgb.train(dtrain,
               nrounds     = 200,nthread = 2,
               param = list(eta         = 0.01,
                            max_depth   = 6,
                            colsample_bytree=0.7,
                            min_child_weight=50,
                            subsample=0.7,
                            objective = "binary:logistic"))
importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

vet.cla_rsf<- rfsrc(status ~., data = veteran[,-3],ntree=500,importance = TRUE)
gg_dta <- gg_vimp(vet.cla_rsf)
plot(gg_dta)
