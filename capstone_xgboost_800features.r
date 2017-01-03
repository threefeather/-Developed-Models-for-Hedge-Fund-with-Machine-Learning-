library(xgboost)
dtrain <- xgb.DMatrix(data = x.ex3[train,], label=f.ex3$target[train])
dtest <- xgb.DMatrix(data = x.ex3[-train,], label=f.ex3$target[-train])
watchlist <- list(test=dtest,train=dtrain)
eta=10^(seq(-2,-1, length = 30))
score_3=data.frame()
n.turn_3=data.frame()
for (i in 1:30){
  for (k in 1:6){
    model.xgboost.cv_ex3<-xgb.train(data=dtrain,
                                   #label = f.ex3$target[train],
                                   max.depth=k,
                                   objective="binary:logistic", ###"reg:linear",
                                   eta=eta[i],
                                   watchlist=watchlist,
                                   nfold =5,verbose = TRUE,
                                   #       feval = TRUE,
                                   #                                  eval_metric = "logloss",
                                   eval_metric="logloss",
                                   early.stop.round = 50,
                                   nrounds = 10000)
    score_3[i,k]=model.xgboost.cv_ex3$bestScore
    n.turn_3[i,k]=model.xgboost.cv_ex3$bestInd
  }
}
which(score_3==min(score_3),arr.ind = TRUE)
###24,4

#pre.test.ex3.xg<-predict(model.xgboost.cv_ex,as.matrix(f.ex3[-train,1:42]))
#logloss(f.ex3$target[-train],pre.test.ex3.xg)

model.xgboost.cv_ex3_best<-xgboost(data=x.ex3,
                                  label = f.ex3$target,
                                  max.depth=4,
                                  objective="binary:logistic", ###"reg:linear",
                                  eta=eta[24],  ###0.06210169
                                  cv=10,
                                  eval_metric = "logloss",
                                  # early.stop.round = 20,
                                  nrounds =n.turn_3[24,4] ) ##64

#pre.test.ex3.xg.best<-predict(model.xgboost.cv_ex3_best,as.matrix(f.ex3[-train,1:42]))
#logloss(f.ex3$target[-train],pre.test.ex3.xg.best)

pre.tour.ex3.xg.best<-predict(model.xgboost.cv_ex3_best,as.matrix(x.tour.ex3))
write.csv(pre.tour.ex3.xg.best,"f_ex3_1214_xgboost.csv")
