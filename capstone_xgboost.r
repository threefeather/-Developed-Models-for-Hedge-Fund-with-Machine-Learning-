library(xgboost)
dtrain <- xgb.DMatrix(data = as.matrix(f.ex[train,1:42]), label=f.ex$target[train])
dtest <- xgb.DMatrix(data = as.matrix(f.ex[-train,1:42]), label=f.ex$target[-train])
watchlist <- list(test=dtest,train=dtrain)
eta=10^(seq(-2,-1, length = 30))
score_1=data.frame()
n.turn_1=data.frame()
for (i in 1:30){
  for (k in 1:6){
    model.xgboost.cv_ex<-xgb.train(data=dtrain,
                                  #label = f.ex$target[train],
                                  max.depth=k,
                                  objective="binary:logistic", ###"reg:linear",
                                  eta=eta[i],
                                  watchlist=watchlist,
                                  nfold =5,verbose = TRUE,
                           #       feval = TRUE,
#                                  eval_metric = "logloss",
                                  eval_metric="error",
                                  early.stop.round = 50,
                                  nrounds = 10000)
    score_1[i,k]=model.xgboost.cv_ex$bestScore
    n.turn_1[i,k]=model.xgboost.cv_ex$bestInd
  }
}
which(score_1==min(score_1),arr.ind = TRUE)
pre.test.ex.xg<-predict(model.xgboost.cv_ex,as.matrix(f.ex[-train,1:42]))
logloss(f.ex$target[-train],pre.test.ex.xg)

model.xgboost.cv_ex_best<-xgboost(data=as.matrix(f.ex[train,1:42]),
                                  label = f.ex$target[train],
                                  max.depth=2,
                                  objective="binary:logistic", ###"reg:linear",
                                  eta=eta[29],
                                  cv=10,
                                  eval_metric = "error",
#                                  early.stop.round = 20,
                                  nrounds =148 )

pre.test.ex.xg.best<-predict(model.xgboost.cv_ex_best,as.matrix(f.ex[-train,1:42]))
logloss(f.ex$target[-train],pre.test.ex.xg.best)

pre.tour.ex.xg.best<-predict(model.xgboost.cv_ex_best,x.tour.ex)
write.csv(pre.tour.ex.xg.best,"f_ex_1214_xgboost_2.csv")
