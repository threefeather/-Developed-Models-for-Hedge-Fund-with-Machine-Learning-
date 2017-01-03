library(dplyr)
library(caret)
library(gbm)
set.seed(0)
data<-read.csv("train.csv")
kag.test<-read.csv("test.csv")[,-1]
data.full<-rbind(data[,-c(1,132)],kag.test)
#dm.all<-model.matrix(~.,data.full)
dm.all.train<-model.matrix(~.,data.full)[1:188318,]
#dm.all.train<-mutate(dm.all.train,loss=log(data$loss))
dm.all.test<-model.matrix(~.,data.full)[188319:313864,]

preProc<-preProcess(dm.all.train, method = "zv")
preProc

save(preProc,file = "./preproczv.rda")
#dm.all.pp<-predict(preProc,dm.all)
dm.all.train.pp<-as.data.frame(predict(preProc,dm.all.train))
dm.all.train.pp$loss<-log(data$loss+1)

dm.all.test.pp<-as.data.frame(predict(preProc,dm.all.test))

set.seed(0)
train<-sample(1:nrow(data),0.8*nrow(data))

n_run <- 50
n_epochs <- 500


library(h2o)

##### Initiallize the h2o frame 
h2o.init(nthreads = -1)
train.h2o<-as.h2o(dm.all.train.pp[train, ])
test.h2o<-as.h2o(dm.all.train.pp[-train, ])

kag.h2o<-as.h2o(dm.all.test.pp)

set.seed(0)
## Create an empty data frame for results
res_tmp <- data.frame(Trial = 1:n_run, Training = NA, Test = NA, Duration = NA)

## Train model and evaluate performance for n times
cat("\n[Experiment 1D]: DNN with Dropout (Inputs/Hidden = 20%/50%) ...\n")

for (n in 1:n_run) {
  
  ## Display
  cat("Run", n, "out of", n_run, "...\n")
  
  ## Start the timer
  tt <- start_timer()
  
  ## Train the model
  model <- h2o.deeplearning(x = 1:1037,  # column numbers for predictors
                            y = 1038,   # column number for label
                            training_frame = train.h2o,
                            activation = "TanhWithDropout",
                            input_dropout_ratio = 0.2,
                            hidden_dropout_ratios = c(0.5,0.5,0.5),
 #                           balance_classes = TRUE,
                            hidden = c(500,200,50),  ## three hidden layers
                            epochs = n_epochs)
 save(model,file ="./model.nn1.rda")
 
 
 
 
  ## Evaluate performance
  yhat_train <- as.data.frame(h2o.predict(model, train.h2o)$predict)
  yhat_train <- as.data.frame(yhat_train)
  sum((yhat_train-as.data.frame(train.h2o$loss)$loss)^2)/nrow.H2OFrame(train.h2o)
  ##0.445656
  yhat_test <- as.data.frame(h2o.predict(model, test.h2o)$predict)
  sum((yhat_test-as.data.frame(test.h2o$loss)$loss)^2)/nrow.H2OFrame(test.h2o)
  ##0.44776
  yhat_test <- as.data.frame(yhat_test))

  result.kag<-as.data.frame(h2o.predict(model, kag.h2o)$predict)
  
  write.csv(exp(result.kag)-1,"nn.csv")
  ## Store Results
  #res_tmp[n, 1] <- n
  #res_tmp[n, 2] <- round(confusionMatrix(yhat_train, y_train)$overall[1], 4)
  #res_tmp[n, 3] <- round(confusionMatrix(yhat_test, y_test)$overall[1], 4)
  #res_tmp[n, 4] <- round(stop_timer(tt), 2)
  
#}
  
  
  
  
  
  normalize = function(x) { 
    return((x - min(x)) / (max(x) - min(x)))
  }
  dm.all.train.pp$loss<-normalize(log(data$loss+1))
  train.h2o<-as.h2o(dm.all.train.pp[train, ])
  test.h2o<-as.h2o(dm.all.train.pp[-train, ])
  
  
  model.2 <- h2o.deeplearning(x = 1:1037,  # column numbers for predictors
                            y = 1038,   # column number for label
                            training_frame = train.h2o,
                            activation = "TanhWithDropout",
                            input_dropout_ratio = 0.2,
                            hidden_dropout_ratios = c(0.5,0.5,0.5),
                            #                           balance_classes = TRUE,
                            hidden = c(500,200,50),  ## three hidden layers
                            epochs = n_epochs)
  
  save(model.2,file = "./nn.h2o.rda")
  a=max(log(data$loss+1)) ###11.70365532
  b=min(log(data$loss+1))  ###0.5128236264
  
  yhat_train.2 <- as.data.frame(h2o.predict(model.2, train.h2o)$predict)
  
  sum((yhat_train.2-as.data.frame(train.h2o$loss)$loss)^2)/nrow.H2OFrame(train.h2o)
  ##0.003556397
  
  yhat_test.2 <- as.data.frame(h2o.predict(model.2, test.h2o)$predict)
  
  sum((yhat_test.2-as.data.frame(test.h2o$loss)$loss)^2)/nrow.H2OFrame(test.h2o)
  ##0.003582277

result.kag.2<-as.data.frame(h2o.predict(model.2, kag.h2o)$predict)

result<-exp(result.kag.2*(a-b)+b)-1
write.csv(result,"nn2.csv")

  
  
  
  
  model.3 <- h2o.deeplearning(x = 1:1037,  # column numbers for predictors
                              y = 1038,   # column number for label
                              training_frame = train.h2o,
                              activation = "TanhWithDropout",
                              input_dropout_ratio = 0.1,
                              hidden_dropout_ratios = c(0.5,0.5,0.5),
                              #                           balance_classes = TRUE,
                              hidden = c(800,500,200),  ## three hidden layers
                              l2 = 0.01,
                              nfolds = 10,
                              epochs = n_epochs)
 
  a=max(log(data$loss+1)) ###11.70365532
  b=min(log(data$loss+1))  ###0.5128236264
  
  yhat_train.3 <- as.data.frame(h2o.predict(model.3, train.h2o)$predict)
  
  sum((yhat_train.3-as.data.frame(train.h2o$loss)$loss)^2)/nrow.H2OFrame(train.h2o)
  ##0.003556397
  
  yhat_test.3 <- as.data.frame(h2o.predict(model.3, test.h2o)$predict)
  
  sum((yhat_test.3-as.data.frame(test.h2o$loss)$loss)^2)/nrow.H2OFrame(test.h2o)
  ##0.003582277
  
  result.kag.3<-as.data.frame(h2o.predict(model.3, kag.h2o)$predict)
  
  result.3<-exp(result.kag.2*(a-b)+b)-1 