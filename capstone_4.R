library(ggplot2)
library(ggthemes)
library(dplyr)
data.num<-read.csv("numerai_training_data_1214.csv")
new.2<-as.data.frame(sapply((-1*data.num[,-22]),FUN = exp))
l<-paste(rep("exp",21),names(data.num[,-22]),sep = "_")

#l<-paste(rep("newfeature",21),seq(1,21,by = 1),sep = "_")
names(new.2)<-l
f.ex<-cbind(new.2,data.num)
new.2$target<-data.num$target
ggplot(data.num)+geom_density(aes(x=log(1+feature21),color=as.factor(target)))+theme_bw()
ggplot(data.num)+geom_density(aes(x=sin(feature21),color=as.factor(target)))+theme_bw()
ggplot(data.num)+geom_density(aes(x=cos(feature21),color=as.factor(target)))+theme_bw()

ggplot(data.num)+geom_density(aes(x=feature21,color=as.factor(target)))+theme_bw()
ggplot(data.num)+geom_density(aes(x=exp(-feature21),color=as.factor(target)))+theme_bw()
ggplot(data.num)+geom_density(aes(x=tanh(feature21),color=as.factor(target)))+###theme_bw()+xlim(c(0,1))
  geom_density(aes(x=exp(-feature21),color=as.factor(target)))+
  geom_density(aes(x=feature21,color=as.factor(target)))+
  geom_density(aes(x=sin(feature21),color=as.factor(target)))+
  geom_density(aes(x=log(1+feature21),color=as.factor(target)))+
  theme_economist()+scale_color_economist()+labs(x="tansfermation of feature21",title="Density Plot of Feature in Different Transformation",color="Target")

new.3<-as.data.frame(sapply(data.num[,-22],FUN = sin))   ###sin term
l.3<-l<-paste(rep("sin",21),names(data.num[,-22]),sep = "_")
names(new.3)<-l.3

new.4<-as.data.frame(sapply(data.num[,-22],FUN = tanh)) ###tanh term
l.4<-l<-paste(rep("tanh",21),names(data.num[,-22]),sep = "_")
names(new.4)<-l.4


new.5<-as.data.frame(sapply((1+data.num[,-22]),FUN = log)) ###log term
l.5<-l<-paste(rep("log",21),names(data.num[,-22]),sep = "_")
names(new.5)<-l.5

new.6<-as.data.frame(sapply(data.num[,-22],FUN = cos))   ###sin term
l.6<-l<-paste(rep("cos",21),names(data.num[,-22]),sep = "_")
names(new.6)<-l.6

f.ex2<-cbind(new.2,new.3,new.4,new.5,new.6,data.num)

write.csv(f.ex2,"126features_1214.csv")

feature.ep.cross<-function(x){
  y<-x
for (i in 1:(length(names(x))-1)){
  for (j in (i+1):(length(names(x))-1)){
    n<-paste(i,j,sep = "_")
    col.x= names(x)[i]
    col.y= names(x)[j]
    y[n]<-x[i]*x[j]
  return(y)
  }
}
}
f.ex3<-f.ex
for (i in 1:(length(names(f.ex[,-43]))-1)){
  for (j in (i+1):(length(names(f.ex[,-43]))-1)){
    n<-paste(i,j,sep = "_")
    col.x= names(f.ex[,-43])[i]
    col.y= names(f.ex[,-43])[j]
    f.ex3[n]<-f.ex[,-43][i]*f.ex[,-43][j]
#    y<-mutate(f.ex[,-43],n=col.x*col.y)
#    f.ex3<-cbind(y,f.ex)
#   print(i,j)
  }
}

x.tour<-model.matrix(~.,tournament.1207)[,-c(1,2)]


write.csv(f.ex3,"f.ex3.csv")

#f.ex3<-feature.ep.cross(f.ex[,-43])

ggplot(new.2)+geom_density(aes(x=feature21,color=as.factor(target)))

library(glmnet)
grid = 10^seq(0, -5, length = 1000)
x= model.matrix(target~., data = f.ex)[,-1]
y= f.ex$target

set.seed(100)
train<- sample(1:nrow(f.ex),0.7*nrow(f.ex))
cv.ridge.out = cv.glmnet(x, y,family="binomial",
                         lambda = grid, alpha = 0, nfolds = 15)

plot(cv.ridge.out, main = "Ridge Regression\n")

f.ex2<-read.csv("f_ex2.csv")[,-1]
set.seed(100)
train<- sample(1:nrow(f.ex2),0.7*nrow(f.ex2))


for (i in 1:(length(names(as.data.frame(x.tour.ex)))-1)){
  for (j in (i+1):(length(names(as.data.frame(x.tour.ex)))-1)){
    n<-paste(i,j,sep = "_")
    col.x= names(x.tour.ex)[i]
    col.y= names(x.tour.ex)[j]
    x.tour.ex3[n]<-as.data.frame(x.tour.ex)[i]*as.data.frame(x.tour.ex)[j]
    #    y<-mutate(f.ex[,-43],n=col.x*col.y)
    #    f.ex3<-cbind(y,f.ex)
    #   print(i,j)
  }
}

library(xgboost)
eta=10^(seq(-2, -0.7, length = 30))
score_3=data.frame()
n.turn_3=data.frame()
dtrain <- xgb.DMatrix(data = as.matrix(f.ex2[train,1:126]), label=f.ex2$target[train])
dtest <- xgb.DMatrix(data = as.matrix(f.ex2[-train,1:126]), label=f.ex2$target[-train])
watchlist <- list(test=dtest,train=dtrain)
for (i in 1:30){
  for (k in 1:6){
    model.xgboost.cv_ex2<-xgb.train(data=dtrain,
                               # label = f.ex2$target[train],
                                max.depth=k,
                                objective="binary:logistic", ###"reg:linear",
                                eta=eta[i],
                                watchlist=watchlist,
                                nfold =5,verbose = TRUE,
                                eval_metric = "logloss",
                                early.stop.round = 50,
                                nrounds = 4000)
  score_3[i,k]=model.xgboost.cv_ex2$bestScore
  n.turn_3[i,k]=model.xgboost.cv_ex2$bestInd
  }
  print(i)
}

model.xgboost.cv_ex2.best<-xgb.train(data=dtrain,
                                # label = f.ex2$target[train],
                                max.depth=4,
                                objective="binary:logistic", ###"reg:linear",
                                eta=eta[26],   ##0.1320352
                                watchlist=watchlist,
                                nfold =5,verbose = TRUE,
                                eval_metric = "logloss",
                                early.stop.round = 50,
                                nrounds = 18)  ###18
x.tour.ex2<-read.csv("x_tour_ex2.csv")[,-1]
pre.tour.ex2<-predict(model.xgboost.cv_ex2.best,as.matrix(x.tour.ex2))
write.csv(pre.tour.ex2,"fex2_1214_xgboost.csv")
