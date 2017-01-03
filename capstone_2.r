library(dplyr)
library(ggplot2)
library(reshape2)
library(ggthemes)
numerai<-read.csv("numerai_training_data_1130.csv")
numerai$target<-as.factor(numerai$target)

###EDA
names.col<-names(numerai)
new<-melt(numerai,id=c("target"),measure.vars = names.col[-22])
####feature balance check
ggplot(new)+geom_histogram(aes(x=variable,fill=as.factor(target)),stat = "count",position = "dodge")+theme_economist()+
  scale_fill_economist()+labs(title="Features Balance verification",x="Features(1-21)",y="populations",fill="Target" )+
  ylim(c(0,60000))

###feature value distribution check
ggplot(new)+geom_boxplot(aes(x=variable,y=value,fill=as.factor(target)),position = "dodge")+theme_economist()+
  scale_fill_economist()+labs(title="Feature values distributions",x="Features(1-21)",y="values",fill="Target" )


ggplot(new)+geom_freqpoly(aes(x=value,color=variable))+facet_grid(~target)+theme_economist()+
  scale_fill_economist()
new.1<-numerai%>%group_by(target)%>%summarise()

normalize<-function(x){
  return (x-mean(x))/(max(x)-min(x))
}
#numerai<-numerai%>%mutate(n.fea1=normalize(feature1))  
#ggplot(numerai)+geom_density(aes(x=n.fea1,color=as.factor(target)))#+facet_grid(~target)
summary(numerai)


###Random Forest
set.seed(100)
train<-sample(1:nrow(numerai),0.7*nrow(numerai))
library(randomForest)
model.rf<-randomForest(target~.,
                      data = numerai,
                      subset = train,
                      importance=TRUE)

table(numerai$target[train],round(model.rf$predicted))

err.true<-c()
err.false<-c()
n.tree<-seq(100,2000,by = 100)
for (i in 1:length(n.tree)){
  model.rf2<-randomForest(target~.,
                          data = numerai,
                          subset = train,
                          ntree=n.tree[i],
                          importance=TRUE)
  err.true[i]=model.rf2$confusion[2,3]
  err.false[i]=model.rf2$confusion[1,3]
}
save(model.rf2,file = "./randomForest_ntreetune.rda")  

tune.rf.ntree<-data.frame(ntree=n.treespecificity=1-err.false,sensitivity=1-err.true,oor=model.rf2$err.rate[n.tree,1])
write.csv(tune.rf.ntree,"tune_rf_ntree.csv")

varImpPlot(model.rf2)

ggplot(tune.rf.ntree)+geom_line(aes(x=n.tree,y=specificity,color="specificity"))+geom_line(aes(x=n.tree,y=sensitivity,color="sensitivity"))+
        geom_line(aes(x=n.tree,y=oor,color="out of bag error"))+theme_economist()+
        scale_fill_economist()+labs(title="Random Forest n.tree Tune",x="Number of trees",y="ratio" )



ggplot()+geom_line(aes(x=n.tree,y=err.true))+theme_economist()+
        scale_fill_economist()+labs(title="Random Forest Tune",x="Number of trees",y="class error of positive " )
ggplot()+geom_line(aes(x=n.tree,y=err.false))+theme_economist()+
        scale_fill_economist()+labs(title="Random Forest Tune",x="Number of trees",y="class error of negetive" )
model.rf2$confusion

ggplot()+geom_line(aes(x=1:2000,y=model.rf2$err.rate[,1]))+theme_economist()+
        scale_fill_economist()+labs(title="Random Forest Tune",x="Number of trees",y="OOB Error " )
oo.rr=c()
err.true.3<-c()
err.false.3<-c()
for (i in 1:21){
       
         model.rf3<-randomForest(target~.,
                                data = numerai,
                                subset = train,
                                ntree=1000,
                                mtry=i,
                                importance=TRUE)
        
        oo.rr[i]=model.rf3$err.rate[1000,1]
        err.true.3[i]=model.rf3$confusion[2,3]
        err.false.3[i]=model.rf3$confusion[1,3]
}
save(model.rf3,file = "./randomForest_mtrytune.rda")

tune.rf.mtry<-data.frame(mtry=1:21,specificity=1-err.false.3,sensitivity=1-err.true.3,oor=oo.rr)
write.csv(tune.rf.mtry,"tune_rf_mtry.csv")

ggplot(tune.rf.mtry)+geom_line(aes(x=1:21,y=specificity,color="specificity"))+geom_line(aes(x=1:21,y=sensitivity,color="sensitivity"))+
        geom_line(aes(x=1:21,y=oor,color="out of bag error"))+theme_economist()+
        scale_fill_economist()+labs(title="Random Forest mtry Tune",x="Number of variables at each split",y="ratio" )

model.rf.best.1207<-randomForest(target~.,
                            data = numerai.1207,
                            ntree=800,
                            mtry=1,
                            importance=TRUE)
save(model.rf.best,file = "./randomForest_best_1130.rda")

numerai.1207<-read.csv("numerai_training_data_1207.csv")
numerai.1207$target<-as.factor(numerai.1207$target)
ncol(numerai.1207)

tournament.1207<-read.csv("numerai_tournament_data.csv")
predict.tour<-predict(model.rf.best.1207,tournament.1207,type = "prob")

write.csv(predict.tour,"mysub_rf_1207.csv")


####Logistic regression


#Values of lambda over which to check.
grid = 10^seq(0, -5, length = 1000)

#Fitting the ridge regression. Alpha = 0 for ridge regression.

set.seed(100)
train<-sample(1:nrow(numerai.1207),0.7*nrow(numerai.1207))

library(glmnet)
x=model.matrix(target~., numerai.1207)[,-1]
y=numerai.1207$target
ridge.models = glmnet(x[train,], y[train], alpha = 0, lambda = grid,family = "binomial")
summary(ridge.models)
plot(ridge.models, xvar = "lambda", label = TRUE, main = "Ridge Regression")

cv.ridge.out = cv.glmnet(x, y,family="binomial",
                         lambda = grid, alpha = 0, nfolds = 15)

plot(cv.ridge.out, main = "Ridge Regression\n")

cv.ridge.out$lambda.min   ##0.01629751

cost_logloss<-c()
L2_nom<-c()
cost_logloss.train<-c()
#grid.2<-10^seq(1,-4,length =100)
grid.2<-seq(0.1,0,by=-0.0001)
n<-length(grid.2)
class.error<-c()
model.ridge.tune<-glmnet(x[train,], y[train], alpha = 0, lambda = grid.2,family = "binomial")
pre.min<-numeric(n)
pre.max<-numeric(n)
pre.median<-numeric(n)
for (i in 1:n){
  pre<-predict(model.ridge.tune,s = grid.2[i], newx = x[-train,],type="response")
  pre.max[i]<-max(pre)
  pre.min[i]<-min(pre)
  pre.median[i]<-median(pre)
  pre.class<-predict(model.ridge.tune,s = grid.2[i], newx = x[-train,],type="class")
  class.error[i]<-sum(pre.class==y[-train])/length(y[-train])
#  pre.train<-predict(model.ridge.tune,s = grid.2[i], newx = x[train,],type="response")
  #theta<-coef(model.rodge.tune)
  #x_0<-rep(1,nrow(x[-train,]))
 # x_0.train<-rep(1,nrow(x[train,]))
  #z<-cbind(x_0,x[-train,])%*%theta
 # z.train<-cbind(x_0.train,x[train,])%*%theta
 # hyp<-sigmod(z)
  #hyp.train<-sigmod(z.train)
#  L2_nom[i]<-sum(theta^2)
  cost_logloss[i]<-logloss(as.numeric(y[-train]),pre)##+sum(theta^2)/length(y[-train])
  cost_logloss.train[i]<- logloss(as.numeric(y[train]),pre.train)
  print(c(cost_logloss[i],L2_nom[i]))
}
index<-which(class.error==max(class.error))
grid.2[index]   ###0.0042
plot(x=grid.2,y=cost_logloss,xlab ="lambda",ylab = "logloss",main = "Ridge Regression---logloss")
plot(x=grid.2,y=class.error,xlab ="lambda",ylab = "class accuracy",main = "Ridge Regression")
plot(x=grid.2,y=pre.min,xlab ="lambda",ylab = "prediction.min",main = "Ridge Regression")
plot(x=grid.2,y=pre.max,xlab ="lambda",ylab = "prediction.max",main = "Ridge Regression")
plot(x=grid.2,y=pre.median,xlab ="lambda",ylab = "prediction.median",main = "Ridge Regression")

x.tour<-model.matrix(~.,tournament.1207)[,-c(1,2)]
pre.tour.ridge<-predict(model.ridge.tune,s=0,newx=x.tour,type = "response")
write.csv(pre.tour.ridge,"ridgenolambada.csv")


###lambda==0.01629751
pre.tour.ridge.la<-predict(model.ridge.tune,s=0.01629751,newx=x.tour,type = "response")
write.csv(pre.tour.ridge.la,"ridgelambada_0.02.csv")


###lambda==0.0042
pre.tour.ridge.la2<-predict(model.ridge.tune,s=0.0042,newx=x.tour,type = "response")
write.csv(pre.tour.ridge.la2,"ridgelambada_0.0042.csv")

####0.008973316
pre.tour.ridge.la3<-predict(model.ridge.tune,s=0.008973316,newx=x.tour,type = "response")
write.csv(pre.tour.ridge.la3,"ridgelambada_0.009.csv")


#### svm
library(e1071)
set.seed(100)
cv.svm.radial = tune(svm,
                     target ~ .,
                     data = numerai.1207[train,],
                     kernel = "radial",
                     ranges = list(cost = 10^(seq(-2, 1, length = 20)),
                                   gamma = 10^(seq(-2, 1, length = 20))))



######Gradient Boosting

library(gbm)
library(caret)
model.gbm<-gbm(target~.,
               data = numerai.1207[train,],
               distribution = "gaussian",
               n.trees = 10000,
               shrinkage = 0.01,
               interaction.depth = 6,
               cv.folds = 10)
ntree.gbm=seq(100,10000,by = 100)
cv.test<-predict(model.gbm,newdata = numerai.1207[-train,],n.trees = ntree.gbm,)
