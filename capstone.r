library(dplyr)
library(ggplot2)
library(reshape2)
library(ggthemes)
numerai<-read.csv("numerai_training_data.csv")
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

ggplot(new)+geom_violin(aes(x=variable,y=value,fill=as.factor(target)),position = "dodge")+theme_economist()+
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
oor<-c()
for (i in 1:21){
  model.rf3<-randomForest(target~.,
                          data = numerai,
                          subset = train,
                          mtry=i,
                          ntree=2000,
                          importance=TRUE)
  oor[i]=model.rf2$err.rate[500, 1]
  err.true[i]=model.rf2$confusion[2,3]
  err.false[i]=model.rf2$confusion[1,3]
  print("interation:",i)
}
ggplot()+geom_line(aes(x=1:21,y=err.true+err.false))+theme_economist()+labs(x="mtry",y="confusion error",title="Random Forest vilidation")
importance(model.rf2)
varImpPlot(model.rf2)

model.rf2<-randomForest(target~.,
                       data = numerai,
                       subset = train,
                       ntree=1000,
                       mtry=7,
                       importance=TRUE)



set.seed(0)
boost.boston2 = gbm(medv ~ ., data = Boston[train, ],
                    distribution = "gaussian",
                    n.trees = 10000,
                    interaction.depth = 4,
                    shrinkage = 0.1)
