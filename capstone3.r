new.2<-as.data.frame(sapply((-1*data.num[,-22]),FUN = exp))
l<-paste(rep("newfeature",21),seq(1,21,by = 1),sep = "_")
names(new.2)<-l
f.ex<-cbind(new.2,data.num)
write.csv(f.ex,"expandfea_1214.csv")

x.ex= model.matrix(target~., data = f.ex)[,-1]
y.ex= f.ex$target
set.seed(100)
train<- sample(1:nrow(f.ex),0.7*nrow(f.ex))
cv.ridge.out.ex = cv.glmnet(x.ex, y.ex,family="binomial",
                         lambda = grid, alpha = 0, nfolds = 15)

model.ridge.ex<-glmnet(x.ex[train,],y.ex[train],alpha=0,lambda=grid)

cv.ridge.out.ex$lambda.min  ###0.01942
summary(model.ridge.ex)
pre.test.ex<-predict(model.ridge.ex,newx = x.ex[-train,],s = cv.ridge.out.ex$lambda.min,
                  type = "class")
sum(round(pre.test.ex)==y[-train])/length(y[-train])


threshold<-seq(0.45,0.55,length = 100)
sens.ex<-numeric(length = 100)
spec.ex<-numeric(100)
class.acc.ex<-numeric(100)
for (i in 1:length(threshold)){
  class.test<-ifelse(pre.test.ex>=threshold[i],1,0)
  temp<-table(True=y[-train],Predict=class.test)
#  print(temp)
  sens.ex[i]<-temp[2,2]/(temp[2,1]+temp[2,2])
  spec.ex[i]<-temp[1,1]/(temp[1,1]+temp[1,2])
  class.acc.ex[i]<-sum(class.test==y[-train])/length(y[-train])
}

ggplot()+geom_line(aes(x=threshold,y=sens.ex,color="Sensitivity"))+geom_line(aes(x=threshold,y=spec.ex,color="Specificity"))+
  geom_line(aes(x=threshold,y=class.acc.ex,color="Class Accuracy"))+theme_economist()+
  scale_color_economist()+labs(title="Threshold Tune",x="Values of Threshold",y="ratio" )


max(class.acc.ex)##0.521893
which(class.acc.ex==max(class.acc.ex))  ##47
threshold[47]    ###0.4964646
data.frame(sen=sens.ex,spe=spec.ex)
threshold[55] ###0.5055556
class.acc.ex[55] ##0.5212828


tour<-read.csv("numerai_tournament_data_1214.csv")
new.tour<-as.data.frame(sapply((-1*tour[,-1]),FUN = exp))
names(new.tour)<-l
f.ex.tour<-cbind(new.tour,tour[,-1])
x.tour.ex<-model.matrix(~.,f.ex.tour)[,-1]
pre.tour.ex<-predict(model.ridge.ex,newx = x.tour.ex,s = cv.ridge.out.ex$lambda.min,
                          type = "response")
write.csv(pre.tour.ex,"ridgelambada_0.019_1214.csv")
write.csv(pre.tour.ex+(0.5-0.4964646),"ridgelambada_maxclass_1214.csv")
#write.csv(pre.tour.ex+(0.5-0.4964646),"ridgelambada_maxclass_1214.csv")
