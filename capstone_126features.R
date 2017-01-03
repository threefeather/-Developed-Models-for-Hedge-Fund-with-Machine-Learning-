f.ex2<-read.csv("126features_1214.csv")[,-1]
x.ex2<-model.matrix(target~.,data = f.ex2)[,-1]
y.ex2<-f.ex2$target

set.seed(100)
train<- sample(1:nrow(f.ex2),0.7*nrow(f.ex2))
cv.ridge.out.ex2 = cv.glmnet(x.ex2[train,], y.ex2[train],family="binomial",
                            lambda = grid, alpha = 0, nfolds = 10)
model.ridge.ex2<-glmnet(x.ex2[train,],y.ex2[train],alpha=0,lambda=grid)
cv.ridge.out.ex2$lambda.min  ###0.09637935
summary(model.ridge.ex2)

pre.test.ex2<-predict(model.ridge.ex2,newx = x.ex2[-train,],s = cv.ridge.out.ex2$lambda.min,
                     type = "class")
sum(round(pre.test.ex2)==y[-train])/length(y[-train])  #0.521234

threshold<-seq(0.45,0.55,length = 100)
sens.ex2<-numeric(length = 100)
spec.ex2<-numeric(100)
class.acc.ex2<-numeric(100)
for (i in 1:length(threshold)){
  class.test<-ifelse(pre.test.ex2>=threshold[i],1,0)
  temp<-table(True=y[-train],Predict=class.test)
  #  print(temp)
  sens.ex2[i]<-temp[2,2]/(temp[2,1]+temp[2,2])
  spec.ex2[i]<-temp[1,1]/(temp[1,1]+temp[1,2])
  class.acc.ex2[i]<-sum(class.test==y[-train])/length(y[-train])
}

ggplot()+geom_line(aes(x=threshold,y=sens.ex,color="Sensitivity"))+geom_line(aes(x=threshold,y=spec.ex,color="Specificity"))+
  geom_line(aes(x=threshold,y=class.acc.ex,color="Class Accuracy"))+theme_economist()+
  scale_color_economist()+labs(title="Threshold Tune",x="Values of Threshold",y="ratio" )


max(class.acc.ex2)##0.5221127
which(class.acc.ex2==max(class.acc.ex2))  ##48
threshold[48]    ###0.4974747
data.frame(sen=sens.ex2,spe=spec.ex2)
threshold[55] ###0.5045455
class.acc.ex2[55] ##0.5200625

tour<-read.csv("numerai_tournament_data_1214.csv")
x.tour.ex2<-featuresexpand.tour(tour[,-1])
x.tour.ex2<-model.matrix(~.,x.tour.ex2)[,-1]
pre.tour.ex2<-predict(model.ridge.ex2,newx = x.tour.ex2,s = cv.ridge.out.ex2$lambda.min,
                     type = "response")
write.csv(pre.tour.ex2,"f_ex2_1214_0.5.csv")
write.csv(pre.tour.ex2+(0.5-0.4974747),"f_ex2_1214_maxclass.csv")
write.csv(pre.tour.ex2+(0.5-0.5045455),"f_ex2_1214_maxss.csv")
