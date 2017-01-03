library(dplyr)
f.ex3<-read.csv("f.ex3.csv")[,-1]
x.ex3<-model.matrix(target~.,data = f.ex3)[,-1]
y.ex3<-f.ex3$target

set.seed(0)
grid = 10^seq(0, -5, length = 1000)

train<- sample(1:nrow(f.ex3),0.7*nrow(f.ex3))
cv.ridge.out.ex3 = cv.glmnet(x = model.matrix(target~.,data = f.ex3)[train,-1], y = y.ex3[train],family="binomial",
                             lambda = grid, alpha = 0, nfolds = 20)
model.ridge.ex3<-glmnet(x.ex3[train,],y.ex3[train],alpha=0,lambda=grid)
cv.ridge.out.ex3$lambda.min  ###0.8608648

pre.test.ex3<-predict(model.ridge.ex3,newx = x.ex3[-train,],s = cv.ridge.out.ex3$lambda.min,
                      type = "class")
sum(round(pre.test.ex3)==y[-train])/length(y[-train]) ##0.5271649


threshold<-seq(0.45,0.55,length = 100)
sens.ex3<-numeric(length = 100)
spec.ex3<-numeric(100)
class.acc.ex3<-numeric(100)
for (i in 1:length(threshold)){
  class.test<-ifelse(pre.test.ex3>=threshold[i],1,0)
  temp<-table(True=y[-train],Predict=class.test)
  #  print(temp)
  sens.ex3[i]<-temp[2,2]/(temp[2,1]+temp[2,2])
  spec.ex3[i]<-temp[1,1]/(temp[1,1]+temp[1,2])
  class.acc.ex3[i]<-sum(class.test==y[-train])/length(y[-train])
}
max(class.acc.ex3)##0.5291419
which(class.acc.ex3==max(class.acc.ex3))  ##48
threshold[48]    ###0.4974747
data.frame(sen=sens.ex3,spe=spec.ex3)
threshold[54] ###0.5035354
class.acc.ex3[54] ##0.5221127

x.tour.ex3<-as.data.frame(x.tour.ex)
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
model.ridge.ex3.all<-glmnet(x.ex3,y.ex3,alpha=0,lambda=grid)

pre.tour.ex3<-predict(model.ridge.ex3,newx = model.matrix(~.,x.tour.ex3)[,-1],s = cv.ridge.out.ex3$lambda.min,
                      type = "response")
write.csv(pre.tour.ex2+(0.5-0.4944444),"f_ex3_1214_maxclass.csv")
write.csv(pre.tour.ex3,"f_ex3_1214_0.5_lamb0.8.csv")



cv.ridge.out.ex3.all = cv.glmnet(x = model.matrix(target~.,data = f.ex3)[,-1], y = y.ex3,family="binomial",
                             lambda = grid, alpha = 0, nfolds = 10)
cv.ridge.out.ex3.all$lambda.min ###0.226128
model.ridge.ex3<-glmnet(x.ex3,y.ex3,alpha=0,lambda=grid)
pre.tour.ex3.all<-predict(model.ridge.ex3,newx = model.matrix(~.,x.tour.ex3)[,-1],s = cv.ridge.out.ex3.all$lambda.min,
                      type = "response")
write.csv(pre.tour.ex3.all,"f_ex3_1214_all_lamb0.2.csv")
