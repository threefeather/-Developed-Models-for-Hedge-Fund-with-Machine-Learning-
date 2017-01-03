featuresexpand.tour<-function(x){
  n=ncol(x)
  new.2<-as.data.frame(sapply((-1*x),FUN = exp))
  l<-paste(rep("exp",n),names(x),sep = "_")
  names(new.2)<-l
  
  new.3<-as.data.frame(sapply(x,FUN = sin))   ###sin term
  l.3<-l<-paste(rep("sin",n),names(x),sep = "_")
  names(new.3)<-l.3
  
  new.4<-as.data.frame(sapply(x,FUN = tanh)) ###tanh term
  l.4<-l<-paste(rep("tanh",n),names(x),sep = "_")
  names(new.4)<-l.4
  
  
  new.5<-as.data.frame(sapply((1+x),FUN = log)) ###log term
  l.5<-l<-paste(rep("log",n),names(x),sep = "_")
  names(new.5)<-l.5
  
  new.6<-as.data.frame(sapply(x,FUN = cos))   ###sin term
  l.6<-l<-paste(rep("cos",n),names(x),sep = "_")
  names(new.6)<-l.6
  
  f.ex2<-cbind(new.2,new.3,new.4,new.5,new.6,x)
  
  return(f.ex2)
}