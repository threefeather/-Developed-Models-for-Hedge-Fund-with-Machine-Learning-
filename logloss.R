logloss<-function(act,pred){
  y=act
  h=pred
  loss=-(y*log(h)+(1-y)*log(1-h))/length(y)
  return(sum(loss))
}