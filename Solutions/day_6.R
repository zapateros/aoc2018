# 6.1 ----
t<-read.csv("input_day_6.txt",header = FALSE)
tbs<-NULL
for(y in 1:2){
  tt<-t+y
  n<-360+2*y
  vc<-NULL
  for(i in 1:n){
    for(j in 1:n){
      x<-abs(tt[,1]-i)+abs(tt[,2]-j)
      mn<-which(x==min(x))
      if(length(mn)>1){
        mn<-51
      }
      vc<-c(vc,mn)
    }
  }
  tb<-table(vc)
  tbs<-cbind(tbs,tb)
}
max(tbs[tbs[,1]==tbs[,2],1])


# 6.2 ----
t<-read.csv("input_day_6.txt",header = FALSE)
n<-360
sf<-0
for(i in 1:n){
  for(j in 1:n){
    x<-abs(t[,1]-i)+abs(t[,2]-j)
    if(sum(x)<10000){
      sf<-sf+1
    }
  }
}
sf
