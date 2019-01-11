# day 11.1
gsn <- 3214
y   <- c(1:300)
xx  <- c(1:300)

gr  <- NULL
for(i in 1:length(xx)){ 
  x  <- i
  yy <- sapply(y,function(z){
     n  <- x+10
     n1 <- n*z
     n2 <- n1+gsn
     n3 <- n2*n
     if(n3 > 99){
        n4 <- floor((n3 / 100) %% 10)
     }else{
        n4 <- 0
     }
     n5 <- n4-5
     })
 gr <- cbind(gr,yy) 
}

gr1 <- NULL
for(t in 1:298){
  tt <- t-1
  ju <- NULL
  for(h in 1:298){
    hh <- h-1
    u  <- sum(gr[c(1:3)+tt,c(1:3)+hh])
    ju <- c(ju,u)
  }
  gr1 <- cbind(gr1,ju)
}

mx  <- max(gr1)
x_c <- which(gr1==mx)%%298
y_c <- ceiling(which(gr1==mx)/298)
paste(x_c,y_c,sep=",")


#day 11.2
gsn <- 3214
y   <- c(1:300)
xx  <- c(1:300)

gr <- NULL
for(i in 1:length(xx)){
  x  <- i
  yy <- sapply(y,function(z){
    n  <- x+10
    n1 <- n*z
    n2 <- n1+gsn
    n3 <- n2*n
    if(n3 > 99){
      n4 <- floor((n3 / 100) %% 10)
    }else{
      n4 <- 0
    }
    n5 <- n4-5
    })
 gr <- cbind(gr,yy)
  
}

all <- NULL
tp  <- NULL
for(sz in 1:300){
  gr1 <- NULL
  for(t in 1:(301-sz)){
    tt <- t-1
    ju <- NULL
    for(h in 1:(301-sz)){
      hh <- h-1
      u  <- sum(gr[c(1:sz)+tt,c(1:sz)+hh])
      ju <- c(ju,u)
    }
    gr1 <- cbind(gr1,ju)
  }
  mx  <- max(gr1)
  x_c <-which(gr1==mx)%%dim(gr1)[1]
  y_c <-ceiling(which(gr1==mx)/dim(gr1)[1])
  all <-c(all,paste(x_c[1],y_c[1],sz,sep=","))
  tp  <-c(tp,mx)
  
  cat(sz,"\n")
}
tp
all
