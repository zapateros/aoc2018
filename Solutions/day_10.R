# For this problem I thought of two ways: to check when te area is smallest or to check when the most lights are exactly above oneother
# I was afraid they added noise, so the area checker would not work. Therefore I chose the second method
# Later I heard they didn't add noise so the first method should also work

t  <- readLines("input_day_10.txt")
t  <- gsub(" ", "", t)
t  <- gsub("position=", "", t)
t  <- gsub("velocity=", "", t)
tt <- t(sapply(strsplit(t, "><"), function(x){x}))
tt <- gsub("<", "", tt)
tt <- gsub(">", "", tt)
t1 <- t(sapply(strsplit(tt[, 1],","),function(x){as.numeric(x)}))
t2 <- t(sapply(strsplit(tt[,2],","),function(x){as.numeric(x)}))

t1[, 1] <- t1[, 1] + 10400 * t2[, 1]
t1[, 2] <- t1[, 2] + 10400 * t2[, 2]
all <- NULL
for(j in 1:200){
  t1[, 1] <- t1[, 1] + t2[, 1]
  t1[, 2] <- t1[, 2] + t2[, 2]
  tb      <- table(t1[, 2])
  one     <- 0
  for(i in 1:length(tb)){
    xen  <- t1[which(t1[, 2] == names(tb[i])), 1]
    ones <- sum(diff(xen[order(xen)]) == 1)
    one  <- one + ones
  }
  all <- c(all, one)
}

#10400 - 10511. probably around 10455
t1      <- t(sapply(strsplit(tt[,1],","),function(x){as.numeric(x)}))
t2      <- t(sapply(strsplit(tt[,2],","),function(x){as.numeric(x)}))
t1[, 1] <- t1[, 1] + 10450 * t2[, 1]
t1[, 2] <- t1[, 2] + 10450 * t2[, 2]

j <- 0
# There is a manual part here. just run the following part until you see the message
j       <- j + 1
t1[, 1] <- t1[, 1] + t2[, 1]
t1[, 2] <- t1[, 2] + t2[, 2]
tb      <- table(t1[, 2])
one     <- 0
mn      <- min(t1[, 1])
mx      <- max(t1[, 1])
  
pic <- NULL
for(i in 1:length(tb)){
  rps    <- rep(0,mx)
  l      <- t1[which(t1[, 2] == names(tb[i])), 1]
  rps[l] <- 4
  pic    <- cbind(pic, rps)
}
pic[c(150:210),]
