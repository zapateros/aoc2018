# I solved 25.1 before I finished the rest. When I eventually finished the other problems, I was ready for one last 
# good brainbreaker but unfortunately it was already the end of this aoc: there was no 25.2.

tt  <- readLines("input_day_25.txt")
t   <- gsub("\n",",",tt)
t   <- matrix(as.numeric(unlist(strsplit(t,","))), ncol = 4, byrow = T)
cst <- rep(0, nrow(t))
cl  <- 1
for(i in 1:nrow(t)){
  j   <- matrix(rep(t[i,], nrow(t)), ncol = 4, byrow = T)
  jj  <- t - j
  sms <- rowSums(abs(jj))
  th  <- which(sms < 4)
  if(sum(cst[th]) > 0){
    ss      <- cst[th]
    hh      <- ss[which(ss>0)]
    h       <- hh[order(hh)][1]
    cst[th] <- h
    cst[which(cst %in% hh)] <- h
    cl      <- max(cst) + 1
  }else{
    cst[th] <- cl
    cl      <- cl + 1
  }
}
length(table(cst))
