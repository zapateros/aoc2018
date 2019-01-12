# Today's problem was the first, and only, one where I had no clue how to do it (22.2). I looked into some solutions online
# and it was actually a small change in my own solution for 22.1. Small, but very helpfull, because it opened my eyes for
# solving many other (future) problems. Bytheway, the solution was to add an extra parameter to the grid for calculating the
# shortest path. So not just x1,y1 to x2,y2 but x1,y1,z1 to x2,y2,z2 with z the climbing gear parameter, where the weighting is 7 
# when changing climbing gear (for example z1 to z2)
# The preparation (make edgelist) for calculating the shortest path in 22.2 is hardcoded (copy-paste). There is a much shorter solution possible

# 22.1 
depth  <- 4848
target <- c(16, 701)
mt     <- matrix(rep(0, 11216), ncol = 16)
for(i in 1:ncol(mt)){
  for(j in 1:nrow(mt)){
    if((i == 1 & j == 1) | (i == 16 & j == 701)){
      mt[j, i] <- depth %% 20183
    }else if(i == 1){
      mt[j, i] <- (((j - 1) * 48271) + depth) %% 20183
    }else if(j == 1){
      mt[j, i] <- (((i - 1) * 16807) + depth) %% 20183
    }else{
      mt[j, i] <- ((mt[j, i - 1] * mt[j - 1, i]) + depth) %% 20183
    }
  }
}
u <- mt %% 3
sum(u)



