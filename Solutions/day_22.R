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



  
# 22.2
depth  <- 4848
target <- c(16, 701)
mt     <- matrix(rep(0, 48000), ncol = 60)
for(i in 1:60){
  for(j in 1:800){
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

wd  <- ncol(u)
ht  <- nrow(u)
all <- NULL
for(i in 1:799){
  for(j in 1:59){
    pt  <- u[i, j]
    ptn <- u[i, j + 1]
    ptu <- u[i + 1, j]
    #point next to
    if(pt == 0){
      if(ptn == 0){
        adn  <- c(i, j, 1, i, j + 1, 1, 1)
        adn1 <- c(i, j, 2, i, j + 1, 2, 1)
        all  <- rbind(all, adn, adn1)
      }
      if(ptn == 1){
        adn  <- c(i, j,1, i, j + 1, 1, 1)
        adn1 <- c(i, j,1, i, j + 1, 3, 8)
        adn2 <- c(i, j,2, i, j + 1, 1, 8)
        adn3 <- c(i, j,2, i, j + 1, 3, 8)
        all  <- rbind(all, adn, adn1, adn2, adn3)
      }
      if(ptn == 2){
        adn  <- c(i, j, 1, i, j + 1, 2, 8)
        adn1 <- c(i, j, 1, i, j + 1, 3, 8)
        adn2 <- c(i, j, 2, i, j + 1, 2, 1)
        adn3 <- c(i, j, 2, i, j + 1, 3, 8)
        all  <- rbind(all, adn, adn1, adn2, adn3)
      }
    }
    if(pt == 1){
      if(ptn == 0){
        adn  <- c(i, j, 1, i, j + 1, 1, 1)
        adn1 <- c(i, j, 1, i, j + 1, 2, 8)
        adn2 <- c(i, j, 3, i, j + 1, 1, 8)
        adn3 <- c(i, j, 3, i, j + 1, 2, 8)
        all  <- rbind(all, adn, adn1, adn2, adn3)
      }
      if(ptn == 1){
        adn  <- c(i, j, 1, i, j + 1, 1, 1)
        adn1 <- c(i, j, 3, i, j + 1, 3, 1)
        all  <- rbind(all, adn, adn1)
      }
      if(ptn == 2){
        adn  <- c(i, j, 1, i, j + 1, 2, 8)
        adn1 <- c(i, j, 1, i, j + 1, 3, 8)
        adn2 <- c(i, j, 3, i, j + 1, 2, 8)
        adn3 <- c(i, j, 3, i, j + 1, 3, 1)
        all<-rbind(all, adn, adn1, adn2, adn3)
      }
    }
    if(pt == 2){
      if(ptn == 0){
        adn  <- c(i, j, 2, i, j + 1, 1, 8)
        adn1 <- c(i, j, 2, i, j + 1, 2, 1)
        adn2 <- c(i, j, 3, i, j + 1, 1, 8)
        adn3 <- c(i, j, 3, i, j + 1, 2, 8)
        all<-rbind(all, adn, adn1, adn2, adn3)
      }
      if(ptn == 1){
        adn  <- c(i, j, 2, i, j + 1, 1, 8)
        adn1 <- c(i, j, 2, i, j + 1, 3, 8)
        adn2 <- c(i, j, 3, i, j + 1, 1, 8)
        adn3 <- c(i, j, 3, i, j + 1, 3, 1)
        all  <- rbind(all, adn, adn1, adn2, adn3)
      }
      if(ptn == 2){
        adn  <- c(i, j, 2, i, j + 1, 2, 1)
        adn1 <- c(i, j, 3, i, j + 1, 3, 1)
        all  <- rbind(all, adn, adn1)
      }
    }
    #point under
    if(pt == 0){
      if(ptu == 0){
        adn  <- c(i, j, 1, i + 1, j, 1, 1)
        adn1 <- c(i, j, 2, i + 1, j, 2, 1)
        all  <- rbind(all, adn, adn1)
      }
      if(ptu == 1){
        adn  <- c(i, j, 1, i + 1, j, 1, 1)
        adn1 <- c(i, j, 1, i + 1, j, 3, 8)
        adn2 <- c(i, j, 2, i + 1, j, 1, 8)
        adn3 <- c(i, j, 2, i + 1, j, 3, 8)
        all  <- rbind(all, adn, adn1, adn2, adn3)
      }
      if(ptu == 2){
        adn  <- c(i, j, 1, i + 1, j, 2, 8)
        adn1 <- c(i, j, 1, i + 1, j, 3, 8)
        adn2 <- c(i, j, 2, i + 1, j, 2, 1)
        adn3 <- c(i, j, 2, i + 1, j, 3, 8)
        all  <- rbind(all, adn, adn1, adn2, adn3)
      }
    }
    if(pt == 1){
      if(ptu == 0){
        adn  <- c(i, j, 1, i + 1, j, 1, 1)
        adn1 <- c(i, j, 1, i + 1, j, 2, 8)
        adn2 <- c(i, j, 3, i + 1, j, 1, 8)
        adn3 <- c(i, j, 3, i + 1, j, 2, 8)
        all  <- rbind(all, adn, adn1, adn2, adn3)
      }
      if(ptu == 1){
        adn  <- c(i, j, 1, i + 1, j, 1, 1)
        adn1 <- c(i, j, 3, i + 1, j, 3, 1)
        all  <- rbind(all, adn, adn1)
      }
      if(ptu == 2){
        adn  <- c(i, j, 1, i + 1, j, 2, 8)
        adn1 <- c(i, j, 1, i + 1, j, 3, 8)
        adn2 <- c(i, j, 3, i + 1, j, 2, 8)
        adn3 <- c(i, j, 3, i + 1, j, 3, 1)
        all  <- rbind(all, adn, adn1, adn2, adn3)
      }
    }
    if(pt == 2){
      if(ptu == 0){
        adn  <- c(i, j, 2, i + 1, j, 1, 8)
        adn1 <- c(i, j, 2, i + 1, j, 2, 1)
        adn2 <- c(i, j, 3, i + 1, j, 1, 8)
        adn3 <- c(i, j, 3, i + 1, j, 2, 8)
        all  <- rbind(all, adn, adn1, adn2, adn3)
      }
      if(ptu == 1){
        adn  <- c(i ,j ,2 ,i + 1, j, 1, 8)
        adn1 <- c(i ,j ,2 ,i + 1, j, 3, 8)
        adn2 <- c(i ,j ,3 ,i + 1, j, 1, 8)
        adn3 <- c(i ,j ,3 ,i + 1, j, 3, 1)
        all  <- rbind(all, adn, adn1, adn2, adn3)
      }
      if(ptu == 2){
        adn  <- c(i, j, 2, i + 1, j, 2, 1)
        adn1 <- c(i, j, 3, i + 1, j, 3, 1)
        all  <- rbind(all, adn, adn1)
      }
    }
 }
 cat(i,"\n")
}

library(igraph)
vc  <- NULL
vc1 <- NULL
for(i in 1:nrow(all)){
  v   <- paste(all[i, 1], all[i, 2], all[i, 3], sep = ",")
  v1  <- paste(all[i, 4], all[i, 5], all[i, 6], sep = ",")
  vc  <- c(vc, v)
  vc1 <- c(vc1, v1)
  if(round(i / 10000) == i / 10000){
    cat(i, "\n")
  }
}
t1           <- graph_from_edgelist(cbind(vc, vc1), directed = FALSE)
E(t1)$weight <- all[, 7]
el           <- get.shortest.paths(t1, from = "1,1,2", to = "701,16,2")
tt           <- names(unlist(el$vpath))
jj           <- sapply(strsplit(tt, ","), function(x){x[3]})
jj1          <- sapply(strsplit(tt, ","), function(x){x[1]})
jj2          <- sapply(strsplit(tt, ","), function(x){x[2]})
h            <- 0
re           <- "2"
for(i in 1:length(jj)){
  if(jj[i] != re){
    h  <- h + 1
    re <- jj[i]
  }
}
  
result <- (length(jj) - 1) + h * 7
result
