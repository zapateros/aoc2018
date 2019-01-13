# For this day's problem (second part), I made a very pragmatic solution which starts with picking random points and find local maxima
# However, the right result is only given if the local maximum is the absolute maximum in the grid. To maximize the chance,
# try as many initial points as computationally suitable.
# While writing the pragmatic solution I realized it was a maximizing/minimizing problem which could be solved with standard 
# functions. However, it had been a while since I implemented such a thing, so I checked other solutions for a quick refresh of the mind
# I have to say: I wish I had a little bit more perseverance to complete this problem without looking at other's solutions because
# my solution was already 'on the tips of my fingers'
# Well.. on the upside.. now I have something to beat next year


# 23.1
tt   <- readLines("input_day_23.txt")
tt   <- gsub("pos=<","",tt)
tt   <- gsub(">","",tt)
tt   <- gsub(" r=","",tt)
tt   <- gsub("\n",",",tt)
tt   <- as.numeric(unlist(strsplit(tt,",")))
t    <- matrix(tt, ncol = 4, byrow = T)
mx   <- which.max(t[, 4])
a    <- abs(t[, 1] - t[mx, 1])
b    <- abs(t[, 2] - t[mx, 2])
c    <- abs(t[, 3] - t[mx, 3])
tog  <- a + b + c
mxrd <- t[mx, 4]
sum(tog <= mxrd)

# 23.2 SOLUTION 1, stochastic chance for the right result. Better solution at the bottom
tt     <- readLines("input_day_23.txt")
tt     <- gsub("pos=<","",tt)
tt     <- gsub(">","",tt)
tt     <- gsub(" r=","",tt)
tt     <- gsub("\n",",",tt)
tt     <- as.numeric(unlist(strsplit(tt,",")))
t      <- matrix(tt, ncol = 4, byrow = T)
rd     <- round(t)
matrix <- rd
amount <- 12000000
x_min  <- min(matrix[, 1])
x_max  <- max(matrix[, 1])
y_min  <- min(matrix[, 2])
y_max  <- max(matrix[, 2])
z_min  <- min(matrix[, 3])
z_max  <- max(matrix[, 3])
xx     <- x_max - x_min
yy     <- y_max - y_min
zz     <- z_max - z_min
x      <- x_min + c(1:floor(xx / amount)) * amount
y      <- y_min + c(1:floor(yy / amount)) * amount
z      <- z_min + c(1:floor(zz / amount)) * amount
cl     <- NULL
lc     <- NULL
for(i in 1:length(x)){
  for(j in 1:length(y)){
    for(k in 1:length(z)){
      mt  <- matrix(rep(c(x[i], y[j], z[k]), nrow(matrix)), ncol = 3, byrow = TRUE)
      uu  <- matrix[,1:3] - mt
      uuu <- rowSums(abs(uu))
      clc <- sum(uuu <= matrix[, 4])
      cl  <- c(cl, clc)
      lc  <- rbind(lc, c(i, j, k))
    }
  }
}
mx  <- lc[which(cl == max(cl)),]
mx1 <- c(x_min, y_min, z_min) + mx * amount
while(1 == 1){
  x_min  <- mx1[1] - amount
  x_max  <- mx1[1] + amount
  y_min  <- mx1[2] - amount
  y_max  <- mx1[2] + amount
  z_min  <- mx1[3] - amount
  z_max  <- mx1[3] + amount
  amount <- 12000000
  xx     <- x_max - x_min
  yy     <- y_max - y_min
  zz     <- z_max - z_min
  x      <- x_min + c(1:floor(xx / amount)) * amount
  y      <- y_min + c(1:floor(yy / amount)) * amount
  z      <- z_min + c(1:floor(zz / amount)) * amount
  cl     <- NULL
  lc     <- NULL
  for(i in 1:length(x)){
    for(j in 1:length(y)){
      for(k in 1:length(z)){
        mt  <- matrix(rep(c(x[i], y[j], z[k]), nrow(matrix)), ncol = 3, byrow = TRUE)
        uu  <- matrix[,1:3] - mt
        uuu <- rowSums(abs(uu))
        clc <- sum(uuu <= matrix[, 4])
        cl  <- c(cl, clc)
        lc  <- rbind(lc, c(i, j, k))
      }
    }
  }
  mx  <- lc[which(cl == max(cl))[1],]
  mx1 <- c(x_min, y_min, z_min) + mx * amount
  mx  <- lc[which(cl == max(cl)),]
  mx1 <- c(x_min, y_min, z_min) + mx * amount
}

# 23.2 SOLUTION 2: optimization solution DEoptim (searches minimum)
tt <- readLines("input_day_23.txt")
tt <- gsub("pos=<","",tt)
tt <- gsub(">","",tt)
tt <- gsub(" r=","",tt)
tt <- gsub("\n",",",tt)
tt <- as.numeric(unlist(strsplit(tt,",")))
t  <- matrix(tt, ncol = 4, byrow = T)

amount_bots<-function(point){
  x  <- point[1]
  y  <- point[2]
  z  <- point[3]
  mt <- matrix(rep(c(x, y, z), nrow(matrix)), ncol = 3, byrow = TRUE)
  u  <- t[, 1:3] - mt
  uu <- sum(rowSums(abs(u)) <= t[, 4])
  1000 - uu
}

#points have slight variations so round them
round_x <- function(x) { x[1:3] <- round(x[1:3]) } 

max_r <- max(t[,4])
min_x <- min(t[,1]) - max_r
min_y <- min(t[,2]) - max_r
min_z <- min(t[,3]) - max_r
max_x <- max(t[,1]) + max_r
max_y <- max(t[,2]) + max_r
max_z <- max(t[,3]) + max_r
out   <- DEoptim(amount_bots, lower=c(min_x, min_y, min_z), upper = c(max_x, max_y, max_z),
         fnMap = round_x, control = DEoptim.control(VTR = 0, itermax = 10000))
sum(out$optim$bestmem)
