# The results of this solution have to be interpreted before submitting. There are a couple of reasons for this:
# First, because R arrays start at index 1, the results should be substracted by 1. Second, R matrices have an y,x-format, which
# made the problem a bit more difficult, especially when you loss track of that. Also, it turned out you had to implement 
# movement order, where my code doesn't really do that. Therefore the results have to be carefully checked. The real solution is
# is this solution +1 or -1, depending on what direction both relevant cars went. I didn't have the time to make it more general.

#13.1
crs <- 1
#13.2
crs <- 8

t   <- readLines("input_day_13.txt")
vel <- rbind(c(1, 0), c(0, -1), c(0, 1), c(-1, 0))
#Make a start matrix
direction <- c(">", "\\^", "v", "<")
dir_c     <- c(1:4)
mat       <- NULL
drs       <- NULL
for(i in dir_c){
  x  <- unlist(gregexpr(direction[i], t)) 
  mt <- NULL
  if(sum(x > 0) > 0){
    c   <- x[x > 0]
    r   <- which(x > 0)
    dir <- rep(dir_c[i], length(c))
    is  <- 1
    mt  <- cbind(c, r)
    dr  <- cbind(dir, is)
    drs <- rbind(drs, dr)
    mat <- rbind(mat, mt)
  }
}
n       <- nrow(mat)
it      <- 0
crashes <- 0
while(1 == 1){
  it    <- it + 1
  mat_b <- mat
  for(i in 1:n){
    mat[i,] <- mat[i,] + vel[drs[i, 1],]
    nx      <- substr(t[mat[i, 2]], mat[i, 1], mat[i, 1])
    if(nx == "\\"){
      if(drs[i, 1] == 1){drs[i, 1]      <- 3}
      else if(drs[i, 1] == 4){drs[i, 1] <- 2}
      else if(drs[i, 1] == 2){drs[i, 1] <- 4}
      else if(drs[i, 1] == 3){drs[i, 1] <- 1}
    }else if(nx == "/"){
      if(drs[i, 1] == 1){drs[i, 1]      <- 2}
      else if(drs[i, 1] == 4){drs[i, 1] <- 3}
      else if(drs[i, 1] == 2){drs[i, 1] <- 1}
      else if(drs[i, 1] == 3){drs[i, 1] <- 4}
    }else if(nx == "+"){
      if(drs[i, 2] == 1){
        if(drs[i, 1] == 1){drs[i, 1]      <- 2}
        else if(drs[i, 1] == 4){drs[i, 1] <- 3}
        else if(drs[i, 1] == 2){drs[i, 1] <- 4}
        else if(drs[i, 1] == 3){drs[i, 1] <- 1}
        drs[i, 2] <- 2
      }else if(drs[i, 2] == 2){
        drs[i, 2] <- 3
      }else if(drs[i, 2] == 3){
        if(drs[i, 1] == 1){drs[i, 1]      <- 3}
        else if(drs[i, 1] == 4){drs[i, 1] <- 2}
        else if(drs[i, 1] == 2){drs[i, 1] <- 1}
        else if(drs[i, 1] == 3){drs[i, 1] <- 4}
        drs[i, 2] <- 1
      }
    }
  }
  yy <- paste(mat[, 1], mat[, 2], sep = ",")
  xx <- duplicated(yy)
  if(sum(xx) > 0){
    ue <- which(duplicated(yy, fromLast = TRUE) | duplicated(yy))
    crashes <- crashes + 1
    cat(it)
  }
  uu  <- mat[, 1]
  uu2 <- mat[, 2]
  ord <- order(uu)
  df1 <- diff(c(-1, uu[ord]))
  df2 <- diff(c(-1, uu2[ord]))
  adf <- abs(df1) + abs(df2)
  if(any(adf == 1)){
    frs  <- ord[which(adf == 1) - 1]
    sc   <- ord[which(adf == 1)]
    dir1 <- drs[frs, 1]
    dir2 <- drs[sc, 1]
    if(mat[frs, 1] == mat[sc, 1]){
      if(((mat[frs, 2] > mat[sc, 2]) & (dir1 == 2) & (dir2 == 3)) | ((mat[frs, 2] < mat[sc, 2]) & (dir1 == 3) & (dir2 == 2))){
         coll    <- paste(mat[frs,], "up_down")
         crashes <- crashes + 1
         n       <- n - 2
         cat(it)
         mat     <- mat[-c(sc, frs),]
         drs     <- drs[-c(sc, frs),]
       }
    }else if(mat[frs, 2] == mat[sc, 2]){
      if(((mat[frs, 1] < mat[sc, 1]) & (dir1 == 1) & (dir2 == 4)) | (mat[frs, 1] > mat[sc, 1]) & (dir1 == 4) & (dir2 == 1)){
        coll    <- paste(mat[frs,], "left_right")
        crashes <- crashes + 1
        n       <- n - 2
        cat(it)
        mat     <- mat[-c(sc, frs),]
        drs     <- drs[-c(sc, frs),]
      }
    }
  }
  if(crashes == crs ){
    break
  }
 }
 
 # 13.1
 coll #do both minus 1
 
 # 13.2
 mat+c(-1,-1) #or mat+c(-2,-1) depending on tick order
