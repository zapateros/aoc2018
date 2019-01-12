# I printed out the input and did the filling of the buckets manually in notepad++. Maybe someday I'll write a computational solution
# This is the (bulky) code to make the grid

tt <- readLines("input_day_17.txt")
mt <- matrix(rep(".", 2000000), ncol = 1000, byrow = TRUE)
t1 <- matrix(unlist(strsplit(tt,", ")), ncol = 2,byrow = TRUE)
for(i in 1:nrow(t1)){
  x1 <- t1[i,]
  x2 <- grep("x", x1)
  x3 <- x1[x2]
  x4 <- gsub("x=", "", x3)
  if(grepl("\\.", x4)){
    x9  <- as.numeric(unlist(strsplit(x4,"\\.\\.")))
    x11 <- x9[1]
    x12 <- x9[2]
  }else{
    x11 <- as.numeric(x4)
    x12 <- as.numeric(x4)
  }
  x5 <- grep("y", x1)
  x6 <- x1[x5]
  x7 <- gsub("y=", "", x6)
  if(grepl("\\.", x7)){
    x8  <- as.numeric(unlist(strsplit(x7,"\\.\\.")))
    y11 <- x8[1]
    y12 <- x8[2]
  }else{
    y11 <- as.numeric(x7)
    y12 <- as.numeric(x7)
  }
  mt[c(y11:y12), c(x11:x12)] <- "#"
}
write.table(mt, "grid.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)
