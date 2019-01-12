# Again, you have to reverse engineer the input to readable code. This is the resulting code.
#for part 1 the answer is the first output
f  <- 12545763
b  <- 65536
it <- 0
te <- NULL
while(TRUE){
  while(b > 255){
    d <- 0
    c <- 256
    while(b >= c){
      d <- d+1
      c <- d+1
      c <- c * 256
    }
    c <- 1
    b <- d
    d <- b %% 2^8
    f <- f + d
    f <- ((f %% 2^24) * 65899) %% 2^24
  }
  if(f %in% te){
    cat(f) 
    break
  }
  te <- c(te, f)
  d  <- 0
  b  <- bitwOr(f,2^16)
  f  <- 4591209
  d  <- b %% 2^8
  f  <- f + d
  f  <- ((f %% 2^24) * 65899) %% 2^24  
}
te[1]

#21.2 ----
#part 2 the answer is the last one before a full round of te (f)
te[length(te)]
