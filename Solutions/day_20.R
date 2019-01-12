# 20.1
tt <- readLines("input_day_20.txt")
t  <- paste0("()", tt, "()")

str_remove <- function(string, from, to){
  first  <-  substr(string, 1, from - 1)
  second <-  substr(string, to + 1, nchar(string))
  tog    <<- paste0(first, second)
}

findOpenParen <- function(text, closePos) {
  openPos <- closePos  
  counter <- 1
  while (counter > 0) {
    openPos <- openPos - 1
    c <- substr(text, openPos, openPos)
    if (c == '(') {
      counter <- counter - 1
    }
    else if (c == ')') {
      counter <- counter +1
    }
  }
  x <<- openPos
}

while(grepl("\\|\\)",t)){
  fr <- unlist(gregexpr("\\|\\)", t))[1] + 1
  findOpenParen(t, fr)
  str_remove(t, x, fr)
  t <- tog
}

i <- 0
while(1 == 1){
  if(i == 0){
    x1 <- unlist(gregexpr("\\|", t))
    x2 <- unlist(gregexpr("\\(", t))
    x3 <- unlist(gregexpr("\\)", t))
  }
  if(x1[1] == -1){ break }
  i  <- i + 1
  c1 <- x1[i]
  c2 <- max(x2[which(x2 < c1)])
  c3 <- min(x2[which(x2 > c1)])
  c4 <- max(x3[which(x3 < c1)])
  c5 <- min(x3[which(x3 > c1)])
  if((c2 > c4) & (c5 < c3)){
    j    <- substr(t, c2 + 1, c5 - 1)
    s    <- unlist(strsplit(j, '\\|'))
    s1   <- nchar(s)
    smax <- which(s1 == max(s1))[1]
    t    <- paste0(substr(t, 1, c2 - 1), s[smax], substr(t, c5 + 1, nchar(t)))
    i    <- 0
  }else{
    #next iteration
  }
}
cat("result: ", nchar(t) - 6)
