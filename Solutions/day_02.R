# 2.1 ----
t     <- read.table("input_day_2.txt")
two   <- 0
three <- 0
for(i in 1:nrow(t)){
  char <- unlist(strsplit(as.character(t$V1[i]), ""))
  tb   <- table(char)
  if(2 %in% tb){two   <- two + 1}
  if(3 %in% tb){three <- three + 1}
}
two*three


# 2.2 ----
t <- read.table("input_day_2.txt")
for(i in 1:nrow(t)){
  ch1 <- unlist(strsplit(as.character(t$V1[i]),""))
  for(j in (i + 1):nrow(t)){
    ch2 <- unlist(strsplit(as.character(t$V1[j]),""))
    df  <- sum(ch1!=ch2)
    if(df == 1){
      break
    }
  }
  if(df == 1){
    tg <- paste(ch1[ch1==ch2],collapse = "")
    cat(i," ",j," ",tg)
    break
  }
}
