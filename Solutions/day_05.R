# 5.1 ----
t   <- readLines("input_day_5.txt")
cmb <- c(paste0(letters, LETTERS), paste0(LETTERS, letters), "opt")

ref <- 0
while(ref == 0){
  occ <- sapply(cmb, function(x){
    regexpr(x, t)[1]
  })
  occ <- occ[-(which(occ == -1))]
  if(length(occ) > 0){
    ltr <- names(occ[occ == min(occ)])
    t   <- sub(ltr, "", t)
  }else{
    ref <- 1
  }
}
nchar(t)

# 5.2 ----
t   <- readLines("input_day_5.txt")
cmb <- c(paste0(letters, LETTERS), paste0(LETTERS, letters), "opt")
rst <- t

lng <- NULL
for(i in 1:length(letters)){
  t <- rst
  t <- gsub(lwr[i], "", t)
  t <- gsub(upr[i], "", t)
  
  ref<-0
  while(ref == 0){
    occ <- sapply(cmb, function(x){
      regexpr(x, t)[1]
    })
    occr <- occ[-(which(occ == -1))]
    if(length(occr) > 0){
      ltr <- names(occr[occr == min(occr)])
      t   <-sub(ltr, "", t)
    }else{
      ref <- 1
    }
  }
  ch  <- nchar(t)
  lng <- c(lng, ch)
}

min(lng)
