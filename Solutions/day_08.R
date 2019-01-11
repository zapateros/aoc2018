# day 8.1
tt <- readLines("input_day_8.txt")
t  <- as.numeric(unlist(strsplit(tt, " ")))

sum_tot <- 0
while(length(t) > 0){
  zr       <- which(t == 0)
  n        <- zr[1]
  n2       <- t[zr[1] + 1]
  all      <- c(n:(n+n2 + 1))
  all_sum  <- all[-c(1, 2)]
  sm       <- sum(t[all_sum])
  sum_tot  <- sum_tot + sm
  t[n - 2] <- t[n - 2] - 1
  t        <- t[-all]
}
sum_tot


#day 8.2
# this one really kept me up at night. I tried several possible solutions, worked out many inputs by hand but none of my solutions
# seemed to work with the given input. I literally had written about 400 lines of code, until I had a eurika-moment and wrote 
# down the solution in about 15 lines in maybe 5 minutes. It was like the solution already was written out somewhere in my head

tt <- readLines("input_day_8.txt")
t  <- unlist(strsplit(tt, " "))
while(1 == 1){
  n0 <- min(which(substr(t, 1, 1) == "0")) #n first character is a 0 
  if(grepl("\\(", t[n0])){
    brs        <- as.numeric(t[c((n0 + 2):(n0 + 1 + as.numeric(t[n0 + 1])))])                        #what branches to sum
    brs_vals   <- as.numeric(regmatches(t[n0], gregexpr("(?<=\\().*?(?=\\))", t[n0], perl=T))[[1]])  #branch values
    brs_vals_z <- c(brs_vals, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)               #add some zeros, ugly
    sum_br     <-sum(brs_vals_z[brs])                                                                #calc the sum of rel br
    if(n0 == 1){ break }  #if root, stop
  }else{
    sum_br <- sum(as.numeric(t[c((n0 + 2):(n0 + 1 + as.numeric(t[n0 + 1])))])) # sum relevant numbers
  }
  t[n0 - 2]               <- paste0(t[n0 - 2], "(", sum_br, ")")               # add sum to t[n0-2]
  substr(t[n0 - 2], 1, 1) <- toString(as.numeric(substr(t[n0 - 2], 1, 1)) - 1) # substract 1 from first number of t[n0-2]
  t                       <- t[-c((n0):(n0 + 1 + as.numeric(t[n0 + 1])))]      # remove the branch from t
}
sum_br
