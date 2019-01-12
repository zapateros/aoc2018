# Both 14.1 and 14.2 are written in R. However, 14.2 is taking very long, so there is also a solution in Python posted

# 14.1
calc_index <- function(vector, number){
    x <- number %% length(vector)
    if(x == 0){
      x <- length(vector)
    }
    x
 }

num_split <- function(number){
  as.numeric(strsplit(as.character(number), "")[[1]])
}

calc_index_char <- function(vector, number){
    x <- number %% nchar(vector)
    if(x == 0){
      x <- nchar(vector)
    }
    x
}
    
#start
rec   <- c(3, 7)
r1    <- 1  
r2    <- 2  
input <- 157901

#loop through recipes and stop if length(rec) > (10 + input)
while(1 == 1){
r1_val <- rec[r1] + 1 + r1
r2_val <- rec[r2] + 1 + r2
nx     <- rec[r1] + rec[r2]  
rec    <- c(rec, num_split(nx))  
r1     <- calc_index(rec, r1_val)
r2     <- calc_index(rec, r2_val)
if(length(rec) > (10 + input)){ break }
}

ans <- rec[c((input + 1):(10 + input))]
paste(ans, collapse = "")

# 14.2
rec   <- "37"
r1    <- 1  
r2    <- 2  
input <- "157901"
while(length(grep(input, substr(rec, nchar(rec) - 7, nchar(rec)))) == 0){
r1_val <- as.numeric(substr(rec, r1, r1)) + 1 + r1
r2_val <- as.numeric(substr(rec, r2, r2)) + 1 + r2
nx     <- paste0(as.numeric(substr(rec, r1, r1)) + as.numeric(substr(rec, r2, r2)))  
rec    <- paste0(rec, nx)  
r1     <- calc_index_char(rec, r1_val)
r2     <- calc_index_char(rec, r2_val)
}
  
grepl(input, rec)
