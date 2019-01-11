# This problem was purely straightforward implementing the rules and many, many copy-pastes. 
# Note to my future self: clean your script before you copy-paste

# day 16.1
tt <- readLines("input_day_16.txt")
t  <- matrix(tt, ncol = 4, byrow = TRUE)

#manipulate to usable data
before <- t[, 1]
instr  <- t[, 2]
after  <- t[, 3]
before <- gsub("Before: \\[","",before)
before <- gsub("\\]","",before)
bf     <- matrix(as.numeric(unlist(strsplit(before,", "))), ncol = 4, byrow = TRUE)
instr  <- matrix(as.numeric(unlist(strsplit(instr," "))), ncol = 4, byrow = TRUE)
after  <- gsub("After:  \\[", "", after)
after  <- gsub("\\]", "", after)
af     <- matrix(as.numeric(unlist(strsplit(after,", "))), ncol = 4, byrow = TRUE)
input  <- bf[i,]
ins    <- instr[i,]
output <- af[i,]

# 1addr (add register) stores into register C the result of adding register A and register B.
addr <- function(input, ins, output){
  x1 <- ins[2]
  x2 <- input[x1 + 1]
  x3 <- ins[3]
  x4 <- input[x3 + 1]
  x5 <- ins[4]
  input[x5 + 1] <- x2 + x4
  sum(input == output) == 4
}

# 2addi (add immediate) stores into register C the result of adding register A and value B.
addi <- function(input, ins, output){
  x1 <- ins[2]
  x2 <- input[x1 + 1]
  x4 <- ins[3]
  x5 <- ins[4]
  input[x5 + 1] <- x2 + x4
  sum(input == output) == 4
}

# 3mulr (multiply register) stores into register C the result of multiplying register A and register B.
mulr <- function(input, ins, output){
  x1 <- ins[2]
  x2 <- input[x1 + 1]
  x3 <- ins[3]
  x4 <- input[x3 + 1]
  x5 <- ins[4]
  input[x5 + 1] <- x2 * x4
  sum(input == output) == 4
}

# 4muli (multiply immediate) stores into register C the result of multiplying register A and value B.
muli <- function(input, ins, output){
  x1 <- ins[2]
  x2 <- input[x1 + 1]
  x4 <- ins[3]
  x5 <- ins[4]
  input[x5 + 1] <- x2 * x4
  sum(input == output) == 4
}

# 5banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
banr <- function(input, ins, output){
  x1 <- ins[2]
  x2 <- input[x1 + 1]
  x3 <- ins[3]
  x4 <- input[x3 + 1]
  x5 <- ins[4]
  input[x5 + 1] <- bitwAnd(x2, x4)
  sum(input == output) == 4
}

# 6bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
bani <- function(input, ins, output){
  x1 <-ins[2]
  x2 <-input[x1 + 1]
  x4 <-ins[3]
  x5 <-ins[4]
  input[x5 + 1] <- bitwAnd(x2, x4)
  sum(input == output) == 4
}

# 7borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
borr <- function(input, ins, output){
  x1 <- ins[2]
  x2 <- input[x1 + 1]
  x3 <- ins[3]
  x4 <- input[x3 + 1]
  x5 <- ins[4]
  input[x5 + 1] <- bitwOr(x2, x4)
  sum(input == output) == 4
}

# 8bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
bori <- function(input, ins, output){
  x1 <- ins[2]
  x2 <- input[x1 + 1]
  x4 <- ins[3]
  x5 <- ins[4]
  input[x5 + 1] <- bitwOr(x2, x4)
  sum(input == output) == 4
 }

# 9setr (set register) copies the contents of register A into register C. (Input B is ignored.)
setr <- function(input, ins, output){
  x1 <- ins[2]
  x2 <- input[x1 + 1]
  x5 <- ins[4]
  input[x5 + 1] <- x2
  sum(input == output) == 4
}

# 10seti (set immediate) stores value A into register C. (Input B is ignored.)
seti <- function(input, ins, output){
  x1 <- ins[2]
  x5 <- ins[4]
  input[x5 + 1] <- x1
  sum(input == output) == 4
}

# 11gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
gtir <- function(input, ins, output){
  x1 <-ins[2]
  x2 <-ins[3]
  x3 <-input[x2 + 1]
  x5 <-ins[4]
  if(x1 > x3){
    input[x5 + 1] <- 1
  }else{
    input[x5 + 1] <- 0
  }
  sum(input == output) == 4
}

# 12gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
gtri <- function(input, ins, output){
  x1 <-ins[2]
  x2 <-input[x1 + 1]
  x3 <-ins[3]
  x5 <-ins[4]
  if(x2 > x3){
    input[x5 + 1] <- 1
  }else{
    input[x5 + 1] <- 0
  }
  sum(input == output) == 4
}

# 13gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
gtrr <- function(input, ins, output){
  x1 <- ins[2]
  x2 <- input[x1 + 1]
  x3 <- ins[3]
  x4 <- input[x3 + 1]
  x5 <- ins[4]
  if(x2 > x4){
    input[x5 + 1] <- 1
  }else{
    input[x5 + 1] <- 0
  }
  sum(input == output) == 4
}

# 14eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
eqir <- function(input, ins, output){
  x1 <- ins[2]
  x2 <- ins[3]
  x3 <- input[x2 + 1]
  x5 <- ins[4]
  if(x1 == x3){
    input[x5 + 1] <- 1
  }else{
    input[x5 + 1] <- 0
  }
  sum(input == output) == 4
}

# 15eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
eqri <- function(input, ins, output){
  x1 <- ins[2]
  x2 <- input[x1 + 1]
  x3 <- ins[3]
  x5 <- ins[4]
  if(x2 == x3){
    input[x5 + 1] <- 1
  }else{
    input[x5 + 1] <- 0
  }
  sum(input == output) == 4
}

# 16eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.
eqrr <- function(input, ins, output){
  x1 <- ins[2]
  x2 <- input[x1 + 1]
  x3 <- ins[3]
  x4 <- input[x3 + 1]
  x5 <- ins[4]
  if(x2 == x4){
    input[x5 + 1] <- 1
  }else{
    input[x5 + 1] <- 0
  }
  sum(input == output) == 4
}

x6 <- NULL
for(i in 1:nrow(af)){
  x1 <- bf[i,]
  x2 <- instr[i,]
  x3 <- af[i,]
  x4 <- c(addr(x1,x2,x3),addi(x1,x2,x3),mulr(x1,x2,x3),muli(x1,x2,x3),banr(x1,x2,x3),bani(x1,x2,x3),borr(x1,x2,x3),bori(x1,x2,x3),
        setr(x1,x2,x3),seti(x1,x2,x3),gtir(x1,x2,x3),gtri(x1,x2,x3),gtrr(x1,x2,x3),eqir(x1,x2,x3),eqri(x1,x2,x3),eqrr(x1,x2,x3))
  x5 <- sum(x4)
  x6 <- c(x6, x5)
}
sum(x6 > 2)


#day 16.2
tfs    <- NULL
firsts <- NULL
for(i in 1:nrow(af)){
  x1 <- bf[i,]
  x2 <- instr[i,]
  x3 <- af[i,]
  x4 <- c(addr(x1,x2,x3),addi(x1,x2,x3),mulr(x1,x2,x3),muli(x1,x2,x3),banr(x1,x2,x3),bani(x1,x2,x3),borr(x1,x2,x3),bori(x1,x2,x3),
        setr(x1,x2,x3),seti(x1,x2,x3),gtir(x1,x2,x3),gtri(x1,x2,x3),gtrr(x1,x2,x3),eqir(x1,x2,x3),eqri(x1,x2,x3),eqrr(x1,x2,x3))
  x5     <- x2[1]
  firsts <- c(firsts, x5)
  tfs    <- rbind(tfs, x4)
}

tog <- cbind(firsts,tfs)
cal <- NULL
for(i in 0:15){
  rel <- tog[which(tog[, 1] == i), c(2:17)]
  n   <- nrow(rel)
  tr  <- colSums(rel) == n
  cal <- rbind(cal, tr)
}

jj <- NULL
for(i in 1:16){
  x         <- min(which(rowSums(cal) == 1))
  xx        <- which(cal[x,] == TRUE)
  j         <- c(x - 1, xx)
  jj        <- rbind(jj, j)
  cal[, xx] <- FALSE
}

ee    <- read.csv("input_day_16_2.txt", sep=" ")
e     <- as.matrix(ee)
see   <-NULL
input <- c(0, 1, 1, 0)
for(i in 1:nrow(ee)){
  
  ins<-e[i,]
  
  
  if(ins[1]==0){
    x1<-ins[2]
    x2<-input[x1+1]
    x3<-ins[3]
    x4<-input[x3+1]
    x5<-ins[4]
    input[x5+1]<-bitwOr(x2,x4)
  }else if(ins[1]==1){
    x1<-ins[2]
    x2<-input[x1+1]
    x3<-ins[3]
    x4<-input[x3+1]
    x5<-ins[4]
    input[x5+1]<-x2+x4
  }else if(ins[1]==2){
    x1<-ins[2]
    x2<-input[x1+1]
    
    x3<-ins[3]
    x4<-input[x3+1]
    
    x5<-ins[4]
    
    if(x2==x4){
      input[x5+1]<-1
    }else{
      input[x5+1]<-0
    }
  }else if(ins[1]==3){
    x1<-ins[2]
    x2<-input[x1+1]
    x4<-ins[3]
    x5<-ins[4]
    input[x5+1]<-x2+x4
  }else if(ins[1]==4){
    x1<-ins[2]
    x2<-input[x1+1]
    
    x3<-ins[3]
    
    x5<-ins[4]
    
    if(x2==x3){
      input[x5+1]<-1
    }else{
      input[x5+1]<-0
    }
  }else if(ins[1]==5){
    x1<-ins[2]
    x2<-ins[3]
    x3<-input[x2+1]
    x5<-ins[4]
    
    if(x1==x3){
      input[x5+1]<-1
    }else{
      input[x5+1]<-0
    }
  }else if(ins[1]==6){
    x1<-ins[2]
    x2<-input[x1+1]
    
    x3<-ins[3]
    
    x5<-ins[4]
    
    if(x2>x3){
      input[x5+1]<-1
    }else{
      input[x5+1]<-0
    }
  }else if(ins[1]==7){
    x1<-ins[2]
    x2<-input[x1+1]
    x3<-ins[3]
    x4<-input[x3+1]
    x5<-ins[4]
    input[x5+1]<-x2*x4
  }else if(ins[1]==8){
    x1<-ins[2]
    x2<-input[x1+1]
    
    x5<-ins[4]
    input[x5+1]<-x2
  }else if(ins[1]==9){
    x1<-ins[2]
    x2<-ins[3]
    x3<-input[x2+1]
    x5<-ins[4]
    
    if(x1>x3){
      input[x5+1]<-1
    }else{
      input[x5+1]<-0
    }
  }else if(ins[1]==10){
    x1<-ins[2]
    x2<-input[x1+1]
    x4<-ins[3]
    x5<-ins[4]
    input[x5+1]<-x2*x4
  }else if(ins[1]==11){
    x1<-ins[2]
    x2<-input[x1+1]
    x3<-ins[3]
    x4<-input[x3+1]
    x5<-ins[4]
    input[x5+1]<-bitwAnd(x2,x4)
    
  }else if(ins[1]==12){
    x1<-ins[2]
    
    x5<-ins[4]
    input[x5+1]<-x1
  }else if(ins[1]==13){
    x1<-ins[2]
    x2<-input[x1+1]
    
    x3<-ins[3]
    x4<-input[x3+1]
    
    x5<-ins[4]
    
    if(x2>x4){
      input[x5+1]<-1
    }else{
      input[x5+1]<-0
    }
  }else if(ins[1]==14){
    x1<-ins[2]
    x2<-input[x1+1]
    x4<-ins[3]
    x5<-ins[4]
    input[x5+1]<-bitwAnd(x2,x4)
  }else if(ins[1]==15){
    x1<-ins[2]
    x2<-input[x1+1]
    x4<-ins[3]
    x5<-ins[4]
    input[x5+1]<-bitwOr(x2,x4)
    
  }
  
  see<-rbind(see,input)
  
}

