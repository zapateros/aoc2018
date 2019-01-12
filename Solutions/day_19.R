# For 19.2 you have to reverse engineer the input. See below for more information
#19.1
tt <- "addi 2 16 2
seti 1 2 4
seti 1 8 1
mulr 4 1 5
eqrr 5 3 5
addr 5 2 2
addi 2 1 2
addr 4 0 0
addi 1 1 1
gtrr 1 3 5
addr 2 5 2
seti 2 6 2
addi 4 1 4
gtrr 4 3 5
addr 5 2 2
seti 1 2 2
mulr 2 2 2
addi 3 2 3
mulr 3 3 3
mulr 2 3 3
muli 3 11 3
addi 5 2 5
mulr 5 2 5
addi 5 8 5
addr 3 5 3
addr 2 0 2
seti 0 4 2
setr 2 5 5
mulr 5 2 5
addr 2 5 5
mulr 2 5 5
muli 5 14 5
mulr 5 2 5
addr 3 5 3
seti 0 8 0
seti 0 5 2"

t1    <- gsub("\n", " ", tt)
t2    <- unlist(strsplit(t1, " "))
mt    <- matrix(t2, ncol = 4, byrow = T)
instr <- mt[, 1]
mat   <- matrix(as.numeric((mt[, 2:4])), ncol = 3)
input <- c(0, 0, 0, 0, 0, 0)
it    <- 0
js    <- NULL
rgs   <- NULL
while( 1 == 1){
  it          <- it + 1
  rgs         <- rbind(rgs, input)
  js          <- c(js, input[3])
  instruction <- instr[input[3] + 1]
  ins         <- mat[input[3] + 1,]
  if(instruction == "addi"){
    x1 <- ins[1]
    x2 <- input[x1 + 1]
    x4 <- ins[2]
    x5 <- ins[3]
    input[x5 + 1] <- x2 + x4
  }else if(instruction == "addr"){
    x1 <- ins[1]
    x2 <- input[x1 + 1]
    x3 <- ins[2]
    x4 <- input[x3 + 1]
    x5 <- ins[3]
    input[x5 + 1] <- x2 + x4
  }else if(instruction == "eqrr"){
    x1 <- ins[1]
    x2 <- input[x1 + 1]
    x3 <- ins[2]
    x4 <- input[x3 + 1]
    x5 <- ins[3]
    if(x2 == x4){
      input[x5 + 1] <- 1
    }else{
      input[x5 + 1] <- 0
    }
  }else if(instruction == "gtrr"){
    x1 <- ins[1]
    x2 <- input[x1 + 1]
    x3 <- ins[2]
    x4 <- input[x3 + 1]
    x5 <- ins[3]
    if(x2 > x4){
      input[x5 + 1] <- 1
    }else{
      input[x5 + 1] <- 0
    }
  }else if(instruction == "muli"){
    x1 <- ins[1]
    x2 <- input[x1 + 1]
    x4 <- ins[2]
    x5 <- ins[3]
    input[x5 + 1] <- x2 * x4
  }else if(instruction == "mulr"){
    x1 <- ins[1]
    x2 <- input[x1 + 1]
    x3 <- ins[2]
    x4 <- input[x3 + 1]
    x5 <- ins[3]
    input[x5 + 1] <- x2 * x4
  }else if(instruction == "seti"){
    x1 <- ins[1]
    x5 <- ins[3]
    input[x5 + 1] <- x1
  }else if(instruction == "setr"){
    x1 <- ins[1]
    x2 <- input[x1 + 1]
    x5 <- ins[3]
    input[x5 + 1] <- x2
  }
  input <- input + c(0, 0, 1, 0, 0, 0)
  if((input[3] + 1) > nrow(mat)){
    break
  }
}
result <- input[1]
result

# 19.2
# reverse engineer the instructions
# check the pattern
# see that the pattern sums all factors of 10551288
# 10551288 is the value f (input[6]) has to pass
# it is the value of d during the repetition pattern of ps (input[3])

FUN <- function(x) {
  x       <- as.integer(x)
  div     <- seq_len(abs(x))
  factors <- div[x %% div == 0L]
  factors <- list(pos = factors)
  return(factors)
}
sum(unlist(FUN(10551288)))
