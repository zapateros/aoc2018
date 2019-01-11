# 1.1 ----
t <- read.table("input_day_1.txt")
sum(t)

# 1.2 ----
t   <- read.table("input_day_1.txt")
rp  <- cumsum(rep(t(t), 500))
dup <- which(duplicated(rp) == TRUE)
rp[min(dup)]
