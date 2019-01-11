# 4.1 ----
t        <- readLines("input_day_4.txt")
tx       <- substring(t, 2)
tx1      <- t(sapply(strsplit(tx, "] "), function(x){x}))
tx1[, 2] <- gsub("Guard ", "", tx1[, 2])
tx1[, 2] <- gsub(" begins shift", "", tx1[, 2])
tx1[, 1] <- strftime(tx1[, 1], "%y-%m-%d %H:%M")
ord      <- tx1[order(tx1[, 1]),]
tm       <- substr(ord[, 1], nchar(ord[, 1])-1, nchar(ord[, 1]))
tg       <- cbind(ord, tm)
hs       <- grep("#", tg[, 2])
hse      <- c(hs, (nrow(tg)+1))

all <- NULL
for(i in 1:(length(hse)-1)){
  if((hse[i+1]-hse[i]) > 1){
    rel <- tg[c((hse[i]):(hse[i+1]-1)),]
    f   <- as.numeric(rel[which(rel[, 2] == "falls asleep"), 3])
    w   <- as.numeric(rel[which(rel[, 2] == "wakes up"), 3])-1
    x   <- paste(f, w, sep = "-")
    ii  <- lapply(x, function(x){
    j   <- as.numeric(unlist(strsplit(x[1], "-")))
    c(j[1]:j[2])
    })
    
    sleep <- unlist(ii)
    slp   <- paste(rel[1, 2], sleep, sep = "-")
    all   <- c(all,slp)
  }
}

sp <- t(sapply(strsplit(all, "-"),function(x){x}))
uu <- table(sp[, 1])
id <- names(which(uu == max(uu)))
rl <- sp[which(sp[, 1] == id),]
kk <- table(rl[, 2])
mn <- names(which(kk == max(kk)))
as.numeric(substring(id, 2))*as.numeric(mn)

# 4.2 ----
# run 4.1 first
tba  <- table(all)
mxm  <- names(tail(tba[order(tba)], 1))
mxmm <- gsub("#", "", mxm)
m    <- strsplit(mxmm, "-")
mm   <- as.numeric(unlist(m))
prod(mm)
