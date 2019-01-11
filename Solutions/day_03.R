# 3.1 ----
t  <- read.csv("input_day_3.csv", header = FALSE)
v3 <- as.character(t$V3)
v3 <- gsub(":", "", v3)
v4 <- as.character(t$V4)
j  <- sapply(strsplit(v3, ","),function(x){x})
jj <- sapply(strsplit(v4, "x"),function(x){x})

fr  <-as.numeric(j[1,])
sc  <-as.numeric(j[2,])
fr1 <-as.numeric(jj[1,])
sc1 <-as.numeric(jj[2,])

smn <- NULL
for(j in 1:length(fr)){
  one <- c((fr[j]):(fr[j]+fr1[j]-1))
  two <- c((sc[j]):(sc[j]+sc1[j]-1))  
  tg  <- NULL
  for(i in 1:length(one)){
  tog <- paste(one[i], two, sep="-")
  tg  <- c(tg, tog)
  }
  smn <- c(smn, tg)
}

tb  <- table(smn)
tbn <- as.numeric(tb)
length(tbn[which(tbn > 1)])

# 3.2 ----
# run 3.1 first
cnt <- NULL
for(k in 1:length(fr)){
  one <- c((fr[k]):(fr[k] + fr1[k]-1))
  two <- c((sc[k]):(sc[k] + sc1[k]-1))
  tg  <- NULL
  for(i in 1:length(one)){
    tog <- paste(one[i], two, sep="-")
    tg  <- c(tg, tog)
  }
  am  <- sum(smn %in% tg)
  lng <- length(tg)
  dif <- am-lng
  cnt <- c(cnt, dif)
}
which(cnt == 0)
