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





library(igraph)
tt<-readLines("input_day_20.txt")


t<-unlist(strsplit(tt,""))

# N E S W
N<-c(-1,0)
E<-c(0,1)
S<-c(1,0)
W<-c(0,-1)

pt<-c(0,0)
from<-NULL
to<-NULL
stpt<-NULL

for(i in 1:length(t)){
  
  if(t[i]=="E" | t[i]=="N" | t[i]=="S" | t[i]=="W"){
    
    from<-rbind(from,pt)
    pt<-pt+eval(parse(text=t[i]))
    to<-rbind(to,pt)
    
  }else if(t[i]=="("){
    
    stpt<-rbind(stpt,pt)
    
  }else if(t[i]=="|"){
    nn<-length(stpt)/2
    if(nn>1){
      pt<-stpt[nn,]
    }else{
      pt<-stpt
    }
    
  }else if(t[i]==")"){
    nn<-length(stpt)/2
    n<-nn-1
    if(nn>1){
      pt<-stpt[nn,]
      stpt<-stpt[0:n,]
    }else{
      pt<-stpt
      stpt<-NULL
    }
    
  }

}

j<-NULL
for(i in 1:nrow(from)){
  xx<-paste(from[i,1],from[i,2],sep=",")
  j<-c(j,xx)
}

d<-NULL
for(i in 1:nrow(to)){
  xx<-paste(to[i,1],to[i,2],sep=",")
  d<-c(d,xx)
}


un<-unique(c(j,d))

smn<-cbind(j,d)
t1<-graph_from_edgelist(smn,directed = FALSE)

all<-NULL
hlp<-NULL
uu<-0
for(i in 1:length(un)){
  uu<-uu+1
  el<-all_shortest_paths(t1,from = "0,0",to = un[i])
  am<-length(el$res[[1]])-1
  all<-c(all,am)
  cat(uu,"\n")
}



