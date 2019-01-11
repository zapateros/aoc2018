#7.1
tt<-read.table("input_day_7.txt")
t<-cbind(as.character(tt$V2),as.character(tt$V8))
s<-NULL
while(1 == 1){
  # which available
  #rest
  s_in_t1<-unlist(lapply(s,function(x){which(t[,1]==x)}))
  if(length(s_in_t1)==0){
    rest<-t
  }else{
    rest<-t[-s_in_t1,]
  }
  
  #first check if it's the last possible row
  if(length(nrow(rest))==0){
    s<-c(s,rest)
    break
  }else{
  #done
  if(length(s_in_t1)==0){
    done<-NULL
  }else{
    done<-t[s_in_t1,]
  }
  
  #a_rest not in brest
  ar_nin_br<-unlist(lapply(rest[,1],function(x)!{x %in% rest[,2]}))
  arest<-rest[ar_nin_br,1]    
  
  #b_done not in s
  bd_in_s<-unlist(lapply(done[,2],function(x)!{x %in% s}))
  if(length(bd_in_s)==0){
    bdone<-NULL
  }else{
    bdone<-done[bd_in_s,2]
  }
  
  #b_done not in b_rest
  bd_nin_br<-unlist(lapply(bdone,function(x)!{x %in% rest[,2]}))
  bdone<-bdone[bd_nin_br]
  if(length(bdone)==0){
    bdone<-NULL
  }
  
  #combine to form possible steps
  psb<-c(arest,bdone)
  psb_un<-unique(psb)
  psb_ord<-psb_un[order(psb_un)]
  psb_first<-psb_ord[1]
  
  #add to string s
  s<-c(s,psb_first)
  }
}
paste(s,collapse="")




#7.2
#I hardcoded the extra players to check if it worked. Haven't set my mind to working out a generic solution yet 
tt<-read.table("input_day_7.txt")
t<-cbind(as.character(tt$V2),as.character(tt$V8))

s<-NULL
one<-c(".")
two<-c(".")
three<-c(".")
four<-c(".")
five<-c(".")
s_one<-c(".")
s_two<-c(".")
s_three<-c(".")
s_four<-c(".")
s_five<-c(".")
i<-0
while(length(s)<26){
  i<-i+1
  if(length(one)<i){
    s_one<-unique(one)
    one[i]<-"."
  }
  if(length(two)<i){
    s_two<-unique(two)
    two[i]<-"."
  }
  if(length(three)<i){
    s_three<-unique(three)
    three[i]<-"."
  }
  if(length(four)<i){
    s_four<-unique(four)
    four[i]<-"."
  }
  if(length(five)<i){
    s_five<-unique(five)
    five[i]<-"."
  }
  
  #find s
  s_tog<-c(s_one,s_two,s_three,s_four,s_five)
  s_tog_dot<-s_tog[-which(s_tog==".")]
  if(length(s_tog_dot)>0){
    s<-s_tog_dot
  }  
  # which available
  #rest
  s_in_t1<-unlist(lapply(s,function(x){which(t[,1]==x)}))
  if(length(s_in_t1)==0){
    rest<-t
  }else{
    rest<-t[-s_in_t1,]
  }
  
  #first check if it's the last possible row
  if(length(nrow(rest))>0){
    
    
    
    #done
    if(length(s_in_t1)==0){
      done<-NULL
    }else{
      done<-t[s_in_t1,]
    }
    
    #a_rest not in brest
    ar_nin_br<-unlist(lapply(rest[,1],function(x)!{x %in% rest[,2]}))
    arest<-rest[ar_nin_br,1]    
    
    #b_done not in s
    bd_in_s<-unlist(lapply(done[,2],function(x)!{x %in% s}))
    if(length(bd_in_s)==0){
      bdone<-NULL
    }else{
      bdone<-done[bd_in_s,2]
    }
    
    #b_done not in b_rest
    bd_nin_br<-unlist(lapply(bdone,function(x)!{x %in% rest[,2]}))
    bdone<-bdone[bd_nin_br]
    if(length(bdone)==0){
      bdone<-NULL
    }
    
    #combine to form possible steps
    psb<-c(arest,bdone)
    psb_un<-unique(psb)
    psb_ord<-psb_un[order(psb_un)]
    
  }else{
    #check if already in s
    if(!(rest[2] %in% s)){
      rest<-rest[1]
    }
    
    psb_ord<-rest
    
  }
  
  #check if step is being handled
  handled<-cbind(one[i],two[i],three[i],four[i],five[i])
  psb_in_hnd<-unlist(lapply(psb_ord,function(x)!{x %in% handled}))
  psb_fr<-psb_ord[psb_in_hnd]
  
  if(length(psb_fr)>0){
    
    #insert in players
    c_psb<-length(psb_fr)
    j<-1
    if(one[i]=="." & j<=c_psb){
      one[i]<-psb_fr[j]
      one<-c(one,rep(psb_fr[j],which(LETTERS==psb_fr[j])+59))
      j<-j+1
    }
    if(two[i]=="." & j<=c_psb){
      two[i]<-psb_fr[j]
      two<-c(two,rep(psb_fr[j],which(LETTERS==psb_fr[j])+59))
      j<-j+1
    }
    if(three[i]=="." & j<=c_psb){
      three[i]<-psb_fr[j]
      three<-c(three,rep(psb_fr[j],which(LETTERS==psb_fr[j])+59))
      j<-j+1
    }
    if(four[i]=="." & j<=c_psb){
      four[i]<-psb_fr[j]
      four<-c(four,rep(psb_fr[j],which(LETTERS==psb_fr[j])+59))
      j<-j+1
    }
    if(five[i]=="." & j<=c_psb){
      five[i]<-psb_fr[j]
      five<-c(five,rep(psb_fr[j],which(LETTERS==psb_fr[j])+59))
      j<-j+1
    }
  }
}
length(one)-1

