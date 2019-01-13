# This one was really killing me. Actually the problem wasn't really hard, but it turned out my reading skills were lacking
# Every time I saw I had a wrong answer, I looked at the rules again and found another twist. Luckily I didn't need to change my
# whole method everytime. However, the code is probably a lot more messy than it should be because of all my additions.
# I copied the input by hand, as I thought this would be faster than writing code for it.
# A few mistakes I made: 
# - Chose wrong selection order because I thought the effective power was set after considering weaknesses
# - If an attacker chose a defender with immunity, both attacker and defender where removed, but it should only be the attacker

# For 24.1 set boost at 0. For 24.2 binary search with varying boost until you
boost <- 0

#types
#1 - bludgeoning
#2 - fire
#3 - slashing
#4 - cold
#5 - radiation

i1     <- c(1, 1, 197, 6697, 1, 2, 0, 312 + boost, 3, 3)
i2     <- c(2, 1, 3803, 8760, 1, 0, 0, 21 + boost, 3, 9)
i3     <- c(3, 1, 5279, 4712, 0, 0, 0, 8 + boost, 4, 7)
i4     <- c(4, 1, 3727, 11858, 3, 0, 0, 25 + boost, 4, 19)
i5     <- c(5, 1, 494, 3486, 5, 0, 1, 70 + boost, 4, 6)
i6     <- c(6, 1, 1700, 8138, 3, 0, 0, 41 + boost, 3, 18)
i7     <- c(7, 1, 251, 4061, 1, 0, 0, 157 + boost, 5, 15)
i8     <- c(8, 1, 87, 1699, 0, 0, 0, 161 + boost, 4, 11)
i9     <- c(9, 1, 1518, 9528, 4, 3, 0, 60 + boost, 3, 2)
i10    <- c(10, 1, 347, 6624, 1, 0, 2, 148 + boost, 3, 12)
immune <- rbind(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10)

i11       <-c(11, 2, 6929, 51693, 0, 0, 0, 13, 3, 5)
i12       <-c(12, 2, 1638, 32400, 1, 0, 0, 27, 1, 16)
i13       <-c(13, 2, 2311, 12377, 2, 0, 4, 9, 3, 8)
i14       <-c(14, 2, 685, 29080, 1, 2, 5, 57, 1, 10)
i15       <-c(15, 2, 1225, 7657, 3, 0, 0, 12, 4, 14)
i16       <-c(16, 2, 734, 52884, 0, 0, 0, 102, 1, 13)
i17       <-c(17, 2, 608, 49797, 5, 0, 3, 162, 1, 1)
i18       <-c(18, 2, 3434, 49977, 0, 0, 0, 28, 5, 4)
i19       <-c(19, 2, 1918, 14567, 3, 0, 0, 13, 1, 20)
i20       <-c(20, 2, 519, 18413, 0, 0, 3, 69, 2, 17)
infection <-rbind(i11, i12, i13, i14, i15, i16, i17, i18, i19, i20)

nms <- c("number", "group", "units", "hitpoints", "weakness_1", "weakness_2", "immune_1", "attack", "type", "initiative")
all <- setNames(as.data.frame(rbind(immune, infection)), nms)

#effective powers
attack_mat <- NULL
for(i in 1:nrow(all)){
  rel    <- all[i,]
  group  <- rel$group
  type   <- rel$type
  pos_op <- all[all$group != group,]
  att_t  <- rep(rel$units*rel$attack, nrow(pos_op))
  
  #weaknesses and immunity
  w1         <- pos_op$weakness_1 %in% type
  att_t[w1]  <- att_t[w1] * 2
  w2         <- pos_op$weakness_2 %in% type
  att_t[w2]  <- att_t[w2] * 2
  im1        <- pos_op$immune_1 %in% type
  att_t[im1] <- att_t[im1] * 0
  att1       <- cbind(rep(rel$number, nrow(pos_op)), rep(rel$units, nrow(pos_op)), rep(rel$initiative, nrow(pos_op)),
                att_t, pos_op$number, pos_op$initiative, pos_op$hitpoints, rep(rel$attack, nrow(pos_op)))
  att        <- setNames(as.data.frame(att1), c("attacker", "units", "attacker_in", "eff_power", "defender",
                                      "defender_in", "defender_hps", "attacker_attack"))
  attack_mat <- rbind(attack_mat, att)
}

while(1==1){
  attack_mat$attacker_in_eff <- attack_mat$units * attack_mat$attacker_attack
  
  #choose opponents
  #add max power of defender
  players <-as.numeric(names(table(attack_mat$attacker)))
  attack_mat$def_eff_power <- 0
  for(x in players){
    indices_1 <- which(attack_mat$attacker == x)
    attp      <- attack_mat[indices_1[1], 2] * attack_mat[indices_1[1], 8]
    indices_2 <- which(attack_mat$defender == x)
    attack_mat[indices_2,]$def_eff_power <- attp
  }
  
  #choose attack order and opponents
  attack_d  <- attack_mat
  opponents <- NULL
  while(nrow(attack_d) > 0){
    att_t <- attack_d[which(attack_d$attacker_in_eff == max(attack_d$attacker_in_eff)),]
        
    #if multiple attacking groups have max eff power, choose the one with highest initiative
    n_poss <- length(table(att_t$attacker))
    if(n_poss > 1){
      att_t <- att_t[which(att_t$attacker_in == max(att_t$attacker_in)),]
    }
    
    attacker_h <- att_t[1, 1]
    att_t      <- attack_d[which(attack_d$attacker == attacker_h),]
    att_t      <- att_t[rev(order(att_t$eff_power, att_t$def_eff_power, att_t$defender_in)),]
    att_t      <- att_t[1,]
    def_n      <- att_t$defender
    att_n      <- att_t$attacker
    if(att_t$eff_power == 0){
      attack_d <- attack_d[-which(attack_d$attacker == att_n),]
    }else{
      attack_d <- attack_d[-which(attack_d$attacker == att_n),]
        if(length(which(attack_d$defender == def_n)) > 0){
        attack_d <- attack_d[-which(attack_d$defender == def_n),]
      }
      opponents <- rbind(opponents, c(att_n, def_n))
    }
  }
  opponents <- setNames(as.data.frame(opponents), c("attacker", "defender"))
    
  #attack round
  #add column with start_units
  attack_mat$start_units <- attack_mat$units
  inis       <- attack_mat[attack_mat$attacker %in% opponents$attacker, 3]
  initiative <- rev(as.numeric(names(table(inis))))
  for(j in initiative){
    if(j %in% attack_mat$attacker_in){
      attacker         <- attack_mat[min(which(attack_mat$attacker_in == j)), 1]
      defender         <- opponents[opponents$attacker == attacker, 2]
      index            <- which((attack_mat$attacker == attacker)  & (attack_mat$defender == defender))
      attacker_power   <- attack_mat[index, 4]
      defender_hps     <- attack_mat[index, 7]
      attacker_units   <- attack_mat[index, 2]
      attacker_s_units <- attack_mat[index, 11]
      test             <- (attacker_units / attacker_s_units) * attacker_power
      kills            <- floor((attacker_units / attacker_s_units) * attacker_power / defender_hps)
      index_def        <- which(attack_mat$attacker == defender)
      attack_mat[index_def, 2] <- attack_mat[index_def, 2] - kills
      if(attack_mat[index_def[1], 2] < 1){
        attack_mat <- attack_mat[-index_def,]
      }
    }
  }
  rem_defenders <- attack_mat$defender %in% attack_mat$attacker
  if(sum(!rem_defenders) == nrow(attack_mat)){
    if(any(attack_mat$attacker %in% c(11, 12, 13, 14, 15, 16, 17, 18, 19, 20))){
      cat("Winner: Infection with ", sum(attack_mat$units), " units")
    }else{
      cat("Winner: Immune with ", sum(attack_mat$units), " units")
    }
    break}
  attack_mat <- attack_mat[attack_mat$defender %in% attack_mat$attacker,]
  attack_mat$eff_power <- attack_mat$units * attack_mat$eff_power / attack_mat$start_units
 }
