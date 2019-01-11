# I really liked this one. After being warned this problem was pure hell, I decided to tackle this problem very systematically. 
# I have to say: I actually didn't encounter any difficulties; maybe because of my systematical approach. 

# ROUNDS
# 1. Combat proceeds in rounds
# 2. in each round, each unit that is still alive takes a turn, 
#    resolving all of its actions before the next unit's turn begins
# 3. On each unit's turn, it tries to move into range of an enemy (if it isn't already) 
#    AND!!!!!!! then attack (if it is in range).
#
# RULES
# 1. Units never move or attack diagonally
# 2. When multiple choices are equally valid, ties are broken in reading order: 
#    top-to-bottom, then left-to-right
# 3. Units cannot move into walls or other units
# 4. Each unit, either Goblin or Elf, has 3 attack power and starts with 200 hit points.

# STEPS
# 1. Each unit begins its turn by identifying all possible targets (enemy units).
# 2. the unit identifies all of the open squares (.) that are in range of each target
# 3. If the unit is not already in range of a target, 
#    and there are no open squares which are in range of a target, the unit ends its turn
# 4. If the unit is already in range of a target, it does not move, but continues its turn with an attack

# MOVE
# 1. the unit first considers the squares that are in range and 
#    determines which of those squares it could reach in the fewest steps
# 2. The unit moves while considering the current positions of units and does not do any prediction
# 3. If the unit cannot reach (find an open path to) any of the squares that are in range, it ends its turn.
# 4. If multiple squares are in range and tied for being reachable in the fewest steps, 
#    the square which is first in reading order is chosen
# 5. The unit then takes a single step toward the chosen square along the shortest path to that square
# 6. If multiple steps would put the unit equally closer to its destination,
#    the unit chooses the step which is first in reading order
# 7. if in range of enemy: ATTACK

# ATTACK
# 1. the unit first determines all of the targets that are in range of it by being immediately adjacent to it
# 2. the adjacent target with the fewest hit points is selected;
#    in a tie, the adjacent target with the fewest hit points which is first in reading order is selected
# 3. The unit deals damage equal to its attack power to the selected target, 
#    reducing its hit points by that amount
# 4. if hitpoints 0 or negative the target dies: its square becomes .

# END
# 1. Combat only ends when a unit finds no targets during its turn.
# 2. number of full rounds that were completed multiplied by 
#    the sum of the hit points of all remaining units at the moment combat ends


# FUNCTIONS
# 1. find adjacent ground pairs (non-directional). works with outer edges are walls
find_adj_gr_pairs <- function(vector){
  adj_pairs <- NULL
  for(i in 1:length(vector)){
    if(vector[i] == "."){
      nxt <- vector[i+1]
      if(nxt == "."){
        adj_pairs <- rbind(adj_pairs,c(i,i+1))
      }
      und <- vector[i+n]
      if(und == "."){
        adj_pairs <- rbind(adj_pairs,c(i,i+n))
      }
    }
  }
  adj_pairs <<- adj_pairs
}

# 2. find if player has an adjacent enemy
is_adjacent <- function(matrix){
  adjs<-NULL
  for(i in 1:nrow(matrix)){
    sort  <- matrix$player[i]
    p_loc <- matrix[!matrix$player==sort,1] 
    loc   <- matrix$loc[i]  
    if(any(p_loc == loc+1 | p_loc == loc-1 | p_loc == loc+n | p_loc == loc-n )){
      adj <- 1
    }else{
      adj <- 0
    }
    adjs <- c(adjs,adj)
  }
  adjs <<- adjs
}

# 3. find neighbours of point, vector is the vector with the relevant neighbours 
neighb_gr <- function(point,vector){
  nbs        <-  c(point - n, point - 1, point + 1, point + n)
  nbs        <-  nbs[nbs %in% vector]
  neighbours <<- nbs  
}

library(igraph)

tt<-"################################
#######..G######################
########.....###################
##########....############.....#
###########...#####..#####.....#
###########G..###GG....G.......#
##########.G#####G...#######..##
###########...G.#...############
#####.#####..........####....###
####.....###.........##.#....###
####.#................G....#####
####......#.................####
##....#G......#####........#####
########....G#######.......#####
########..G.#########.E...######
########....#########.....######
#######.....#########.....######
#######...G.#########....#######
#######...#.#########....#######
####.G.G.....#######...#.#######
##...#...G....#####E...#.#######
###..#.G.##...E....E.......###.#
######...................#....E#
#######...............E.########
#G###...#######....E...#########
#..##.######.E#.#.....##########
#..#....##......##.E...#########
#G......###.#..##......#########
#....#######....G....E.#########
#.##########..........##########
#############.###.......########
################################"

t <- gsub("\n","",tt)
t <- unlist(strsplit(t,""))
n <- 32

#create combat table
combat          <- NULL
combat$loc      <- which(t == "G" | t == "E")
combat$player   <- t[combat$loc]
combat$hp       <- 200
combat$power    <- 3
combat          <- as.data.frame(combat)
is_adjacent(combat)
combat$adjacent <- adjs

it <-0
i  <- 0
while(length(unique(combat$player)) > 1){  
  i <- i + 1
  if(i > nrow(combat)){
    i <- 1
    #new loop
    it <- it + 1
    #after for loop, reorder combat for correct fighting order
    combat <- combat[order(combat$loc),]
    if(round(it) == (it / 10)){
      cat(it, ": ", combat$hp)
    }
  }
  ground <- which(t == ".")
  
  #MOVING PHASE
  if(combat$adjacent[i] == 0){   
    #calculate possible startpoints. If none, next unit
    rel_point    <- combat$loc[i]
    neighb_gr(rel_point, ground)
    start_points <- neighbours
    if(length(start_points) > 0){
      
      #calculate possible endpoints. If none, next unit
      rel_player <- combat$player[i]
      rel_c      <- combat[which(combat$player != rel_player),]
      end_points <- NULL
      for(j in 1:nrow(rel_c)){
        neighb_gr(rel_c$loc[j], ground)
        end_points <- c(end_points, neighbours)
      }
      
      if(length(end_points) > 0){
        end_points <- unique(end_points)
        #make adjacency list
        find_adj_gr_pairs(t)
        #if there is just one dot between two enemies, add to adjacent pairs
        if(any(start_points %in% end_points)){
          extra_pair_point <- min(start_points[start_points %in% end_points])
          adj_pairs        <- rbind(adj_pairs, c(rel_point, extra_pair_point))
        }
        edge_list <- graph_from_edgelist(adj_pairs, directed = FALSE)
        
        #create a list of all shortest paths from every start- to endpoint
        s_paths <- NULL
        for(k in 1:length(start_points)){         
            #making sure end- and start points are in adjacency pairs
            if(any(end_points %in% adj_pairs) & (start_points[k] %in% adj_pairs)){
              sps     <- shortest_paths(edge_list, start_points[k], end_points)
              s_paths <- c(s_paths, sps$vpath)  
          }  
        }
        
        if(length(unlist(s_paths)) == 0){ next }else{
          #choose the shortest path, with lowest endpoint and lowest starting point (in that order)
          if(length(s_paths) > 1){
            length_paths <- sapply(s_paths, function(x){length(x)})
            s_paths      <- s_paths[length_paths > 0]
            length_paths <- sapply(s_paths, function(x){length(x)})
            min_lngth    <- min(length_paths)
            w_s_paths    <- which(length_paths == min_lngth)
            s_paths      <- s_paths[w_s_paths]
          }
          if(length(s_paths) > 1){
            s_paths_ends <- sapply(s_paths, function(x){x[min_lngth]})
            min_end      <- min(s_paths_ends)
            w_min_end    <- which(s_paths_ends == min_end)
            s_paths      <- s_paths[w_min_end]
          }
          s_paths_starts <- sapply(s_paths, function(x){x[1]})
          min_start      <- min(s_paths_starts)
          #w_min_start   <- which(s_paths_starts == min_start)
          #s_paths       <- s_paths[w_min_start]
         
          #move the relevant player in both t and combat
          t[rel_point]  <- "."
          t[min_start]  <- as.character(rel_player)
          combat$loc[i] <- min_start
        }        
      }else{
        next
      }
      #calculate if adjacent to enemy again
      is_adjacent(combat)
      combat$adjacent <- adjs
    }else{
      next
    }
 }
   
  #ATTACKING PHASE 
  if(combat$adjacent[i] == 1){
    rel_point    <- combat$loc[i]
    rel_player   <- combat$player[i]
    attack_power <- combat$power[i]
    
    #find the relevant enemy: order on hp and location
    rel_c           <- combat[which(combat$player != rel_player),]
    nbs             <- c(rel_point - n, rel_point - 1, rel_point + 1, rel_point + n)
    nbs_enemies     <- rel_c[rel_c$loc %in% nbs,]
    nbs_enemies_ord <- nbs_enemies[order(nbs_enemies$hp, nbs_enemies$loc),]
    enemy           <- row.names(nbs_enemies_ord[1,])
    
    #attack the enemy
    enemy_index            <- which(rownames(combat) == enemy)
    enemy_hp               <- combat$hp[enemy_index]
    combat$hp[enemy_index] <- enemy_hp - attack_power 
    
    #remove dead players
    if(combat$hp[enemy_index] < 1){
      en_ind <- which(combat$hp < 1)
      if(en_ind < i){ 
        i <- i - 1
      }
      enemy_point    <- combat$loc[enemy_index]
      combat         <- combat[combat$hp > 0 ,]
      t[enemy_point] <- "."
      is_adjacent(combat)
      combat$adjacent <- adjs
    }
  }
  combat
}

it * sum(combat$hp)
