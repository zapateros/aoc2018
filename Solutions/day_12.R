# 12.1
tt1 <- "#..#. => .
..#.. => .
..#.# => #
##.#. => .
.#... => #
#.... => .
##### => #
.#.## => .
#.#.. => .
#.### => #
.##.. => #
##... => .
#...# => #
####. => #
#.#.# => .
#..## => .
.#### => .
...## => .
..### => #
.#..# => .
##..# => #
.#.#. => .
..##. => .
###.. => .
###.# => #
#.##. => #
..... => .
.##.# => #
....# => .
##.## => #
...#. => #
.###. => ."

t1  <- unlist(strsplit(tt1,"\\n"))
tt  <- "..##.#######...##.###...#..#.#.#..#.##.#.##....####..........#..#.######..####.#.#..###.##..##..#..#"
t   <- paste0("...", tt, ".....")
zr  <- -3
ip  <- substr(t1, 1, 5)
rp  <- substr(t1, 10, 10)
nw  <- t
cnt <- 0
for(j in 1:20){
  t <- nw
  for(i in 1:(nchar(t)-5)){
    rl <- substr(t, 0 + i, 4 + i)
    if(sum(rl == ip) > 0){
      ad <- rp[rl == ip]
      substr(nw, i + 2, i + 2) <- ad
    }else{
      substr(nw, i + 2, i + 2) <- "."
    }
  }
  fo  <- nchar(nw)
  af  <- nchar(gsub("#", "", nw))
  cnt <- cnt + fo - af
  fsd <- unlist(gregexpr("#", nw))[1]
  if(fsd < 6){
    nw <- paste0(paste(rep(".", 6 - fsd), collapse = ""), nw)
    zr <- zr + fsd - 6
  }
  if(fsd > 6){
    nw <- substr(nw, fsd - 5, nchar(nw))
    zr <- zr + fsd - 6
  }
  lsd  <- regexpr("\\#[^\\#]*$", nw)[1]
  lsd1 <- nchar(nw) - lsd
  if(lsd1 < 5){
    nw <- paste0(nw, paste(rep(".", 5 - lsd1), collapse = ""))
  }
}
sum(unlist(gregexpr("#", nw)) + zr - 1)

#12.2
tt1 <- "#..#. => .
..#.. => .
..#.# => #
##.#. => .
.#... => #
#.... => .
##### => #
.#.## => .
#.#.. => .
#.### => #
.##.. => #
##... => .
#...# => #
####. => #
#.#.# => .
#..## => .
.#### => .
...## => .
..### => #
.#..# => .
##..# => #
.#.#. => .
..##. => .
###.. => .
###.# => #
#.##. => #
..... => .
.##.# => #
....# => .
##.## => #
...#. => #
.###. => ."

t1     <- unlist(strsplit(tt1, "\\n"))
tt     <- "..##.#######...##.###...#..#.#.#..#.##.#.##....####..........#..#.######..####.#.#..###.##..##..#..#"
t      <- paste0("...", tt, ".....")
zr     <- -3
ip     <- substr(t1, 1, 5)
rp     <- substr(t1, 10, 10)
nw     <- t
cnt    <- 0
pat    <- NULL
smm    <- NULL
cnt_df <- NULL
ii     <- 0
while(1 == 1){
  ii <- ii + 1
  t  <- nw
  for(i in 1:(nchar(t) - 5)){
    rl <- substr(t, 0 + i, 4 + i)
    if(sum(rl == ip) > 0){
      ad <- rp[rl == ip]
      substr(nw, i + 2, i + 2) <- ad
    }else{
      substr(nw, i + 2, i + 2) <- "."
    }
  }
  fo     <- nchar(nw)
  af     <- nchar(gsub("#", "", nw))
  cnt    <- cnt + fo - af
  cnt_df <- c(cnt_df, fo - af)
  fsd    <- unlist(gregexpr("#", nw))[1]
  if(fsd < 6){
    nw <- paste0(paste(rep(".", 6 - fsd), collapse = ""), nw)
    zr <- zr + fsd - 6
  }
  if(fsd > 6){
    nw <- substr(nw, fsd - 5, nchar(nw))
    zr <- zr + fsd - 6
  }
  lsd  <- regexpr("\\#[^\\#]*$", nw)[1]
  lsd1 <- nchar(nw) - lsd
  if(lsd1 < 5){
    nw <- paste0(nw, paste(rep(".", 5 - lsd1), collapse = ""))
  }
  pat <- c(pat, nw)
  smm <- c(smm, sum(unlist(gregexpr("#", nw)) + zr - 1))
  if(sum(duplicated(pat)) > 1){break}
}

n <- 50000000000 #input 12.2
difference <- smm[duplicated(pat)][2] - smm[duplicated(pat)][1]
format((n - ii + 2) * difference + (smm[ii - 2]), scientific = F)
