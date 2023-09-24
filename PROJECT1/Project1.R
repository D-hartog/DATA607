library(tidyverse)
setwd("/Users/dirkhartog/Desktop/CUNY_MSDS/DATA_607/Project1")
chess_txt <- read.csv("chess.txt")

pattern <- "\\w+\\s?\\w+"
chess_txt
#player <- unlist(str_match_all(chess_txt[seq(4,3),], "\\w+\\s?\\w+"))
#player[1]

#chess_txt[seq(4,195,3),]

#chess_txt[seq(5,195,3),]
#data <- tibble(chess_txt[seq(4,195,3),])

#colnames(data) = c("Some Stuff")
#data


# ----------------------
# Use bind_cols to separate out the rows into columns 
data2 <- bind_cols(chess_txt[seq(4,195,3),], chess_txt[seq(5,195,3),])

# Rename the two columns
colnames(data2) <- c("top_row", "bottom_row")

#player <- unlist(str_match_all(chess_txt[seq(4,3),], "\\w+\\s?\\w+"))
#str_match(data2$top_row, "\\w+\\s\\w+\\s?\\w+\\s?\\w+")
#print(data2, n = 64)
#str_match(data2$top_row, "\\w+\\s?\\w+\\s?\\w+\\s?\\w+\\s?.\\w+")

# Separate each column by "|" to create new columns
data2 <- data2 %>%  
  separate_wider_delim(
    top_row,
    delim = "|",
    names = c("num", "player_name", "total_points"), 
    too_many = "debug"
  )

# Remove unneccessary columns
data2$num <- NULL
data2$top_row <- NULL
data2$top_row_ok <-  NULL
data2$top_row_pieces <- NULL

# Separate each column in bottom_row by "|" to create new columns
data2 <- data2 %>%  
  separate_wider_delim(
    bottom_row,
    delim = "|",
    names = c("state", "pre_rank"), 
    too_many = "debug"
  )

# Remove unneccessary columns 
data2$bottom_row <- NULL
data2$bottom_row_ok <- NULL
data2$bottom_row_remainder <- NULL
data2$bottom_row_pieces <- NULL

# Find the pre - rank of each player
# Parse pre - rank column to pull out only the pre rank
rank_match_list <- str_match_all(data2$pre_rank, "\\d{3,4}")
rank_match_list
p_rank <- c()
for (i in 1:64){
  player_pr <- rank_match_list[[i]][3,1]
  p_rank <- append(p_rank, player_pr)
}

# Assign the pre - ranks to a new column
data2$pre_rank <- p_rank             

# Finding the average pre ranks of each players opponents 

# Parse out the opponents from the "top_row_remainder" column
opponents_list <- str_match_all(data2$top_row_remainder, "\\d{1,2}")
opponents <- opponents_list[[1]]

# Find the pre-ranks of each players opponents
# practice
opponents[1:length(opponents),1]
opp_pr <- data2[opponents[1:length(opponents),1],"pre_rank"]
opp_pr_vec <- as.numeric(unlist(opp_pr))

# Find the pre-ranks of each players opponents
# Take the list of opponents 
opp_pre_rank <- c()

for(i in c(1:64)){
  opponents <- opponents_list[[i]]
  opponents_pr <- data2[opponents[1:length(opponents),1],"pre_rank"]
  opponents_pr_num <- as.numeric(unlist(opponents_pr))
  avg_opp_rank <- mean(opponents_pr_num)
  opp_pre_rank <- append(opp_pre_rank,avg_opp_rank)
}

#opp_pre_rank

data2$opponent_avgpr <- round(opp_pre_rank)
data2$top_row_remainder <- NULL

# RE-ORDER THE COLUMNS
data2 <- data2[,c(1,3,2,4,5)]

data2$player_name <- str_trim(data2$player_name)
data2$state <- str_trim(data2$state)
data2$total_points <- str_trim(data2$total_points)
data2$total_points <- as.numeric(data2$total_points)
data2$pre_rank <- as.numeric(data2$pre_rank)
data2

