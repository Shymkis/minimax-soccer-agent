remove(list = ls())

library(mdsr)
library(dplyr)
library(ggplot2)
library(scales)
library(readr)

game_statistics <- read_csv("School/15th Grade/Spring/Artificial Intelligence/Projects/Project 3/cs4253-master/game_statistics.csv")

game_statistics %>% 
  group_by(max_depth, pruning) %>% 
  summarize(n=n(), num_wins=sum(!is.na(winner)), avg_wins=num_wins/n,
            tot_time=sum(total_time), avg_time=tot_time/sum(num_moves))



# Number of games won per max_depth
game_statistics %>% 
  group_by(max_depth, pruning) %>% 
  summarize(n=n(), num_wins=sum(!is.na(winner)), avg_wins=num_wins/n) %>% 
  ggplot(aes(x=max_depth, y=avg_wins, color=pruning)) +
  labs(title="Proportion of Games Resulting in a Win at Each Max Depth", color="Pruning") +
  scale_x_continuous(name="Max Depth", breaks=seq(2,7), minor_breaks=NULL) +
  scale_y_continuous(name="Proportion of Wins") +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)

# Who won at each max_depth
game_statistics %>% filter(!is.na(winner)) %>% 
  ggplot(aes(x=max_depth, fill=winner)) +
  facet_wrap(~pruning, labeller=label_both) +
  labs(title="Proportion of Wins per Team at Each Max Depth", fill="Winner") +
  scale_x_continuous(name="Max Depth", breaks=seq(2,7), minor_breaks=NULL) +
  scale_y_continuous(name="Win Proportions") +
  scale_fill_manual(values=c("#00bfc4", "#f8766d")) +
  geom_bar(position="fill")

# Average time vs. max depth
game_statistics %>% 
  group_by(max_depth, pruning) %>% 
  summarize(n=n(), tot_time=sum(total_time), avg_time=tot_time/sum(num_moves)) %>% 
  ggplot(aes(x=max_depth, y=avg_time, color=pruning)) +
  labs(title="Average Time vs. Max Depth", color="Pruning") +
  scale_x_continuous(name="Max Depth", breaks=seq(2,7), minor_breaks=NULL) +
  scale_y_continuous(name="Average Time (s)") +
  geom_point() +
  geom_line()



# Total time vs. number of moves, grouped by depth
game_statistics %>% ggplot(aes(x=num_moves, y=total_time, color=as.factor(max_depth))) +
  facet_wrap(~pruning, labeller=label_both) +
  labs(title="Total Time vs. Number of Moves", color="Max Depth") +
  scale_x_continuous(name="Number of Moves", breaks=seq(0,100,10), minor_breaks=NULL) +
  scale_y_continuous(name="Total Time (s)", breaks=seq(0,200,10), minor_breaks=NULL) +
  scale_color_discrete(guide=guide_legend(reverse=TRUE)) +
  geom_smooth(method=lm, se=FALSE) +
  geom_point()

# Avg time vs. number of moves, grouped by depth, logarithmic
game_statistics %>% ggplot(aes(x=num_moves, y=avg_time, color=as.factor(max_depth))) +
  facet_wrap(~pruning, labeller=label_both) +
  labs(title="Average Time vs. Number of Moves", color="Max Depth") +
  scale_x_continuous(name="Number of Moves", breaks=seq(0,100,10), minor_breaks=NULL) +
  scale_y_continuous(name="Average Time (s)", trans="log2",
                     breaks=trans_breaks("log2", function(x) 2^x),
                     labels=trans_format("log2", math_format(2^.x))) +
  scale_color_discrete(guide=guide_legend(reverse=TRUE)) +
  geom_point()

# Avg time vs. number of moves, grouped by winner, logarithmic
ggplot() +
  facet_wrap(~pruning, labeller=label_both) +
  labs(title="Average Time vs. Number of Moves", color="Winner") +
  scale_x_continuous(name="Number of Moves", breaks=seq(0,100,10), minor_breaks=NULL) +
  scale_y_continuous(name="Average Time (s)", trans="log10",
                     breaks=trans_breaks("log2", function(x) 2^x),
                     labels=trans_format("log2", math_format(2^.x))) +
  scale_color_manual(values=c("gray", "#00bfc4", "#f8766d"), guide=guide_legend(reverse=TRUE)) +
  geom_point(data=subset(game_statistics, is.na(winner)), aes(x=num_moves, y=avg_time, color="Neither")) +
  geom_point(data=subset(game_statistics, !is.na(winner)), aes(x=num_moves, y=avg_time, color=winner))

# Distribution of Wins over number of moves
game_statistics %>% filter(!is.na(winner)) %>% 
  ggplot(aes(x=num_moves, fill=winner)) +
  labs(title="Distribution of Wins Over Number of Moves", fill="Winner") +
  scale_x_continuous(name="Number of Moves") +
  scale_y_continuous(name="Number of Wins") +
  scale_fill_manual(values=c("#00bfc4", "#f8766d"), guide=guide_legend(reverse=TRUE)) +
  geom_histogram(binwidth=1)



# Field plot
ggplot() +
  labs(title="Empty Field") +
  scale_x_continuous(name="x", minor_breaks=NULL, breaks=seq(0,21), limits=c(0,21)) +
  scale_y_continuous(name="y", minor_breaks=NULL, breaks=seq(0,15), limits=c(0,15)) +
  geom_rect(aes(xmin=1, xmax=20, ymin=1, ymax=14), color="green", alpha=0) +
  geom_rect(aes(xmin=0, xmax=1, ymin=5, ymax=10), color="black", alpha=0) +
  geom_rect(aes(xmin=20, xmax=21, ymin=5, ymax=10), color="black", alpha=0) +
  geom_vline(aes(xintercept=10.5), color="black") +
  geom_point(aes(x=10.5, y=7.5), color="black", size=5)

# Start and end position distributions in wins
game_statistics %>% 
  filter(winner == "Team.RED") %>% ggplot() +
  labs(title="Red Wins: Start Positions") +
  scale_x_continuous(name="x", minor_breaks=NULL, breaks=seq(0,21), limits=c(0,21)) +
  scale_y_continuous(name="y", minor_breaks=NULL, breaks=seq(0,15), limits=c(0,15)) +
  geom_rect(aes(xmin=1, xmax=20, ymin=1, ymax=14), color="green", alpha=0) +
  geom_rect(aes(xmin=0, xmax=1, ymin=5, ymax=10), color="black", alpha=0) +
  geom_rect(aes(xmin=20, xmax=21, ymin=5, ymax=10), color="black", alpha=0) +
  geom_vline(aes(xintercept=10.5), color="black") +
  geom_point(aes(x=10.5, y=7.5), color="black", size=5) +
  geom_point(aes(x=blue_start_x+0.5, y=blue_start_y+0.5), color="blue", size=6, shape="square") +
  geom_point(aes(x=red_start_x+0.5, y=red_start_y+0.5), color="red", size=6, shape="square")

game_statistics %>% 
  filter(winner == "Team.RED") %>% ggplot() +
  labs(title="Red Wins: End Positions") +
  scale_x_continuous(name="x", minor_breaks=NULL, breaks=seq(0,21), limits=c(0,21)) +
  scale_y_continuous(name="y", minor_breaks=NULL, breaks=seq(0,15), limits=c(0,15)) +
  geom_rect(aes(xmin=1, xmax=20, ymin=1, ymax=14), color="green", alpha=0) +
  geom_rect(aes(xmin=0, xmax=1, ymin=5, ymax=10), color="black", alpha=0) +
  geom_rect(aes(xmin=20, xmax=21, ymin=5, ymax=10), color="black", alpha=0) +
  geom_vline(aes(xintercept=10.5), color="black") +
  geom_point(aes(x=20.5, y=7.5), color="black", size=5) +
  geom_point(aes(x=blue_end_x+.5, y=blue_end_y+.5), color="blue", size=6, shape="square") +
  geom_point(aes(x=red_end_x+.5, y=red_end_y+.5), color="red", size=6, shape="square")

game_statistics %>% 
  filter(winner == "Team.BLUE") %>% ggplot() +
  labs(title="Blue Wins: Start Positions") +
  scale_x_continuous(name="x", minor_breaks=NULL, breaks=seq(0,21), limits=c(0,21)) +
  scale_y_continuous(name="y", minor_breaks=NULL, breaks=seq(0,15), limits=c(0,15)) +
  geom_rect(aes(xmin=1, xmax=20, ymin=1, ymax=14), color="green", alpha=0) +
  geom_rect(aes(xmin=0, xmax=1, ymin=5, ymax=10), color="black", alpha=0) +
  geom_rect(aes(xmin=20, xmax=21, ymin=5, ymax=10), color="black", alpha=0) +
  geom_vline(aes(xintercept=10.5), color="black") +
  geom_point(aes(x=10.5, y=7.5), color="black", size=5) +
  geom_point(aes(x=red_start_x+.5, y=red_start_y+.5), color="red", size=6, shape="square") +
  geom_point(aes(x=blue_start_x+.5, y=blue_start_y+.5), color="blue", size=6, shape="square")

game_statistics %>% 
  filter(winner == "Team.BLUE") %>% ggplot() +
  labs(title="Blue Wins: End Positions") +
  scale_x_continuous(name="x", minor_breaks=NULL, breaks=seq(0,21), limits=c(0,21)) +
  scale_y_continuous(name="y", minor_breaks=NULL, breaks=seq(0,15), limits=c(0,15)) +
  geom_rect(aes(xmin=1, xmax=20, ymin=1, ymax=14), color="green", alpha=0) +
  geom_rect(aes(xmin=0, xmax=1, ymin=5, ymax=10), color="black", alpha=0) +
  geom_rect(aes(xmin=20, xmax=21, ymin=5, ymax=10), color="black", alpha=0) +
  geom_vline(aes(xintercept=10.5), color="black") +
  geom_point(aes(x=0.5, y=7.5), color="black", size=5) +
  geom_point(aes(x=red_end_x+.5, y=red_end_y+.5), color="red", size=6, shape="square") +
  geom_point(aes(x=blue_end_x+.5, y=blue_end_y+.5), color="blue", size=6, shape="square")


