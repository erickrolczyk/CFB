#devtools::install_github("lbenz730/ncaahoopR")

#https://github.com/lbenz730/ncaahoopR

#https://gist.github.com/spfleming/2527a6ca2b940af2a8aa1fee9320171d

#https://rdrr.io/github/meysubb/cfbscrapR/man/



install.packages('tidyverse')
install.packages("devtools")
devtools::install_github(repo = "meysubb/cfbscrapR")
remotes::install_github("rstudio/gt")
install.packages("ggimage")
install.packages("cfbscrapR")

install.packages("remotes")
remotes::install_github("saiemgilani/cfbscrapR")
install.packages("mgcv")

library(mgcv)
library(tidyverse)
library(dplyr)
library(cfbscrapR)
library(gt)
library(ggimage)
library(devtools)


pbp_2020 <- data.frame()

for(i in 1:7){
  data <- cfb_pbp_data(year = 2020, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2020 <- bind_rows(pbp_2020, df)
}


cfblogos <- read.csv("https://raw.githubusercontent.com/spfleming/CFB/master/logos.csv") %>% select(school, logo)

attach(pbp_2020)

levels(factor(pbp_2020$play_type))
pbp_2020 %>% count(play_type, sort = TRUE)


plays <- pbp_2020 %>% filter(rush == 1 | pass == 1)
plays_ex_gt <- plays %>% filter((period ==1 & abs(score_diff) <= 43) | (period == 2 & abs(score_diff) <= 37)| (period == 3 & abs(score_diff) <= 27)| (period == 4 & abs(score_diff) <= 21))

#getting avg yards per play by team
offense <- plays_ex_gt %>%  group_by(offense_play) %>% summarise(ypa = mean(yards_gained[pass==1]), ypr = mean(yards_gained[rush==1]), num.plays = n()) #%>% filter(num.plays > 300)
offense <- offense %>% left_join(cfblogos, by = c("offense_play" = "school"))
offense %>% arrange(desc(ypr))
offense %>% arrange(desc(ypa))

offense %>% 
  ggplot(aes(x=ypa, y=ypr)) + geom_image(image = offense$logo, asp = 16/9) +
  labs(x = "YPA",
       y = "YPR",
       title = "2020 YPA and YPR by Team",
       subtitle = "All Teams | Non-Garbage Time")+
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))+
  annotate("text", x = 1, y = 2.5, label = "Bad Rush / Bad Pass", size = 3)+
  annotate("text", x = 1, y = 7.5, label = "Good Rush / Bad Pass", size = 3)+
  annotate("text", x = 11, y = 2.5, label = "Bad Rush / Good Pass", size = 3)+
  annotate("text", x = 11, y = 7.5, label = "Good Rush / Good Pass", size = 3)


#getting avg EPA o and d by team
offense <- plays_ex_gt %>% group_by(offense_play) %>% summarise(epa.off = mean(EPA, na.rm = T),epa.pass.off = mean(EPA[pass==1]), epa.rush.off = mean(EPA[rush==1]), num.plays = n()) #%>% filter(num.plays > 300)
defense <- plays_ex_gt %>% group_by(defense_play) %>% summarise(epa.def= mean(EPA, na.rm = T),epa.pass.def = mean(EPA[pass==1]), epa.rush.def = mean(EPA[rush==1]), num.plays = n()) #%>% filter(num.plays > 300)
team.epa <- left_join(offense, defense, by = c("offense_play" = "defense_play")) 
team.epa <- team.epa %>% left_join(cfblogos, by = c("offense_play" = "school"))
team.epa <- team.epa %>% filter(offense_play != "Missouri State" & offense_play != "Eastern Kentucky" & offense_play != "Austin Peay")
head(team.epa)

#getting avg success o and d by team
offense_success <- plays %>% group_by(offense_play) %>% summarise(success.pass.off = mean(success[pass==1]), success.rush.off = mean(success[rush==1]), num.plays = n()) #%>% filter(num.plays > 300)
defense_success <- plays %>% group_by(defense_play) %>% summarise(success.pass.def = mean(success[pass==1]), success.rush.def = mean(success[rush==1]), num.plays = n()) #%>% filter(num.plays > 300)
team.success <- left_join(offense_success, defense_success, by = c("offense_play" = "defense_play"))
team.success <- team.success %>% left_join(cfblogos, by = c("offense_play" = "school"))
head(team.success)

#plotting success rate
team.success %>% ggplot(aes(x=success.rush.off, y=success.pass.off)) + geom_point()
team.success %>% ggplot(aes(x=success.rush.def, y=success.pass.def)) + geom_point()
cor(team.success$success.rush.off,team.success$success.pass.off)
cor(team.success$success.rush.def,team.success$success.pass.def)

#plotting epa
team.epa %>% ggplot(aes(x=epa.rush.off, y=epa.pass.off)) + geom_point()
team.epa %>% ggplot(aes(x=epa.rush.def, y=epa.pass.def)) + geom_point()

#plotting avg EPA
team.epa %>% ggplot(aes(x=epa.rush.off, y=epa.pass.off)) + geom_image(image = team.epa$logo, asp = 16/9) +
  geom_vline(xintercept = mean(team.epa$epa.rush.off), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(team.epa$epa.pass.off), linetype = "dashed", color = "blue") +
  labs(x = "Rush EPA/Play", y= "Pass EPA/Play",
       title = "2020 NCAA Team Efficiency",
       subtitle = "All Teams | Non-Garbage Time | Data from cfbscrapr") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

#plotting avg O and D EPA
team.epa %>% ggplot(aes(x=epa.off, y=epa.def)) + geom_image(image = team.epa$logo, asp = 16/9) +
  geom_vline(xintercept = mean(team.epa$epa.off), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(team.epa$epa.def), linetype = "dashed", color = "blue") +
  labs(x = "Off EPA/Play", y= "Def EPA/Play",
       title = "2020 NCAA Team EPA",
       subtitle = "All Teams | Non-Garbage Time | Data from cfbscrapr") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))


##Mich offense EPA
mich <- plays_ex_gt %>% filter(offense_play == "Michigan")

mich %>%
  ggplot(aes(x=adj_yd_line_end, y=EPA)) +
  geom_point() +
  labs(x = "Field Position",
       y = "EPA",
       title = "Expected Points Added by Field Position",
       subtitle = "Mich Offense 2019") +
  geom_abline(slope=0, intercept = 0, alpha = 0.5, col = "blue") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))
count(plays %>% filter(offense_play == "Michigan" & EPA >0))
count(plays %>% filter(offense_play == "Michigan" & EPA <0))


#EPA by down and distance
offense_epa <- plays_ex_gt %>% group_by(offense_play, down) %>% summarise(epa.off = mean(EPA), num.plays = n()) #%>% filter(num.plays > 100)
defense_epa <- plays_ex_gt %>% group_by(defense_play,down) %>% summarise(epa.def = mean(EPA), num.plays = n()) #%>% filter(num.plays > 100)
team_epa_down <- left_join(offense_epa, defense_epa, by = c("offense_play" = "defense_play"))
team_epa_down <- team_epa_down %>% left_join(cfblogos, by = c("offense_play" = "school"))
head(team.epa)

offense_epa <- plays_ex_gt %>% group_by(down,adj_yd_line_end) %>% summarise(epa.off = mean(EPA), num.plays = n())
defense_epa <- plays_ex_gt %>% group_by(down,adj_yd_line_end) %>% summarise(epa.def = mean(EPA), num.plays = n())

offense_epa %>%
  ggplot(aes(x=adj_yd_line_end, y=epa.off, colour=as.factor(down))) +
  geom_point() +
  labs(x = "Field Position",
       y = "EPA",
       title = "Expected Points Added by Field Position - Offense",
       subtitle = "EPA by Down and Distance")+
  geom_abline(slope=0, intercept = 0, alpha = 0.5, col = "blue") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

defense_epa %>%
  ggplot(aes(x=adj_yd_line_end, y=epa.def, colour=as.factor(down))) +
  geom_point() +
  labs(x = "Field Position",
       y = "EPA",
       title = "Expected Points Added by Field Position",
       subtitle = "EPA by Down and Distance")+
  geom_abline(slope=0, intercept = 0, alpha = 0.5, col = "blue") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))


offense_success <- plays %>% group_by(down,adj_yd_line_end) %>% summarise(success = mean(success), num.plays = n())%>% filter(num.plays > 3)
offense_success %>%
  ggplot(aes(x=adj_yd_line_end, y=success, colour=as.factor(down))) +
  geom_point() +
  labs(x = "Field Position",
       y = "Success Rate",
       title = "Success Rate by Field Position",
       subtitle = "Success by Down and Distance")+
  geom_abline(slope=0, intercept = 0, alpha = 0.5, col = "blue") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

michigan_success <- mich  %>% group_by(down) %>% summarise(success = mean(success), num.plays = n())
ggplot(data=michigan_success, aes(x=down, y=success)) +
  geom_bar(stat="identity", width=0.5)+
  labs(x = "Down",
       y = "Success Rate",
       title = "Success Rate by Down",
       subtitle = "Michigan Only")
all_team_success <- plays%>% group_by(down) %>% summarise(success = mean(success), num.plays = n())
ggplot(data=all_team_success, aes(x=down, y=success)) +
  geom_bar(stat="identity", width=0.5)+
  labs(x = "Down",
       y = "Success Rate",
       title = "Success Rate by Down",
       subtitle = "All Teams")

#success rate by down by each team
by_team_success <- plays_ex_gt %>% group_by(offense_play,down) %>% summarise(success = mean(success), num.plays = n()) #%>% filter(((down ==1 | down == 2 | down == 3) & num.plays > 100) | (down == 4 & num.plays > 20))
cfblogos <- read.csv("https://raw.githubusercontent.com/spfleming/CFB/master/logos.csv") %>% select(school, logo)
by_team_success <- by_team_success %>% left_join(cfblogos, by = c("offense_play" = "school"))

by_team_success %>% 
  ggplot(aes(x=down, y=success)) + geom_image(image = by_team_success$logo, asp = 8/6) +
  labs(x = "Down",
       y = "Success Rate",
       title = "Success Rate by Down",
       subtitle = "All Teams")+
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))


#BIG TEN TEAMS ONLY
by_team_success <- plays_ex_gt%>% group_by(offense_play,down) %>% summarise(success = mean(success), num.plays = n())#%>% #filter(((down ==1 | down == 2 | down == 3) & num.plays > 100) | (down == 4 & num.plays > 30))
big10_team_success <- by_team_success %>% filter(offense_play == "Michigan"|offense_play == "Ohio State"|offense_play == "Michigan State"|
                                                   offense_play == "Penn State"|offense_play == "Rutgers"|offense_play == "Maryland"|
                                                   offense_play == "Wisconsin"|offense_play == "Minnesota"|offense_play == "Nebraska"|
                                                   offense_play == "Iowa"|offense_play == "Purdue"|offense_play == "Illinois"|
                                                   offense_play == "Indiana"|offense_play == "Northwestern")
big10_team_success <- big10_team_success %>% left_join(cfblogos, by = c("offense_play" = "school"))

big10_team_success %>% 
  ggplot(aes(x=down, y=success)) + geom_image(image = big10_team_success$logo, asp = 8/6) +
  labs(x = "Down",
       y = "Success Rate",
       title = "Success Rate by Down",
       subtitle = "B1G Teams Only")+
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))


#SEC TEN TEAMS ONLY
by_team_success <- plays%>% group_by(offense_play,down) %>% summarise(success = mean(success), num.plays = n())#%>% #filter(((down ==1 | down == 2 | down == 3) & num.plays > 100) | (down == 4 & num.plays > 20))
sec_team_success <- by_team_success %>% filter(offense_play == "Alabama"|offense_play == "Auburn"|offense_play == "Ole Miss"|
                                                   offense_play == "Mississippi State"|offense_play == "Georgia"|offense_play == "Florida"|
                                                   offense_play == "Vanderbilt"|offense_play == "South Carolina"|offense_play == "Arkansas"|
                                                   offense_play == "LSU"|offense_play == "Missouri"|offense_play == "Texas A&M"|
                                                   offense_play == "Tennessee"|offense_play == "Kentucky")
sec_team_success <- sec_team_success %>% left_join(cfblogos, by = c("offense_play" = "school"))

sec_team_success %>% 
  ggplot(aes(x=down, y=success)) + geom_image(image = sec_team_success$logo, asp = 8/6) +
  labs(x = "Down",
       y = "Success Rate",
       title = "Success Rate by Down",
       subtitle = "SEC Teams Only")+
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))


#Big 12 TEN TEAMS ONLY
by_team_success <- plays%>% group_by(offense_play,down) %>% summarise(success = mean(success), num.plays = n())#%>% #filter(((down ==1 | down == 2 | down == 3) & num.plays > 100) | (down == 4 & num.plays > 20))
big12_team_success <- by_team_success %>% filter(offense_play == "Oklahoma"|offense_play == "Oklahoma State"|offense_play == "Texas"|
                                                 offense_play == "Kansas State"|offense_play == "Kansas"|offense_play == "Texas Tech"|
                                                 offense_play == "Baylor"|offense_play == "Iowa State"|offense_play == "TCU"|
                                                 offense_play == "West Virginia")
big12_team_success <- big12_team_success %>% left_join(cfblogos, by = c("offense_play" = "school"))

big12_team_success %>% 
  ggplot(aes(x=down, y=success)) + geom_image(image = big12_team_success$logo, asp = 8/6) +
  labs(x = "Down",
       y = "Success Rate",
       title = "Success Rate by Down",
       subtitle = "Big 12 Teams Only")+
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

#PAC 12 TEN TEAMS ONLY
by_team_success <- plays%>% group_by(offense_play,down) %>% summarise(success = mean(success), num.plays = n())#%>% #filter(((down ==1 | down == 2 | down == 3) & num.plays > 100) | (down == 4 & num.plays > 20))
pac12_team_success <- by_team_success %>% filter(offense_play == "Stanford"|offense_play == "USC"|offense_play == "California"|
                                                   offense_play == "Oregon"|offense_play == "Oregon State"|offense_play == "Washington State"|
                                                   offense_play == "Washington"|offense_play == "Arizona"|offense_play == "Arizona State"|
                                                   offense_play == "UCLA" |offense_play == "Utah"|offense_play == "Colorado")
                                                   
pac12_team_success <- pac12_team_success %>% left_join(cfblogos, by = c("offense_play" = "school"))

pac12_team_success %>% 
  ggplot(aes(x=down, y=success)) + geom_image(image = pac12_team_success$logo, asp = 8/6) +
  labs(x = "Down",
       y = "Success Rate",
       title = "Success Rate by Down",
       subtitle = "PAC 12 Teams Only")+
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))



#by half all team success rate
by_team_by_half_success <- plays_ex_gt %>% group_by(offense_play) %>% summarise(first_half_success = mean(success[half == 1]),  second_half_success = mean(success[half == 2]), num_play = n())%>% filter(num_play > 200)

by_team_by_half_success <- by_team_by_half_success %>% left_join(cfblogos, by = c("offense_play" = "school"))

by_team_by_half_success %>% 
  ggplot(aes(x=first_half_success, y=second_half_success)) + geom_image(image = by_team_by_half_success$logo, asp = 16/9) +
  labs(x = "First Half Success Rate",
       y = "Second Half Success Rate",
       title = "Success Rate by Half",
       subtitle = "All Teams | Non-Garbage Time")+
  geom_abline(slope=1, intercept = 0, alpha = 0.5, col = "blue") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))+
        annotate("text", x = .535, y = .5, label = "Good 1st Half/Good 2nd Half")+
        annotate("text", x = .535, y = .35, label = "Good 1st Half/Bad 2nd Half")+
        annotate("text", x = .35, y = .5, label = "Bad 1st Half/Good 2nd Half")



#B1G half all team epa
big10_team_by_half_epa <- plays_ex_gt %>% group_by(offense_play) %>% summarise(first_half_epa = mean(EPA[half == 1]),  second_half_epa = mean(EPA[half == 2]), num_play = n())%>% filter(num_play > 200)

big10_team_by_half_epa <- big10_team_by_half_epa %>% filter(offense_play == "Michigan"|offense_play == "Ohio State"|offense_play == "Michigan State"|
                                                   offense_play == "Penn State"|offense_play == "Rutgers"|offense_play == "Maryland"|
                                                   offense_play == "Wisconsin"|offense_play == "Minnesota"|offense_play == "Nebraska"|
                                                   offense_play == "Iowa"|offense_play == "Purdue"|offense_play == "Illinois"|
                                                   offense_play == "Indiana"|offense_play == "Northwestern")

big10_team_by_half_epa <- big10_team_by_half_epa %>% left_join(cfblogos, by = c("offense_play" = "school"))

big10_team_by_half_epa %>% 
  ggplot(aes(x=first_half_epa, y=second_half_epa)) + geom_image(image = big10_team_by_half_epa$logo, asp = 16/9) +
  labs(x = "First Half Avg EPA",
       y = "Second Half Avg EPA",
       title = "EPA by Half",
       subtitle = "All Teams | Non-Garbage Time")+
  geom_abline(slope=1, intercept = 0, alpha = 0.5, col = "blue") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

#SEC half all team epa
sec_team_by_half_epa <- plays_ex_gt %>% group_by(offense_play) %>% summarise(first_half_epa = mean(EPA[half == 1]),  second_half_epa = mean(EPA[half == 2]), num_play = n())%>% filter(num_play > 200)

sec_team_by_half_epa <- sec_team_by_half_epa %>% filter(offense_play == "Alabama"|offense_play == "Auburn"|offense_play == "Ole Miss"|
                                                          offense_play == "Mississippi State"|offense_play == "Georgia"|offense_play == "Florida"|
                                                          offense_play == "Vanderbilt"|offense_play == "South Carolina"|offense_play == "Arkansas"|
                                                          offense_play == "LSU"|offense_play == "Missouri"|offense_play == "Texas A&M"|
                                                          offense_play == "Tennessee"|offense_play == "Kentucky")

sec_team_by_half_epa <- sec_team_by_half_epa %>% left_join(cfblogos, by = c("offense_play" = "school"))

sec_team_by_half_epa %>% 
  ggplot(aes(x=first_half_epa, y=second_half_epa)) + geom_image(image = sec_team_by_half_epa$logo, asp = 16/9) +
  labs(x = "First Half Avg EPA",
       y = "Second Half Avg EPA",
       title = "EPA by Half",
       subtitle = "All Teams | Non-Garbage Time")+
  geom_abline(slope=1, intercept = 0, alpha = 0.5, col = "blue") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))


#pac 12 half all team epa
pac_team_by_half_epa <- plays_ex_gt %>% group_by(offense_play) %>% summarise(first_half_epa = mean(EPA[half == 1]),  second_half_epa = mean(EPA[half == 2]), num_play = n())%>% filter(num_play > 200)

pac_team_by_half_epa <- pac_team_by_half_epa %>% filter(offense_play == "Stanford"|offense_play == "USC"|offense_play == "California"|
                                                          offense_play == "Oregon"|offense_play == "Oregon State"|offense_play == "Washington State"|
                                                          offense_play == "Washington"|offense_play == "Arizona"|offense_play == "Arizona State"|
                                                          offense_play == "UCLA" |offense_play == "Utah"|offense_play == "Colorado")

pac_team_by_half_epa <- pac_team_by_half_epa %>% left_join(cfblogos, by = c("offense_play" = "school"))

pac_team_by_half_epa %>% 
  ggplot(aes(x=first_half_epa, y=second_half_epa)) + geom_image(image = pac_team_by_half_epa$logo, asp = 16/9) +
  labs(x = "First Half Avg EPA",
       y = "Second Half Avg EPA",
       title = "EPA by Half",
       subtitle = "All Teams | Non-Garbage Time")+
  geom_abline(slope=1, intercept = 0, alpha = 0.5, col = "blue") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

#big 12 half all team epa
big12_team_by_half_epa <- plays_ex_gt %>% group_by(offense_play) %>% summarise(first_half_epa = mean(EPA[half == 1]),  second_half_epa = mean(EPA[half == 2]), num_play = n())%>% filter(num_play > 200)

big12_team_by_half_epa <- big12_team_by_half_epa %>% filter(offense_play == "Oklahoma"|offense_play == "Oklahoma State"|offense_play == "Texas"|
                                                              offense_play == "Kansas State"|offense_play == "Kansas"|offense_play == "Texas Tech"|
                                                              offense_play == "Baylor"|offense_play == "Iowa State"|offense_play == "TCU"|
                                                              offense_play == "West Virginia")

big12_team_by_half_epa <- big12_team_by_half_epa %>% left_join(cfblogos, by = c("offense_play" = "school"))

big12_team_by_half_epa %>% 
  ggplot(aes(x=first_half_epa, y=second_half_epa)) + geom_image(image = big12_team_by_half_epa$logo, asp = 16/9) +
  labs(x = "First Half Avg EPA",
       y = "Second Half Avg EPA",
       title = "EPA by Half",
       subtitle = "All Teams | Non-Garbage Time")+
  geom_abline(slope=1, intercept = 0, alpha = 0.5, col = "blue") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

#2018 vs. 2019
plays_18_19 <- pbp_18_19 %>% filter(rush == 1 | pass == 1)

plays_18_19_ex_gt <- plays_18_19 %>% filter((period ==1) | (period == 2 & abs(offense_score - defense_score) <=37)| (period == 3 & abs(offense_score - defense_score) <=27)| (period == 4 & abs(offense_score - defense_score) <=21))

sucess_yoy <- plays_18_19_ex_gt %>% group_by(offense_play) %>% summarise(success_2019 = mean(EPA[year == 2019]),  success_2018 = mean(EPA[year == 2018]), num_play = n())%>% filter(num_play > 400)

sucess_yoy <- sucess_yoy %>% left_join(cfblogos, by = c("offense_play" = "school"))

sucess_yoy %>% 
  ggplot(aes(x=success_2019, y=success_2018)) + geom_image(image = sucess_yoy$logo, asp = 16/9) +
  labs(x = "2019 Avg EPA",
       y = "2018 Avg EPA",
       title = "Avg EPA YoY",
       subtitle = "All Teams | Non-Garbage Time")+
  geom_abline(slope=1, intercept = 0, alpha = 0.5, col = "blue") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))+
        annotate("text", x = .2, y = .2, label = "Good LY/Good TY")+
        annotate("text", x = .2, y = -.2, label = "Bad LY/Good TY")+
        annotate("text", x = -.2, y = .2, label = "Good LY/Bad TY")+
        annotate("text", x = -.2, y = -.2, label = "Bad LY/Bad TY")
  

#offense + defense EPA

o_epa <- plays_ex_gt %>% group_by(offense_play) %>% summarise(o_epa = mean(EPA, na.rm = T), o.num_play = n()) #%>% filter(o.num_play > 200)
d_epa <- plays_ex_gt %>% group_by(defense_play) %>% summarise(d_epa = mean(EPA, na.rm = T), d.num_play = n()) #%>% filter(d.num_play > 200)

o_d_epa <- o_epa %>% left_join(d_epa, by = c("offense_play" = "defense_play"))

o_d_epa <- o_d_epa %>% left_join(cfblogos, by = c("offense_play" = "school"))

o_d_epa <- o_d_epa %>% filter(offense_play != "Missouri State" | offense_play != "Eastern Kentucky")

o_d_epa %>% 
  ggplot(aes(x=o_epa, y=d_epa)) + geom_image(image = o_d_epa$logo, asp = 16/9) +
  labs(x = "Offense Avg EPA",
       y = "Defense Avg EPA",
       title = "2020 Avg EPA for O & D",
       subtitle = "All Teams | Non-Garbage Time")+
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))+
        annotate("text", x = .2, y = .2, label = "Good O/Bad D")+
        annotate("text", x = .2, y = -.2, label = "Good O/Good D")+
        annotate("text", x = -.2, y = .2, label = "Bad O/Bad D")+
        annotate("text", x = -.2, y = -.2, label = "Bad O/Good D")

#explosive plays
pbp_2020$explosive_play <- case_when((pbp_2020$yards_gained >= 10 & pbp_2020$rush == 1) |
                                      (pbp_2020$yards_gained >= 15 & pbp_2020$pass == 1)~ 1,
                                     TRUE ~ 0)

pbp_2020$negative_play <- case_when(pbp_2020$yards_gained <= -1 ~ 1, TRUE ~ 0)

plays <- pbp_2020 %>% filter(rush == 1 | pass == 1)
plays_ex_gt <- plays %>% filter((period ==1 & abs(score_diff) <= 43) | (period == 2 & abs(score_diff) <= 37)| (period == 3 & abs(score_diff) <= 27)| (period == 4 & abs(score_diff) <= 21))


explosive_plays_O <- plays_ex_gt %>% group_by(offense_play) %>% summarise(num_explosive_plays_O = sum(explosive_play), num_negative_plays_O = sum(negative_play),avg_epa = mean(EPA), o.num_play = n()) #%>% filter(o.num_play > 200)
explosive_plays_D <- plays_ex_gt %>% group_by(defense_play) %>% summarise(num_explosive_plays_D = sum(explosive_play), num_negative_plays_D = sum(negative_play), avg_epa = mean(EPA), d.num_play = n()) #%>% filter(d.num_play > 200)
explosive_plays_O_D <- explosive_plays_O %>% left_join(explosive_plays_D, by = c("offense_play" = "defense_play"))

explosive_plays_O_D <- explosive_plays_O_D %>% left_join(cfblogos, by = c("offense_play" = "school"))

explosive_plays_O_D$pct_explosive_plays_O <- explosive_plays_O_D$num_explosive_plays_O/explosive_plays_O_D$o.num_play
explosive_plays_O_D$pct_explosive_plays_D <- explosive_plays_O_D$num_explosive_plays_D/explosive_plays_O_D$d.num_play
explosive_plays_O_D$pct_negative_plays_O <- explosive_plays_O_D$num_negative_plays_O/explosive_plays_O_D$o.num_play
explosive_plays_O_D$pct_negative_plays_D <- explosive_plays_O_D$num_negative_plays_D/explosive_plays_O_D$d.num_play

explosive_plays_O_D <- explosive_plays_O_D %>% filter(offense_play != "Missouri State" & offense_play != "Eastern Kentucky")


explosive_plays_O_D %>% 
  ggplot(aes(x=pct_explosive_plays_O, y=pct_explosive_plays_D)) + geom_image(image = explosive_plays_O_D$logo, asp = 16/9) +
  labs(x = "Pct of Explosive Plays Gained",
       y = "Pct of Explosive Plays Allowed",
       title = "2020 Explosive Plays on O and D",
       subtitle = "All Teams | Non-Garbage Time | (R >=10 yds | P >=15 yds)" ) + 
        labs(caption = "Data from CFBscrapr") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 7))

explosive_plays_O_D %>% 
  ggplot(aes(x=pct_explosive_plays_O, y=pct_negative_plays_D)) + geom_image(image = explosive_plays_O_D$logo, asp = 16/9) +
  labs(x = "Pct of Explosive Plays Gained (Offense)",
       y = "Pct of Neagtive Plays Made (Defense) (<= -1 yds)",
       title = "Explosive Plays on O & Negative Plays on D",
       subtitle = "All Teams | Non-Garbage Time") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))


##down and distance
down_distance <- plays_ex_gt %>% group_by(offense_play, down, distance) %>% summarise(avg_epa = mean(EPA), num_play = n()) %>% filter(num_play > 2)

down_distance <- down_distance %>% left_join(cfblogos, by = c("offense_play" = "school"))

down_distance <- down_distance %>% filter(offense_play == "Michigan")

down_distance %>%
  ggplot(aes(x=distance, y=avg_epa, colour=as.factor(down))) +
  geom_point() +
  labs(x = "Distance to First Down",
       y = "Avg EPA",
       title = "Avg EPA by Down and Distance",
       subtitle = "Michigan Only | Excluding Garbage Time")+
  geom_abline(slope=0, intercept = 0, alpha = 0.5, col = "blue") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))


##red zone

red_zone <- plays_ex_gt %>% filter(yards_to_goal <=30)

rz_o_epa <- red_zone %>% group_by(offense_play) %>% summarise(rz_o_epa = mean(EPA), rz_o.num_play = n()) %>% filter(rz_o.num_play > 10)
rz_d_epa <- red_zone %>% group_by(defense_play) %>% summarise(rz_d_epa = mean(EPA), rz_d.num_play = n()) %>% filter(rz_d.num_play > 5)

rz_o_d_epa <- rz_o_epa %>% left_join(rz_d_epa, by = c("offense_play" = "defense_play"))

rz_o_d_epa <- rz_o_d_epa %>% left_join(cfblogos, by = c("offense_play" = "school"))

rz_o_d_epa %>% 
  ggplot(aes(x=rz_o_epa, y=rz_d_epa)) + geom_image(image = rz_o_d_epa$logo, asp = 16/9) +
  labs(x = "Offense Avg EPA",
       y = "Defense Avg EPA",
       title = "2020 Avg Redzone EPA for O & D",
       subtitle = "All Teams | Non-Garbage Time")+
  geom_abline(slope=1, intercept = 0, alpha = 0.5, col = "blue") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))+
  annotate("text", x = .5, y = .6, label = "Good O / Bad D")+
  annotate("text", x = .5, y = -.75, label = "Good O / Good D")+
  annotate("text", x = -.5, y = .6, label = "Bad O / Bad D")+
  annotate("text", x = -.5, y = -.75, label = "Bad O / Good D")

### Pythag

games_19 <- cfb_game_info(2019)

# Total season points
season.pts1 <- games_19 %>% group_by(home_team) %>% 
  summarise(points.for = sum(home_points), points.against = sum(away_points)) %>% rename("team" = home_team)
season.pts2 <- games_19 %>% group_by(away_team) %>% 
  summarise(points.for = sum(away_points), points.against = sum(home_points)) %>% rename("team" = away_team)

season.pts <- left_join(season.pts1, season.pts2, by = "team") %>% 
  mutate(points.for = points.for.x + points.for.y, points.against = points.against.x + points.against.y,
         pt.diff = points.for - points.against) %>%
  select(team, points.for, points.against, pt.diff)


# Create total wins and losses (Regular season only)
# Same method as before - home wins and away wins, then merge and add. 

gbg1 <- games_19 %>% mutate(home_win = ifelse(home_points - away_points > 0, 1, 0),
                            away_win = ifelse(away_points - home_points > 0, 1, 0)) 
gbg2 <- gbg1 %>% group_by(home_team) %>% summarize(
  wins.h = sum(home_win), games = n()) %>% mutate("team" = home_team)

gbg3 <- gbg1 %>% group_by(away_team) %>% summarize(
  wins.a = sum(away_win), games = n()) %>% mutate("team" = away_team)

record <- left_join(gbg3, gbg2, by = "team") %>% filter(!is.na(wins.h)) %>%
  mutate(wins = wins.h + wins.a, games = games.x + games.y) %>% select(team, wins, games) %>%
  mutate(win.pct = wins/games)

# Create pythagorean expectation of wins, using basic formula. p = 2 ish minimizes MAD, but you could play around with it. 

pythag19 <- season.pts %>% mutate(p = 2) %>%
  mutate(pyth = (points.for^p)/(points.for^p + points.against^p))

# Join pythag to actual record for the graph
pythag219 <- pythag19 %>% left_join(record, by = "team")

# join and clean up 
chartdata <- left_join(pythag219, cfblogos, by = c("team" = "school")) %>% filter(!is.na(win.pct))

# chart
chartdata %>% ggplot(aes(x=pyth, y=win.pct)) + geom_image(image = chartdata$logo) +
  geom_hline(yintercept = mean(chartdata$win.pct), linetype = "dashed", color = "red", alpha =0.4) +
  geom_vline(xintercept = mean(chartdata$pyth), linetype = "dashed", color = "red", alpha = 0.4) +
  geom_abline(intercept = 0, slope == 1, color = "black", alpha = 0.4) +
  theme_minimal()



#Success Rate and Explosiveness

success_explosive_plays_O <- plays_ex_gt %>% group_by(offense_play) %>% 
            summarise(num_explosive_plays_O = sum(explosive_play), num_negative_plays_O = sum(negative_play),
            avg_epa = mean(EPA), o.num_play = n(), percent_explosive_plays = num_explosive_plays_O/o.num_play,
            success_rate = mean((success)))
success_explosive_plays_O <- success_explosive_plays_O %>% left_join(cfblogos, by = c("offense_play" = "school")) 
success_explosive_plays_O <- success_explosive_plays_O %>% filter(offense_play != "Missouri State")

success_explosive_plays_O %>% 
  ggplot(aes(x=percent_explosive_plays, y=success_rate)) + geom_image(image = success_explosive_plays_O$logo, asp = 16/9) +
  labs(x = "% Explosive Plays on O",
       y = "Success Rate",
       title = "2020 Explosiveness vs. Success Rate",
       subtitle = "All Teams | Non-Garbage Time")+
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12)) #+
  #annotate("text", x = .5, y = .6, label = "Good O / Bad D")+
  #annotate("text", x = .5, y = -.75, label = "Good O / Good D")+
  #annotate("text", x = -.5, y = .6, label = "Bad O / Bad D")+
  #annotate("text", x = -.5, y = -.75, label = "Bad O / Good D")

#4 And Analysis

fourth_down <- plays_ex_gt %>% filter(down == 4 & (distance <= 3))
fourth_down_success <- fourth_down %>% group_by(distance) %>% summarise(success.off = mean(success), success.pass.off = mean(success[pass==1]), 
                                                                  success.rush.off = mean(success[rush==1]), num.plays = n())
fourth_down_success_team_distance <- fourth_down %>% group_by(offense_play, distance) %>% summarise(success.off = mean(success), 
                                                                                  success.pass.off = mean(success[pass==1]), 
                                                                                  success.rush.off = mean(success[rush==1]), num.plays = n())
fourth_down_success_team <- fourth_down %>% group_by(offense_play) %>% summarise(success.off = mean(success), 
                                                                                                    success.pass.off = mean(success[pass==1]), 
                                                                                                    success.rush.off = mean(success[rush==1]), num.plays = n())

#YPP O and D
offense_YPP <- plays_ex_gt %>%  group_by(offense_play) %>% summarise(ypp_off = mean(yards_gained), num.plays.off = n()) #%>% filter(num.plays > 300)
defense_YPP <- plays_ex_gt %>%  group_by(defense_play) %>% summarise(ypp_def = mean(yards_gained), num.plays.def = n())
ypp_team <- offense_YPP %>% left_join(defense_YPP, by=c("offense_play" = "defense_play"))
ypp_team <- ypp_team %>% left_join(cfblogos, by = c("offense_play" = "school"))
ypp_team <- ypp_team %>% filter(offense_play != "Missouri State")
ypp_team %>% 
  ggplot(aes(x=ypp_off, y=ypp_def)) + geom_image(image = ypp_team$logo, asp = 16/9) +
  geom_vline(xintercept = mean(ypp_team$ypp_off), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(ypp_team$ypp_def), linetype = "dashed", color = "blue") +
  labs(x = "Yards/Play Off",
       y = "Yards/Play Def",
       title = "2020 Yards per Play",
       subtitle = "All Teams | Non-Garbage Time")+
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

#HAVOC Plays


pbp_2020$havoc_plays <- case_when ((pbp_2020$play_type == "Fumble Recovery (Opponent)" |
                                      pbp_2020$play_type == "Fumble Recovery (Opponent) Touchdown" |  
                                      pbp_2020$play_type == "Fumble Recovery (Own)" |
                                      pbp_2020$play_type == "Fumble Return Touchdown"|
                                      pbp_2020$play_type == "Sack"|
                                      pbp_2020$play_type == "Interception Return"|
                                      pbp_2020$play_type == "Interception Return Touchdown" |
                                      (pbp_2020$rush == 1 & pbp_2020$yards_gained <= -1)|
                                      (pbp_2020$play_type == "Pass Incompletion" & grepl("broken",pbp_2020$play_text,fixed = T) == T))
                                   ~ 1,TRUE ~ 0)

plays <- pbp_2020 %>% filter(rush == 1 | pass == 1)
plays_ex_gt <- plays %>% filter((period ==1 & abs(score_diff) <= 43) | (period == 2 & abs(score_diff) <= 37)| (period == 3 & abs(score_diff) <= 27)| (period == 4 & abs(score_diff) <= 21))

havoc_play_rate <- plays_ex_gt %>% group_by(defense_play) %>% 
  summarise(num_havoc_plays = sum(havoc_plays), d.num_play = n(), havoc_rate = num_havoc_plays/d.num_play, avg_epa = mean(EPA, na.rm = T))
havoc_play_rate <- havoc_play_rate %>% left_join(cfblogos, by = c("defense_play" = "school"))

havoc_play_rate %>% 
  ggplot(aes(x=havoc_rate, y=avg_epa)) + geom_image(image = havoc_play_rate$logo, asp = 16/9) +
  labs(x = "Havoc Rate",
       y = "Avg EPA",
       title = "2020 Havoc Rate",
       subtitle = "All Teams | Non-Garbage Time")+
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

#Explosiveness PPP

Explosiveness_PPP_O <- plays_ex_gt %>% group_by(offense_play, game_id, drive_number) %>% 
  summarise(points_O = max(drive_pts), num_o_plays = max(drive_play_number))
Explosiveness_PPP_O <- Explosiveness_PPP_O %>% group_by(offense_play) %>% summarise(total_points_O = sum(points_O), 
                                                                                    num_plays_o = sum(num_o_plays),
                                                                                    ppp_o = total_points_O/num_plays_o)


Explosiveness_PPP_D <- plays_ex_gt %>% group_by(defense_play, game_id, drive_number) %>% 
  summarise(points_D = max(drive_pts), num_d_plays = max(drive_play_number))
Explosiveness_PPP_D <- Explosiveness_PPP_D %>% group_by(defense_play) %>% summarise(total_points_d = sum(points_D), 
                                                                                    num_plays_d = sum(num_d_plays),
                                                                                    ppp_d = total_points_d/num_plays_d)
Explosiveness_PPP <- Explosiveness_PPP_O %>% left_join(Explosiveness_PPP_D, by = c("offense_play" = "defense_play"))
Explosiveness_PPP <- Explosiveness_PPP %>% left_join(cfblogos, by = c("offense_play" = "school"))
Explosiveness_PPP <- Explosiveness_PPP %>% filter(offense_play != "Eastern Kentucky",offense_play != "Stephen F. Austin",offense_play != "Missouri State",offense_play != "North Alabama",offense_play != "Austin Peay")

Explosiveness_PPP %>% 
  ggplot(aes(x=ppp_o, y=ppp_d)) + geom_image(image = Explosiveness_PPP$logo, asp = 16/9) +
  geom_vline(xintercept = mean(Explosiveness_PPP$ppp_o), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(Explosiveness_PPP$ppp_d), linetype = "dashed", color = "blue") +
  labs(x = "Points per Play O",
       y = "Points per Play D",
       title = "Explosiveness - 2020 Points per Play",
       subtitle = "All Teams | Non-Garbage Time")+
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

#Redzone Efficiency
plays_ex_gt$red_zone_40 <- case_when(plays_ex_gt$yards_to_goal <=40 ~1, TRUE ~ 0)
plays_ex_gt$red_zone_25 <- case_when(plays_ex_gt$yards_to_goal <=25 ~1, TRUE ~ 0)
plays_ex_gt$drive_score <- case_when(plays_ex_gt$drive_result == "FG" | plays_ex_gt$drive_result == "TD" ~1, TRUE ~ 0)


Red_Zone_O <- plays_ex_gt %>% group_by(offense_play, game_id, drive_number) %>% 
  summarise(red_zone_drive_o = max(red_zone_40), scored_o = max(drive_score), points_O = max(drive_pts))
Red_Zone_O <- Red_Zone_O %>% group_by(offense_play) %>% summarise(num_red_zone_drives_o = sum(red_zone_drive_o), ttl_num_drives_o = n(),
                                                                  red_zone_points_o = sum(points_O[red_zone_drive_o==1]),
                                                                  red_zone_rate_o = num_red_zone_drives_o/ttl_num_drives_o,
                                                                  points_per_red_zone_trip_o = red_zone_points_o/num_red_zone_drives_o)


Red_Zone_D <- plays_ex_gt %>% group_by(defense_play, game_id, drive_number) %>% 
  summarise(red_zone_drive_d = max(red_zone_40), scored_d = max(drive_score), points_D = max(drive_pts))
Red_Zone_D <- Red_Zone_D %>% group_by(defense_play) %>% summarise(num_red_zone_drives_d = sum(red_zone_drive_d), ttl_num_drives_d = n(),
                                                                  red_zone_points_d = sum(points_D[red_zone_drive_d==1]),
                                                                  red_zone_rate_d = num_red_zone_drives_d/ttl_num_drives_d,
                                                                  points_per_red_zone_trip_d = red_zone_points_d/num_red_zone_drives_d)

Red_Zone <- Red_Zone_O %>% left_join(Red_Zone_D, by = c("offense_play" = "defense_play"))
Red_Zone <- Red_Zone %>% left_join(cfblogos, by = c("offense_play" = "school"))
Red_Zone <- Red_Zone %>% filter(offense_play != "Eastern Kentucky",offense_play != "Stephen F. Austin",offense_play != "Missouri State",offense_play != "North Alabama",offense_play != "Austin Peay")

Red_Zone %>% 
  ggplot(aes(x=red_zone_rate_o, y=points_per_red_zone_trip_o)) + geom_image(image = Red_Zone$logo, asp = 16/9) +
  geom_vline(xintercept = mean(Red_Zone$red_zone_rate_o), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(Red_Zone$points_per_red_zone_trip_o), linetype = "dashed", color = "blue") + 
  labs(x = "Red Zone Rate",
       y = "Points per Trip",
       title = "Red Zone Efficiency",
       subtitle = "All Teams | Non-Garbage Time | Inside Opp 40 yrd")+
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

Red_Zone %>% 
  ggplot(aes(x=points_per_red_zone_trip_o, y=points_per_red_zone_trip_d)) + geom_image(image = Red_Zone$logo, asp = 16/9) +
  geom_vline(xintercept = mean(Red_Zone$points_per_red_zone_trip_o), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(Red_Zone$points_per_red_zone_trip_d), linetype = "dashed", color = "blue") + 
  labs(x = "Points per Trip O",
       y = "Points per Trip D",
       title = "Red Zone Efficiency",
       subtitle = "All Teams | Non-Garbage Time | Inside Opp 40 yrd")+
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

#Field Position

field_position_O <- plays_ex_gt %>% group_by(offense_play) %>% 
  summarise(avg_start_field_position_O = (100 - mean(yards_to_goal[drive_play_number ==1], na.rm = T)))

field_position_D <- plays_ex_gt %>% group_by(defense_play) %>% 
  summarise(avg_start_field_position_D = (100 - mean(yards_to_goal[drive_play_number == 1], na.rm = T)))

field_position <- field_position_O %>% left_join(field_position_D, by = c("offense_play" = "defense_play"))
field_position <- field_position %>% left_join(cfblogos, by = c("offense_play" = "school"))
field_position <- field_position %>% filter(offense_play != "Eastern Kentucky",offense_play != "Stephen F. Austin",offense_play != "Missouri State",offense_play != "North Alabama",offense_play != "Austin Peay")

field_position %>% 
  ggplot(aes(x=avg_start_field_position_O, y=avg_start_field_position_D)) + geom_image(image = field_position$logo, asp = 16/9) +
  geom_vline(xintercept = mean(field_position$avg_start_field_position_O), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(field_position$avg_start_field_position_D), linetype = "dashed", color = "blue") + 
  labs(x = "Starting Field Position O",
       y = "Starting Field Position D",
       title = "Field Position",
       subtitle = "All Teams | Non-Garbage Time")+
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))


#TURNOVERS

turnovers_d <- plays_ex_gt %>% group_by(defense_play, game_id, drive_number) %>% 
  summarise(turnover = max(turnover_vec+safety))

turnovers_d <- turnovers_d %>% group_by(defense_play) %>% 
  summarise(turnover_d = sum(turnover), num_drives_d = n(), turnover_rate_d = turnover_d/num_drives_d)

turnovers_o <- plays_ex_gt %>% group_by(offense_play, game_id, drive_number) %>% 
  summarise(turnover = max(turnover_vec+safety))

turnovers_o <- turnovers_o %>% group_by(offense_play) %>% 
  summarise(turnover_o = sum(turnover), num_drives_o = n(), turnover_rate_o = turnover_o/num_drives_o)

turnovers <- turnovers_o %>% left_join(turnovers_d, by = c("offense_play" = "defense_play"))
turnovers <- turnovers %>% left_join(cfblogos, by = c("offense_play" = "school"))
turnovers <- turnovers %>% filter(offense_play != "Eastern Kentucky",offense_play != "Stephen F. Austin",offense_play != "Missouri State",offense_play != "North Alabama",offense_play != "Austin Peay")

turnovers %>% 
  ggplot(aes(x=turnover_rate_o, y=turnover_rate_d)) + geom_image(image = turnovers$logo, asp = 16/9) +
  geom_vline(xintercept = mean(turnovers$turnover_rate_o), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(turnovers$turnover_rate_d), linetype = "dashed", color = "blue") + 
  labs(x = "Offensive Turnover Rate",
       y = "Defensive Turnover Rate",
       title = "Turnover Battle",
       subtitle = "All Teams | Non-Garbage Time")+
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

#Success Rate

success_rate_o <- plays_ex_gt %>% group_by(offense_play) %>% summarise(success_o = mean(success))
success_rate_d <- plays_ex_gt %>% group_by(defense_play) %>% summarise(success_d = mean(success))
team_success <- success_rate_o %>% left_join(success_rate_d, by = c("offense_play" = "defense_play"))
team_success <- team_success %>% filter(offense_play != "Eastern Kentucky",offense_play != "Stephen F. Austin",offense_play != "Missouri State",offense_play != "North Alabama",offense_play != "Austin Peay")
names(team_success)[names(team_success) == "offense_play"] <- "team"

#Team Profile

team_profile <- team_success %>% left_join(field_position, by = c("team" = "offense_play"))
team_profile <- team_profile %>% left_join(turnovers, by = c("team" = "offense_play"))
team_profile <- team_profile %>% left_join(Red_Zone, by = c("team" = "offense_play"))
team_profile <- team_profile %>% left_join(Explosiveness_PPP, by = c("team" = "offense_play"))

team_percentiles <- data.frame(team_profile$team)
team_percentiles$success_o <- ecdf(team_profile$success_o)(team_profile$success_o)
team_percentiles$success_d <-ecdf(-team_profile$success_d)(-team_profile$success_d)
team_percentiles$avg_start_field_position_o <- ecdf(team_profile$avg_start_field_position_O)(team_profile$avg_start_field_position_O)
team_percentiles$avg_start_field_position_d <- ecdf(-team_profile$avg_start_field_position_D)(-team_profile$avg_start_field_position_D)
team_percentiles$turnover_rate_o <- ecdf(-team_profile$turnover_rate_o)(-team_profile$turnover_rate_o)
team_percentiles$turnover_rate_d <-ecdf(team_profile$turnover_rate_d)(team_profile$turnover_rate_d)
team_percentiles$redzone_o <- ecdf(team_profile$points_per_red_zone_trip_o)(team_profile$points_per_red_zone_trip_o)
team_percentiles$redzone_d <- ecdf(-team_profile$points_per_red_zone_trip_d)(-team_profile$points_per_red_zone_trip_d)
team_percentiles$PPP_o <- ecdf(team_profile$ppp_o)(team_profile$ppp_o)
team_percentiles$PPP_d <- ecdf(-team_profile$ppp_d)(-team_profile$ppp_d)
team_percentiles <- team_percentiles %>% left_join(cfblogos, by = c("team_profile.team" = "school"))

team_ranks <- data.frame(team_profile$team)
team_ranks$O_rank <- ((.35*team_percentiles$PPP_o)+(.25*team_percentiles$success_o)+(.15*team_percentiles$avg_start_field_position_d)
                      +(.15*team_percentiles$redzone_o)+(.1*team_percentiles$turnover_rate_o))
team_ranks$D_rank <- ((.35*team_percentiles$PPP_d)+(.25*team_percentiles$success_d)+(.15*team_percentiles$avg_start_field_position_o)
                      +(.15*team_percentiles$redzone_d)+(.1*team_percentiles$turnover_rate_d))
team_ranks$O_percentile <- ecdf(team_ranks$O_rank)(team_ranks$O_rank)
team_ranks$D_percentile <- ecdf(team_ranks$D_rank)(team_ranks$D_rank)
team_ranks <- team_ranks %>% left_join(cfblogos, by = c("team_profile.team" = "school"))

team_ranks %>% 
  ggplot(aes(x=O_rank, y=D_rank)) + geom_image(image = team_ranks$logo, asp = 16/9) +
  geom_vline(xintercept = mean(team_ranks$O_rank), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean(team_ranks$D_rank), linetype = "dashed", color = "blue") + 
  labs(x = "Offensive Ranking",
       y = "Defensive Ranking",
       title = "Offense and Defense Rankings",
       subtitle = "All Teams | Non-Garbage Time")+
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

team_ranks$Team_rank <- (.6*team_ranks$O_rank+.4*team_ranks$D_rank)
team_ranks$Team_percentile <- ecdf(team_ranks$Team_rank)(team_ranks$Team_rank)
team_ranks$Team_ranking <- rank(-team_ranks$Team_rank, na.last = T, ties.method = c("random"))



team_ranks %>% 
ggplot(aes(x=team_profile.team, y=Team_ranking)) + 
  geom_image(image = team_ranks$logo, asp = 16/9, size = .03) +   # Draw points
  geom_segment(aes(x=team_profile.team, 
                   xend=team_profile.team, 
                   y=min(Team_ranking), 
                   yend=max(Team_ranking)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(x = "Team",
       y = "Team Ranking",
      title="Team Rankings - Week 7", 
       subtitle="Non-Opponent Adjusted | Non-Garbage Time", 
       caption="Data from CFBscrapr") +  
  coord_flip()

setwd("~/Downloads/CFB")
write.csv(team_ranks, "Week 7 Rankings.csv")
write.csv(team_profile, "Week 7 Team Profile.csv")
