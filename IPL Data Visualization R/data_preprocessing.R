library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(RMySQL)
library(stringr)


data <- read.csv('main_data/all_matches.csv')
match_df <- read.csv('main_data/match_data.csv')

match_df <- match_df %>% 
  mutate(Season = str_extract(Match_days, "\\d{4}")) %>%
  mutate(Season= as.integer(Season))


players <- sort(unique(c(data$Striker, data$Non_Striker, data$Bowler)))
venues <- sort(unique(match_df$Stadium))
teams <- sort(unique(c(data$Batting_Team, data$Bowling_Team)))


batting_data <- data %>%
  inner_join(match_df, by='Match_Id') %>%
  group_by(Striker, Bowler, Batting_Team, Bowling_Team, Stadium, Season) %>%
  summarise(Runs = sum(Runs_off_bat), Wickets = sum(!is.na(Wicket_Type) & Player_Dismissed == Striker), Balls = sum(Wide==0), Run_Out=sum(!is.na(Wicket_Type) & Player_Dismissed == Striker & Wicket_Type=='Run Out')) 

non <- data %>%
  inner_join(match_df, by='Match_Id') %>%
  group_by(Non_Striker, Bowler, Batting_Team, Bowling_Team, Stadium, Season) %>%
  summarise(Out=sum(!is.na(Wicket_Type) & Wicket_Type == 'Run Out' & Player_Dismissed == Non_Striker)) %>%
  rename(Striker=Non_Striker)

batting_data <- batting_data %>%
  left_join(non, by=c('Striker', 'Bowler', 'Batting_Team', 'Bowling_Team', 'Stadium', 'Season')) %>%
  mutate(Out=ifelse(is.na(Out), 0, Out)) %>%
  mutate(Wickets=Wickets+Out, Run_Out=Run_Out+Out) %>%
  select(-Out)%>%
mutate(Average = Runs/(Wickets-Run_Out), Strike_Rate = Runs/Balls*100)

bowling_data <- data %>%
  inner_join(match_df, by='Match_Id') %>%
  group_by(Bowler, Striker, Bowling_Team, Batting_Team, Stadium, Season) %>%
  summarise(Runs = sum(Runs_off_bat+Wide+No_Ball), Wickets = sum(!is.na(Wicket_Type) & Wicket_Type!='Run Out'), Balls = sum(Wide==0 & No_Ball==0))  %>%
  mutate(Average=Runs/Wickets, Economy=Runs/Balls*6)



team_batting_data <- data %>%
  inner_join(match_df, by='Match_Id') %>%
  group_by(Batting_Team, Striker, Bowling_Team, Stadium, Season) %>%
  summarise(Runs = sum(Runs_off_bat), Wickets = sum(!is.na(Wicket_Type) & Player_Dismissed == Striker & Wicket_Type!='Retired Hurt'), Balls = sum(Wide==0), Run_Out=sum(!is.na(Wicket_Type) & Player_Dismissed == Striker & Wicket_Type=='Run Out')) 

non_team <- data %>%
  inner_join(match_df, by='Match_Id') %>%
  group_by(Batting_Team, Non_Striker,  Bowling_Team, Stadium, Season) %>%
  summarise(Out=sum(!is.na(Wicket_Type) & Wicket_Type == 'Run Out' & Player_Dismissed == Non_Striker & Wicket_Type!='Retired Hurt')) %>%
  rename(Striker=Non_Striker)

team_batting_data <- team_batting_data %>%
  left_join(non_team, by=c('Batting_Team', 'Striker', 'Bowling_Team', 'Stadium', 'Season')) %>%
  mutate(Out=ifelse(is.na(Out), 0, Out)) %>%
  mutate(Wickets=Wickets+Out) %>%
  select(-c(Out, Run_Out)) %>%
  mutate(Average = Runs/Wickets, Strike_Rate = Runs/Balls*100)


team_bowling_data <- data %>%
  inner_join(match_df, by='Match_Id') %>%
  group_by(Bowling_Team, Bowler, Batting_Team, Stadium, Season) %>%
  summarise(Runs = sum(Runs_off_bat+Wide+No_Ball), Wickets = sum(!is.na(Wicket_Type) & Wicket_Type!='Run Out' & Wicket_Type!='Retired Hurt'), Balls = sum(Wide==0 & No_Ball==0))  %>%
  mutate(Average=Runs/Wickets, Economy=Runs/Balls*6)



batting_metric <- c('Runs', 'Batting Average', 'Strike Rate')
bowling_metric <- c('Wickets', 'Bowling Average', 'Economy')

print(names(batting_data))

# 1 All filters
venue_all_batting <- batting_data %>%
  group_by(Striker, Season, Bowler, Batting_Team, Bowling_Team) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls), Run_Out=sum(Run_Out)) %>%
  mutate(Stadium='All') %>%
  mutate(Average = Runs/Dismissed, Strike_Rate = Runs/Balls*100)

bowling_team_all_batting <- batting_data %>%
  group_by(Striker, Season, Bowler, Batting_Team, Stadium) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls), Run_Out=sum(Run_Out)) %>%
  mutate(Bowling_Team='All') %>%
  mutate(Average = Runs/Dismissed, Strike_Rate = Runs/Balls*100)

batting_team_all_batting <- batting_data %>%
  group_by(Striker, Season, Bowler, Bowling_Team, Stadium) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls), Run_Out=sum(Run_Out)) %>%
  mutate(Batting_Team='All') %>%
  mutate(Average = Runs/Dismissed, Strike_Rate = Runs/Balls*100)

bowler_all_batting <- batting_data %>%
  group_by(Striker, Season, Batting_Team, Bowling_Team, Stadium) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls), Run_Out=sum(Run_Out)) %>%
  mutate(Bowler='All') %>%
  mutate(Average = Runs/(Dismissed-Run_Out), Strike_Rate = Runs/Balls*100)

# 2 All filters
bowling_team_venue_all_batting <- batting_data %>%
  group_by(Striker, Season, Bowler, Batting_Team) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls), Run_Out=sum(Run_Out)) %>%
  mutate(Stadium='All', Bowling_Team='All') %>%
  mutate(Average = Runs/Dismissed, Strike_Rate = Runs/Balls*100)

batting_team_venue_all_batting <- batting_data %>%
  group_by(Striker, Season, Bowler, Bowling_Team) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls), Run_Out=sum(Run_Out)) %>%
  mutate(Stadium='All', Batting_Team='All') %>%
  mutate(Average = Runs/Dismissed, Strike_Rate = Runs/Balls*100)

bowler_venue_all_batting <- batting_data %>%
  group_by(Striker, Season, Batting_Team, Bowling_Team) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls), Run_Out=sum(Run_Out)) %>%
  mutate(Bowler='All', Stadium='All') %>%
  mutate(Average = Runs/Dismissed, Strike_Rate = Runs/Balls*100)


batting_team_bowling_team_all_batting <- batting_data %>%
  group_by(Striker, Season, Bowler, Stadium) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls), Run_Out=sum(Run_Out)) %>%
  mutate(Bowling_Team='All', Batting_Team='All') %>%
  mutate(Average = Runs/Dismissed, Strike_Rate = Runs/Balls*100)

bowler_bowling_team_all_batting <- batting_data %>%
  group_by(Striker, Season, Batting_Team, Stadium) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls), Run_Out=sum(Run_Out)) %>%
  mutate(Bowling_Team='All', Bowler='All') %>%
  mutate(Average = Runs/Dismissed, Strike_Rate = Runs/Balls*100)


bowler_batting_team_all_batting <- batting_data %>%
  group_by(Striker, Season, Bowling_Team, Stadium) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls), Run_Out=sum(Run_Out)) %>%
  mutate(Batting_Team='All', Bowler='All') %>%
  mutate(Average = Runs/Dismissed, Strike_Rate = Runs/Balls*100)


# 3 All filter
batting_team_bowling_team_venue_all_batting <- batting_data %>%
  group_by(Striker, Season, Bowler) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls), Run_Out=sum(Run_Out)) %>%
  mutate(Stadium='All', Bowling_Team='All', Batting_Team='All') %>%
  mutate(Average = Runs/Dismissed, Strike_Rate = Runs/Balls*100)

bowler_batting_team_venue_all_batting <- batting_data %>%
  group_by(Striker, Season, Bowling_Team) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls), Run_Out=sum(Run_Out)) %>%
  mutate(Stadium='All', Bowler='All', Batting_Team='All') %>%
  mutate(Average = Runs/Dismissed, Strike_Rate = Runs/Balls*100)

bowler_bowling_team_venue_all_batting <- batting_data %>%
  group_by(Striker, Season, Batting_Team) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls), Run_Out=sum(Run_Out)) %>%
  mutate(Stadium='All', Bowler='All', Bowling_Team='All') %>%
  mutate(Average = Runs/Dismissed, Strike_Rate = Runs/Balls*100)

bowler_batting_team_bowling_team_all_batting <- batting_data %>%
  group_by(Striker, Season, Stadium) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls), Run_Out=sum(Run_Out)) %>%
  mutate(Bowler='All', Bowling_Team='All', Batting_Team='All') %>%
  mutate(Average = Runs/Dismissed, Strike_Rate = Runs/Balls*100)


# 4 filters
bowler_batting_team_bowling_team_venue_all_batting <- batting_data %>%
  group_by(Striker, Season) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls), Run_Out=sum(Run_Out)) %>%
  mutate(Bowler='All', Bowling_Team='All', Batting_Team='All', Stadium='All') %>%
  mutate(Average = Runs/Dismissed, Strike_Rate = Runs/Balls*100)



# 1 All filters
venue_all_bowling <- bowling_data %>%
  group_by(Bowler, Season, Striker, Batting_Team, Bowling_Team) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Stadium='All') %>%
  mutate(Average=Runs/Wickets, Economy=Runs/Balls*6)

bowling_team_all_bowling <- bowling_data %>%
  group_by(Bowler, Season, Striker, Batting_Team, Stadium) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Bowling_Team='All') %>%
  mutate(Average=Runs/Wickets, Economy=Runs/Balls*6)

batting_team_all_bowling <- bowling_data %>%
  group_by(Bowler, Season, Striker, Bowling_Team, Stadium) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Batting_Team='All') %>%
  mutate(Average=Runs/Wickets, Economy=Runs/Balls*6)

striker_all_bowling <- bowling_data %>%
  group_by(Bowler, Season, Batting_Team, Bowling_Team, Stadium) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Striker='All') %>%
  mutate(Average=Runs/Wickets, Economy=Runs/Balls*6)

# 2 All filters
bowling_team_venue_all_bowling <- bowling_data %>%
  group_by(Bowler, Season, Striker, Batting_Team) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Stadium='All', Bowling_Team='All') %>%
  mutate(Average=Runs/Wickets, Economy=Runs/Balls*6)

batting_team_venue_all_bowling <- bowling_data %>%
  group_by(Bowler, Season, Striker, Bowling_Team) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Stadium='All', Batting_Team='All') %>%
  mutate(Average=Runs/Wickets, Economy=Runs/Balls*6)

striker_venue_all_bowling <- bowling_data %>%
  group_by(Bowler, Season, Batting_Team, Bowling_Team) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Striker='All', Stadium='All') %>%
  mutate(Average=Runs/Wickets, Economy=Runs/Balls*6)


bowling_team_batting_team_all_bowling <- bowling_data %>%
  group_by(Bowler, Season, Striker, Stadium) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Bowling_Team='All', Batting_Team='All') %>%
  mutate(Average=Runs/Wickets, Economy=Runs/Balls*6)

striker_bowling_team_all_bowling <- bowling_data %>%
  group_by(Bowler, Season, Batting_Team, Stadium) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Bowling_Team='All', Striker='All') %>%
  mutate(Average=Runs/Wickets, Economy=Runs/Balls*6)


striker_batting_team_all_bowling <- bowling_data %>%
  group_by(Bowler, Season, Bowling_Team, Stadium) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Batting_Team='All', Striker='All') %>%
  mutate(Average=Runs/Wickets, Economy=Runs/Balls*6)


# 3 All filter
bowling_team_batting_team_venue_all_bowling <- bowling_data %>%
  group_by(Bowler, Season, Striker) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Stadium='All', Bowling_Team='All', Batting_Team='All') %>%
  mutate(Average=Runs/Wickets, Economy=Runs/Balls*6)

striker_batting_team_venue_all_bowling <- bowling_data %>%
  group_by(Bowler, Season, Bowling_Team) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Stadium='All', Striker='All', Batting_Team='All') %>%
  mutate(Average=Runs/Wickets, Economy=Runs/Balls*6)

striker_bowling_team_venue_all_bowling <- bowling_data %>%
  group_by(Bowler, Season, Batting_Team) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Stadium='All', Striker='All', Bowling_Team='All') %>%
  mutate(Average=Runs/Wickets, Economy=Runs/Balls*6)

striker_bowling_team_batting_team_all_bowling <- bowling_data %>%
  group_by(Bowler, Season, Stadium) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Striker='All', Bowling_Team='All', Batting_Team='All') %>%
  mutate(Average=Runs/Wickets, Economy=Runs/Balls*6)


# 4 filters
striker_bowling_team_batting_team_venue_all_bowling <- bowling_data %>%
  group_by(Bowler, Season) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Striker='All', Bowling_Team='All', Batting_Team='All', Stadium='All') %>%
  mutate(Average=Runs/Wickets, Economy=Runs/Balls*6)


# Team Batting Data
# 1 filter
venue_team_all_batting <- team_batting_data %>%
  group_by(Batting_Team, Striker, Season, Bowling_Team) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Stadium='All') %>%
  mutate(Average = Runs/Dismissed, Strike_Rate = Runs/Balls*100)

bowling_team_team_all_batting <- team_batting_data %>%
  group_by(Batting_Team, Striker, Season, Stadium) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Bowling_Team='All') %>%
  mutate(Average = Runs/Dismissed, Strike_Rate = Runs/Balls*100)

season_team_all_batting <- team_batting_data %>%
  group_by(Batting_Team, Striker, Bowling_Team, Stadium) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Season='All') %>%
  mutate(Average = Runs/Dismissed, Strike_Rate = Runs/Balls*100)

# 2 filter
bowling_team_venue_team_all_batting <- team_batting_data %>%
  group_by(Batting_Team, Striker, Season) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Bowling_Team='All', Stadium='All') %>%
  mutate(Average = Runs/Dismissed, Strike_Rate = Runs/Balls*100)


season_venue_team_all_batting <- team_batting_data %>%
  group_by(Batting_Team, Striker, Bowling_Team) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Season='All', Stadium='All') %>%
  mutate(Average = Runs/Dismissed, Strike_Rate = Runs/Balls*100)

season_bowling_team_team_all_batting <- team_batting_data %>%
  group_by(Batting_Team, Striker, Stadium) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Bowling_Team='All', Season='All') %>%
  mutate(Average = Runs/Dismissed, Strike_Rate = Runs/Balls*100)


# 3 filter
season_bowling_team_venue_team_all_batting <- team_batting_data %>%
  group_by(Batting_Team, Striker) %>%
  summarise(Runs = sum(Runs), Dismissed = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Bowling_Team='All', Stadium='All', Season='All') %>%
  mutate(Average = Runs/Dismissed, Strike_Rate = Runs/Balls*100)


# Team Bowling Data
# 1 filter
venue_team_all_bowling <- team_bowling_data %>%
  group_by(Bowling_Team, Bowler, Season, Batting_Team) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Stadium='All') %>%
  mutate(Average = Runs/Wickets, Economy = Runs/Balls*6)

batting_team_team_all_bowling <- team_bowling_data %>%
  group_by(Bowling_Team, Bowler, Season, Stadium) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Batting_Team='All') %>%
  mutate(Average = Runs/Wickets, Economy = Runs/Balls*6)

season_team_all_bowling <- team_bowling_data %>%
  group_by(Bowling_Team, Bowler, Batting_Team, Stadium) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Season='All') %>%
  mutate(Average = Runs/Wickets, Economy = Runs/Balls*6)

# 2 filter
batting_team_venue_team_all_bowling <- team_bowling_data %>%
  group_by(Bowling_Team, Bowler, Season) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Batting_Team='All', Stadium='All') %>%
  mutate(Average = Runs/Wickets, Economy = Runs/Balls*6)


season_venue_team_all_bowling <- team_bowling_data %>%
  group_by(Bowling_Team, Bowler, Batting_Team) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Season='All', Stadium='All') %>%
  mutate(Average = Runs/Wickets, Economy = Runs/Balls*6)


season_batting_team_team_all_bowling <- team_bowling_data %>%
  group_by(Bowling_Team, Bowler, Stadium) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Season='All', Batting_Team='All') %>%
  mutate(Average = Runs/Wickets, Economy = Runs/Balls*6)


# 3 filter
season_batting_team_venue_team_all_bowling <- team_bowling_data %>%
  group_by(Bowling_Team, Bowler) %>%
  summarise(Runs = sum(Runs), Wickets = sum(Wickets), Balls = sum(Balls)) %>%
  mutate(Batting_Team='All', Stadium='All', Season='All') %>%
  mutate(Average = Runs/Wickets, Economy = Runs/Balls*6)


# Storing Data into csv files
dataframes <- ls(pattern = '_data')
dataframes <- c(dataframes, ls(pattern = '_batting'))
dataframes <- c(dataframes, ls(pattern = '_bowling'))


for (df in dataframes){
  path <- file.path('preprocessed_data', paste0(df, '.csv'))
  write.csv(get(df), path, row.names = FALSE)
}


