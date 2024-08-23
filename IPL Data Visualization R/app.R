

library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(RMySQL)
library(stringr)


csv_files <- list.files(path = "preprocessed_data", pattern = "\\.csv$", full.names = TRUE)
data <- read.csv('main_data/all_matches.csv')
match_data <- read.csv('main_data/match_data.csv')

for (csv_file in csv_files) {
  dataframe_name <- tools::file_path_sans_ext(basename(csv_file))
  assign(dataframe_name, read.csv(csv_file))
}


players <- sort(unique(c(data$Striker, data$Non_Striker, data$Bowler)))
venues <- sort(unique(match_data$Stadium))
teams <- sort(unique(c(data$Batting_Team, data$Bowling_Team)))
batting_metric <- c('Runs', 'Batting Average', 'Strike Rate')
bowling_metric <- c('Wickets', 'Bowling Average', 'Economy')


# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "purple",
                    dashboardHeader(
                      title = "IPL Data Visualization"
                    ),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("About", tabName = "about"),
                        menuItem("Players", tabName = 'players'),
                        menuItem('Teams', tabName = 'teams'),
                        menuItem('Summary', tabName = 'summary')
                      )
                    ),
                    
                    dashboardBody(
                      tabItems(
                      tabItem("about",
                        mainPanel(
                          h1('IPL Insights: Unveiling Trends Through Data'),
                          p(' '),
                          p(' '),
                          div(
                            style = "font-family: 'Times New Roman'; text-align: justify;font-size: 15px;",
                            HTML('The Indian Premier League, commonly known as the IPL, is an annual multi-franchise-based T20 league
established in 2008. Often hailed as the biggest festival in the country, the Indian Premier League currently
holds the pinnacle position in world cricket. It is also one of the most valuable sporting leagues globally,
second only to the NFL. Cricketers from all around the world participate in it, creating a battle among the
biggest stars of world cricket. In this league, anything is possible, and no one is declared a winner until the
last ball is delivered.')),
                          p(' '),
                          div(
                            style = "font-family: 'Times New Roman'; text-align: justify;font-size: 15px;",
                            HTML('Cricket has always been statistics-driven since its early days, and with the advent of modern technology,
data analysis has become an integral part of the decision-making process. The IPL is no exception, as every
team employs its own data analysis team to provide insights on auction strategies, team formation, possible
match-ups, and more. In one interview, a data analyst revealed that they collected over 140 data points for
each ball delivered'),
                          ),
                          

                        )
                      ),
                      
                      tabItem('players',
                        fluidPage(
                          fluidRow(
                            div(style = "display: inline-block; width: 33%;",selectInput('Player_1', h4('Player'), choices = players, selected = 'Virat Kohli')),
                            div(style = "display: inline-block; width: 33%;",selectInput('Player_a1',  h4('Against Player'), choices = c('All',players))),
                            div(style = "display: inline-block; width: 33%;",selectInput('Team_1', h4('Team'), choices = c('All', teams))),
                            div(style = "display: inline-block; width: 33%;",selectInput('Team_a1', h4('Against Team'), choices = c('All', teams))),
                            div(style = "display: inline-block; width: 33%;",selectInput('Venue_1', h4('Venue'), choices = c('All', venues))),
                          ),
                          fluidRow(
                            checkboxGroupInput('Metric_1', h4('Metrics'), choices = c('Runs', 'Wickets', 'Batting Average', 'Bowling Average', 'Strike Rate', 'Economy'), selected = 'Runs', inline = TRUE)
                          ),
                        
                          fluidRow(
                            column(6, h4('Batting Stats'), tableOutput('Bat_1')),
                            column(6, h4('Bowling Stats'), tableOutput('Bowl_1')),
                          ),
                        plotOutput('plot_1')
                        )
                      ),
                      tabItem('teams',
                        fluidPage(
                          fluidRow(
                            div(style = "display: inline-block; width: 33%;",selectInput('Team_2', h4('Team'), choices = c(teams), selected = 'Kolkata Knight Riders')),
                            div(style = "display: inline-block; width: 33%;",selectInput('Team_a2', h4('Against Team'), choices = c('All', teams))),
                            div(style = "display: inline-block; width: 33%;",selectInput('Venue_2', h4('Venue'), choices = c('All', venues))),
                            div(style = "display: inline-block; width: 33%;",selectInput('Season_2', h4('Season'), choices = c('All', seq(2008, 2022))))
                          ),
                          fluidRow(
                            selectInput('Metric_2', h4('Metrics'), choices = c('Runs', 'Wickets', 'Batting Average', 'Bowling Average', 'Strike Rate', 'Economy'))
                          ),
                          tableOutput('Bat_2'),
                          plotOutput('plot_2')
                        )        
                      ),
                      tabItem('summary',
                              h1('Summary'),
                              div(
                                style = "font-family: 'Times New Roman'; text-align: justify;font-size: 20px;",
                                HTML('1) The batters who are top scorers in the IPL are also top scorers at all the venues on average, and their
home ground doesn’t significantly inflate their run-scoring.')),
                              p(),
                              div(
                                style = "font-family: 'Times New Roman'; text-align: justify;font-size: 20px;",
                                HTML('2) Having a high average doesn’t necessarily make a player the best batter, as strike rate also plays a
crucial role in the IPL.')),
                              p(),
                              div(
                                style = "font-family: 'Times New Roman'; text-align: justify;font-size: 20px;",
                                HTML('3) To get a sense of the best average of a particular team, we need to consider a threshold of matches played by the players.'))
                      )
                    )),
                    
                    )



server <- function(input, output){
  
  bat_dynamic<- reactive({
    df_name <- paste(
      if (input$Player_a1 == "All") "bowler_",
      if (input$Team_1 == "All") "batting_team_",
      if (input$Team_a1 == "All") "bowling_team_",
      if (input$Venue_1 == "All") "venue_",
      "all_batting",
      sep = ''
    )
    if (df_name == "all_batting"){
      df_name <- 'batting_data'
    }
    df <- get(df_name)
    df %>% filter(Striker == input$Player_1,
                  Bowler == input$Player_a1,
                  Batting_Team == input$Team_1,
                  Bowling_Team == input$Team_a1 ,
                  Stadium == input$Venue_1) %>%
      select(-c(Bowler, Batting_Team, Bowling_Team, Stadium, Run_Out))
  })
  
  
  
  bowl_dynamic <- reactive({
    df_name <- paste(
      if (input$Player_a1 == "All") "striker_",
      if (input$Team_1 == "All") "bowling_team_",
      if (input$Team_a1 == "All") "batting_team_",
      if (input$Venue_1 == "All") "venue_",
      "all_bowling",
      sep = ''
    )
    if (df_name == "all_bowling"){
      df_name <- 'bowling_data'
    }
    df <- get(df_name)
    df %>% filter(Striker == input$Player_a1,
                  Bowler == input$Player_1,
                  Bowling_Team == input$Team_1,
                  Batting_Team == input$Team_a1 ,
                  Stadium == input$Venue_1) %>%
      select(-c(Striker, Batting_Team, Bowling_Team, Stadium))
    
    
  })
  
  
  output$Bat_1 <- renderTable(bat_dynamic())
  output$Bowl_1 <- renderTable(bowl_dynamic())
  
  output$plot_1 <- renderPlot({
  gg <- ggplot()
  

  if ('Runs' %in% input$Metric_1){
    gg <- gg +
      geom_line(data=bat_dynamic(), aes(x=Season, y=Runs, color="Run"), linewidth=1)
  }
  if('Batting Average' %in% input$Metric_1){
    gg <- gg +
      geom_line(data=bat_dynamic(), aes(x=Season, y=Average, color="Batting_Average"), linewidth=1)
  }
  if('Strike Rate' %in% input$Metric_1){
    gg <- gg +
      geom_line(data=bat_dynamic(), aes(x=Season, y=Strike_Rate, color="Strike Rate"), linewidth=1)
  }
  
  if ('Wickets' %in% input$Metric_1){
    gg <- gg +
      geom_line(data=bowl_dynamic(), aes(x=Season, y=Wickets, color="Wickets"), linewidth=1)
  }
  
  if ('Economy' %in% input$Metric_1){
    gg <- gg +
      geom_line(data=bowl_dynamic(), aes(x=Season, y=Economy, color="Economy"), linewidth=1)
  }
  
  if ('Bowling Average' %in% input$Metric_1){
    gg <- gg +
      geom_line(data=bowl_dynamic(), aes(x=Season, y=Average, color="Bowling Average"), linewidth=1)
  }
  
  gg <- gg + 
    labs(
      y = "",
      color = 'Metrics'
    )
  gg
  })
  
  
  # Team's tab
  
  team_bat_dynamic <- reactive({
    df_name <- paste(
      if (input$Season_2 == "All") "season_",
      if (input$Team_a2 == "All") "bowling_team_",
      if (input$Venue_2 == "All") "venue_",
      "team_all_batting",
      sep = ''
    )
    if (df_name=="team_all_batting"){
      df_name <- "team_batting_data"
    }
    filter_data <- get(df_name)
    filter_data <- filter_data %>% filter(Batting_Team == input$Team_2,
                                          Season == input$Season_2,
                                          Bowling_Team == input$Team_a2,
                                          Stadium == input$Venue_2) %>%
      select(-c(Batting_Team, Bowling_Team, Stadium, Season))
    
    if ('Runs' %in% input$Metric_2){
      filter_data <- filter_data  %>%
        arrange(desc(Runs)) %>%
        slice_head(n=10)
    }
    if('Batting Average' %in% input$Metric_2){
      filter_data <- filter_data  %>%
        filter(!is.infinite(Average)) %>%
        arrange(desc(Average)) %>%
        slice_head(n=10)
    }
    if('Strike Rate' %in% input$Metric_2){
      filter_data <- filter_data  %>%
        arrange(desc(Strike_Rate)) %>%
        slice_head(n=10)
    }
    
    return(filter_data)
  })
  
  
  team_bowl_dynamic <- reactive({
    df_name <- paste(
      if (input$Season_2 == "All") "season_",
      if (input$Team_a2 == "All") "batting_team_",
      if (input$Venue_2 == "All") "venue_",
      "team_all_bowling",
      sep = ''
    )
    if (df_name=="team_all_bowling"){
      df_name <- "team_bowling_data"
    }
    filter_data <- get(df_name)
    filter_data <- filter_data %>% filter(Bowling_Team == input$Team_2,
                                          Season == input$Season_2,
                                          Batting_Team == input$Team_a2,
                                          Stadium == input$Venue_2) %>%
      select(-c(Batting_Team, Bowling_Team, Stadium, Season))
    
    if ('Wickets' %in% input$Metric_2){
      filter_data <- filter_data  %>%
        arrange(desc(Wickets)) %>%
        slice_head(n=10)
    }
    
    if ('Economy' %in% input$Metric_2){
      filter_data <- filter_data  %>%
        arrange(Economy) %>%
        slice_head(n=10)
    }
    
    if ('Bowling Average' %in% input$Metric_2){
      filter_data <- filter_data  %>%
        arrange(Average) %>%
        slice_head(n=10)
    }
    
    return(filter_data)
  })
  
  observe({
    if (input$Metric_2 %in% batting_metric) {
      output$Bat_2 <- renderTable({
        team_bat_dynamic()
      })
    } else {
      output$Bat_2 <- renderTable({
        team_bowl_dynamic()
      })
    }
  })
  
  output$plot_2 <- renderPlot({
    gg <- ggplot()
    
    
    if ('Runs' %in% input$Metric_2){
      gg <- ggplot(team_bat_dynamic(), aes(x= reorder(Striker, -Runs), y = Runs)) +
        geom_bar(stat = 'identity', fill='gold')
    }
    if('Batting Average' %in% input$Metric_2){
      gg <- ggplot(team_bat_dynamic(), aes(x= reorder(Striker, -Average), y = Average)) +
        geom_bar(stat = 'identity', fill='gold')
    }
    if('Strike Rate' %in% input$Metric_2){
      gg <- ggplot(team_bat_dynamic(), aes(x= reorder(Striker, -Strike_Rate), y = Strike_Rate)) +
        geom_bar(stat = 'identity', fill='gold')
    }
    
    if ('Wickets' %in% input$Metric_2){
      gg <- ggplot(team_bowl_dynamic(), aes(x= reorder(Bowler, -Wickets), y = Wickets)) +
        geom_bar(stat = 'identity', fill='gold')
    }
    
    if ('Economy' %in% input$Metric_2){
      gg <- ggplot(team_bowl_dynamic(), aes(x= reorder(Bowler, Economy), y = Economy)) +
        geom_bar(stat = 'identity', fill='gold')
    }
    
    if ('Bowling Average' %in% input$Metric_2){
      gg <- ggplot(team_bowl_dynamic(), aes(x= reorder(Bowler, Average), y = Average)) +
        geom_bar(stat = 'identity', fill='gold')
    }
    
    gg <- gg +
      labs(x = 'Player')+
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    gg
  })
}

shinyApp(ui, server)

