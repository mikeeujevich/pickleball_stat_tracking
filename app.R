library(shiny) # For app
library(dplyr) # For data wrangling
library(stringr) # For string manipulation
library(shinyWidgets) # May use for widgets
library(DT) # Data tables
library(sqldf) # Create StatSheet
library(shinyjs)

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(".space-below-button { margin-top: 45px; }")
    )
  ),
  titlePanel("Pickleball Stat Tracker"),
  tabsetPanel(
    tabPanel("Player Evaluation: Data Entry",
    sidebarLayout(
    sidebarPanel(
      # Player Entry
      fluidRow(column(4,
      textInput("team1_player1", "Team 1 Player 1 Name", "")),
      column(4,
      textInput("team1_player2", "Team 1 Player 2 Name", ""))),
      fluidRow(column(4,
      textInput("team2_player1", "Team 2 Player 1 Name", "")),
      column(4,
      textInput("team2_player2", "Team 2 Player 2 Name", ""))),
      selectInput("active_player", "Active Player", choices = "", selected = ""),
      fluidRow(
        column(6,
               dateInput("selected_date", "Select Date:", value = Sys.Date())
        ),
        column(6,
               radioButtons("game_type", "Select Game Type:",
                            choices = c("Singles", "Doubles"),
                            selected = "Doubles")
        )
      ),
      h3("Actions"),
      actionButton("undo_action", "Undo Action"), # Undo button action most recently taken
      actionButton("clear_teams", "Clear Teams"),  # Clear team names
      actionButton("end_game", "End Game"),  # Will end the game logging, and reset the form
      # New Serve Section
      h3("Serve"),
      actionButton("record_serve_ace", "Serve Ace"),
      actionButton("record_serve", "Serve In-Play"),
      actionButton("record_serve_error", "Serve Error"),
      # New Serve Return Section
      h3("Serve Return"),
      actionButton("record_serve_return", "Serve Return In-Play Normal"),
      actionButton("record_serve_return_deep", "Serve Return In-Play Deep"),
      actionButton("record_serve_return_error", "Serve Return Error"),
      # New Third Shot Section
      h3("Third Shot"),
      actionButton("record_third_shot_drop", "Third Shot Drop In-Play"),
      actionButton("record_third_shot_drop_error", "Third Shot Drop Error"),
      actionButton("record_third_shot_drive", "Third Shot Drive In-Play"),
      actionButton("record_third_shot_drive_error", "Third Shot Drive Error"),
      actionButton("record_third_shot_lob", "Third Shot Lob In-Play"),
      actionButton("record_third_shot_lob_error", "Third Shot Lob Error"),
      # New Post Third Shot Section
      h3("Post Third Shot"),
      actionButton("record_dink", "Dink In-Play"),
      actionButton("record_dink_error", "Dink Error"),
      actionButton("record_drive", "Drive In-Play"),
      actionButton("record_drive_error", "Drive Error"),
      actionButton("record_speed_up", "Speed Up In-Play"),
      actionButton("record_speed_up_error", "Speed Up Error"),
      actionButton("record_defend", "Defend In-Play"),
      actionButton("record_defend_error", "Defend Error"),
      actionButton("record_lob", "Lob In-Play"),
      actionButton("record_lob_error", "Lob Error"),
      actionButton("record_overhead", "Overhead In-Play"),
      actionButton("record_overhead_error", "Overhead Error"),
      actionButton("record_reset", "Reset In-Play By"),
      actionButton("record_reset_error", "Reset Error")
    ),
    mainPanel(
      # YouTube video container
     uiOutput("youtube_container"),
     # Game History section
     column(8,h3("Game History"),
     verbatimTextOutput("game_history"))
    )
  )
),

    tabPanel("Player Evaluation: Statistics",  # Add a new tab for the game history
             mainPanel(
               h3("Daily Performance History"),
               selectInput("player_selector", "Select a Player", choices = NULL),
               DTOutput("game_history_table"),  # Display game history in a table
               h3("Career Performance History"),
               DTOutput("career_history_table")
             )
    )
  )
)

# Initializing
server <- function(input, output, session) {
  output$youtube_container <- renderUI({
    fluidPage(
      tags$head(
        tags$style(
          HTML(".add-margin-top { margin-top: 20px; }")
        )
      ),
      fluidRow(
        column(
          width = 2,
          tags$div(
            class = "add-margin-top",
            textInput("youtube_link", "YouTube Video Link")
          )
        ),
        column(
          width = 2, # Adjust the width as needed
          # offset = -10, # Add an offset to create spacing
          actionButton("embed_youtube", "Embed YouTube Video", class = "space-below-button")
        )
      ),
      fluidRow(
        column(
          width = 6,
          uiOutput("youtube_player")
        )
      )
    )
  })
  
  observeEvent(input$embed_youtube, {
    video_link <- input$youtube_link
    if (!is.null(video_link) && video_link != "") {
      output$youtube_player <- renderUI({
        HTML(paste0('<iframe width="135%" height="400" src="', video_link, '" frameborder="0" allowfullscreen></iframe>'))
      })
      shinyjs::show("youtube_player")
    }
  })
  
  active_player <- reactiveVal("")
  game_history <- reactiveVal(character(0))
  player_names <- reactiveVal(list(
    "Team 1 Player 1" = "",
    "Team 1 Player 2" = "",
    "Team 2 Player 1" = "",
    "Team 2 Player 2" = ""
  ))
  
  observe({
    # Update the active player drop down based on player names
    team1_players <- c(input$team1_player1, input$team1_player2)
    team2_players <- c(input$team2_player1, input$team2_player2)
    all_players <- c(team1_players, team2_players)
    updateSelectInput(session, "active_player", choices = all_players, selected = input$active_player)
    # Update the player names for use in game history
    player_names(list(
      "Team 1 Player 1" = input$team1_player1,
      "Team 1 Player 2" = input$team1_player2,
      "Team 2 Player 1" = input$team2_player1,
      "Team 2 Player 2" = input$team2_player2
    ))
  })
  
  # This function will be used for generating a unique game ID
  randGame <- function(n = 5000) {
    a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
    paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
  }
  
  # Create a data frame to store game history
  game_history_data <- data.frame(Game = character(0), GameType = character(0), Date = character(0), Action = character(0), Team = character(0), Player = character(0))
  
  # Undo the most recent entry into the game history
  observeEvent(input$undo_action, {
    if (length(game_history()) > 0) {
      game_history(game_history()[-1])
    }
  })
  
  output$active_player <- renderText({
    paste("Active Player:", input$active_player)
  })
  
  # Ensure the game history is creating a new line for each action
  output$game_history <- renderText({
    paste(game_history(), collapse = "\n")
  })
  
  # Function to get the team affiliation of a player
  getTeam <- function(player_name) {
    if (player_name %in% c(input$team1_player1, input$team1_player2)) {
      return("Team 1")
    } else {
      return("Team 2")
    }
  }
  
  # Serve Ace button action
  observeEvent(input$record_serve_ace, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Serve Ace by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Serve In-Play button action
  observeEvent(input$record_serve, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Serve In-Play by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Serve Error button action
  observeEvent(input$record_serve_error, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Serve Error by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Serve Return In-Play Normal button action
  observeEvent(input$record_serve_return, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Serve Return In-Play Normal by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Serve Return Deep button action
  observeEvent(input$record_serve_return_deep, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Serve Return In-Play Deep by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Serve Return Error button action
  observeEvent(input$record_serve_return_error, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Serve Return Error by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Third Shot Drop In-Play button action
  observeEvent(input$record_third_shot_drop, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Third Shot Drop In-Play by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Third Shot Drop Error button action
  observeEvent(input$record_third_shot_drop_error, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Third Shot Drop Error by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Third Shot Drive In-Play button action
  observeEvent(input$record_third_shot_drive, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Third Shot Drive In-Play by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Third Shot Drive Error button action
  observeEvent(input$record_third_shot_drive_error, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Third Shot Drive Error by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Third Shot Lob In-Play button action
  observeEvent(input$record_third_shot_lob, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Third Shot Lob In-Play by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Third Shot Lob Error button action
  observeEvent(input$record_third_shot_lob_error, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Third Shot Lob Error by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Post Third Shot - Dink In-Play button action
  observeEvent(input$record_dink, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Dink In-Play by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Post Third Shot - Dink Error button action
  observeEvent(input$record_dink_error, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Dink Error by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Post Third Shot - Drive In-Play button action
  observeEvent(input$record_drive, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Drive In-Play by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Post Third Shot - Drive Error button action
  observeEvent(input$record_drive_error, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Drive Error by", team, ":", input$active_player), game_history()))
    }
  })

  # Post Third Shot - Speed Up In-Play button action
  observeEvent(input$record_speed_up, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Speed Up In-Play by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Post Third Shot - Speed Up Success button action
  observeEvent(input$record_speed_up_error, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Speed Up Error by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Post Third Shot - Defend In-Play button action
  observeEvent(input$record_defend, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Defend In-Play by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Post Third Shot - Defend Error button action
  observeEvent(input$record_defend_error, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Defend Error by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Post Third Shot - Lob In-Play button action
  observeEvent(input$record_lob, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Lob In-Play by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Post Third Shot - Lob Error button action
  observeEvent(input$record_lob_error, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Lob Error by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Post Third Shot - Overhead In-Play button action
  observeEvent(input$record_overhead, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Overhead In-Play by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Post Third Shot - Overhead Error button action
  observeEvent(input$record_overhead_error, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Overhead Error by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Post Third Shot - Reset In-Play button action
  observeEvent(input$record_reset, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Reset In-Play by", team, ":", input$active_player), game_history()))
    }
  })
  
  # Post Third Shot - Reset Error button action
  observeEvent(input$record_reset_error, {
    if (input$active_player != "") {
      team <- getTeam(input$active_player)
      game_history(c(paste("Reset Error by", team, ":", input$active_player), game_history()))
    }
  })

  # Clear player names from the text boxes
  observeEvent(input$clear_teams, {
    updateTextInput(session, "team1_player1", value = "")
    updateTextInput(session, "team1_player2", value = "")
    updateTextInput(session, "team2_player1", value = "")
    updateTextInput(session, "team2_player2", value = "")
  })
  
  # End game button to clear the game history, and record the game in a local database
  observeEvent(input$end_game, {
    if (length(game_history()) > 0) {
      actions <- game_history()
      players <- sapply(actions, function(action) {
        parts <- unlist(strsplit(action, ":"))
        if (length(parts) >= 2) return(trimws(parts[2]))
        else return("")
      })
      teams <- str_sub(sub("\\:.*", "", actions),-7,-1)
      actions <- sub(" by .*", "", actions)
      # Add a game code, the game type, and date to the data frame
      game_history_data <<- data.frame(Game = randGame(1),
                                       GameType = input$game_type,
                                       Date = input$selected_date,
                                       Action = actions,
                                       Team = teams,
                                       Player = players)
      
      # Export the data frame to a CSV file
      write.table(game_history_data, "game_history.csv", sep = ",", row.names = F, col.names = !file.exists("game_history.csv"), append = T)
      
      # Clear game history from the UI
      game_history(character(0))
    }
  })

  # Function to load game history data from the CSV file
  loadGameHistoryData <- function() {
    if (file.exists("game_history.csv")) {
      game_history_data <- read.csv("game_history.csv", stringsAsFactors = FALSE)
      new_df <- data.frame(sqldf('
      SELECT
      GameType, Player, Date, 
      COUNT(DISTINCT Game) AS games,
    
      COUNT(CASE WHEN Action IN ("Serve In-Play","Serve Error","Serve Ace") THEN Action END) AS serves,
      ROUND(CAST(COUNT(CASE WHEN Action IN ("Serve In-Play","Serve Ace") THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action IN ("Serve In-Play","Serve Error","Serve Ace") THEN Action END),0),4) serve_accuracy_rate,
      ROUND(CAST(COUNT(CASE WHEN Action = "Serve Ace" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action IN ("Serve In-Play","Serve Error","Serve Ace") THEN Action END),0),4) serve_ace_rate,
    
      COUNT(CASE WHEN Action LIKE "%Serve Return%" THEN Action END) AS serve_returns,
      ROUND(CAST(COUNT(CASE WHEN Action IN ("Serve Return In-Play Normal","Serve Return In-Play Deep") THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Serve Return%" THEN Action END),0),4) serve_return_accuracy_rate,
      ROUND(CAST(COUNT(CASE WHEN Action = "Serve Return In-Play Deep" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action IN ("Serve Return In-Play Normal","Serve Return In-Play Deep") THEN Action END),0),4) serve_return_in_deep_rate,
    
      COUNT(CASE WHEN Action LIKE "%Third Shot%" THEN Action END) AS third_shots,
      ROUND(CAST(COUNT(CASE WHEN Action LIKE "%Third Shot Drive%" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Third Shot%" THEN Action END),0),4) AS third_shot_drive_tendency,
      ROUND(CAST(COUNT(CASE WHEN Action LIKE "%Third Shot Drop%" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Third Shot%" THEN Action END),0),4) AS third_shot_drop_tendency,
      ROUND(CAST(COUNT(CASE WHEN Action LIKE "%Third Shot Lob%" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Third Shot%" THEN Action END),0),4) AS third_shot_lob_tendency,
      ROUND(CAST(COUNT(CASE WHEN Action = "Third Shot Drive In-Play" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Third Shot Drive%" THEN Action END),0),4) AS third_shot_drive_accuracy_rate,
      ROUND(CAST(COUNT(CASE WHEN Action = "Third Shot Drop In-Play" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Third Shot Drop%" THEN Action END),0),4) AS  third_shot_drop_accuracy_rate,
      ROUND(CAST(COUNT(CASE WHEN Action = "Third Shot Lob In-Play" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Third Shot Lob%" THEN Action END),0),4) AS  third_shot_lob_accuracy_rate,
      ROUND(CAST(COUNT(CASE WHEN Action LIKE "%Third Shot%" AND Action LIKE "%In-Play%" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Third Shot%" THEN Action END),0),4) AS  total_third_shot_accuracy_rate,
    
      COUNT(CASE WHEN Action LIKE "%Defend%" THEN Action END) AS defends,
      ROUND(CAST(COUNT(CASE WHEN Action = "Defend In-Play" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Defend%" THEN Action END),0),4) defend_accuracy_rate,
      COUNT(CASE WHEN Action LIKE "%Dink%" THEN Action END) AS dinks,
      ROUND(CAST(COUNT(CASE WHEN Action = "Dink In-Play" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Dink%" THEN Action END),0),4) dink_accuracy_rate,
      COUNT(CASE WHEN Action IN ("Drive Error","Drive In-Play") THEN Action END) AS drives,
      ROUND(CAST(COUNT(CASE WHEN Action = "Drive In-Play" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action IN ("Drive Error","Drive In-Play") THEN Action END),0),4) drive_accuracy_rate,
      COUNT(CASE WHEN Action LIKE "%Overhead%" THEN Action END) AS overheads,
      ROUND(CAST(COUNT(CASE WHEN Action = "Overhead In-Play" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Overhead%" THEN Action END),0),4) overhead_accuracy_rate,
      COUNT(CASE WHEN Action IN ("Lob Error","Lob In-Play") THEN Action END) AS lobs,
      ROUND(CAST(COUNT(CASE WHEN Action = "Lob In-Play" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action IN ("Lob Error","Lob In-Play") THEN Action END),0),4) lob_accuracy_rate,
      COUNT(CASE WHEN Action LIKE "%Speed Up%" THEN Action END) AS speed_ups,
      ROUND(CAST(COUNT(CASE WHEN Action = "Speed Up In-Play" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Speed Up%" THEN Action END),0),4) speed_up_accuracy_rate,
      COUNT(CASE WHEN Action LIKE "%Reset%" THEN Action END) AS resets,
      ROUND(CAST(COUNT(CASE WHEN Action = "Reset In-Play" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Reset%" THEN Action END),0),4) reset_accuracy_rate
      FROM game_history_data
      GROUP BY GameType, Player, Date'))
      return(new_df)
    } else {
      return(NULL)
    }
  }
  
  # Render the game history table
  output$game_history_table <- renderDT({
    game_history_data <- loadGameHistoryData()
    if (!is.null(game_history_data)) {
      datatable(game_history_data, options = list(pageLength = 10),
                colnames = c('Row Number', 'Game Format','Player', 'Date','Games','Serves','Serve Accuracy Rate',
                             'Serve Ace Rate','Serve Returns','Serve Return Accuracy Rate',
                             'Serve Return In Deep Rate','Third Shots','Third Shot Drive Tendency',
                             'Third Shot Drop Tendency','Third Shot Lob Tendency','Third Shot Drive Accuracy Rate',
                             'Third Shot Drop Accuracy Rate','Third Shot Lob Accuracy Rate','Total Third Shot Accuracy Rate','Defends','Defend Accuracy Rate',
                             'Dinks','Dink Accuracy Rate','Drives','Drive Accuracy Rate','Overheads','Overhead Accuracy Rate',
                             'Lobs','Lob Accuracy Rate','Speed Ups', 'Speed Up Accuracy Rate','Resets','Reset Accuracy Rate')
      )
    }
  })
  
  # Function to load game history data from the CSV file
  loadCareerHistoryData <- function() {
    if (file.exists("game_history.csv")) {
      career_history_data <- read.csv("game_history.csv", stringsAsFactors = FALSE)
      new_df2 <- data.frame(sqldf('
      SELECT
      GameType, Player,
      COUNT(DISTINCT Date) AS dates,
      COUNT(DISTINCT Game) AS games,
    
      COUNT(CASE WHEN Action IN ("Serve In-Play","Serve Error","Serve Ace") THEN Action END) AS serves,
      ROUND(CAST(COUNT(CASE WHEN Action IN ("Serve In-Play","Serve Ace") THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action IN ("Serve In-Play","Serve Error","Serve Ace") THEN Action END),0),4) serve_accuracy_rate,
      ROUND(CAST(COUNT(CASE WHEN Action = "Serve Ace" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action IN ("Serve In-Play","Serve Error","Serve Ace") THEN Action END),0),4) serve_ace_rate,
    
      COUNT(CASE WHEN Action LIKE "%Serve Return%" THEN Action END) AS serve_returns,
      ROUND(CAST(COUNT(CASE WHEN Action IN ("Serve Return In-Play Normal","Serve Return In-Play Deep") THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Serve Return%" THEN Action END),0),4) serve_return_accuracy_rate,
      ROUND(CAST(COUNT(CASE WHEN Action = "Serve Return In-Play Deep" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action IN ("Serve Return In-Play Normal","Serve Return In-Play Deep") THEN Action END),0),4) serve_return_in_deep_rate,
    
      COUNT(CASE WHEN Action LIKE "%Third Shot%" THEN Action END) AS third_shots,
      ROUND(CAST(COUNT(CASE WHEN Action LIKE "%Third Shot Drive%" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Third Shot%" THEN Action END),0),4) AS third_shot_drive_tendency,
      ROUND(CAST(COUNT(CASE WHEN Action LIKE "%Third Shot Drop%" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Third Shot%" THEN Action END),0),4) AS third_shot_drop_tendency,
      ROUND(CAST(COUNT(CASE WHEN Action LIKE "%Third Shot Lob%" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Third Shot%" THEN Action END),0),4) AS third_shot_lob_tendency,
      ROUND(CAST(COUNT(CASE WHEN Action = "Third Shot Drive In-Play" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Third Shot Drive%" THEN Action END),0),4) AS third_shot_drive_accuracy_rate,
      ROUND(CAST(COUNT(CASE WHEN Action = "Third Shot Drop In-Play" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Third Shot Drop%" THEN Action END),0),4) AS  third_shot_drop_accuracy_rate,
      ROUND(CAST(COUNT(CASE WHEN Action = "Third Shot Lob In-Play" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Third Shot Lob%" THEN Action END),0),4) AS  third_shot_lob_accuracy_rate,
      ROUND(CAST(COUNT(CASE WHEN Action LIKE "%Third Shot%" AND Action LIKE "%In-Play%" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Third Shot%" THEN Action END),0),4) AS  total_third_shot_accuracy_rate,
    
      COUNT(CASE WHEN Action LIKE "%Defend%" THEN Action END) AS defends,
      ROUND(CAST(COUNT(CASE WHEN Action = "Defend In-Play" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Defend%" THEN Action END),0),4) defend_accuracy_rate,
      COUNT(CASE WHEN Action LIKE "%Dink%" THEN Action END) AS dinks,
      ROUND(CAST(COUNT(CASE WHEN Action = "Dink In-Play" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Dink%" THEN Action END),0),4) dink_accuracy_rate,
      COUNT(CASE WHEN Action IN ("Drive Error","Drive In-Play") THEN Action END) AS drives,
      ROUND(CAST(COUNT(CASE WHEN Action = "Drive In-Play" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action IN ("Drive Error","Drive In-Play") THEN Action END),0),4) drive_accuracy_rate,
      COUNT(CASE WHEN Action LIKE "%Overhead%" THEN Action END) AS overheads,
      ROUND(CAST(COUNT(CASE WHEN Action = "Overhead In-Play" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Overhead%" THEN Action END),0),4) overhead_accuracy_rate,
      COUNT(CASE WHEN Action IN ("Lob Error","Lob In-Play") THEN Action END) AS lobs,
      ROUND(CAST(COUNT(CASE WHEN Action = "Lob In-Play" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action IN ("Lob Error","Lob In-Play") THEN Action END),0),4) lob_accuracy_rate,
      COUNT(CASE WHEN Action LIKE "%Speed Up%" THEN Action END) AS speed_ups,
      ROUND(CAST(COUNT(CASE WHEN Action = "Speed Up In-Play" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Speed Up%" THEN Action END),0),4) speed_up_accuracy_rate,
      COUNT(CASE WHEN Action LIKE "%Reset%" THEN Action END) AS resets,
      ROUND(CAST(COUNT(CASE WHEN Action = "Reset In-Play" THEN Action END) AS REAL)/NULLIF(COUNT(CASE WHEN Action LIKE "%Reset%" THEN Action END),0),4) reset_accuracy_rate
      FROM career_history_data
      GROUP BY GameType, Player'))
      return(new_df2)
    } else {
      return(NULL)
    }
  }
  
  # Render the game history table
  output$career_history_table <- renderDT({
    career_history_data <- loadCareerHistoryData()
    if (!is.null(career_history_data)) {
      datatable(career_history_data, options = list(pageLength = 10),
                colnames = c('Row Number', 'Game Format','Player','Dates','Games','Serves','Serve Accuracy Rate',
                             'Serve Ace Rate','Serve Returns','Serve Return Accuracy Rate',
                             'Serve Return In Deep Rate','Third Shots','Third Shot Drive Tendency',
                             'Third Shot Drop Tendency','Third Shot Lob Tendency','Third Shot Drive Accuracy Rate',
                             'Third Shot Drop Accuracy Rate','Third Shot Lob Accuracy Rate','Total Third Shot Accuracy Rate','Defends','Defend Accuracy Rate',
                             'Dinks','Dink Accuracy Rate','Drives','Drive Accuracy Rate','Overheads','Overhead Accuracy Rate',
                             'Lobs','Lob Accuracy Rate','Speed Ups', 'Speed Up Accuracy Rate','Resets','Reset Accuracy Rate')
      )
    }
  })
  
  # Update the player selector choices
  observe({
    game_history_data <- loadGameHistoryData()
    if (!is.null(game_history_data)) {
      players <- unique(game_history_data$Player)
      updateSelectInput(session, "player_selector", choices = players)
    }
    career_history_data <- loadCareerHistoryData()
    if (!is.null(career_history_data)) {
      players_c <- unique(career_history_data$Player)
      updateSelectInput(session, "player_selector", choices = players)
    }
  })
  
  # Filter the data table based on player selection
  observe({
    game_history_data <- loadGameHistoryData()
    if (!is.null(game_history_data)) {
      selected_player <- input$player_selector
      filtered_data <- game_history_data %>%
        filter(Player == selected_player)
      proxy = dataTableProxy("game_history_table")
      replaceData(proxy, filtered_data, resetPaging = FALSE)
    }
    career_history_data <- loadCareerHistoryData()
    if (!is.null(career_history_data)) {
      selected_player_c <- input$player_selector
      filtered_data_c <- career_history_data %>%
        filter(Player == selected_player_c)
      proxy_c = dataTableProxy("career_history_table")
      replaceData(proxy_c, filtered_data_c, resetPaging = FALSE)
    }
  })
}

shinyApp(ui, server)