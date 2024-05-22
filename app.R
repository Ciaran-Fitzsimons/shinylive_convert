library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(readxl)
library(scales)
library(ggplot2)
library(openxlsx)
source("functions.R") #load in created functions
source("Draw_Pitch_6aside.R") #load in pitch

ui <- fluidPage(
  tags$style(
    HTML("
      .output-wrapper {
        display: flex;
        align-items: center;
        margin-bottom: 10px;
      }
      h4 {
        margin-right: 10px;
      }
    ")
  ),
  
  titlePanel("Football stats app"),
  
  tabsetPanel(
    tabPanel("Set-up",
      # Input: load in data
      fileInput(inputId = "file",
                label = "Upload your shot data"),
      # Downloading a sample file to be used in the app
      downloadButton("download_sample_data", "Download sample data")
    ),
    tabPanel("Shot map",
  
      sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          
          # Input: Slider for the week(s) played ----
          sliderInput(inputId = "week",
                      label = "Week(s) played:",
                      min = 1,
                      max = 1,  # Initialize with a default value
                      value = c(1, 1),  # Initialize with a default value
                      step = 1),
          
          # Input: Outcome of shot ----
          selectInput(inputId = "outcome",
                      label = "Select Shot Outcome",
                      c("All")),
          
          # Input: Foot used for shot ----
          selectInput(inputId = "foot",
                      label = "Select which foot was used for shot",
                      c("Either")),
          
          # Input: Shot type ----
          selectInput(inputId = "type",
                      label = "Select what type the shot was",
                      c("All")),
          
          # Input: Shot first time ----
          selectInput(inputId = "first_time",
                      label = "Select whether the shot was taken first time or not",
                      c("Either")),
          
          # Input: Which team was I on ----
          selectInput(inputId = "team",
                      label = "Select which team I played on that week",
                      c("Either")),
          
          # Input: Team result ----
          selectInput(inputId = "result",
                      label = "Select my team's result",
                      c("Any")),
          
          # Input: Slider for the number of players aside ----
          sliderInput(inputId = "aside",
                      label = "Players aside:",
                      min = 1,
                      max = 1,
                      value = c(1, 1),
                      step = 1),
          
          # Button that resets all inputs to their default
          actionButton("resetButton", "Clear selections")
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
          
          # Output: Shot Map ----
          plotlyOutput(outputId = "shotPlot"),
          
          #Output: Datatable for shots stats ----
          dataTableOutput("shots_table")
          
        )
      )
    ),
    tabPanel("Breakdown of shots",
             mainPanel(
               fluidRow(
                 column(width = 6,
                        #Output: Breakdown of shots based on result ----
                        tags$h2("Stats based on result"),
                        selectInput(inputId = "result_for_calculations",
                                    label = "Select my team's result",
                                    c("Any")),
                        div(class = "output-wrapper",
                            h4("Goals per game:"),
                            div(textOutput("goals_per_game_based_on_result"))),
                        div(class = "output-wrapper",
                            h4("Conversion rate:"),
                            div(textOutput("goals_per_shot_based_on_result"))),
                        h4("Conversion rate with first time"),
                        dataTableOutput("goals_by_first_time_shots_based_on_result"),
                        h4("Conversion rate by foot"),
                        dataTableOutput("goals_by_foot_based_on_result"),
                        h4("Conversion rate by type of shot"),
                        dataTableOutput("goals_by_shot_type_based_on_result")
                 ),
                 column(width = 6,
                        #Output: Breakdown of shots based on how many aside ----
                        tags$h2("Stats based on how many aside"),
                        pickerInput(inputId = "aside_for_calculations",
                                    label = "Select how many players there were aside",
                                    choices = c("Any"),
                                    selected = "Any",
                                    multiple = TRUE,
                                    options = pickerOptions(actionsBox = TRUE, selectedTextFormat = "count", liveSearch = T)),
                        div(class = "output-wrapper",
                            h4("Goals per game:"),
                            div(textOutput("goals_per_game_based_on_aside"))),
                        div(class = "output-wrapper",
                            h4("Conversion rate:"),
                            div(textOutput("goals_per_shot_based_on_aside"))),
                        h4("Conversion rate with first time"),
                        dataTableOutput("goals_by_first_time_shots_based_on_aside"),
                        h4("Conversion rate by foot"),
                        dataTableOutput("goals_by_foot_based_on_aside"),
                        h4("Conversion rate by type of shot"),
                        dataTableOutput("goals_by_shot_type_based_on_aside")
                 )
               )
             )
    ),
    tabPanel("Weekly stats",
      mainPanel(
          #Output: Datatable for week by week stats ----
          dataTableOutput("stats_table")
      )
    ),
    tabPanel("Heatmap",
      plotlyOutput(outputId = "heatmap")
    )
  )
)

# Define server logic required for the Shiny app ----
server <- function(input, output, session) {
  
  # Downloading a sample file to be used in the app
  output$download_sample_data <- downloadHandler(
    filename = function() {
      "Ciaran_shot_data.xlsx"
    },
    
    content = function(file) {
      # Read both sheets from the sample file
      sheet1 <- read_excel("Ciaran_shot_data.xlsx", sheet = 1)
      sheet2 <- read_excel("Ciaran_shot_data.xlsx", sheet = 2)
      
      # Create a new workbook and add the sheets
      wb <- createWorkbook()
      addWorksheet(wb, "Sheet1")
      writeData(wb, "Sheet1", sheet1)
      addWorksheet(wb, "Sheet2")
      writeData(wb, "Sheet2", sheet2)
      
      # Save the workbook to the specified file
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  #### First tab ----
  filtered_data <- reactive({
    
    if(!is.null(input$file)) {
      shot_data <- read_excel(input$file$datapath, sheet = 1)
      shot_data <- shot_data %>% separate(Location, into = c("x_shot_location", "y_shot_location"), sep = ", ", convert = T) %>%
        separate("End location", into = c("x_end_location", "y_end_location"), sep = ", ", convert = T)
      shot_data$Outcome_filter <- ifelse(shot_data$Outcome == 'Goal', 'Goal', 'No goal')
      match_stats <- read_excel(input$file$datapath, sheet = 2)
      shot_data <- shot_data %>% inner_join(match_stats, by = "Week")
      filtered_data <- shot_data
      
      selected_weeks <- input$week[1]:input$week[2]
      filtered_data <- filtered_data[filtered_data$Week %in% selected_weeks, ]
      
      if (input$outcome != "All") {
        filtered_data <- filtered_data[filtered_data$Outcome_filter == input$outcome, ]
      }
      
      if (input$foot != "Either") {
        filtered_data <- filtered_data[filtered_data$Foot == input$foot, ]
      }
      
      if (input$type != "All") {
        filtered_data <- filtered_data[filtered_data$Type == input$type, ]
      }
      
      if (input$first_time != "Either") {
        filtered_data <- filtered_data[filtered_data$`First time` == input$first_time, ]
      }
      
      if (input$team != "Either") {
        filtered_data <- filtered_data[filtered_data$Team == input$team, ]
      }
      
      if (input$result != "Any") {
        filtered_data <- filtered_data[filtered_data$`Team result` == input$result, ]
      }
      
      aside <- input$aside[1]:input$aside[2]
      filtered_data <- filtered_data[filtered_data$`How many aside` %in% aside, ]
      
      return(filtered_data)
    }
  })
  
  # Updating ui (e.g. sliders & select input values)
  observe({
    
    if(!is.null(input$file)) {
      shot_data <- read_excel(input$file$datapath, sheet = 1)
      shot_data$Outcome_filter <- ifelse(shot_data$Outcome == 'Goal', 'Goal', 'No goal')
      match_stats <- read_excel(input$file$datapath, sheet = 2)
      shot_data <- shot_data %>% inner_join(match_stats, by = "Week")
      
      updateSliderInput(session, "week", max = max(shot_data$Week), value = c(1, max(shot_data$Week)))
      updateSelectInput(session, "outcome", choices = c("All", unique(shot_data$Outcome_filter)))
      updateSelectInput(session, "foot", choices = c("Either", unique(shot_data$Foot)))
      updateSelectInput(session, "type", choices = c("All", unique(shot_data$Type)))
      updateSelectInput(session, "first_time", choices = c("Either", unique(shot_data$`First time`)))
      updateSelectInput(session, "team", choices = c("Either", unique(shot_data$Team)))
      updateSelectInput(session, "result", choices = c("Any", unique(shot_data$`Team result`)))
      updateSliderInput(session, "aside", min = min(shot_data$`How many aside`), max = max(shot_data$`How many aside`), value = c(min(shot_data$`How many aside`), max(shot_data$`How many aside`)))
    }
  })
  
  # Define a function to reset inputs to default values
  resetInputs <- function() {
    observe({
      
      if(!is.null(input$file)) {
        shot_data <- read_excel(input$file$datapath, sheet = 1)
        shot_data$Outcome_filter <- ifelse(shot_data$Outcome == 'Goal', 'Goal', 'No goal')
        match_stats <- read_excel(input$file$datapath, sheet = 2)
        shot_data <- shot_data %>% inner_join(match_stats, by = "Week")
        
        updateSliderInput(session, "week", max = max(shot_data$Week), value = c(1, max(shot_data$Week)))
        updateSelectInput(session, "outcome", selected = "All")
        updateSelectInput(session, "foot", selected = "Either")
        updateSelectInput(session, "type", selected = "All")
        updateSelectInput(session, "first_time", selected = "Either")
        updateSelectInput(session, "team", selected = "Either")
        updateSelectInput(session, "result", selected = "Any")
        updateSliderInput(session, "aside", value = c(min(shot_data$`How many aside`), max(shot_data$`How many aside`)))
      }
    })
  }
  
  # Create an observe event for the reset button
  observeEvent(input$resetButton, {
    resetInputs()
  })
  
  # Create a reactive table for shot data
  output$shots_table <- renderDataTable({
    shot_data <- filtered_data()
    
    if (is.null(shot_data)) {
      return(NULL)
    }
    
    shot_data %>% select(-c(x_shot_location, y_shot_location, x_end_location, y_end_location, Outcome_filter))
  }, options = list(lengthMenu = c(10, 25, 50), pageLength = 50))
    
  output$shotPlot <- renderPlotly({
    shot_data <- filtered_data()
    
    if (is.null(shot_data)) {
      return(NULL)
    }
    
    shot_data$WrappedDescription <- str_wrap(shot_data$Description, width = 40)
    
    shot_plot <- hori5 +
      geom_point(data = shot_data, aes(x = x_shot_location, y = y_shot_location, color = Outcome_filter,
                                       text = paste("Week:", Week, "\nFoot used:", Foot, "\nType of shot:", Type, "\nDescription:", WrappedDescription)), size = 3) +
      geom_segment(data = shot_data, aes(xend = x_end_location, yend = y_end_location, x = x_shot_location, y = y_shot_location, color = Outcome_filter),
                   arrow = arrow(type = "closed", length = unit(0.2, "inches")), lineend = "round", size = 0.2) +
      guides(color = "none") +
      scale_color_manual(values = c('No goal' = "#FF0000", Goal = "#008B45"))
    
    ggplotly(shot_plot, tooltip = "text")
  })
  
  #### Tables and numbers in second tab ----
  ### Updating ui (e.g. sliders & select input values)
  observe({
    
    if(!is.null(input$file)) {
      shot_data <- read_excel(input$file$datapath, sheet = 1)
      shot_data <- shot_data %>% separate(Location, into = c("x_shot_location", "y_shot_location"), sep = ", ", convert = T) %>%
        separate("End location", into = c("x_end_location", "y_end_location"), sep = ", ", convert = T)
      shot_data$Outcome_filter <- ifelse(shot_data$Outcome == 'Goal', 'Goal', 'No goal')
      match_stats <- read_excel(input$file$datapath, sheet = 2)
      shot_data <- shot_data %>% inner_join(match_stats, by = "Week")
      how_many_aside_sorted <- sort(unique(match_stats$`How many aside`))
      
      updatePickerInput(session, "aside_for_calculations", choices = how_many_aside_sorted, selected = how_many_aside_sorted)
      updateSelectInput(session, "result_for_calculations", choices = c("Any", unique(match_stats$`Team result`)))
    }
  })
  
  ### Based on teams result
  ## For filters using match stats
  match_stats_calculations <- reactive({
    
    if(!is.null(input$file)) {
      match_stats <- read_excel(input$file$datapath, sheet = 2)
      req(input$result_for_calculations)
      if (input$result_for_calculations != "Any") {
        filtered_stats <- match_stats[match_stats$`Team result` == input$result_for_calculations, ]
        return(filtered_stats)
      } else {
        return(match_stats)
      }
    }
  })
  
  # Goals per game
  output$goals_per_game_based_on_result <- renderText({
    filtered_data <- match_stats_calculations()
    games <- nrow(filtered_data)
    if (games > 0) {
      goals <- sum(filtered_data$Goals)
      goals_per_game <- goals / games
      return(round(goals_per_game, 2))
    } else {
      return("No data available")
    }
  })
  
  # Conversion rate
  output$goals_per_shot_based_on_result <- renderText({
    filtered_data <- match_stats_calculations()
    games <- nrow(filtered_data)
    if (games > 0) {
      goals <- sum(filtered_data$Goals)
      shots <- sum(filtered_data$Shots)
      goals_per_shot <- goals / shots
      return(percent(goals_per_shot))
    } else {
      return("No data available")
    }
  })
  
  ## For filters using shot and goal stats
  # Shot stats
  shot_stats_calculations <- reactive({
    if(!is.null(input$file)) {
      shot_data <- read_excel(input$file$datapath, sheet = 1)
      shot_data <- shot_data %>% separate(Location, into = c("x_shot_location", "y_shot_location"), sep = ", ", convert = T) %>%
        separate("End location", into = c("x_end_location", "y_end_location"), sep = ", ", convert = T)
      shot_data$Outcome_filter <- ifelse(shot_data$Outcome == 'Goal', 'Goal', 'No goal')
      match_stats <- read_excel(input$file$datapath, sheet = 2)
      shot_data <- shot_data %>% inner_join(match_stats, by = "Week")
      req(input$result_for_calculations)
      if (input$result_for_calculations != "Any") {
        filtered_shot_stats <- shot_data[shot_data$`Team result` == input$result_for_calculations, ]
        return(filtered_shot_stats)
      } else {
        return(shot_data)
      }
    }
  })
  
  # Goal stats
  goal_stats_calculations <- reactive({
    if(!is.null(input$file)) {
      shot_data <- read_excel(input$file$datapath, sheet = 1)
      shot_data <- shot_data %>% separate(Location, into = c("x_shot_location", "y_shot_location"), sep = ", ", convert = T) %>%
        separate("End location", into = c("x_end_location", "y_end_location"), sep = ", ", convert = T)
      shot_data$Outcome_filter <- ifelse(shot_data$Outcome == 'Goal', 'Goal', 'No goal')
      match_stats <- read_excel(input$file$datapath, sheet = 2)
      shot_data <- shot_data %>% inner_join(match_stats, by = "Week")
      goal_data <- shot_data %>% filter(Outcome == "Goal")
      req(input$result_for_calculations)
      if (input$result_for_calculations != "Any") {
        filtered_goal_stats <- goal_data[goal_data$`Team result` == input$result_for_calculations, ]
        return(filtered_goal_stats)
      } else {
        return(goal_data)
      }
    }
  })
  
  # Conversion rate of first time shots
  output$goals_by_first_time_shots_based_on_result <- renderDataTable({
    filtered_shot_data <- shot_stats_calculations()
    filtered_goal_data <- goal_stats_calculations()
    goals <- nrow(filtered_goal_data)
    if (goals > 0) {
      conversion_rate_by_first_time <- calculate_conversion_rate_by_first_time(filtered_shot_data, filtered_goal_data)
      return(conversion_rate_by_first_time)
    } else {
      return("No data available")
    }
  }, options = list(
    searching = FALSE, # Disable search
    paging = FALSE,    # Disable pagination
    lengthMenu = list(c(-1), c("All")),  # Show all entries
    dom = 't' # Don't show any additional information
  ))
  
  # Conversion rate by foot
  output$goals_by_foot_based_on_result <- renderDataTable({
    filtered_shot_data <- shot_stats_calculations()
    filtered_goal_data <- goal_stats_calculations()
    goals <- nrow(filtered_goal_data)
    if (goals > 0) {
      conversion_rate_by_foot <- calculate_conversion_rate_by_foot(filtered_shot_data, filtered_goal_data)
      return(conversion_rate_by_foot)
    } else {
      return("No data available")
    }
  }, options = list(
    searching = FALSE, # Disable search
    paging = FALSE,    # Disable pagination
    lengthMenu = list(c(-1), c("All")),  # Show all entries
    dom = 't' # Don't show any additional information
  ))
  
  # Conversion rate by type of shot
  output$goals_by_shot_type_based_on_result <- renderDataTable({
    filtered_shot_data <- shot_stats_calculations()
    filtered_goal_data <- goal_stats_calculations()
    goals <- nrow(filtered_goal_data)
    if (goals > 0) {
      conversion_rate_by_type_of_shot <- calculate_conversion_rate_by_type_of_shot(filtered_shot_data, filtered_goal_data)
      return(conversion_rate_by_type_of_shot)
    } else {
      return("No data available")
    }
  }, options = list(
    searching = FALSE, # Disable search
    paging = FALSE,    # Disable pagination
    lengthMenu = list(c(-1), c("All")),  # Show all entries
    dom = 't' # Don't show any additional information
  ))
  
  
  ### Based on how many aside
  ## For filters using match stats
  match_stats_calculations_aside <- reactive({
    if(!is.null(input$file)) {
      match_stats <- read_excel(input$file$datapath, sheet = 2)
      req(input$aside_for_calculations)
      if ("Any" %in% input$aside_for_calculations) {
        filtered_stats_aside <- match_stats
      } else {
        filtered_stats_aside <- match_stats[match_stats$`How many aside` %in% input$aside_for_calculations, ]
      }
      return(filtered_stats_aside)
    }
  })
  
  # Goals per game
  output$goals_per_game_based_on_aside <- renderText({
    filtered_data_aside <- match_stats_calculations_aside()
    games <- nrow(filtered_data_aside)
    if (games > 0) {
      goals <- sum(filtered_data_aside$Goals)
      goals_per_game <- goals / games
      return(round(goals_per_game, 2))
    } else {
      return("No data available")
    }
  })
  
  # Conversion rate
  output$goals_per_shot_based_on_aside <- renderText({
    filtered_data_aside <- match_stats_calculations_aside()
    games <- nrow(filtered_data_aside)
    if (games > 0) {
      goals <- sum(filtered_data_aside$Goals)
      shots <- sum(filtered_data_aside$Shots)
      goals_per_shot <- goals / shots
      return(percent(goals_per_shot))
    } else {
      return("No data available")
    }
  })
  
  ## For filters using shot and goal stats
  # Shot stats
  shot_stats_calculations_aside <- reactive({
    if(!is.null(input$file)) {
      shot_data <- read_excel(input$file$datapath, sheet = 1)
      shot_data <- shot_data %>% separate(Location, into = c("x_shot_location", "y_shot_location"), sep = ", ", convert = T) %>%
        separate("End location", into = c("x_end_location", "y_end_location"), sep = ", ", convert = T)
      shot_data$Outcome_filter <- ifelse(shot_data$Outcome == 'Goal', 'Goal', 'No goal')
      match_stats <- read_excel(input$file$datapath, sheet = 2)
      shot_data <- shot_data %>% inner_join(match_stats, by = "Week")
      req(input$aside_for_calculations)
      if ("Any" %in% input$aside_for_calculations) {
        return(shot_data)
      } else {
        filtered_shot_stats_aside <- shot_data[shot_data$`How many aside` %in% input$aside_for_calculations, ]
        return(filtered_shot_stats_aside)
      }
    }
  })
  
  # Goal stats
  goal_stats_calculations_aside <- reactive({
    if(!is.null(input$file)) {
      shot_data <- read_excel(input$file$datapath, sheet = 1)
      shot_data <- shot_data %>% separate(Location, into = c("x_shot_location", "y_shot_location"), sep = ", ", convert = T) %>%
        separate("End location", into = c("x_end_location", "y_end_location"), sep = ", ", convert = T)
      shot_data$Outcome_filter <- ifelse(shot_data$Outcome == 'Goal', 'Goal', 'No goal')
      match_stats <- read_excel(input$file$datapath, sheet = 2)
      shot_data <- shot_data %>% inner_join(match_stats, by = "Week")
      goal_data <- shot_data %>% filter(Outcome == "Goal")
      req(input$aside_for_calculations)
      if ("Any" %in% input$aside_for_calculations) {
        return(goal_data)
      } else {
        filtered_goal_stats_aside <- goal_data[goal_data$`How many aside` %in% input$aside_for_calculations, ]
        return(filtered_goal_stats_aside)
      }
    }
  })
  
  # Conversion rate of first time shots
  output$goals_by_first_time_shots_based_on_aside <- renderDataTable({
    filtered_shot_data_aside <- shot_stats_calculations_aside()
    filtered_goal_data_aside <- goal_stats_calculations_aside()
    goals <- nrow(filtered_goal_data_aside)
    if (goals > 0) {
      conversion_rate_by_first_time_aside <- calculate_conversion_rate_by_first_time(filtered_shot_data_aside, filtered_goal_data_aside)
      return(conversion_rate_by_first_time_aside)
    } else {
      return("No data available")
    }
  }, options = list(
    searching = FALSE, # Disable search
    paging = FALSE,    # Disable pagination
    lengthMenu = list(c(-1), c("All")),  # Show all entries
    dom = 't' # Don't show any additional information
  ))
  
  # Conversion rate by foot
  output$goals_by_foot_based_on_aside <- renderDataTable({
    filtered_shot_data_aside <- shot_stats_calculations_aside()
    filtered_goal_data_aside <- goal_stats_calculations_aside()
    goals <- nrow(filtered_goal_data_aside)
    if (goals > 0) {
      conversion_rate_by_foot_aside <- calculate_conversion_rate_by_foot(filtered_shot_data_aside, filtered_goal_data_aside)
      return(conversion_rate_by_foot_aside)
    } else {
      return("No data available")
    }
  }, options = list(
    searching = FALSE, # Disable search
    paging = FALSE,    # Disable pagination
    lengthMenu = list(c(-1), c("All")),  # Show all entries
    dom = 't' # Don't show any additional information
  ))
  
  # Conversion rate by type of shot
  output$goals_by_shot_type_based_on_aside <- renderDataTable({
    filtered_shot_data_aside <- shot_stats_calculations_aside()
    filtered_goal_data_aside <- goal_stats_calculations_aside()
    goals <- nrow(filtered_goal_data_aside)
    if (goals > 0) {
      conversion_rate_by_type_of_shot_aside <- calculate_conversion_rate_by_type_of_shot(filtered_shot_data_aside, filtered_goal_data_aside)
      return(conversion_rate_by_type_of_shot_aside)
    } else {
      return("No data available")
    }
  }, options = list(
    searching = FALSE, # Disable search
    paging = FALSE,    # Disable pagination
    lengthMenu = list(c(-1), c("All")),  # Show all entries
    dom = 't' # Don't show any additional information
  ))
  
  
  #### Table with full weekly stats ----
  weekly_stats <- reactive({
    if(!is.null(input$file)) {
      match_stats <- read_excel(input$file$datapath, sheet = 2)
      
      return(match_stats)
    }
  })
  
  output$stats_table <- renderDataTable(
    weekly_stats(),
    options=list(lengthMenu=list(c(20,-1),list('20','All')),
               pageLength=20,dom='Bipt'))
  
  #### Heatmap ----
  heatmap_data <- reactive({
    
    if(!is.null(input$file)) {
      shot_heatmap_data <- read_excel(input$file$datapath, sheet = 1) %>%
        select("Outcome", "Location") %>%
        separate(Location, into = c("x_shot_location", "y_shot_location"), sep = ", ", convert = T)
      shot_heatmap_data$Outcome <- ifelse(shot_heatmap_data$Outcome == 'Goal', 1, 0)
      
      shot_heatmap_data <- shot_heatmap_data %>%
        mutate(y_shot_location = ifelse(y_shot_location >= 0 & y_shot_location <= 5, 2.5,
                                        ifelse(y_shot_location > 5 & y_shot_location <= 10, 7.5,
                                               ifelse(y_shot_location > 10 & y_shot_location <= 15, 12.5,
                                                      ifelse(y_shot_location > 15 & y_shot_location <= 20, 17.5,
                                                             ifelse(y_shot_location > 20 & y_shot_location <= 25, 22.5,
                                                                    ifelse(y_shot_location > 25 & y_shot_location <= 30, 27.5,
                                                                           ifelse(y_shot_location > 30 & y_shot_location <= 35, 32.5,
                                                                                  ifelse(y_shot_location > 35 & y_shot_location <= 40, 37.5, y_shot_location)))))))))
      
      shot_heatmap_data <- shot_heatmap_data %>%
        mutate(x_shot_location = ifelse(x_shot_location >= 0 & x_shot_location <= 5, 2.5,
                                        ifelse(x_shot_location > 5 & x_shot_location <= 10, 7.5,
                                               ifelse(x_shot_location > 10 & x_shot_location <= 15, 12.5,
                                                      ifelse(x_shot_location > 15 & x_shot_location <= 20, 17.5,
                                                             ifelse(x_shot_location > 20 & x_shot_location <= 25, 22.5,
                                                                    ifelse(x_shot_location > 25 & x_shot_location <= 30, 27.5,
                                                                           ifelse(x_shot_location >= 30 & x_shot_location <= 35, 32.5,
                                                                                  ifelse(x_shot_location > 35 & x_shot_location <= 40, 37.5,
                                                                                         ifelse(x_shot_location > 40 & x_shot_location <= 45, 42.5,
                                                                                                ifelse(x_shot_location > 45 & x_shot_location <= 50, 47.5,
                                                                                                       ifelse(x_shot_location > 50 & x_shot_location <= 55, 52.5,
                                                                                                              ifelse(x_shot_location > 55 & x_shot_location <= 60, 57.5, x_shot_location)))))))))))))
      
      goals_heatmap <- shot_heatmap_data %>%
        group_by(x_shot_location, y_shot_location) %>%
        summarise(goals = sum(Outcome))
      
      shots_heatmap <- shot_heatmap_data %>%
        group_by(x_shot_location, y_shot_location) %>%
        summarise(shots = n())
      
      football_heatmap <- left_join(goals_heatmap, shots_heatmap, by = c("x_shot_location", "y_shot_location"))
      football_heatmap <- football_heatmap %>% mutate(`Conversion rate` = round((goals/shots)*100, 0))
      
      return(football_heatmap)
    }
  })
  
  output$heatmap <- renderPlotly({
    football_heatmap <- heatmap_data()
    
    if (is.null(football_heatmap)) {
      return(NULL)
    }
    
    # create markings on football pitch
    football_pitch_lines <- data.frame(
      x = c(0, 30, 60, 0, 0),
      y = c(0, 0, 0, 0, 40),
      xend = c(0, 30, 60, 60, 60),
      yend = c(40, 40, 40, 0, 40)
    )
    center_circle <- circle_function_heatmap(center = c(30,20))
    penalty_arc_att <- circle_function_heatmap(center = c(20, -60), npoints = 1000, end = 1, rotation_angle = pi / 2)
    penalty_arc_def <- circle_function_heatmap(center = c(20, 0), npoints = 1000, end = 1, rotation_angle = pi / 2)
    penalty_arc_def$x <- -penalty_arc_def$x
    penalty_spot_att <- circle_function_heatmap(center = c(7,20), diameter = 0.2)
    penalty_spot_def <- circle_function_heatmap(center = c(53,20), diameter = 0.2)
    
    colfunc <- colorRampPalette(c("#BCE4D8", "#2C5985"))
    
    heatmap_with_pitch <- ggplot(football_heatmap, aes(x_shot_location, y_shot_location)) +
      geom_tile(aes(fill=`Conversion rate`, text = paste0("Conversion rate: ", `Conversion rate`, "%", "\nGoals: ", goals, "\nShots: ", shots)), size = 3) +
      scale_fill_gradientn(colors = colfunc(30)) +
      theme_classic() +
      theme(panel.background = element_rect(fill = 'black'),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            legend.position = "none") +
      geom_segment(data = football_pitch_lines, aes(x = x, y = y, xend = xend, yend = yend), color = "white", size = 1) +
      geom_point(data = center_circle, aes(x = x, y = y), color = "white", size = 0.2) +
      geom_point(data = penalty_arc_att, aes(x = x, y = y), color = "white", size = 0.2) +
      geom_point(data = penalty_arc_def, aes(x = x, y = y), color = "white", size = 0.2) +
      geom_point(data = penalty_spot_att, aes(x = x, y = y), color = "white", size = 0.6) +
      geom_point(data = penalty_spot_def, aes(x = x, y = y), color = "white", size = 0.6)
    
    plotly_heatmap <- ggplotly(heatmap_with_pitch, tooltip = "text") %>%
      layout(autosize = TRUE,  margin = list(autoexpand = TRUE), height = 700, width = 1200)
    
    return(plotly_heatmap)
    
  })
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)