#####################################################
# Hunter Mott                                       #
# https://github.com/Hunter-Mott-31/Hunter-Mott-31  #
#####################################################



# Load R packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)
library(DT)

# Load in MLB data sets for pitchers and hitters
pitch <- read.csv("pitcher_stats.csv")
hitter <- read.csv("hitter_stats.csv")

# Clean data
pitch_clean <- pitch[, -which(colnames(pitch) %in% c("last_name..first_name", "player_id", "year", "X"))]
hitter_clean <- hitter[, -which(colnames(hitter) %in% c("player_id", "year", "X"))]

names(hitter_clean)

# Subset data for Pitcher Graph 1
speed_data <- pitch_clean %>%
  summarise(
    Category = c("FB", "SL", "CB", "CH"),
    Value = c(
      mean(ff_avg_speed, na.rm = TRUE),
      mean(sl_avg_speed, na.rm = TRUE),
      mean(cu_avg_speed, na.rm = TRUE),
      mean(ch_avg_speed, na.rm = TRUE)
    )
  )




# Factor the pitch types
speed_data <- speed_data %>% mutate(Category = factor(Category, levels = c("FB", "SL", "CB", "CH")))

# Custom labels and color palette
custom_labels <- c("FB" = "Fastball", "SL" = "Slider", "CB" = "Curveball", "CH" = "Changeup")
color_palette <- brewer.pal(n = 4, name = "Set1")

# Subset data for Pitcher Graph 2
spin_data <- pitch_clean %>%
  summarise(
    Category = c("FB", "SL", "CB", "CH"),
    Value = c(
      mean(ff_avg_spin, na.rm = TRUE),
      mean(sl_avg_spin, na.rm = TRUE),
      mean(cu_avg_spin, na.rm = TRUE),
      mean(ch_avg_spin, na.rm = TRUE)
    )
  )

# Factor the pitch types
spin_data <- spin_data %>% mutate(Category = factor(Category, levels = c("FB", "SL", "CB", "CH")))

#UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel(h1("2023 Statcast Averages for MLB Players", style = "font-size: 36px;")),
  navbarPage(
    theme = "cerulean",
    "Options : ",
    tabPanel("Pitcher",
             selectInput("pitcherGraph", "Select Graph",
                         choices = c("Average MLB Pitcher Velocity by Pitch", "Average MLB Pitcher Spin Rates by Pitch")),
             plotOutput("selectedPitcherPlot", height = "800px", width = "1000px")), # Adjust the height and width here
    tabPanel("Hitter",
             selectInput("hitterTable", "Select Hitter Table",
                         choices = c("Default: All 133 Qualified Hitters", "Top 30 Hitters by Exit Velocity", "Top 30 Hitters by Sprint Speed"),
                         selected = "Default: All 133 Qualified Hitters"),
             dataTableOutput("hitter_summary_table")
    ),
    tabPanel("About",
             h2("Purpose of the App:"),
             p(
               "This Shiny app provides statistics and visualizations for MLB players based on Statcast data from the 2023 season. ",
               "It allows high school, college, and professional players to compare their metrics to the average major leaguer. ",
               "The app focuses on objective stats like exit velocity and pitcher's velocity, making comparisons easier."
             ),
             
             h2("App Info:"),
             p(
               "The data used for this app was downloaded from Baseball Savant using the link: ",
               a("Baseball Savant Leaderboard", href = "https://baseballsavant.mlb.com/leaderboard/custom?year=2023&type=batter&filter=&sort=4&sortDir=desc&min=q&selections=xba,xslg,xwoba,xobp,xiso,exit_velocity_avg,launch_angle_avg,barrel_batted_rate,&chart=false&x=xba&y=xba&r=no&chartType=beeswarm", target="_blank"),
               ". Ensure you choose the necessary stats and player type filters for accurate data."
             ),
             
             p(
               "This app was created by Hunter Mott using R Shiny. You can find the code on my GitHub: ",
               a("GitHub - Hunter Mott", href = "https://github.com/Hunter-Mott-31/2023_Baseball_Statcast_App/tree/main", target="_blank"),
               ". Feel free to contact me with any comments or recommendations about the app. Stay tuned for future versions!"
             ),
             
             h3("Contact Information:"),
             p(
               "LinkedIn: ", a("Hunter Mott - LinkedIn", href = "https://www.linkedin.com/in/hunter-mott/", target="_blank"),
               "Email: h-mott@outlook.com"
             )
    )
  )
)
# Define server function
server <- function(input, output) {
  output$txtout <- renderText({
    paste(input$txt1, input$txt2, sep = " ")  # sends to ui
  })
  
  # Reactive expressions to store data
  speed_reactive <- reactive({
    speed_data
  })
  
  spin_reactive <- reactive({
    spin_data
  })
  
  output$selectedPitcherPlot <- renderPlot({
    selected_graph <- switch(input$pitcherGraph,
                             "Average MLB Pitcher Velocity by Pitch" = {
                               speed_data <- speed_reactive()
                               
                               ggplot(speed_data, aes(x = Category, y = Value, fill = Category)) +
                                 geom_bar(stat = "identity", position = "dodge") +
                                 geom_text(aes(label = round(Value, 1)), vjust = -0.5, position = position_dodge(width = 0.9), size = 15) +
                                 labs(
                                   title = "Average MLB Pitcher Velocity by Pitch",
                                   x = "Pitch Type",
                                   y = "Velocity (mph)"
                                 ) +
                                 scale_fill_manual(
                                   values = color_palette,
                                   breaks = names(custom_labels),
                                   labels = custom_labels
                                 ) +
                                 theme_minimal() +
                                 theme(
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   panel.border = element_blank(),
                                   axis.line = element_line(color = "black"),
                                   plot.title = element_text(size = 36),
                                   axis.title.x = element_text(size = 22),
                                   axis.title.y = element_text(size = 22),
                                   axis.text.x = element_text(size = 18),
                                   axis.text.y = element_text(size = 18),
                                   legend.text = element_text(size = 18)
                                 ) +
                                 coord_cartesian(ylim = c(70, 100))
                             },
                             "Average MLB Pitcher Spin Rates by Pitch" = {
                               spin_data <- spin_reactive()
                               
                               ggplot(spin_data, aes(x = Category, y = Value, fill = Category)) +
                                 geom_bar(stat = "identity", position = "dodge") +
                                 geom_text(aes(label = round(Value)), vjust = -0.5, position = position_dodge(width = 0.9), size = 15) +
                                 labs(
                                   title = "Average MLB Pitcher Spin Rates by Pitch",
                                   x = "Pitch Type",
                                   y = "Spin Rate (rpm)"
                                 ) +
                                 scale_fill_manual(
                                   values = color_palette,
                                   breaks = names(custom_labels),
                                   labels = custom_labels
                                 ) +
                                 theme_minimal() +
                                 theme(
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   panel.border = element_blank(),
                                   axis.line = element_line(color = "black"),
                                   plot.title = element_text(size = 40),
                                   axis.title.x = element_text(size = 22),
                                   axis.title.y = element_text(size = 22),
                                   axis.text.x = element_text(size = 18),
                                   axis.text.y = element_text(size = 18),
                                   legend.text = element_text(size = 18)
                                 ) +
                                 coord_cartesian(ylim = c(1500, 2800))
                             })
    
    return(selected_graph)
  })
  
  # Create reactive expression for power hitters
  hitter_clean_power <- reactive({
    top_power_hitters <- hitter_clean %>%
      arrange(desc(exit_velocity_avg)) %>%
      slice(1:30)
    return(top_power_hitters)
  })
  
  # Create reactive expression for speed hitters
  hitter_clean_speed <- reactive({
    top_speed_hitters <- hitter_clean %>%
      arrange(desc(sprint_speed)) %>%
      slice(1:30)
    return(top_speed_hitters)
  })
  
  # Create reactive expression for selected table
  selected_table <- reactive({
    switch(input$hitterTable,
           "Default: All 133 Qualified Hitters" = hitter_clean,
           "Top 30 Hitters by Exit Velocity" = hitter_clean_power(),
           "Top 30 Hitters by Sprint Speed" = hitter_clean_speed())
  })
  
  # Render the DataTable for Hitters tabPanel
  output$hitter_summary_table <- renderDataTable({
    # Calculate the average values for selected Hitters table
    avg_data_frame <- selected_table() %>%
      summarise(
        'Avg. Exit Velo (MPH)' = paste0(round(mean(exit_velocity_avg, na.rm = TRUE), 1), " MPH"),
        'Avg. Launch Angle (deg.)' = paste0(round(mean(launch_angle_avg, na.rm = TRUE), 1), " Deg."),
        'Avg. Groundball %' = paste0(round(mean(groundballs_percent, na.rm = TRUE), 1), "%"),
        'Avg. Whiff %' = paste0(round(mean(whiff_percent, na.rm = TRUE), 1), "%"),
        'Avg. Flyball %' = paste0(round(mean(flyballs_percent, na.rm = TRUE), 1), "%"),
        'Avg. Linedrive %' = paste0(round(mean(linedrives_percent, na.rm = TRUE), 1), "%"),
        'Avg. Popup %' = paste0(round(mean(popups_percent, na.rm = TRUE), 1), "%"),
        'Avg. Home to 1st' = paste0(round(mean(hp_to_1b, na.rm = TRUE), 1), " Sec."),
        'Avg. Sprint Speed' = paste0(round(mean(sprint_speed, na.rm = TRUE), 1), " MPH")
      ) %>%
      pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Average")
    
    # Return the DataTable with adjusted width and height, without rownames, and with specific styling
    datatable(
      avg_data_frame,
      options = list(
        rownames = FALSE,
        searching = FALSE,
        lengthChange = FALSE,
        columnDefs = list(list(targets = "_all", className = "dt-center"))
      ),
      class = 'cell-border stripe',  # Specify a valid theme here
      caption = tags$caption(style = "font-size: 18px; color: black;", "2023 MLB Statistics for Qualified Hitters")) %>%
      formatStyle(c(1, 2), `border-left` = "solid 1px") %>%
      formatStyle(c(2, 5, 7), `border-right` = "solid 1px")
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)