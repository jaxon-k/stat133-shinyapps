# Title: Visualizing California Car Crash Data (2021-2023)
# Description: This Shiny application visualizes car crash data from the state of
#              California from the years 2021-2023
# Author: Jaxon Kaeller
# Date: Spring 2025


# =======================================================
# Packages (you can use other packages if you want)
# =======================================================
library(shiny)
library(tidyverse)  # data wrangling and graphics
library(maps)       # includes map of USA counties
library(leaflet)    # web interactive maps
library(plotly)     # web interactive graphics
library(sf)
library(shinyWidgets)
library(stringr)


# =======================================================
# Import data
#
# For demo purposes, in this "template" we use storms data
# (but you will have to replace this with the crash data)
#
# Uncomment the lines below and fill in the missing arguments to import the data!!!
# We're assuming the CSV file is in the working directory of your app.
# =======================================================
crashes = read_csv(
  file = "crashes_2021_2023.csv", 
  col_types = list(
    col_double(),    #  1) CASE_ID
    col_double(),    #  2) ACCIDENT_YEAR
    col_date(),      #  3) COLLISION_DATE 
    col_double(),    #  4) COLLISION_TIME 
    col_double(),    #  5) HOUR 
    col_integer(),   #  6) DAY_OF_WEEK 
    col_character(), #  7) COLLISION_SEVERITY 
    col_integer(),   #  8) NUMBER_KILLED 
    col_integer(),   #  9) NUMBER_INJURED 
    col_character(), # 10) VIOLATION_CATEGORY 
    col_character(), # 11) TYPE_OF_COLLISION 
    col_character(), # 12) PEDESTRIAN_ACCIDENT 
    col_character(), # 13) BICYCLE_ACCIDENT 
    col_character(), # 14) MOTORCYCLE_ACCIDENT 
    col_character(), # 15) TRUCK_ACCIDENT 
    col_character(), # 16) COUNTY 
    col_character(), # 17) CITY 
    col_character(), # 18) LOCATION
    col_double(),    # 19) POINT_X 
    col_double()     # 20) POINT_Y 
  ))

crashes = crashes |>
  filter(ACCIDENT_YEAR %in% c(2021, 2022, 2023))

# =======================================================
# Map of California counties ("sf" object)
#
# st_transform() is used to prevent leaflet from giving a 
# warning since the county polygons are in low scale
# =======================================================
cal_counties_sf = st_as_sf(maps::map("county", plot = FALSE, fill = TRUE)) |> 
  mutate(state = str_extract(ID, "\\w+")) |> 
  filter(state == "california") |> 
  st_transform(crs = 4326) |>
  mutate(COUNTY = str_to_lower(str_replace(ID, "california,", "")))

# ===============================================
# Define "ui" for application
# ===============================================
ui <- fluidPage(
  
  # Application title
  titlePanel("Visualizing Car Crashes in California (2021-2023)"),
  
  # -------------------------------------------------------
  # Input widgets 
  # Customize the following dummy widgets with your own inputs
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # ---------------------------------------------
      # input widgets of first tab
      # (adapt code with widgets of your choice)
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==1",
        h4("Map with Location of Crashes"),
        # replace with your widgets
        sliderInput(inputId = "year1",
                    label = "Select Year:",
                    min = 2021,
                    max = 2023,
                    value = c(2021, 2022),
                    step = 1),
        pickerInput(inputId = "county",
                     label = "Select LOCATION:",
                     choices = sort(unique(crashes$LOCATION)),
                     selected = "Temecula",
                     multiple = TRUE,
                     options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        pickerInput(inputId = "violation",
                    label = "Violation Categories:",
                    choices = sort(unique(crashes$VIOLATION_CATEGORY)),
                    selected = "DUI",
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
      ),  
      
      # ---------------------------------------------
      # input widgets of second tab
      # (adapt code with widgets of your choice)
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==2",
        h4("Choropleth Map"),
        sliderInput(inputId = "year2",
                    label = "Select Year:",
                    min = 2021,
                    max = 2023,
                    value = c(2021, 2022),
                    step = 1),
        # which day of week?
        pickerInput(inputId = "day",
                    label = "Select Day of Week:",
                    choices = sort(unique(crashes$DAY_OF_WEEK)),
                    selected = 1,
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        selectInput(inputId = "severity",
                     label = "Crash Type:",
                     choices = sort(unique(crashes$COLLISION_SEVERITY))),
      ), # closes 2nd panel
      
      # ---------------------------------------------
      # input widgets of third tab
      # (adapt code with widgets of your choice)
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==3",
        h4("Exploratory Analysis"),
        # variables needed: day of week, collison severity, top violation categories, collision severity and death counts
        sliderInput(inputId = "year3",
                    label = "Select Year:",
                    min = 2021,
                    max = 2023,
                    value = c(2021, 2022),
                    step = 1),
        pickerInput(inputId = "county2",
                    label = "Select LOCATION:",
                    choices = sort(unique(crashes$LOCATION)),
                    selected = "Temecula",
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
        selectInput(inputId = "topviolation",
                     label = "Most Amount of Crashes by Violation Category:",
                     choices = c(5, 10, 15, 20),
                     selected = 10,
                     multiple = FALSE),
        pickerInput(inputId = "day",
                    label = "Select Day of Week:",
                    choices = sort(unique(crashes$DAY_OF_WEEK)),
                    selected = 1,
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)),
      ) # closes 3rd panel
      
    ), # closes sidebarPanel
    
    
    # -------------------------------------------------------
    # Main Panel with 3 tabsets: 
    # tab1: map of crash locations
    # tab2: choropleth map (California Counties)
    # tab3: other graphics or summary outputs
    # -------------------------------------------------------
    mainPanel(
      tabsetPanel(
        type = "tabs",
        # first tab (map of crash locations)
        tabPanel(title = "Crash Locations in California",
                 value = 1,
                 leafletOutput("map1", height = 600)),
        # second tab (choropleth map)
        tabPanel(title = "Choropleth Map",
                 value = 2,
                 leafletOutput("map2", height = 600)),
        # third tab (other graphics or summary outputs)
        tabPanel(title = "More",
                 value = 3,
                 plotlyOutput(outputId = "plot1"),
                 hr(),
                 plotlyOutput(outputId = "plot2"),
                 hr(),
                 plotlyOutput(outputId = "plot3"),
                 hr()
        ),
        # selected tab
        id = "tabselected"
        
      ) # closes tabsetPanel
    ) # closes mainPanel
    
    
  ) # closes sidebarLayout
) # closes fluidPage (UI)



# ===============================================
# Define server logic
# ===============================================
server <- function(input, output) {
  
  # ------------------------------------------------
  # Output for first TAB (i.e. map of crashes locations)
  # (adapt code to your analysis)
  # ------------------------------------------------
  output$map1 <- renderLeaflet({
    
    validate(
      need(length(input$violation) > 0, "Please select at least one violation category.")
    )
    validate(
      need(length(input$county) > 0, "Please select at least one county.")
    )
    
    crashes_map1 = crashes |>
      filter(ACCIDENT_YEAR %in% input$year1,
             VIOLATION_CATEGORY %in% input$violation,
             LOCATION %in% input$county) |>
      mutate(LOCATION = str_to_lower(LOCATION))
    
    pal = colorFactor("viridis", domain = crashes_map1$COLLISION_SEVERITY)
    
    crashes_map1 |>
      leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addCircleMarkers(lng = ~POINT_X,
               lat = ~POINT_Y,
               color = ~pal(COLLISION_SEVERITY),
               radius = 6,
               label = ~paste0(
                 "Date: ", COLLISION_DATE, " | ",
                 "Severity: ", COLLISION_SEVERITY, " | ",
                 "Injured: ", NUMBER_INJURED, " | Killed: ", NUMBER_KILLED),
                 clusterOptions = markerClusterOptions()) |>
      addLegend("bottomright", pal = pal, values = ~COLLISION_SEVERITY, title = "Severity")
  })
  
  
  # -----------------------------------------------
  # Output for second TAB (i.e. choropleth map, California counties)
  # (adapt code to make your map of crashes)
  # -----------------------------------------------
  output$map2 <- renderLeaflet({
    
    validate(
      need(length(input$day) > 0, "Please select at least one day of the week.")
    )
    
    crashes_by_county = crashes |>
      filter(ACCIDENT_YEAR %in% input$year2,
             DAY_OF_WEEK %in% input$day,
             COLLISION_SEVERITY == input$severity) |>
      mutate(COUNTY = str_to_lower(COUNTY)) |>
      group_by(COUNTY) |>
      summarize(crash_count = n(), .groups = "drop") 
    
    crash_county_data = left_join(cal_counties_sf, crashes_by_county, by = "COUNTY")
    
    crash_county_data$crash_count[is.na(crash_county_data$crash_count)] = 0
    
    
    pal = colorNumeric("viridis", domain = crash_county_data$crash_count)
    
    crash_county_data |> 
      leaflet() |> 
      addTiles() |> 
      addPolygons(
        fillColor = ~pal(crash_count),
        weight = 1.3,
        color = "black",
        fillOpacity = 0.75,
        label = ~paste0(COUNTY, ": ", crash_count, " crashes")) |>
      addLegend(position = "bottomright",
                pal = pal,              
                values = ~crash_count,          
                title = "Number of Crashes",
                opacity = 0.5)
  })
  
  
  # -----------------------------------------------
  # Output for third TAB (i.e. additional graphics)
  # (adapt code to show additional graphics)
  # -----------------------------------------------
  output$plot1 <- renderPlotly({
    # collision severity by day of week, facet wrap

    crash_summary <- crashes |>
      filter(
        ACCIDENT_YEAR %in% input$year3,
        LOCATION %in% input$county2
      ) |>
      group_by(DAY_OF_WEEK, COLLISION_SEVERITY) |>
      summarise(crash_count = n(), .groups = "drop")
    
    plot1 = ggplot(crash_summary, aes(x = factor(DAY_OF_WEEK), y = crash_count, 
            text = paste0("Day of Week: ", DAY_OF_WEEK, " Total Crashes: ", crash_count))) +
      geom_col(fill = "navy") +
      facet_wrap(~ COLLISION_SEVERITY) +
      labs(
        title = "Crashes by Day of Week",
        subtitle = "By Collision Severity",
        x = "Day of Week (1 = Sunday, 7 = Saturday)",
        y = "Number of Crashes") +
      theme_bw()
    
    ggplotly(plot1, tooltip = "text")
  })
  
  
  output$plot2 <- renderPlotly({
     # fatalities by collision type
    crash_summary2 = crashes |>
      filter(ACCIDENT_YEAR %in% input$year3,
             LOCATION %in% input$county2,
             DAY_OF_WEEK %in% input$day) |>
      group_by(TYPE_OF_COLLISION) |>
      summarize(
        Killed = sum(NUMBER_KILLED, na.rm = TRUE),
        .groups = "drop"
      )
    
    
    plot2 = ggplot(crash_summary2, aes(x = reorder(TYPE_OF_COLLISION, Killed))) +
      geom_col(aes(y = Killed, fill = "Killed"), position = position_nudge(x = 0.2), width = 0.4) +
      scale_fill_manual(values = "#fc9272") +
      coord_flip() +
      labs(
        title = "Fatalities by Collision Type",
        x = "Collision Type",
        y = "Number of People",
        fill = "Outcome"
      ) +
      theme_bw()
    
    ggplotly(plot2, tooltip = "text")
    
  })
  
  output$plot3 <- renderPlotly({
    # most crash counts by violation category

      crashes_summary3 = crashes |>
        filter(ACCIDENT_YEAR %in% input$year3,
               LOCATION %in% input$county2) |>
        group_by(VIOLATION_CATEGORY) |>
        summarise(crash_count = n(), .groups = "drop") |>
        slice_max(crash_count, n = as.numeric(input$topviolation))
      
      
      plot3 = ggplot(crashes_summary3, aes(x = reorder(VIOLATION_CATEGORY, crash_count), y = crash_count,
                     text = paste0("Type of Violation: ", VIOLATION_CATEGORY, " Total Crashes: ", crash_count))) +
        geom_col(fill = "gold") +
        coord_flip() +
        labs(
          title = "Most Accidents by Violation Categories",
          x = "Violation Category",
          y = "Number of Crashes"
        ) +
        theme_bw()
      
      ggplotly(plot3, tooltip = "text")
  })
  
  
} # closes server



# ===============================================
# Run the application
# ===============================================
shinyApp(ui = ui, server = server)
