# ===============================================
# Fill in the following fields
# ===============================================
# Title: State of the Union Text Analysis Shiny App
# Description: App that analyzes the text of State of
#              Union speeches through a bigram analysis 
#              based on party and a sentiment analysis (nrc) over time
# Author: Jaxon Kaeller
# Date: Spring 2025


# ===============================================
# Required packages
# (you can use other packages if you want to)
# ===============================================
library(shiny)
library(tidyverse)  # for data manipulation and graphics
library(tidytext)   # for text mining
library(DT)         # to render Data Tables nicely
library(plotly)     # if you are interested in using ggplotly()



# ===============================================
# Import data
# ===============================================
sotu = read_csv("sotu.csv")
# "C:/Users/jaxon/OneDrive/Desktop/STAT133/shinyapps/app2/
sotu = sotu |>
  filter(year >= 1970)

nrc = read_csv("nrc.csv")
# C:/Users/jaxon/OneDrive/Desktop/STAT133/shinyapps/app2/sentiment-lexicons/
# ===============================================
# Define "ui" for application
# ===============================================

ui <- fluidPage(
  
  # Application title
  titlePanel("Text Analysis of SOTU Addresses by Each POTUS"),
  
  # -------------------------------------------------------
  # Input widgets 
  # Customize the following dummy widgets with your own inputs
  # -------------------------------------------------------
  fluidRow(
    # choose how many bigrams to show
    column(3,
           p(em("Analysis 1")),
           numericInput(inputId = "top_n_bigrams", 
                       label = "Number of Top Bigrams",
                       value = 10,
                       min = 5,
                       max = 50)),
    
    # filters speeches by year
    column(3,
           p(em("Analysis 1 & 2")),
           sliderInput(inputId = "year_range", 
                        label = "Select Year Range:",
                        min = min(sotu$year),
                        max = max(sotu$year),
                        value = c(2000, 2020),
                        step = 1)),
                  
   # focus on one party's sentiment           
   column(3,
          p(em("Analysis 2")),
          selectInput(inputId = "party", 
                        label = "Choose party",
                        choices = c("Democratic", "Republican", "Both"),
                        selected = "Democratic")),
                         
   # narrow sentiment terms                
   column(3,
          p(em("Analysis 2")),
          checkboxGroupInput("selected_emotions",
                       label = "Select Emotions to Display:",
                       choices = c("anger", "anticipation", "disgust", "fear",
                                          "joy", "sadness", "surprise", "trust"),
                       selected = c("joy", "trust", "fear")))
  ), # closes fluidRow
  
  hr(), # horizontal rule
  
  # -------------------------------------------------------
  # Tabset Panel of outputs
  # Customize the following output elements with your own outputs
  # -------------------------------------------------------
  tabsetPanel(type = "tabs",
              tabPanel("Bigram Analysis",
                       plotOutput("plot1"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("NRC Lexicon Analysis", 
                       plotOutput("plot2"),
                       hr(),
                       dataTableOutput('table2'))
  ) # closes tabsetPanel
  
) # closes ui



# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # you may need to create reactive objects
  # (e.g. dummy data frame to be used in plot1)
  
  # tokenization by political party and bigrams
  bigrams_by_party = reactive({
    sotu |>
      filter(year >= input$year_range[1],
             year <= input$year_range[2]) |>
      unnest_tokens(bigram, text, token = "ngrams", n = 2) |>
      separate(bigram, into = c("word1", "word2"), sep = " ") |>
      filter(!word1 %in% stop_words$word,
             !word2 %in% stop_words$word,
             !is.na(word1), !is.na(word2)) |>
      unite(bigram, word1, word2, sep = " ") |>
      count(party, bigram, sort = TRUE) |>
      group_by(party) |>
      slice_max(n, n = input$top_n_bigrams, with_ties = FALSE) |>
      ungroup()
  })
  
  
  # ===============================================
  # Outputs for the first TAB
  # ===============================================
  
  # code for plot1
  output$plot1 = renderPlot({
    bigrams_by_party() |>
      mutate(bigram = reorder_within(bigram, n, party)) |>
      ggplot(aes(x = bigram, y = n, fill = party)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c("Democratic" = "blue", "Republican" = "red")) +
      facet_wrap(~party, scales = "free_y") +
      scale_x_reordered() +
      coord_flip() +
      labs(x = "Bigram", y = "Count", title = "Top Bigrams by Republicans and Democrats",
           subtitle = "Over selected year range") +
      theme_gray()
  })
  
  # code for numeric summaries of frequencies
  output$table1 <- renderDataTable({
    bigrams_by_party()
  })
  
  
  # ===============================================
  # Outputs for the second TAB
  # ===============================================
  
  # tokenization based on nrc sentiment 
  
  sentiment_by_party <- reactive({
    sotu_filtered = sotu |>
      filter(year >= input$year_range[1],
             year <= input$year_range[2]) |>
      unnest_tokens(word, text) |>
      inner_join(nrc, by = "word") |>
      filter(!sentiment %in% c("positive", "negative"))
    
    # Filter based on party selection
    if (input$party == "Democratic") {
      sotu_filtered = sotu_filtered |> filter(party == "Democratic")
    } else if (input$party == "Republican") {
      sotu_filtered = sotu_filtered |> filter(party == "Republican")
    }
    
    if (isTruthy(input$selected_emotions)) {
      sotu_filtered = sotu_filtered |>
        filter(sentiment %in% input$selected_emotions)
    }
    
    sotu_filtered |>
      count(party, sentiment, sort = TRUE)
  })
  
  
  # code for plot2
  output$plot2 <- renderPlot({
    sentiment_by_party() |>
      ggplot(aes(x = party, y = n, fill = party)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c("Democratic" = "blue", "Republican" = "red")) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(
        title = "Emotional Language by Party",
        subtitle = "Based on SOTU Addresses and Selected Years",
        x = "Party", y = "Word Frequency") +
      theme_minimal()
  })
  
  # code for statistics
  output$table2 <- renderDataTable({
    sentiment_by_party()
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

