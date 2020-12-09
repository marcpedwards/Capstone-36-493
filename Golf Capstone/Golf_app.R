library(shiny)
library(tidyverse)
library(ggplot2)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(readxl)
library(gtools)
library(shinyWidgets)
library(gridExtra)

# Import Data
GolfData <- read_csv("total.csv")
gain <- read_csv("gained_putting.csv")

GolfData <- GolfData %>% 
  filter(., Round == "Competitive" | Round == "Qualifying" | Round == "Tournament") %>% 
  merge(., gain[, c("PuttLength", "Exp_Putts")], by = "PuttLength") %>% 
  mutate(., strokesgained = Exp_Putts - Putts) %>% 
  mutate(., FIR = recode(FIR, "Miss" = "Missed"))


button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"

# Define UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "shortcut icon", type = "image/png", href = "CMU.png")),
  navbarPage(
    title = tags$div(img(src="CMU.png", height = '35px', width = '35px'), "Golf Capstone"),
             theme = shinytheme('flatly'),
             tabPanel("Team", fluid = TRUE, icon = icon("users"),
                      titlePanel("Team Performance"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          fluidRow(column(12, 
                                          checkboxGroupButtons(inputId = "GenderFinder", 
                                                               label = "Select Team(s):", 
                                                               choices = c("Men" = "Male", 
                                                                           "Women" = "Female"), 
                                                               justified = TRUE, status = "primary",
                                                               selected = "Male",
                                                               checkIcon = list(yes = icon("ok", 
                                                                                           lib = "glyphicon")))
                                          
                          ),
                          hr(),
                          column(12, 
                                 pickerInput(inputId = "CourseFinder",
                                             label = "Select Course:",
                                             choices = unique(GolfData$Course),
                                             selected = "Longue Vue Club",
                                             options = list(`actions-box` = TRUE, 
                                                            style = "btn-primary"),
                                             multiple = T)
                          ),
                          
                          # Select Round Type
                          hr(),
                          column(12, 
                                 awesomeCheckboxGroup(inputId = "RoundFinder", 
                                                      label = "Select Round Type:", 
                                                      choices = unique(GolfData$Round), 
                                                      selected = "Qualifying",
                                                      status = "primary")
                          ),
                          
                          # Metric
                          hr(),
                          column(12, 
                                 awesomeRadio(inputId = "MetricFinder", 
                                              label = "Select Performance Metric:", 
                                              choices = c("Scoring Average", "Par Scoring",
                                                          "Putting Distribution", "Fairways/Greens in Regulation",
                                                          "Up & Down", "Strokes Gained Putting", 
                                                          "Strokes Gained Approach"),
                                              selected = "Scoring Average", 
                                              status = "primary")
                                 
                          )
                          )
                        ),
                        
                        mainPanel(
                          plotOutput(outputId = "distPlot"),
                          DT::dataTableOutput('rawdata')
                        )
                      )
             ),
             
             tabPanel("Individual", fluid = TRUE, icon = icon("user-alt"),
                      titlePanel("Player Performance"),
              
                          fluidRow(
                            column(4,
                                   wellPanel(pickerInput(inputId = "PlayerFinder",
                                               label = "Select Player:",
                                               #ab[mixedorder(as.character(ab$a)),]
                                               choices = unique(GolfData$Name)[mixedorder(unique(GolfData$Name))],
                                               selected = "Player 1",
                                               options = list(`actions-box` = TRUE, 
                                                              style = "btn-primary")))),
                            column(8, plotOutput(outputId = "IndPlot"))),

                      fluidRow(
                          column(12, DT::dataTableOutput('IndRaw'))
                        )
                      
             ),
             
             tabPanel("Developers", fluid = TRUE, icon = icon("laptop-code"),
                      p(a("Xinzhe Qi", href = "mailto:xqi@andrew.cmu.edu"), style = "font-size:25px"),
                      p("e-mail: xqi@andrew.cmu.edu", style = "font-size:20px"),
                      p(a("Yedin Lui", href = "mailto:yclui@andrew.cmu.edu"), style = "font-size:25px"),
                      p("email: yclui@andrew.cmu.edu", style = "font-size:20px"),
                      p(a("Marc Edwards", href = "mailto:mpedward@andrew.cmu.edu"), style = "font-size:25px"),
                      p("e-mail: mpedward@andrew.cmu.edu", style = "font-size:20px"))
  )
)




# Define server

server <- function(input, output) {
  Team <- reactive({
    req(input$GenderFinder)
    req(input$CourseFinder)
    req(input$RoundFinder)
    filter(GolfData, Gender %in% input$GenderFinder) %>% 
      filter(Course %in% input$CourseFinder) %>% 
      filter(Round %in% input$RoundFinder)
  })
  
  output$distPlot <- renderPlot({
    if (input$MetricFinder == "Scoring Average") {
      ggplot(subset(Team(), Hole == "Total"), 
             aes(x = reorder(Name, -Score, FUN = median), 
                 y = Score, fill = "#BD2031")) +
        geom_boxplot() +
        theme_bw() +
        coord_flip() +
        labs(
          x = "Names",
          y = "Score",
          title = "Scoring Distribution") +
        theme(legend.position = "none")}
    
    else if (input$MetricFinder == "Par Scoring") {
      ggplot(subset(Team(), Par == 3 | Par == 4 | Par == 5), 
             aes(x = as.factor(Score),
                 fill = ifelse(Score == Par, "Highlighed", "Normal"))) +
        scale_fill_manual(values=c("#BD2031", "gray")) +
        geom_bar(aes(y = (..count..)/sum(..count..))) +
        scale_y_continuous(labels=scales::percent) +
        facet_wrap(~ Par) +
        theme_bw() +
        labs(
          x = "Score",
          y = "Count",
          title = "Par Scoring") +
        theme(legend.position = "none")}
    
    else if (input$MetricFinder == "Putting Distribution") {
      ggplot(subset(Team(), Par == 3 | Par == 4 | Par == 5), aes(x = Putts)) +
        geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#BD2031") +
        geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),2)),
                      y = ((..count..)/sum(..count..)/2)), stat="count", 
                  color = "white", size = 6 ) + 
        scale_y_continuous(labels=scales::percent) +
        theme_bw() +
        theme(legend.position = "none") +
        labs(
          x = "Number of Putts",
          y = "Count",
          title = "Putting Distribution") }
    
    else if (input$MetricFinder == "Fairways/Greens in Regulation") {
      fir <- ggplot(subset(Team(), Par == 3 | Par == 4 | Par == 5), aes(x = FIR)) +
        geom_bar(mapping = aes(x = factor(1), fill = FIR)) +
        coord_polar(theta = "y") +
        scale_fill_manual(values=c("#BD2031", "#808080", "lightgray")) +
        theme_void() +
        theme(legend.position="bottom") + 
        theme(legend.title = element_blank()) +
        labs(title = "Fairways in Regulation") +
        theme(plot.title = element_text(hjust = 0.5))
      
      gir <- ggplot(subset(Team(), Par == 3 | Par == 4 | Par == 5), aes(x = GIR)) +
        geom_bar(mapping = aes(x = factor(1), fill = GIR)) +
        coord_polar(theta = "y") +
        scale_fill_manual(values=c("#BD2031", "#808080", "lightgray")) +
        theme_void() +
        theme(legend.position="bottom") + 
        theme(legend.title = element_blank()) +
        labs(title = "Greens in Regulation") +
        theme(plot.title = element_text(hjust = 0.5))
      
      grid.arrange(fir, gir, nrow = 1) }
    
    else if (input$MetricFinder == "Strokes Gained Putting") {
      ggplot(Team(), aes(x = PuttLength, y = strokesgained)) +
        geom_point() +
        geom_smooth(method = lm, se = TRUE, col = "#BD2031") +
        theme_bw() +
        labs(x = "Length of Putt",
             y = "Strokes Gained",
             title = "Strokes Gained: Putting")
    }
  })
  
  Player <- reactive({
    subset(GolfData, 
           Name == paste("Player", as.numeric(gsub("Player ", "", input$PlayerFinder))))
    
  })
  
  output$IndPlot <- renderPlot({
    ggplot(subset(Player(), Hole == "Total"), 
           aes(x = reorder(Course, -Score, FUN = median), 
               y = Score, fill = "BD2031")) +
      geom_boxplot() +
      theme_bw() +
      coord_flip() +
      labs(
        x = "Courses",
        y = "Score",
        title = paste("Scoring Distribution of", input$PlayerFinder)) +
      theme(legend.position = "none")
  })
  
  # show raw data
  # observeEvent(input$RoundFinder, {
  #   print(paste0("You have chosen: ", input$RoundFinder))
  # })
  
  Team_metrics <- reactive({
    if(input$MetricFinder == "Scoring Average") {
      gainputt <- GolfData %>% 
        filter(Gender %in% input$GenderFinder) %>% 
        filter(Course %in% input$CourseFinder) %>% 
        filter(Round %in% input$RoundFinder) %>%
        group_by(Name) %>% 
        summarize(., "Scoring Average" = mean(Score[Hole == "Total"])) %>% 
        mutate_if(is.numeric, round, 2) }
    else if (input$MetricFinder == "Par Scoring") {
      gainputt <- GolfData %>% 
        filter(Gender %in% input$GenderFinder) %>% 
        filter(Course %in% input$CourseFinder) %>% 
        filter(Round %in% input$RoundFinder) %>%
        summarize(., "Par 3" = mean(Score[Par == 3]),
                  "Par 4" = mean(Score[Par == 4]),
                  "Par 5" = mean(Score[Par == 5])) %>% 
        mutate_if(is.numeric, round, 4) }
    else if (input$MetricFinder == "Putting Distribution") {
      gainputt <- GolfData %>% 
        filter(Gender %in% input$GenderFinder) %>% 
        filter(Course %in% input$CourseFinder) %>% 
        filter(Round %in% input$RoundFinder) %>%
        summarize(., "Average Putts" = mean(Putts[Par == 3 | Par == 4 | Par == 5])) %>% 
        mutate_if(is.numeric, round, 4) }
    else if (input$MetricFinder == "Fairways/Greens in Regulation") {
      gainputt <- GolfData %>% 
        filter(Gender %in% input$GenderFinder) %>% 
        filter(Course %in% input$CourseFinder) %>% 
        filter(Round %in% input$RoundFinder) %>%
        summarize(., "FIR %" = mean(FIR[Par == 3 | Par == 4 | Par == 5] == "Hit")*100,
                  "GIR %" = mean(GIR[Par == 3 | Par == 4 | Par == 5] == "Hit")*100) %>% 
        mutate_if(is.numeric, round, 4) }
    else if (input$MetricFinder == "Strokes Gained Putting") {
      gainputt <- GolfData %>% 
        filter(Gender %in% input$GenderFinder) %>% 
        filter(Course %in% input$CourseFinder) %>% 
        filter(Round %in% input$RoundFinder) %>%
        group_by(Name) %>% 
        summarize(., "Strokes Gained" = mean(strokesgained)) %>% 
        mutate_if(is.numeric, round, 4) 
      }
  })
  
  options(DT.options = list(pageLength = 20))
  output$rawdata <- DT::renderDataTable(Team_metrics(),
                                        style = "bootstrap", 
                                        class = 'table-bordered table-condensed')
  output$IndRaw <- DT::renderDataTable(Player(), 
                                       style = "bootstrap", 
                                       class = 'table-bordered table-condensed')
}


# Shiny
shinyApp(ui = ui, server = server)









