
library(shiny)
library(tidyverse)
library(conflicted)
conflicts_prefer(dplyr::filter)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("NSC Enrollment Lost"),
  
  # Sidebar with a slider input for picking college years 
  sidebarLayout(
    sidebarPanel(
      uiOutput("year_range_ui")
    ),
    mainPanel(
      plotOutput("barPlot"),
      plotOutput("barPlot2")
    )))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  load("data/nsc.fake.Rdata")
  
  undergrad.fake_nsc <- fake_nsc[fake_nsc$`Class.Level` %in% c(
    "Freshman (Undergraduate)", 
    "Sophomore (Undergraduate)",
    "Junior (Undergraduate)", 
    "Senior (Undergraduate)", 
    "Certificate (Undergraduate)", 
    "Unspecified (Undergraduate)", 
    "Bachelor's (Undergraduate)", 
    "Associate's"), ]
  
  output$year_range_ui <- renderUI({
    selectInput("year_range", "Select a College Year:",
                choices = unique(fake_nsc$`TO.College.Year`),
                selected = unique(fake_nsc$`TO.College.Year`)[1])
    
  })
  
  
  
  output$barPlot <- renderPlot({
    req(input$year_range)
    
    
    filtered_year <- undergrad.fake_nsc %>%
      filter(`TO.College.Year` == input$year_range) %>%
      filter(`College.Sequence` != "1") %>%
      filter(`College.Name` != "CALIFORNIA STATE UNIVERSITY - CHICO")
    
    top.undergrad <-  filtered_year %>% 
      count(`College.Name`) %>%
      top_n(10,n) %>%
      arrange(desc(n))
    
    ggplot(top.undergrad, aes(x=reorder(`College.Name`, n), y=n)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      geom_text(aes(label=n), vjust = 0.5, color="black") +
      labs(title = paste("Top 10 Undergrad Colleges Students Transferred to in the", input$year_range, "College Year"),
           x = "College Names", y="Count") +
      coord_flip() +
      theme_minimal()
  })
  
  output$barPlot2 <- renderPlot({
    
    chico <- undergrad.fake_nsc %>%
      filter(`College.Name` == "CALIFORNIA STATE UNIVERSITY - CHICO") 
    
    ggplot(chico, aes(x = `LE.College.Year`)) +
      geom_bar( fill = "lightblue") +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = 0.5, color = "black")+
      theme_minimal() +
      labs(fill = "Highlighted") +
      theme(axis.text.x = element_text(angle=90)) +
      labs(title ="Last Enrollment College Years for Chico State",
           x = "LE College Year", y="Count") +
      theme(axis.text.x = element_text(angle=90))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
