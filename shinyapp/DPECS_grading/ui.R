library(shiny)

shinyUI(fluidPage(
  titlePanel("Grading MC Exam at the DPECS"),
  
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      numericInput(inputId = "n_quest",
                   label = "Number of MC Questions:",
                   value = 40),
      
      selectInput(inputId = "n_choices",
                  label = "Number of Choices:",
                  choices=c(2,3,4,5), 
                  selected = 4),
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Table", tableOutput("view")),
                  tabPanel("Graph", plotOutput("graph")),
                  tabPanel("Formular", uiOutput("formula"))
      )
    )
  )
))

