library(shiny)

eur_title <- function(title) {
  rtn = paste0('<table style="background-color:#FFA300" width=100%>',
               '<tr><td valign="middle" style="padding-left: 10px;" >')
  rtn = paste0(rtn, "<H1>", title, "</H1>")
  rtn = paste0(rtn, '</td><td align="right" style="padding-right: 20px;">
        <img src="./Logo-EUR-black.png" width="150" alt="Erasmus University">
        </td></tr></table>',
               '<br><br>')
  return(HTML(rtn))
}

shinyUI(fluidPage(

  eur_title("Grading MC Exams at the DPECS"),

  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(

      h3("Exam Type"),
      numericInput(inputId = "n_quest",
                   label = "Number of MC Questions:",
                   min = 5,
                   value = 40),

      selectInput(inputId = "n_choices",
                  label = "Number of Choices:",
                  choices=c(2,3,4,5),
                  selected = 4),


      h3("Adjustments"),

      h5("Number of Adjustments"),

      selectInput(inputId = "n_disabled",
                  label = "Disabled:",
                  choices=c(0,1,2,3),
                  selected = 0),

      selectInput(inputId = "n_bonus",
                   label = "Bonus:",
                  choices=c(0,1,2,3),
                  selected = 0),

      selectInput(inputId = "n_full_points",
                   label = "Full Points:",
                  choices=c(0,1,2,3),
                  selected = 0),

      h4("Rounding"),

      selectInput(inputId = "decimals",
                  label = "Decimals:",
                  choices=c(1,2,3),
                  selected = 1),

      selectInput(inputId = "trunc",
                  label = "Type:",
                  choices=c("rounding","truncating"),
                  selected = "round"),
    ),

    mainPanel(
      uiOutput("subtitle"),
      tabsetPanel(type = "tabs",
                  id = "panels",
                  tabPanel("Table", tableOutput("view")),
                  tabPanel("Graph/Formular", uiOutput("formula"), plotOutput("graph")),
                  tabPanel("Explanation", uiOutput("explanation"),
                           HTML("<p style='margin-top:50px;text-align:right'>
                           <i><small>(c) <a href='http://www.eur.nl/people/oliver-lindemann'>
                                O. Lindemann</a></small></i></p>"))
      )
    )
  )
))


