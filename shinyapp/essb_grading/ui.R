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

      numericInput(inputId = "n_quest",
                   label = "Number of MC Questions:",
                   value = 40),

      selectInput(inputId = "n_choices",
                  label = "Number of Choices:",
                  choices=c(2,3,4,5),
                  selected = 4),
    ),

    mainPanel(
      uiOutput("subtitle"),
      tabsetPanel(type = "tabs",
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


