# VERSION 2
# + new handling of bonus questions

library(shiny)
library(ggplot2)
library(dplyr)
source("essb_grading_schema.R")



print_table_long <- function(schema, decimals, truncating) {
    df <- arrange(grading_table(schema, omit_low_scores = TRUE,
                                decimals=decimals,
                                truncating=truncating),
                  desc(score))

}

grading_formular <- function(schema) {
  rtn = "<p/>"

  c_g = round(schema$c_g, 3)
  d = (schema$max_score - schema$c_g) / 10
  rtn = paste0(rtn, "The grades \\(g_i\\) for the test scores \\(x_i\\) are given by
         $$\\begin{align}
         g_i &= \\frac{x_i - ", c_g, "}{", schema$max_score , "-", c_g, "} * 10
             = \\frac{x_i - ", c_g, "}{", round(d, 3), "}
         \\end{align}$$
         ")
    paste0(rtn," </p>")
    return(rtn)
}


plot_grades <- function(schema, max_score, truncating) {
    #points = data.frame(x=c(schema$passing_score, schema$corr_n_quest),
    #                    y=c(schema$passing_grade, schema$highest_grade))
    d = grading_table(schema, max_score=max_score, decimals=1,
                      truncating=truncating)
    ggplot() +
        geom_line(data=d, aes(x=score, y=grade)) +
        #geom_point(data = points, aes(x,y), size=3, alpha = 0.75) +
        geom_hline(yintercept = schema$passing_grade, colour = "#999999") +
        scale_y_continuous(name="Grade", breaks=schema$lowest_grade:schema$highest_grade) +
        xlab('Test Score') +
        theme_bw() +
        theme(text = element_text(size = 20))
}

explanation_html <- function() {
    rtn = "<p>Variables: $$\\begin{align}
          N &= \\text{number of questions} \\\\
          n_\\text{choices} &= \\text{number of choices in each question} \\\\
          n_\\text{disabled} &= \\text{number of disabled questions} \\\\
          n_\\text{bonus} &= \\text{number of bonus questions}  \\\\
          n_\\text{full} &= \\text{number of full point questions}
           \\end{align}$$ </p>"

    rtn = paste0(rtn, "<p>Guessing correction is defined as
                 $$c_g = \\frac{N - n_\\text{disabled} - n_\\text{full}}{n_\\text{choices}}$$ </p>")

    rtn = paste0(rtn, "<p>The grades \\(g_i\\) for the test scores \\(x_i\\) are given by

                   $$ g_i = \\frac{x_i - c_g}{N - n_\\text{disabled} - n_\\text{bonus} - c_g} * 10$$ </p>")

    ## rtn = paste0(rtn, "<p>The result will be truncated to one decimal place (and not rounded).</p>")

    return(rtn)
}

# Define server logic to summarize and view selected dataset ----
shinyServer(function(input, output) {

    schema <- reactive({
        essb_grading_schema(n_questions = input$n_quest,
                            n_choices = as.numeric(input$n_choices),
                            n_bonus=as.numeric(input$n_bonus),
                            n_disabled=as.numeric(input$n_disabled),
                            n_full=as.numeric(input$n_full_points))

    })
    decimals <- reactive(as.numeric(input$decimals))

    # Show the first "n" observations ----
    output$view <- renderTable(na = "", digits = decimals, {
        print_table_long(schema(), decimals=decimals(),
                         truncating=input$trunc=="truncate")
    })
    output$graph <- renderPlot({
        plot_grades(schema(), max_score = input$n_quest,
                    truncating=input$trunc=="truncate")
    })
    output$formula <- renderUI({
        withMathJax(HTML(grading_formular(schema())))
    })

    output$explanation <- renderUI({
        withMathJax(HTML(explanation_html()))
    })

    output$subtitle <- renderUI({
      n = input$n_quest  - as.numeric(input$n_disabled)
      rtn <- paste0("<H3>Grading procedure for ", n,
                      " questions with ",input$n_choices," choices</H3>")
      rtn <- paste0(rtn, "<H4>")
      if (input$n_bonus > 0) {
        rtn <- paste0(rtn, " and ", input$n_bonus, " <b>bonus</b> question(s)")
      }
      if (input$n_full_points > 0) {
        rtn <- paste0(rtn, " and ", input$n_full_points, " <b>full point</b> question(s)")
      }
      rtn <- paste0(rtn, "</H4>")
      HTML(rtn)
    })

    observeEvent(input$link_to_graph, {
      updateTabsetPanel(inputId="panels", selected="Graph/Formular")
    })

})
