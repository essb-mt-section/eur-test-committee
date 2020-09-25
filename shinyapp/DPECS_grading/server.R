library(shiny)
library(ggplot2)
library(dplyr)
source("grading_schema.R")


print_table_wide <- function(schema) {
    df = data.frame()
    x = 1
    c = 1
    while(TRUE) {
        scores = x:(x+19)
        scores[scores>schema$n_questions] = NA
        df[1:20, paste0("Score", c)] = scores
        df[1:20, paste0("Grade", c)] = grades(scores, schema, rounding_digits = 1)
        x = x + 20
        c = c + 1
        if (x>schema$n_questions) break
    }
    return(df)
}

print_table_long <- function(schema) {
    df <- arrange(grading_table(schema, omit_low_scores = TRUE), 
                  desc(score))
    
}

grading_formular <- function(schema) {
    rtn = "Formular: $$grade = \\begin{cases}"
    rtn = paste0(rtn, round(schema$interp_pass$const, 3), "+", round(schema$interp_pass$b, 3),
                 "\\cdot score & \\text{if } score > ", round(schema$lowest_grade_breakpoint,2),
                 "\\\\")
    rtn = paste0(rtn, schema$lowest_grade, " & \\text{if } score \\leq",
                 round(schema$lowest_grade_breakpoint,2))
    return(paste0(rtn," \\end{cases}$$"))
}


plot_grades <- function(schema) {
    points = data.frame(x=c(schema$passing_score, schema$n_questions),
                        y=c(schema$passing_grade, schema$highest_grade))
    ggplot() +
        geom_line(data=grading_table(schema), aes(x=score, y=grade)) +
        geom_point(data = points, aes(x,y), size=3, alpha = 0.75) +
        geom_hline(yintercept = schema$passing_grade, colour = "#999999") +
        scale_y_continuous(name="Grade", breaks=schema$lowest_grade:schema$highest_grade) +
        xlab('Test Score') +
        theme_bw()
}



# Define server logic to summarize and view selected dataset ----
shinyServer(function(input, output) {
    
    schema <- reactive({
        n_choices = as.numeric(input$n_choices)
        grading_schema(n_questions = input$n_quest,
                           guessing_score = input$n_quest/n_choices,
                           rounding_up_passing_score = FALSE,
                           bilinear_interpolation = FALSE)
    })
    
    # Show the first "n" observations ----
    output$view <- renderTable(na = "", digits = 1, {
        print_table_long(schema())
    })
    output$graph <- renderPlot({
        plot_grades(schema())
    })
    output$formula <- renderUI({
        withMathJax(HTML(paste("<p>", grading_formular(schema()), "</p>")))
    })
    
})
