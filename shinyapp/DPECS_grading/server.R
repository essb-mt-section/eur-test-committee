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
        theme_bw() +
        theme(text = element_text(size = 20))
}

explanation_html <- function(nq, nc) {
    # nq: n questions, nc: numbre choices
    g = round(nq/nc, 2)
    sp = round(.55 * (nq- g) + g, 2)
    rtn = paste0("<p>",
                 "To calculate the grades from the scores of a multiple choices test, the ",
                 "DPECS uses a single linear interpolation between the point of the pass-fail criterion, ",
                 "\\(P_1\\), and the highest possible grade, \\(P_2\\). The smallest passing score has to fulfil the ",
                 "criterion of at least 55% knowledge, that is, 55% correct questions after guessing correction.",
                 "</p>") 
    
    rtn = paste0(rtn, "<p>",
                 "If we have n=",nq, " questions with ",nc," choices, the guessing correction is: $$c_g=", nq, "/", nc, "=", g, "$$ ",
                 "With a knowledge criterion \\(k_p=.55\\) and a passing grade \\(g_{p}=5.5\\), the passing score \\(s_p\\) is :",
                 "$$s_p = k_p ( n - c_g) + c_g = .55\\, (",nq, "-", g, ")+",g,"= ",sp,"$$",
                 "Thus, if the highest possible grade is \\(g_{h}=10\\), the interpolation uses the following two points: ",
                 "$$P_1 = (s_p, g_p)=(", sp, ",5.5)$$ $$P_2 =(n, g_h)=(",nq, ",10) $$",
                 "</p>") 
    
    return(rtn)
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
    
    output$explanation <- renderUI({
        withMathJax(HTML(explanation_html(input$n_quest, as.numeric(input$n_choices))))
    })
    
    output$subtitle <- renderUI({
        rtn <- paste0("<H4>Grading procedure for ", input$n_quest, 
                      " questions with ",input$n_choices," choices</H4>")
        HTML(rtn)
    })

    
})
