# version 0.1
#
# (c) Oliver Lindemann
# MIT licence

essb_grading_schema <- function(n_questions,
                                n_choices,
                                n_bonus=0,
                                n_disabled=0,
                                n_full=0,
                                lowest_grade=1,
                                max_grade = 10) {

  rtn <- list(
    n_questions = n_questions,
    n_choices = n_choices,
    n_bonus= n_bonus,
    n_disabled= n_disabled,
    n_full=n_full,
    max_grade = max_grade,
    lowest_grade = lowest_grade,
    c_g = (n_questions - n_disabled - n_full) / n_choices, # guessing correction/probability score
    max_score = n_questions - n_disabled - n_bonus # max score
    )

  class(rtn) <- "essb grading schema"
  return(rtn)
}

#' Calculating grades from scores using grading schema object
#'
#' @param scores   the scores to be converted
#' @param schema   the essb grading schema object
#' @param rounding_digits  number of digits of rounding (default: 1)
#'
#' @seealso documentation and examples in grading_schema
#'
grades <- function(scores, schema, rounding_digits=1) {
  gs <- schema

  x <- gs$max_grade * (scores - gs$c_g) / (gs$max_score - gs$c_g)
  x <- round(x, rounding_digits)
  x[x<gs$lowest_grade] = gs$lowest_grade
  x[x>gs$max_grade] = gs$max_grade
  return(x)
}


#' grading table of a schema
#'
#' @param schema
#' @param omit_low_scores  omits the very lower scores for which the grade is 1
#' @param rounding_digits  number of digits of rounding (default: 1)
#' @return grading table as data frame
grading_table <- function(schema, omit_low_scores=FALSE,  rounding_digits=1) {
  gs <- schema
  s = c(0:gs$max_score)
  df <- data.frame(score=s, grade=grades(s, schema,
                                         rounding_digits = rounding_digits))
  if (omit_low_scores) {
    i = max(which(df$grade==1))
    df <- df[i:nrow(df), ]
  }
  return(df)
}


