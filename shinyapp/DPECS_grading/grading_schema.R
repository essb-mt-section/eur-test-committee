# (c) O. Lindemann, MIT licence
# version 0.2.2

#' Generate regression function to convert multiple choice scores to grades
#'
#' @param n_questions number of questions in the MC exam
#' @param guessing_score score for guessing correction, which is usually
#'                       n_questions divided by number of answer options
#' @param lowest_grade   lowest possible grade (default: 1)
#' @param highest_grade  highest possible grade (default: 10)
#' @param passing_grade  grade of passing the exam (default: 5.5)
#' @param passing_score_percentage percentage of correct answers that is
#'                       corresponding to the passing grade (default: 55)
#' @param bilinear_interpolation   bilinear interpolation, that is, separate
#'                      interpolations for pass and failed scores (default: FALSE)
#' @param rounding_up_passing_score if TRUE, calculated passing scores will
#'                                  be rounded up, if FALSE an exact caesura will
#'                                  be used (default: TRUE)
#'
#' @return grading schema object that can be used to calculate the grades with
#'         the `grades` function. 
#'         
#'         A grading schema comprises in addition to the parameter
#'         $interp_pass: constant and regression weight of linear
#'                       interpolation of passing scores
#'         $interp_fail: linear interpolation of failing scores
#'         $passing_score: calculated passing score
#'         $lowest_grade_breakpoint: regression crossing lowest grade
#'                       all scores lower (or higher for reversed scaling)
#'                       this break point are the lowest grade
#'
#' @examples
#' # Example Risbo grading
#' schema <- grading_schema(n_questions=40,
#'              guessing_score=40/4,
#'              lowest_grade=1,
#'              highest_grade=10,
#'              passing_grade=5.5,
#'              passing_score_percentage=55,
#'              bilinear_interpolation=FALSE,
#'              rounding_up_passing_score=TRUE)
#'
#' # convert vector with my scorce to grades
#' scores = c(23, 26, 27, 28, 17, 34, 40, 32, 11)
#' grades(scores, schema)
#'
#' # print grading table
#' grading_table(schema)
#'
#' # Example Remindo grading, grades between 1 and 10
#'=
#' schema <- grading_schema(n_questions=40,
#'              guessing_score=40/4,
#'              lowest_grade=1,
#'              highest_grade=10,
#'              passing_grade=5.5,
#'              passing_score_percentage=55,
#'              bilinear_interpolation=TRUE,
#'              rounding_up_passing_score=FALSE)
#'
#' # Example German grading system, grades from 6 to 1
#'
#' schema <- grading_schema(n_questions=40,
#'              guessing_score=40/4,
#'              lowest_grade=6,
#'              highest_grade=1,
#'              passing_grade=4.5,
#'              passing_score_percentage=55,
#'              rounding_up_passing_score=FALSE)
#'
#' @author Oliver Lindemann <lindemann@essb.eur.nl>
grading_schema <- function(n_questions,
                           guessing_score,
                           lowest_grade=1,
                           highest_grade=10,
                           passing_grade=5.5,
                           passing_score_percentage=55,
                           rounding_up_passing_score=TRUE,
                           bilinear_interpolation=FALSE) {

  passing_score = (n_questions-guessing_score) * passing_score_percentage/100  + guessing_score
  if (rounding_up_passing_score) {
    passing_score = ceiling(passing_score) }

  # interpolation
  ia = .lin_interpol(p1 = c(passing_score, passing_grade),
                     p2 = c(n_questions, highest_grade))
  if (bilinear_interpolation) {
    ib = .lin_interpol(p1 = c(passing_score, passing_grade),   # for lower grades
                       p2 = c(guessing_score, lowest_grade))
  } else {ib=NA}

  # lowest_grade_breakpoint = regression crossing lowest grade
  #     all scores lower (or higher for reversed scaling) this break point
  #     are the lowest grade
  if (!is.na(ib[1])) tmp=ib else tmp=ia
  lowest_grade_breakpoint <- (lowest_grade-tmp$const)/tmp$b

  rtn <- list(
              interp_pass = ia,
              interp_fail = ib,
              passing_score=passing_score,
              lowest_grade_breakpoint = lowest_grade_breakpoint,
              n_questions = n_questions,
              guessing_score=guessing_score,
              passing_grade=passing_grade,
              lowest_grade=lowest_grade,
              highest_grade=highest_grade,
              bilinear_interpolation=bilinear_interpolation)

  class(rtn) <- "grading schema"
  return(rtn)
}

#' Calculating grades from scores using grading schema object
#' 
#' @param scorces          the scores to be converted
#' @param grading_schema   the grading schema object
#' @param rounding_digits  number of digits of rounding (default: 2)
#' 
#' @seealso documentation and examples in grading_schema
#' 
grades <- function(scores, grading_schema, rounding_digits=2) {
  idx_na <- 
  gs <- grading_schema
  grd = round(gs$interp_pass$const + gs$interp_pass$b * scores,
              rounding_digits)
  bilinear_interpolation <- !is.na(gs$interp_fail[1])
  if (bilinear_interpolation) {
    grd_b = round(gs$interp_fail$const + gs$interp_fail$b * scores,
                  rounding_digits)
    idx <- scores < gs$passing_score
    idx <- idx[!is.na(idx)]
    grd[idx] = grd_b[idx]
  }

  # lowest grade
  inverse_grading_scale = gs$lowest_grade > gs$highest_grade
  if (inverse_grading_scale) {
    grd[grd > gs$lowest_grade] = gs$lowest_grade
  } else {
    grd[grd < gs$lowest_grade] = gs$lowest_grade
  }
  return(grd)
}

#' grading table of a schema
#' 
#' @param schema
#' @param omit_low_scores  omits the very lower scores for wich the grade is 1
#' @return grading table as data frame 
grading_table <- function(schema, omit_low_scores=FALSE) {
  s = c(0:schema$n_questions)
  df <- data.frame(score=s, grade=grades(s, schema))
  if (omit_low_scores) {
    i = max(which(df$grade==1))
    df <- df[i:nrow(df), ]
  }
  return(df)
}
  
  
.lin_interpol <- function(p1, p2) {
  b = (p2[2]-p1[2]) / (p2[1]-p1[1])
  c = p2[2] - b*p2[1]
  return(list(const = c, b = b))
}
