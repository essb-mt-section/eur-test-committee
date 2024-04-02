# version 0.3
#
# (c) Oliver Lindemann
# MIT licence


round_half_up = function(x, digits) {
  # round half up (R default is "round half to even")
  # see round2() at https://stackoverflow.com/questions/12688717/round-up-from-5
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*sign(x)
}

#constants
formula_guessing_correction = "$$c_g = \\frac{N - n_\\text{disabled} - n_\\text{full}}{n_\\text{choices}}$$"
formula_grade = "$$ g_i = 10* \\bigg( \\frac{x_i - n_\\text{full} - c_g}{N - n_\\text{disabled} - n_\\text{bonus}  - n_\\text{full} - c_g} * \\frac{N-n_\\text{full}}{N} +  \\frac{n_\\text{full}}{N} \\bigg) $$"
formula_grade_no_fullpoints = "$$ g_i = 10* \\frac{x_i  - c_g}{N - n_\\text{disabled} - n_\\text{bonus} - c_g} $$"

essb_grading_schema <- function(n_questions,
                                n_choices,
                                n_bonus=0,
                                n_disabled=0,
                                n_full=0,
                                lowest_grade=1,
                                highest_grade = 10,
                                passing_grade=5.5
                                ) {

  rtn <- list(
    n_questions = n_questions,
    n_choices = n_choices,
    n_bonus= n_bonus,
    n_disabled= n_disabled,
    n_full=n_full,
    highest_grade = highest_grade,
    lowest_grade = lowest_grade,
    passing_grade = passing_grade,
    c_g = (n_questions - n_disabled - n_full) / n_choices # guessing correction/probability score
    )

  class(rtn) <- "essb grading schema"
  return(rtn)
}

#' Calculating grades from scores using grading schema object
#'
#' @param scores   the scores to be converted
#' @param schema   the essb grading schema object
#' @param decimals  number of digits of rounding (default: 1)
#' @param truncating   truncating and not rounding to decimal place (default: FALSE)
#'
#' @seealso documentation and examples in grading_schema
#
grades <- function(scores,
                   schema,
                   decimals=1,
                   truncating=FALSE) {
  gs = schema
  N = gs$n_questions
  ratio = (scores-gs$n_full-gs$c_g) /
          (N - gs$n_disabled - gs$n_bonus - gs$n_full - gs$c_g)

  x <- gs$highest_grade * (ratio * (N-gs$n_full) / N + (gs$n_full/N) )
  if (truncating) {
    x <- floor(x*10^decimals)/10^decimals
  } else {
    x <- round_half_up(x, decimals)
  }
  x[x<gs$lowest_grade] = gs$lowest_grade
  x[x>gs$highest_grade] = gs$highest_grade
  return(x)
}


#' grading table of a schema
#'
#' @param schema
#' @param omit_low_scores  omits the very lower scores for which the grade is 1
#' @param max_score  if not defines (n_questions - n_disabled) is used
#' @param decimals  number of digits of rounding (default: 1)
#' @param truncating   truncating and not rounding to decimal place (default: FALSE)
#' @return grading table as data frame
grading_table <- function(schema,
                          omit_low_scores=FALSE,
                          decimals=1,
                          max_score=NA,
                          truncating=FALSE) {
  if (is.na(max_score)) {
    max_score = schema$n_questions - schema$n_disabled
  }
  s = c(0:max_score)
  df <- data.frame(score=s, grade=grades(s, schema,
                                         decimals = decimals,
                                         truncating=truncating))
  if (omit_low_scores) {
    i = max(which(df$grade==1))
    df <- df[i:nrow(df), ]
  }
  return(df)
}


#' make latex grading formular for grading
#'
#' @param schema
#' @param omit_low_sco
grading_formular <- function(schema) {
    nom_str = "x_i" # nominator str
    denom = (schema$n_questions - schema$n_disabled - schema$n_bonus - schema$n_full - schema$c_g)
    denom_str = paste0(schema$n_questions) # denominator str

    add_full_str = ""
    brk = c("", "") # brackets

    if (schema$n_disabled>0) {
      denom_str = paste0(denom_str, "-", schema$n_disabled )
    }
    if (schema$n_bonus>0) {
      denom_str = paste0(denom_str, "-", schema$n_bonus )
    }
    if (schema$n_full>0) {
      denom_str = paste0(denom_str, "-", schema$n_full )
      nom_str = paste0(nom_str, "-", schema$n_full )
      n_q = schema$n_questions - schema$n_full
      add_full_str = paste0("* \\frac{", n_q,"}{", schema$n_questions,"} + \\frac{",schema$n_full,"}{",schema$n_questions,"}")
      brk = c("\\bigg(", "\\bigg)") # bracket
      fx =  paste0("\\frac{",n_q ,"\\,x_i - ", round_half_up(n_q*(schema$c_g+schema$n_full), 3), "}{", round_half_up(schema$n_questions*denom/10, 3), "}+ \\frac{", 10*schema$n_full,"}{",schema$n_questions,"}")
    } else {
      fx =  paste0(" \\frac{x_i - ", round_half_up(schema$c_g, 3), "}{", round_half_up(denom/10, 3), "}")
    }
    denom_str = paste0(denom_str, "-", round_half_up(schema$c_g, 3))
    nom_str = paste0(nom_str, "-", round_half_up(schema$c_g, 3))

    return(paste0("$$ g_i = 10 * ", brk[1], "\\frac{",  nom_str, "}{", denom_str, "}",
                  add_full_str, brk[2], " = ", fx, "$$"))
}


x = essb_grading_schema(40, 4, n_full = 1)
print(grading_table(x))

