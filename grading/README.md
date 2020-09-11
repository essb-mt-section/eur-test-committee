## Calculating grades from scores of multiple choise exams

For the **description of the grading procedures** and example **grading tables** see `grading_mc_exam.pdf` (produced by `grading_mc_exam.Rmd`).


The script `grading.R` provides functions to generate a grading schemata and to caluate grades from scores. For example, the method used by Risbo for a exam with 40 question and 4 choises.

```R
source("grading.R")

# define  grading schema
schema <- grading_schema(n_questions=40,
                         guessing_score=40/4,
                         lowest_grade=1,
                         highest_grade=10,
                         passing_grade=5.5,
                         passing_score_percentage=55,
                         bilinear_interpolation=FALSE,
                         rounding_up_passing_score=TRUE)

# convert vector with my scorce to grades
scores = c(23, 26, 27, 28, 17, 34, 40, 32, 11)
grades(scores, schema)

# print grading table
grading_table(schema)

```



---

(c) O. Lindemann, MIT licence