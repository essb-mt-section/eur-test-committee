# Processing of exam result files provided by *Risbo* or *Remindo*

* `risbo_zip_files.R`: collection of functions to process Risbo exam results file (Risbo zip-file) in `R`
* `remindo_results.py`: Python class to process Remindo exam result files (JSON files).

## Processing Risbo results in `R`

Reading the zip-file send by Risbo:
```R
# R code
source('risbo_zip_files.R')

print(read_exam_info("RISBO_RESULTS.ZIP"))

# individual answers for own statistics and calulations
answer <- read_individual_answers("RISBO_RESULTS.ZIP")

# grades and item statistics
statistics <- read_mc_exam_statistics("RISBO_RESULTS.ZIP")

```


## Getting the results of Remindo exams as csv

1. Export JSON files for Remindo website: Click on "Results/Analysis". Go to "test moment" and select the test of interest. Click on "Result details". At the end of the page, you can now the option tp export the result details as JSON file.

2. Convert JSON file to a csv with Python using  `remindo_results.py`:

```python
# python code
from remindo_results import RemindoResults
rr = RemindoResults("remindo_result_exam_A.json",
                        n_questions=41, ignored_questions=[0])
rr.save_csv_files()
````

---
(c) O. Lindemann
