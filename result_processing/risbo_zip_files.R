# reading Risbo zip files
#
# (c) Oliver Lindemann, MIT licence

.version_risbo_zip_files = "0.5"

library(tidyverse)
library(stringi)
# requires also zip package (do not import zip library globally, because of conflicts with this script [unclear bug in zip-library])


zipfile_check <- function(zipfile, warnings=FALSE) {
  # checks if file is a risbo file in good shape
  # needs to be a zip that includes one instance of the following files:
  #    *.sta.txt, *.lsr.txt, *.frm.ini
  #
  # returns list with relevant files name in zip or NULL

  fls = NULL
  try(fls <- unzip(zipfile, list=TRUE),
      silent = TRUE)

  if (is.null(fls)) {
    if (warnings) warning(paste(zipfile, ": Can't open zip file"))
    return(NULL)

  } else {
    sta_file <- filter(fls, str_ends(Name,  "-sta.txt"))
    lsr_file <- filter(fls, str_ends(Name,  "-lsr.txt"))
    ini_file <- filter(fls, str_ends(Name,  "-frm.ini"))
    dta_file <- filter(fls, str_ends(Name,  "-dta.txt"))
    if (nrow(sta_file)!=1 | nrow(lsr_file)!=1 | nrow(ini_file)!=1 | nrow(dta_file)!=1) {
      if (warnings)
        warning(paste0(zipfile, ": File not in good shape."))
    } else
    return(list(sta_file=sta_file$Name,
                lsr_file=lsr_file$Name,
                ini_file=ini_file$Name,
                dta_file=dta_file$Name))
  }
}


read_exam_info <- function(zipfile) {
  # reading basic exam information
  
  rtn <- list()
  rtn$n_options = NULL

  fls <- zip::zip_list(zipfile) # do not import zip
  ini_file <- filter(fls, str_ends(filename,  "-frm.ini"))
  if (nrow(ini_file)!=1) {
    stop(paste0(zipfile, "is not a correct risbo file. No *-frm.ini file"))
  }

  tags = c("ProcessingNumber", "CourseCode", "ProfName", "ExamName","ExamDate",
           "ExamType", "NumQuestion", "NumGroup")

  fl = unz(zipfile, ini_file$filename)
  lines <- readLines(fl)
  for (x in str_split(lines, "=")) {
    if (sum(x[1] == tags)>0) {
      cmd <- stri_unescape_unicode(paste0("rtn$", x[1], "<-'", x[2], "'"))
      eval(parse(text=cmd ))
    } else if (is.null(rtn$n_options) & x[1] == "Question") {
       # assuming n_options is the same for all questions in this test
      rtn$n_options = as.numeric(unlist(str_split(x[2], ":"))[3])
    }
  }
  close(fl)

  rtn$file = ini_file$filename
  rtn$timestamp = ini_file$timestamp
  rtn$NumQuestion = as.numeric(rtn$NumQuestion)
  rtn$NumGroup = as.numeric(rtn$NumGroup)
  rtn$ProcessingNumber = as.numeric(rtn$ProcessingNumber)

  return(rtn)
}


read_individual_answers <- function(zipfile) {
  # read individual answers from risbo zip file
  
  message(paste0("reading ", zipfile))
  fls = zipfile_check(zipfile)
  fl = unz(zipfile, fls$dta_file)
  answers = read_csv(file=fl, comment = "*", col_names = FALSE)  %>% 
    select(-X2, -X3, -X4)
  
  info = read_exam_info(zipfile)
  colnames(answers) = c("student", paste0("Q", str_c(c(1:info$NumQuestion))))
  return(answers)
}

read_mc_exam_statistics <- function(zipfile) {
  # read grades and item statistics

  message(paste0("reading ", zipfile))
  fls = zipfile_check(zipfile)
  info = read_exam_info(zipfile)
  rtn <- list(processing_number = as.numeric(info$ProcessingNumber),
              info = info,
              grades = .parse_grades(zipfile, fls$lsr_file))

  rtn <- append(rtn, .parse_item_statistics(zipfile, fls$sta_file))

  class(rtn) <- "RisboMCExam"
  return(rtn)
}



.parse_grades <- function(zipfile, lsr_file) {
  fl = unz(zipfile, lsr_file)
  rtn <- read_delim(fl, delim="\t", col_names = FALSE,
                    skip=1, trim_ws = TRUE, na=c("NO"),
                    col_types = c(col_integer(),col_double()))
  names(rtn) <- c("student_id", "grade")
  return(rtn)
}


.parse_item_statistics <- function(zipfile, sta_file) {
  question_id = NA
  item_statistics_paragraph= FALSE
  tmp_table = NA
  choices_profile = data.frame() # --> choices_profile
  choices = data.frame()
  M_values = data.frame()
  restcorr = data.frame()
  item_statistics = data.frame()
  lcnt = 0
  
  fl = unz(zipfile, sta_file)
  for (l in readLines(fl)) {
    if (str_starts(l, "Vraag") & !item_statistics_paragraph) {
      x = str_split(l, " ")[[1]]
      if (length(x)==2) {
        question_id = as.numeric(x[2])
        tmp_table = data.frame()
        lcnt = 0
      }
    }
    else if (str_starts(l, "STATISTISCH OVERZICHT")) {
      question_id = NA
      item_statistics_paragraph = TRUE
      tmp_table = data.frame()
      lcnt = 0
    }
    else if (!is.na(question_id)) {
      lcnt = lcnt + 1
      if (lcnt == 4) { # varnames
        varnames = .parse_columns(l, skip_chars = 55)
        n_options = length(varnames) - 2
        is_true_false = (varnames[length(varnames)] == "score")
        if (is_true_false) {n_options = 2}
        correct = which(str_locate(varnames, "[*]")[,1]>0) -1
        n_correct = length(correct)
        varnames = str_remove_all(varnames, "[*]")
        if (n_correct>2) {
          message(paste0("WARNING: More than 2 options are correct in question ", question_id, " in ", zipfile))
        }
        
      } else if (lcnt>=6 & lcnt<= 10) {
        # table choices_profile
        d = .parse_columns(l, skip_chars = 55, as_numeric = TRUE)
        tmp_table = rbind(tmp_table, data.frame(t(d)))
        if (lcnt==10) {
          names(tmp_table) = varnames
          tmp_table$group = rownames(tmp_table)
          tmp_table$question = question_id
          if (is_true_false) {
            tmp_table$score = c()
          }
          choices_profile = rbind(choices_profile, tmp_table)
          tmp_table = NA
        }
        
      } else if (lcnt == 12) {
        #choices
        tmp = .parse_columns(l, skip_chars = 55, as_numeric = TRUE)
        tmp = data.frame(t(tmp))
        names(tmp) = varnames
        if (n_options == 3) { #three options only, add empty d options for later joining
          tmp$d = NA
        }
        tmp$n_options = n_options
        tmp$correct = correct[1]
        if (n_correct>1) tmp$correct2 = correct[2]
        else tmp$correct2 = NA
        tmp$n_correct = n_correct
        tmp$correct_choosen = tmp[,correct[1] + 1]
        tmp$question = question_id
        if (is_true_false) {
          tmp$score = c()
        }
        choices = rbind(choices, tmp)
        
      } else if (lcnt == 14) {
        # m-values
        tmp = .parse_columns(l, skip_chars = 65, as_numeric = TRUE)
        tmp = data.frame(t(tmp))
        if (is_true_false) {
          names(tmp) = varnames[2:(length(varnames)-1)]
        } else
          names(tmp) = varnames[2:length(varnames)]
        tmp$question = question_id
        M_values = rbind(M_values, tmp)
      } else if (lcnt == 15) {
        # restcorr
        tmp = .parse_columns(l, skip_chars = 65, as_numeric = TRUE)
        tmp = data.frame(t(tmp))
        if (is_true_false) {
          names(tmp) = varnames[2:(length(varnames)-1)]
        } else {
          names(tmp) = varnames[2:length(varnames)]
        }
        tmp$question = question_id
        restcorr = rbind(restcorr, tmp)
        
      }
    } # end !is.na(question_id)
    
    else if (item_statistics_paragraph) {
      lcnt = lcnt + 1
      
      if (lcnt==3) {
        varnames = .parse_columns(l)
        varnames[length(varnames)] = "Vraag2"
      }
      else if (lcnt>3) {
        
        if (str_detect(l, "TENTAMENPROGRAMMA")) {
          item_statistics_paragraph = FALSE
        }
        else if (!str_starts(l, "---")) {
          tmp = .parse_columns(l, as_numeric = TRUE)
          tmp = data.frame(t(tmp))
          names(tmp) = varnames
          item_statistics = rbind(item_statistics, tmp)
        }
      }
      
    }
  } # end for
  close(fl)
  item_statistics = tibble(item_statistics) %>%
    select(-Vraag2) %>%
    rename("question"="Vraag")
  
  return(list(choices_profile=tibble(choices_profile),
              item_statistics=item_statistics,
              choices = tibble(choices),
              M_values=tibble(M_values),
              restcorr=tibble(restcorr)))
}

.parse_columns <- function(txt, skip_chars=0, as_numeric=FALSE) {
  row = str_split(str_sub(txt, start=skip_chars+1), " ", simplify = TRUE)
  row = Filter(str_length, row) # remove null length
  if (as_numeric)  return(suppressWarnings(as.numeric(row)))
  else return(row)
}




