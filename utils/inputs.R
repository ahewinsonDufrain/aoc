## common utils

library(readr)
library(glue)
library(stringr)
library(magrittr)

readInputs <- function(year) {
  dname <- glue("{year}/inputs/")
  ifiles <- list.files(dname)
  ivars <- str_replace_all(ifiles, ".txt", "")
  ivars
  mapply(ivars, ifiles,
         FUN = function(v, f) assign(v, read_file(glue("{dname}{f}")), envir = .GlobalEnv))
  return(NULL)
}

split_newlines <- function(x) {
  x %>% 
    str_split("[\r\n]+") %>% 
    unlist()
}

remove_empty_lines <- function(x) {
  x[x!=""]
}

standard_parse_text <- function(x) {
  x %>% split_newlines() %>% remove_empty_lines()
}

standard_parse_number <- function(x) {
  x %>% standard_parse_text %>% as.numeric()
}

split_double_line_break <- function(x) {
  x %>% 
    str_split("(\r*\n){2,}") %>% 
    unlist() %>% 
    lapply(str_split, "\r*\n") %>% 
    lapply(unlist)
}
