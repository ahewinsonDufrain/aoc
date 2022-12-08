library(tidyverse)
library(magrittr)

source("utils/all_utils.R")
readInputs(2022)
day <- 7

input7 %<>% standard_parse_text()
test7 %<>% standard_parse_text()

# day 7 utils -------------------------------------------------------------
track_wd <- function(x) {
  wd <- character(length(x))
  for (i in seq_along(x)) {
    wd[i] <- if(x[i] == "$ cd /") "/"
    else if (str_detect(x[i], "\\$ cd \\.\\.")) str_extract(wd[i-1], "(.*/)") %>% str_replace("(.+)/$", "\\1")
    else if (str_detect(x[i], "\\$ cd [^\\.]")) paste(wd[i-1], "/", str_replace(x[i], "\\$ cd ", ""), sep="") %>% str_replace_all("//", "/")
    else wd[i-1]
  }
  wd
}
extract_sizes <- function(x) {
  str_extract_all(x, "^[0-9]+") %>% as.numeric() %>% replace_na(0)
}

# part a ------------------------------------------------------------------
## solve function for part a
solve7a <- function(inp) {
  dafr <- tibble(command=inp, wd=track_wd(inp), size=extract_sizes(inp))
  wds <- paste0("^", dafr %>% select(wd) %>% unique() %>% unlist())
  sizes <- sapply(wds, function(q) {
    dafr %>% 
      filter(str_detect(wd, q)) %>% 
      select(size) %>% 
      unlist() %>% 
      sum()
  })
  sum(sizes[sizes <= 100000])
}

## test part a
test_answer_a <- solve7a(test7)
expected_a    <- 95437

## solve part a
pp_try(test_answer_a, expected_a, solve7a(input7), day, "a")

# part b ------------------------------------------------------------------
## solve function for part b
solve7b <- function(inp) {
  dafr <- tibble(command=inp, wd=track_wd(inp), size=extract_sizes(inp))
  wds <- paste0("^", dafr %>% select(wd) %>% unique() %>% unlist())
  sizes <- sapply(wds, function(q) {
    dafr %>% 
      filter(str_detect(wd, q)) %>% 
      select(size) %>% 
      unlist() %>% 
      sum()
  })
  cap = 70000000
  used = sizes[1]
  free = cap - used
  min(sizes[sizes > 30000000-free])
}

## test part b
test_answer_b <- solve7b(test7)
expected_b    <- 24933642

## solve part b
pp_try(test_answer_b, expected_b, solve7b(input7), day, "b")
