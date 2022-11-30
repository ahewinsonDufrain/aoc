library(glue)
pp <- function(x, day) {
  print("The answer to day {day}: {x}")
}

pp_try <- function(test_answer, expected, function_if_successful, day, part) {
  if (test_answer == expected) {
    print(glue("Answer to day {day} part {part} = {function_if_successful}"))
  } else {
    print(glue("Error: Expected value {expected} not equal to {test_answer}"))
  }
  NULL
}
