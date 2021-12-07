
#' Internal function to capitalize the first letter of a character
#' 
#' Internal helper function to capitalize the first letter of a character value.
#' The use case is to create a plot label like 'Age' from a variable name like
#' 'age'.
#' 
#' @param char Character value whose first letter should be capitalized
#' 
#' @import checkmate
#' 
capitalize_firstLetter <- function(char) {
  
  checkmate::assert_character(char, len = 1)
  
  
  char_cap <- paste0(toupper(substr(char, 1, 1)), substr(char, 2, nchar(char)))
  
  return(char_cap)
}