
#' Internal function to capitalize the first letter of a character
#' 
#' Internal helper function to capitalize the first letter of a character value.
#' The use case is to create a plot label like 'Age' from a variable name like
#' 'age'.
#' 
#' @param char Character value whose first letter should be capitalized
#' 
capitalize_firstLetter <- function(char) {
  
  char_cap <- paste0(toupper(substr(char, 1, 1)), substr(char, 2, nchar(char)))
  
  return(char_cap)
}