#' Clean names
#'
#' @param x name to clean
#' @param rm_whit Flag (TRUE/FALSE) for whether whitespace wants to be removed from the names
#'
#' @return cleaned name
#' @export
#'
#' @examples
#'  clean_name('John Leonard Knapp')
#'  clean_name('Pierre-Joseph Redouté')
#'
#' @details
#' "Cleans" name by changing to lower case and translating to Latin-ASCII (i.e removing accents, etc). Can also remove whitespace by setting rm_whit = T.
clean_name <- function(x, rm_whit = F){
  if(rm_whit == T){ x = x |> stringi::stri_replace_all_charclass(pattern = "\\p{WHITE_SPACE}", "")}
  x |> tolower() |> stringi::stri_trans_general(id = "Latin-ASCII")
}

