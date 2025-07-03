#' Check ID Column Format and Establishment Number Consistency
#'
#' This function checks that all IDs in the specified ID column of a data frame:
#' \itemize{
#'   \item Have exactly 10 characters in length.
#'   \item Contain a substring from position 3 to 5 that matches the corresponding value
#'         in the establishment number column.
#' }
#'
#' @param data A data frame containing the ID and establishment number columns.
#' @param ID_column A string specifying the name of the ID column in `data`.
#' @param estab_no_column A string specifying the name of the establishment number column in `data`.
#'
#' @return A list containing messages and row indices of any rows with issues.
#'         If no issues are found, the function returns `NULL`.
#'
#' @examples
#' data <- data.frame(ID = c('1233344444', '3455666666'),
#'                    estab_no = c('333', '555'))
#' check_ID(data, "ID", "estab_no")
#'
#' @export
check_ID <- function(data, ID_column, estab_no_column) {
  IDs <- data[[ID_column]]
  estab_nos <- data[[estab_no_column]]

  # Check 1: ID length
  bad_length_idx <- which(stringr::str_length(IDs) != 10)

  # Check 2: Matching 3rd to 5th digit with estab_no
  mid_digits <- substr(IDs, 3, 5)
  mismatched_idx <- which(mid_digits != estab_nos)

  messages <- list()

  if (length(bad_length_idx) > 0) {
    messages <- append(messages, list(
      list(message = "Not all IDs have length 10", index = bad_length_idx)
    ))
    warning("Not all IDs have length 10")
  }

  if (length(mismatched_idx) > 0) {
    messages <- append(messages, list(
      list(message = "Digits 3-5 of ID do not match establishment number", index = mismatched_idx)
    ))
    warning("Digits 3-5 of ID do not match establishment number")
  }

  if (length(messages) > 0) {
    return(messages)
  }

  return(NULL)
}
