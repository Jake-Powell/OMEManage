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

#' Expand Rows by Name Variants
#'
#' Splits first and last name columns (containing multiple names separated by a delimiter)
#' into multiple rows so that each row corresponds to a single FN/LN pair.
#' Other columns are repeated accordingly.
#'
#' @param data A data frame containing at least the FN, LN, and UPI columns.
#' @param FN_column A string indicating the name of the first name/s column.
#' @param LN_column A string indicating the name of the last name/s column.
#' @param sep A string delimiter used to separate multiple names in a single cell (default is `'---'`).
#'
#' @return A data frame where each row corresponds to a single combination of FN and LN variant,
#'         with all original columns preserved and replicated as necessary.
#' @export
#'
#' @examples
#' botanists <- data.frame(
#'   FNs = c("Carl---Karl", "José", "Alexander---Alex---Alex", "Agnes", "  Jane  "),
#'   LNs = c("Linnaus---Linnaus", "Banks", "Humboldt---Humboldt---Humbouldt", "Arber", "Coldstream"),
#'   UPI = c("CL001", "JB002", "AH003", "AA004", "JC005"),
#'   stringsAsFactors = FALSE
#' )
#'
#' expand_name_variants(botanists, FN_column = "FNs", LN_column = "LNs")
expand_name_variants <- function(data, FN_column, LN_column, sep = '---') {
  rows <- lapply(seq_len(nrow(data)), function(i) {
    fns <- unlist(strsplit(data[[FN_column]][i], sep))
    lns <- unlist(strsplit(data[[LN_column]][i], sep))

    # Ensure equal length by recycling shorter vector
    len <- max(length(fns), length(lns))
    if (length(fns) != len) fns <- rep(fns, length.out = len)
    if (length(lns) != len) lns <- rep(lns, length.out = len)

    # Repeat all other columns
    base_row <- data[i, , drop = FALSE]
    replicated_rows <- base_row[rep(1, len), , drop = FALSE]

    # Replace FN and LN with variants
    replicated_rows[[FN_column]] <- fns
    replicated_rows[[LN_column]] <- lns

    return(replicated_rows)
  })

  do.call(rbind, rows)
}
