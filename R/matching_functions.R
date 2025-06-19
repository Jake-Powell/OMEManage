#' Title
#'
#' @param FN First name
#' @param LN Last name
#' @param data Data frame containing name and UPI columns
#' @param FN_column Column name for first names (default: "First Name")
#' @param LN_column Column name for surnames (default: "Surname")
#' @param UPI_column Column name for Unique Person Identifer (UPI) (default: "UPI")
#' @param ... parameters passed to clean_name().
#'
#' @return A list with UPI, people row, and a message
#' @export
#'
#' @examplesIf FALSE
#' botanists <- data.frame(
#'   FN = c("Carl", "José", "Alexander", "Agnes", "  Jane  "),
#'   LN = c("Linnæus", "Banks", "Humboldt", "Arber", "Coldstream"),
#'   UPI = c("CL001", "JB002", "AH003", "AA004", "JC005"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Basic exact match with accents
#' match_person_to_data("Jose", "Banks", botanists)  # Matches "José"
#'
#' # Name with extra spaces
#' match_person_to_data("Jane", "Coldstream", botanists)  # Matches "  Jane  "
#'
#' # Lowercase input, uppercase and accent in data
#' match_person_to_data("carl", "linnaeus", botanists)  # Matches "Carl" "Linnæus"
#'
#' # Swapped names
#' match_person_to_data("Linnæus", "Carl", botanists)  # Swap detection
#'
#' # No match
#' match_person_to_data("Greg", "Mendel", botanists)  # No match
#'
#' botanists <- data.frame(
#'   `First Name` = c("Carl", "José", "Alexander", "Agnes", "  Jane  ", "Jake"),
#'   Surname = c("Linnæus", "Banks", "Humboldt", "Arber", "Coldstream", "Banks"),
#'   UPI = c("CL001", "JB002", "AH003", "AA004", "JC005", "CL002"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # LN only with multiple individuals
#' match_person_to_data("John", "Banks", botanists)  # Matches "José"
#'
match_person_to_data <- function(FN, LN, data, FN_column = 'FN', LN_column = 'LN', UPI_column = 'UPI', ...){
  FN = FN |> clean_name(...) ; LN = LN |> clean_name(...)
  data[[FN_column]] =data[[FN_column]] |> clean_name(...)
  data[[LN_column]] =data[[LN_column]] |> clean_name(...)

  FN_index = which(data[[FN_column]] == FN)
  LN_index = which(data[[LN_column]]  == LN)
  both_index = intersect(FN_index, LN_index)
  FN_swap_index = which(data[[FN_column]] == LN)
  LN_swap_index = which(data[[LN_column]] == FN)
  both_swap_index = intersect(FN_swap_index, LN_swap_index)

  if(both_index |> length() > 0){
    return(list(UPI = data[[UPI_column]][both_index], people = data[both_index, ], message = 'EXACT'))
  }
  if(LN_index |> length() > 0){
    return(list(UPI = data[[UPI_column]][LN_index], people = data[LN_index, ], message = 'LN only'))
  }
  if(FN_index |> length() > 0){
    return(list(UPI = data[[UPI_column]][FN_index], people = data[FN_index, ], message = 'FN only'))
  }
  if(both_swap_index |> length() > 0){
    return(list(UPI = data[[UPI_column]][both_swap_index], people = data[both_swap_index, ], message = 'EXACT (Swap)'))
  }
  if(LN_swap_index |> length() > 0){
    return(list(UPI = data[[UPI_column]][LN_swap_index], people = data[LN_swap_index, ], message = 'LN only (Swap)'))
  }
  if(FN_swap_index |> length() > 0){
    return(list(UPI = data[[UPI_column]][FN_swap_index], people = data[FN_swap_index, ], message = 'FN only (Swap)'))
  }
  return(list(UPI = NA, people = NULL, message = 'Neither FN or LN'))
}


# ## Needs fixing
# Append_UPI_data_by_group <- function(data, UPIdata, group_var = 'URN'){
#   # Loop over URNs and match
#   parts = lapply(URNs, function(URN){
#     teachers = Teacher_Worforce[which(Teacher_Worforce$URN == URN),]
#     teachers_db_name = apply(teachers[,c(3,4)], 1, function(x) paste0(x, collapse = ' ')) |> sort()
#     cur = WondeTeachers[which(WondeTeachers$URN == URN),]
#     cur$teacherName = cur$AllTeacher
#     cl_teach = cur
#     teacher_match = lapply(1:nrow(cl_teach), function(index){
#       dd = cl_teach[index,]
#       out = match_teacher(FN = dd[5], LN = dd[6], teachers = teachers)
#       out
#     })
#     names(teacher_match) =  cur$teacherName
#     UPIs = lapply(teacher_match, function(x) x$UPI) |> unlist()
#     message = lapply(teacher_match, function(x) x$message) |> unlist()
#     match_teachers = lapply(teacher_match, function(x){
#       if(is.null(x$teacher)){ return(NA)}
#       apply(x$teacher[,c(3,4)], 1, function(y)paste0(y,collapse = ' ') ) |> paste0(collapse = ' OR ')
#     })  |> unlist()
#     cur$TeacherMatchName = (match_teachers |> as.character())[match(cur$teacherName, names(match_teachers))]
#     cur$TeacherUPI = (UPIs |> as.character())[match(cur$teacherName, names(UPIs))]
#     cur$TeacherMessage = (message |> as.character())[match(cur$teacherName, names(message))]
#     cur$AllTeacherSchool = teachers_db_name |> paste0(collapse = '\n')
#     return(cur)
#
#
#   })
#   TeacherClass2.0 = do.call(rbind, parts) # rejoin together.
# }
