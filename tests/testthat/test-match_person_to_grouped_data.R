library(testthat)

test_that("Exact match works within correct group", {
  data <- data.frame(
    FN = c("Carl", "Joseph", "George"),
    LN = c("Linnaeus", "Banks", "Bentham"),
    UPI = c("CL001", "JB002", "GB003"),
    Expedition = c("Sweden", "Endeavour", "Australia"),
    stringsAsFactors = FALSE
  )

  result <- match_person_to_grouped_data(FN = "Carl", LN = "Linnaeus", data, group_column = "Expedition", group_value = "Sweden")
  expect_equal(result$UPI, "CL001")
  expect_equal(result$message, "EXACT")
})

test_that("Returns NA if match is outside the group", {
  data <- data.frame(
    FN = c("Carl", "Joseph"),
    LN = c("Linnaeus", "Banks"),
    UPI = c("CL001", "JB002"),
    Expedition = c("Sweden", "Endeavour"),
    stringsAsFactors = FALSE
  )

  result <- match_person_to_grouped_data("Carl", "Linnaeus", data, group_column = "Expedition", group_value = "Endeavour")
  expect_true(is.na(result$UPI))
  expect_equal(result$message, "No match")
})

test_that("Swapped names still work within group", {
  data <- data.frame(
    FN = c("Alexander", "Carl"),
    LN = c("von Humboldt", "Linnaeus"),
    UPI = c("AH003", "CL001"),
    Expedition = c("South America", "Sweden"),
    stringsAsFactors = FALSE
  )

  result <- match_person_to_grouped_data("von Humboldt", "Alexander", data, group_column = "Expedition", group_value = "South America")
  expect_equal(result$UPI, "AH003")
  expect_equal(result$message, "EXACT (Swap)")
})


test_that("Partial fuzzy match within group returns best", {
  data <- data.frame(
    FN = c("Agnes"),
    LN = c("Arber"),
    UPI = c("AA004"),
    Expedition = c("UK"),
    stringsAsFactors = FALSE
  )

  result <- match_person_to_grouped_data("Agness", "Smith", data, group_column = "Expedition", group_value = "UK", max_dist = 2)
  expect_equal(result$UPI, "AA004")
  expect_match(result$message, "PARTIAL", ignore.case = TRUE)
})

test_that("Works with custom column names and grouping", {
  data <- data.frame(
    Given = c("Janet"),
    Family = c("Browne"),
    Code = c("JB003"),
    Group = c("UK"),
    stringsAsFactors = FALSE
  )

  result <- match_person_to_grouped_data("Janet", "Browne", data,
                                         FN_column = "Given",
                                         LN_column = "Family",
                                         UPI_column = "Code",
                                         group_column = "Group",
                                         group_value = "UK")
  expect_equal(result$UPI, "JB003")
  expect_equal(result$message, "EXACT")
})
