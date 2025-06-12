library(testthat)

test_that("Exact match handles case insensitivity and accents", {
  data <- data.frame(
    FN = c("José", "Carl"),
    LN = c("Banks", "Linnæus"),
    UPI = c("JB002", "CL001"),
    stringsAsFactors = FALSE
  )

  result1 <- match_person_to_data("jose", "banks", data)
  expect_equal(result1$UPI, "JB002")
  expect_equal(result1$message, "EXACT")

  result2 <- match_person_to_data("carl", "linnaeus", data)
  expect_equal(result2$UPI, "CL001")
  expect_equal(result2$message, "EXACT")
})

test_that("Whitespace trimming works when rm_whit = TRUE", {
  data <- data.frame(
    FN = c("  Jane  "),
    LN = c("Coldstream"),
    UPI = c("JC005"),
    stringsAsFactors = FALSE
  )

  result <- match_person_to_data("Jane", "Coldstream", data, rm_whit = TRUE)
  expect_equal(result$UPI, "JC005")
  expect_equal(result$message, "EXACT")
})

test_that("Swapped names are matched correctly", {
  data <- data.frame(
    FN = c("Carl"),
    LN = c("Linnaeus"),
    UPI = c("CL001"),
    stringsAsFactors = FALSE
  )

  result <- match_person_to_data("Linnaeus", "Carl", data)
  expect_equal(result$UPI, "CL001")
  expect_equal(result$message, "EXACT (Swap)")
})

test_that("Handles partial matches correctly", {
  data <- data.frame(
    FN = c("Carl", "Agnes"),
    LN = c("Linnaeus", "Arber"),
    UPI = c("CL001", "AA004"),
    stringsAsFactors = FALSE
  )

  result1 <- match_person_to_data("James", "Linnaeus", data)
  expect_equal(result1$message, "LN only")
  expect_equal(result1$UPI, "CL001")

  result2 <- match_person_to_data("Agnes", "Smith", data)
  expect_equal(result2$message, "FN only")
  expect_equal(result2$UPI, "AA004")
})

test_that("Returns NA and message when no match is found", {
  data <- data.frame(
    FN = c("Carl"),
    LN = c("Linnaeus"),
    UPI = c("CL001"),
    stringsAsFactors = FALSE
  )

  result <- match_person_to_data("Gregor", "Mendel", data)
  expect_true(is.na(result$UPI))
  expect_equal(result$message, "Neither FN or LN")
})

test_that("Works with custom column names", {
  data <- data.frame(
    Given = c("Agnes"),
    Family = c("Arber"),
    Code = c("AA004"),
    stringsAsFactors = FALSE
  )

  result <- match_person_to_data("Agnes", "Arber", data,
                                 FN_column = "Given",
                                 LN_column = "Family",
                                 UPI_column = "Code")
  expect_equal(result$UPI, "AA004")
  expect_equal(result$message, "EXACT")
})
