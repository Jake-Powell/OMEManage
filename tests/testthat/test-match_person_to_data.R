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
  expect_equal(result$message, "No match")
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

test_that("Multiple exact matches return multiple UPIs", {
  data <- data.frame(
    FN = c("Carl", "Carl"),
    LN = c("Linnaeus", "Linnaeus"),
    UPI = c("CL001", "CL002"),
    stringsAsFactors = FALSE
  )

  result <- match_person_to_data("carl", "linnaeus", data)
  expect_equal(length(result$UPI), 2)
  expect_equal(sort(result$UPI), c("CL001", "CL002"))
  expect_equal(result$message, "EXACT")
})

test_that("Multiple FN-only matches return fuzzy or no match message", {
  data <- data.frame(
    FN = c("Carl", "Carl"),
    LN = c("Smith", "Jones"),
    UPI = c("CL003", "CL004"),
    stringsAsFactors = FALSE
  )

  result <- match_person_to_data("carl", "linnaeus", data)
  expect_true(result$message %in% c("FUZZY PARTIAL MATCH (best FN or LN)", "No match", "FN only", "FUZZY MATCH (best both FN & LN)"))
  expect_true(all(result$UPI %in% c("CL003", "CL004", NA)))
})

test_that("Multiple LN-only matches return fuzzy or no match message", {
  data <- data.frame(
    FN = c("Greg", "Gregor"),
    LN = c("Mendel", "Mendel"),
    UPI = c("GM001", "GM002"),
    stringsAsFactors = FALSE
  )

  result <- match_person_to_data("isaac", "mendel", data)
  expect_true(result$message %in% c("FUZZY PARTIAL MATCH (best FN or LN)", "No match", "LN only", "FUZZY MATCH (best both FN & LN)"))
  expect_true(all(result$UPI %in% c("GM001", "GM002", NA)))
})

test_that("Multiple swap matches are handled", {
  data <- data.frame(
    FN = c("Linnaeus", "Linnaeus"),
    LN = c("Carl", "Carl"),
    UPI = c("CL005", "CL006"),
    stringsAsFactors = FALSE
  )

  result <- match_person_to_data("Carl", "Linnaeus", data)
  expect_equal(length(result$UPI), 2)
  expect_equal(sort(result$UPI), c("CL005", "CL006"))
  expect_equal(result$message, "EXACT (Swap)")
})

test_that("Swapped match takes priority over LN-only match", {
  data <- data.frame(
    FN = c("Carl", "Agnes", "Jeff"),
    LN = c("Linnaeus", "Arber", "Carl"),
    UPI = c("CL001", "AA004", "JC005"),
    stringsAsFactors = FALSE
  )

  result <- match_person_to_data("Linnaeus", "Carl", data)

  # Expect it to match the swapped name "Carl Linnaeus"
  expect_equal(result$UPI, "CL001")
  expect_equal(result$message, "EXACT (Swap)")
})
