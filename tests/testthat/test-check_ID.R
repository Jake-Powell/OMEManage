test_that("check_ID detects IDs with incorrect length", {
  test_data <- data.frame(
    ID = c("1233344444", "34556666"),  # Second ID is too short
    estab_no = c("333", "455")
  )

  result <- check_ID(test_data, "ID", "estab_no")
  expect_type(result, "list")
  expect_equal(result[[1]]$message, "Not all IDs have length 10")
  expect_equal(result[[1]]$index, 2)
})

test_that("check_ID detects mismatch in digits 3-5", {
  test_data <- data.frame(
    ID = c("1233344444", "3455666666"),
    estab_no = c("333", "999")  # Second ID has 455 in digits 3-5, which doesn't match 999
  )

  result <- check_ID(test_data, "ID", "estab_no")
  expect_type(result, "list")
  expect_equal(result[[1]]$message, "Digits 3-5 of ID do not match establishment number")
  expect_equal(result[[1]]$index, 2)
})

test_that("check_ID detects both length and pattern issues", {
  test_data <- data.frame(
    ID = c("1233344444", "3466"),  # Second: wrong length and wrong pattern
    estab_no = c("333", "666")
  )

  result <- check_ID(test_data, "ID", "estab_no")
  expect_length(result, 2)
  expect_equal(result[[1]]$message, "Not all IDs have length 10")
  expect_equal(result[[1]]$index, 2)
  expect_equal(result[[2]]$message, "Digits 3-5 of ID do not match establishment number")
  expect_equal(result[[2]]$index, 2)
})

test_that("check_ID returns NULL for valid input", {
  test_data <- data.frame(
    ID = c("1233344444", "3455567890"),
    estab_no = c("333", "555")
  )

  result <- check_ID(test_data, "ID", "estab_no")
  expect_null(result)
})
