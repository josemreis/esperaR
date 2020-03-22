
context("get_wait_times")

test_that("get_wait_times stops wrong argument specifications ", {

  expect_error(get_wait_times(hospital_id = "",
                              output_format = "data_frame",
                              data_type = "emergency"))
  expect_error(get_wait_times(hospital_id = NA,
                              output_format = "data_frame",
                              data_type = "emergency"))
  expect_error(get_wait_times(hospital_id = NULL,
                              output_format = "data_frame",
                              data_type = "emergency"))
  expect_error(get_wait_times(hospital_id = " 333fooh baaa 333",
                              output_format = "data_frame",
                              data_type = "emergency"))
  expect_error(get_wait_times(hospital_id = 333,
                              output_format = "data_frame",
                              data_type = "baah"))
  expect_error(get_wait_times(hospital_id = 333935253295,
                              output_format = "data_frame",
                              data_type = "baah"))
  expect_error(get_wait_times(hospital_id = 333935253295,
                              output_format = "data.frame",
                              data_type = "baah"))


})

test_that("data_type with different capitalization or hospital id as string should not stop the function ", {

  expect_is(get_wait_times(hospital_id = " 213",
                           output_format = "data_frame",
                           data_type = "surgery"), "tbl")
  expect_is(get_wait_times(hospital_id = 213,
                           output_format = "data_frame",
                           data_type = "SurgEry"), "tbl")
  expect_is(get_wait_times(hospital_id = 213,
                           output_format = "json",
                           data_type = "SurgEry"), "character")
  expect_is(get_wait_times(hospital_id = " 213",
                           request_headers = c(from = "fooh@baah.com"),
                           output_format = "data_frame",
                           data_type = "surgery"), "tbl")

})



test_that("get_wait_times all with sleep_time = 1 ", {

  expect_is(get_wait_times_all(output_format = "data_frame",
                               data_type = "emergency",
                               sleep_time = 1), "tbl")

  expect_is(get_wait_times_all(output_format = "data_frame",
                               data_type = "surgery",
                               sleep_time = 1), "tbl")

  expect_is(get_wait_times_all(output_format = "data_frame",
                               data_type = "consultation",
                               sleep_time = 1), "tbl")

})
