
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


})

test_that("data_type with different capitalization or hospital id as string should not stop the function ", {

  expect_success(get_wait_times(hospital_id = " 333",
                              output_format = "data_frame",
                              data_type = "surgery"))
  expect_success(get_wait_times(hospital_id = 333,
                                output_format = "data_frame",
                                data_type = "SurgEry"))

})
