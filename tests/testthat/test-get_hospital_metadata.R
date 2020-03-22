
context("get_hospital_metadata")

test_that("get_hospital_metadata stops when request headers is not a named character vector ", {

  expect_error(get_hospital_metadata(output_format = "json", request_headers = list(not_cv = "baah")))

})

test_that("get_hospital_metadata should not stop", {

  expect_output(get_hospital_metadata("data_frame"), "tbl")

})
