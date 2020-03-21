
## error handling - hospital id
check_id <- function(hospital_id = hospital_id) {
  tryCatch(stopifnot(is_empty(hospital_id) == FALSE,
                     is.na(hospital_id) == FALSE,
                     grepl(pattern = "^(\\s+)?[0-9]+(\\s+)?$", x = as.character(hospital_id)),
                     length(hospital_id) == 1),
           error = function(c) {
             error_msg_varying <- case_when(
               grepl(pattern = "empty", x = c) ~ "Hospital ID can't be a NULL object.\nUse 'get_hospital_metadata()' to retrieve the existing IDs",
               grepl(pattern = "is.na", x = c) ~ "Hospital ID can't be a NA object.\nUse 'get_hospital_metadata()' to retrieve the existing IDs",
               grepl(pattern = "grepl", x = c) ~ "Hospital ID provided does not seem to be a number.\nUse 'get_hospital_metadata()' to retrieve the existing IDs",
               grepl(pattern = "length", x = c) ~ "This function only retrieves data for one id at the time.\nIf you want to collect data on all hospitals, see ?get_wait_times_all.\nIf some but not all IDs, just loop it.",
             )
             c$message <- paste0(c, error_msg_varying)
             stop(c)
           },
           warning = function(c) c,
           message = function(c) c
  )
}

## error handling - output_format
check_output_format <- function(output_format = output_format) {
  tryCatch(stopifnot(is_empty(output_format) == FALSE,
                     is.na(output_format) == FALSE,
                     output_format %in% c("data_frame", "json")),
           error = function(c) {
             error_msg_varying <- case_when(
               grepl(pattern = "empty", x = c) ~ "output_format can't be a NULL object.\nChoose one of the available formats: 'data_frame' or 'json'",
               grepl(pattern = "is.na", x = c) ~ "output_format can't be a NA object.\nChoose one of the available formats: 'data_frame' or 'json'",
               grepl(pattern = "c(", x = c, fixed = TRUE) ~ "Format not available.\nChoose one of the available formats: 'data_frame' or 'json'",
             )
             c$message <- paste0(c, error_msg_varying)
             stop(c)
           },
           warning = function(c) c,
           message = function(c) c
  )
}

## error handling - data_type
check_data_type <- function(data_type = data_type) {
  tryCatch(stopifnot(is_empty(data_type) == FALSE,
                     is.na(data_type) == FALSE,
                     data_type %in% c("emergency", "consultation", "surgery")),
           error = function(c) {
             error_msg_varying <- case_when(
               grepl(pattern = "empty", x = c) ~ "data_type can't be a NULL object.\nChoose one of the available data types: 'emergency', 'consultation', or  'surgery'",
               grepl(pattern = "is.na", x = c) ~ "data_type can't be a NA object.\nChoose one of the available data types: 'emergency', 'consultation', or  'surgery'",
               grepl(pattern = "c(", x = c, fixed = TRUE) ~ "data_type not available.\nChoose one of the available data types: 'emergency', 'consultation', or  'surgery'",
             )
             c$message <- paste0(c, error_msg_varying)
             stop(c)
           },
           warning = function(c) c,
           message = function(c) c
  )
}



