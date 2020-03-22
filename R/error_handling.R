#' Error handling helper functions
#' A couple of error handling helper fuctions used in \code{\link{get_hospital_metadata}}, \code{\link{get_wait_times}} and \code{\link{hospital_data_availability}}
#' @export
#'

## error handling - hospital id
prep_id <- function(hospital_id = NULL, metadata = NULL) {

  # Get the metadata if not passed as argument
  if (identical(metadata, NULL)) {

    metadata <- get_hospital_metadata(output_format = "data_frame",
                                               request_headers = "")

  }

  tryCatch(stopifnot(identical(hospital_id, NULL) == FALSE,
                     is.na(hospital_id) == FALSE,
                     grepl(pattern = "^(\\s+)?[0-9]+(\\s+)?$", x = as.character(hospital_id)),
                     length(hospital_id) == 1,
                     as.integer(hospital_id) %in% metadata$id),
           error = function(c) {

             error_msg_varying <- dplyr::case_when(
               grepl(pattern = "NULL", x = c) ~ "Hospital ID can't be a NULL object.\nUse 'get_hospital_metadata()' to retrieve the existing IDs",
               grepl(pattern = "is.na", x = c) ~ "Hospital ID can't be a NA object.\nUse 'get_hospital_metadata()' to retrieve the existing IDs",
               grepl(pattern = "grepl", x = c) ~ "Hospital ID provided does not seem to be a number.\nUse 'get_hospital_metadata()' to retrieve the existing IDs",
               grepl(pattern = "length", x = c) ~ "This function only retrieves data for one id at the time.\nIf you want to collect data on all hospitals, see ?get_wait_times_all.\nIf some but not all IDs, just loop it.",
               grepl(pattern = "%in%", x = c) ~ "Hospital id not found.\nUse 'get_hospital_metadata()' to retrieve the existing IDs."
             )

             c$message <- paste0(c, error_msg_varying)

             stop(c)
           }
  )

  to_return <- as.integer(hospital_id)

  return(to_return)
}

## error handling - output_format
check_output_format <- function(output_format = NULL) {

  tryCatch(stopifnot(identical(output_format, NULL) == FALSE,
                     is.na(output_format) == FALSE,
                     output_format %in% c("data_frame", "json")),
           error = function(c) {

             error_msg_varying <- dplyr::case_when(
               grepl(pattern = "NULL", x = c) ~ "output_format can't be a NULL object.\nChoose one of the available formats: 'data_frame' or 'json'",
               grepl(pattern = "is.na", x = c) ~ "output_format can't be a NA object.\nChoose one of the available formats: 'data_frame' or 'json'",
               grepl(pattern = "c(", x = c, fixed = TRUE) ~ "Format not available.\nChoose one of the available formats: 'data_frame' or 'json'",
             )

             c$message <- paste0(c, error_msg_varying)

             stop(c)
           }
  )
}

## error handling - data_type
prep_data_type <- function(data_type = NULL) {

  tryCatch(stopifnot(identical(data_type, NULL) == FALSE,
                     is.na(data_type) == FALSE,
                     tolower(data_type) %in% c("emergency", "consultation", "surgery")),
           error = function(c) {

             error_msg_varying <- dplyr::case_when(
               grepl(pattern = "NULL", x = c) ~ "data_type can't be a NULL object.\nChoose one of the available data types: 'emergency', 'consultation', or  'surgery'",
               grepl(pattern = "is.na", x = c) ~ "data_type can't be a NA object.\nChoose one of the available data types: 'emergency', 'consultation', or  'surgery'",
               grepl(pattern = "c(", x = c, fixed = TRUE) ~ "data_type not available.\nChoose one of the available data types: 'emergency', 'consultation', or  'surgery'",
             )

             c$message <- paste0(c, error_msg_varying)

             stop(c)
           }
  )

  to_return <- tolower(data_type)

  return(to_return)
}

### error handling check sleep_time
prep_sleep_time <- function(sleep_time = NULL) {

  tryCatch(stopifnot(identical(sleep_time, NULL) == FALSE,
                     is.na(sleep_time) == FALSE,
                     grepl(pattern = "^(\\s+)?[0-9]+(\\s+)?$", x = as.character(sleep_time))),
           error = function(c) {

             error_msg_varying <- dplyr::case_when(
               grepl(pattern = "NULL", x = c) ~ "sleep_time can't be a NULL object, it must be a numerical object.",
               grepl(pattern = "is.na", x = c) ~ "sleep_time can't be a NA object, it must be a numerical object.",
               grepl(pattern = "grepl", x = c, fixed = TRUE) ~ "This does not seem to be a number, select a numerical object",
             )

             c$message <- paste0(c, error_msg_varying)

             stop(c)
           }
  )

  to_return <- as.integer(sleep_time)

  return(to_return)
}


### END
