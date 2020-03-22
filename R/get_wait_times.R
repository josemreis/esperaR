#' Get wait times for emergencies, consultations, and surgeries in the Hospitals covered by the "tempos" API
#'
#' This function provides a wrapper to the API endpoints for getting the live wait times for emergencies, consultations, and surgeries at the following domain: \link{http://tempos.min-saude.pt}.
#' \code{\link{get_wait_times_all}} runs this function on all existing ids.
#'
#' @param hospital_id Integer containing the hospital id, get it through \code{\link{get_hospital_metadata}} under the collumn \code{id}
#' @param output_format defines the format of the final output resulting form the API call. Two options:
#' (1) \code{output_format = "data_frame"} returns a tibble object;
#' (2) \code{output_format = "json"} returns the data as a json file
#' @param request_headers character vector with the HTTP headers to be added to \code{\link[httr]{GET}} via \code{\link[httr]{add_headers}}. Defaults to \code{NULL}
#' @param data_type Character string determining the type of data to request. Can be either (1) \code{"emergency"}, (2) \code{"consultation"}, or \code{"surgery"}
#' @return json string or tibble containing the relevant metadata. The relevant wait times for \code{"emergency"} are in both seconds as integer or \code{\link[lubridate]{ymd_hms}}. For \code{"consultation"} or \code{"surgery"} they unit of analysis is day as integer. All datasets also contain a variable measuring wait times by the number of people waiting.
#' @export
#' @import httr jsonlite magrittr tidyr dplyr purrr lubridate
#' @examples
#'library(jsonlite)
#'library(esperaR)
#'
#'\dontrun{
#'library(ggplot2)
#'## extract emergency data as a data frame
#'wait_dta <- get_wait_times(hospital_id = 333,
#'                           output_format = "data_frame",
#'                           request_headers = NULL
#'                           data_type = "emergency")
#'
#'wait_dta %>%
#'  ggplot(aes(triage_type, wait_time_secs)) +
#'  geom_boxplot()
#'}
#'

#### Get wait times given a hospital id----------------------------------------
get_wait_times <- function(hospital_id = NULL,
                           output_format = c("json", "data_frame"),
                           request_headers = NULL,
                           data_type = c("emergency", "consultation", "surgery")) {

  ## Prep the hospital_id -> coerces it to integer
  hospital_id <- prep_id(hospital_id = hospital_id)

  # check data type
  check_data_type(data_type = tolower(data_type))

  ## Check if the hospital shares data, if not throw an error
  # Get the metadata
  meta <- get_hospital_metadata(output_format = "data_frame")

  # get the data available
  available <- hospital_data_availability(hospital_id = hospital_id,
                                          hospital_metadata = meta)

  if (available$availability[available$data_type == data_type] == FALSE) {

    ## prep the error message
    missing_msg <- paste0("It seems that '", data_type, "' data is not available for this Hospital.\n")

    available_msg <- paste("However, the following data is:",
                            paste(available$data_type[available$availability == TRUE], collapse = ", "),
                            "\n")

    issue_msg <- "If you have any reason to believe that the requested data exists in the API, please file an issue at\nhttps://github.com/josemreis/esperaR/issues\n"

    stop(paste0(missing_msg, available_msg, issue_msg))

  }


  ## API call
  # Prepare the endpoint
  endpoint <- dplyr::case_when(
    data_type == "emergency" ~ "api.php/standbyTime",
    data_type == "consultation" ~ "api.php/standbyTimeCTH",
    data_type == "surgery" ~ "api.php/standbyTimeSIGLIC"
    ) %>%
    paste(., hospital_id, sep = "/")

  # prep headers. Check if NULL, NA or not a named vector
  if (rlang::is_empty(request_headers) || is.na(request_headers) || (is.vector(request_headers, mode = "character") & any(is.na(names(t))))) {

    request_headers <- ""

  }

  # Send the HTTP GET request
  resp <- httr::RETRY(verb = "GET",
                      url = "http://tempos.min-saude.pt",
                      path = endpoint,
                      config = add_headers(request_headers),
                      pause_base = 20,
                      times = 5)

  httr::stop_for_status(resp)

  # check output format
  check_output_format(output_format = output_format)

  ## Parse the response
  if (output_format == "data_frame") {

    # extract the content as raw json
    content_raw <- content(resp, as = "text")

    # parse the json to df and clean variable names
    dta_raw <- try(jsonlite::fromJSON(content_raw, flatten = TRUE) %>%
                     .[["Result"]] %>%
                     purrr::set_names(.,
                                      gsub(pattern = "\\.", replacement = "\\_", names(.))),
                   silent = TRUE)
    # Test it
    if (class(dta_raw) == "try-error") {

      stop(paste0("Something went wrong when parsing the JSON file. Double check its format at: ", resp$url, "\nError message:\n", dta_raw))

    }

    ## Wrangle - different wrangling procedure depending on data_type requested
    # logical condition: data type
    if (data_type == "emergency") {

      # wide to long format
      long_dta <- dta_raw %>% ## turn to wide format
        tidyr::pivot_longer(.,
                     cols = matches("Time|Length"),
                     names_to = "triage_colour",
                     values_to = "value_raw") %>% ## separate the triage from the metric type
        separate(.,
                 col = "triage_colour",
                 sep = "_",
                 into = c("triage_colour", "metric_type")) %>%
        mutate(metric_type = dplyr::case_when(
          metric_type == "Length" ~ "people_n",
          metric_type == "Time" ~ "wait_time_secs"
        )) %>% # turn the time/people var into two different variables
        tidyr::pivot_wider(.,
                    names_from = metric_type,
                    values_from = value_raw)

      # Parse dates
      times_parsed <- long_dta %>%
        mutate(sec_p = lubridate::seconds_to_period(wait_time_secs),
               hours = lubridate::hour(sec_p),
               minutes = lubridate::minute(sec_p),
               wait_time = dplyr::case_when(
                 hours == 0 & minutes == 0  ~ paste0("00:00:", wait_time_secs),
                 hours == 0 & minutes > 0 ~ paste0("00:", minutes, ":00"),
                 hours > 0 ~ paste0(hours, ":", minutes, ":00")
               ),
               wait_time = lubridate::hms(wait_time)) %>%
        select(-c(hours, minutes, -sec_p))

      # Add the last updated variable, remove some vars, and turn into final object
      dta_cleaned <- times_parsed %>%
        mutate(last_update = ymd_hms(LastUpdate),
               triage_colour = dplyr::case_when(
                 triage_colour == "Blue" ~ "Not urgent\n(blue)",
                 triage_colour == "Green" ~ "Less urgent\n(green)",
                 triage_colour == "Yellow" ~ "Urgent\n(yellow)",
                 triage_colour == "Orange" ~ "Very urgent\n(orange)",
                 triage_colour == "Red" ~ "Emergency\n(red)"
               ),
               id = hospital_id) %>%
        select(id,
               last_update,
               emergency_code = Emergency_Code,
               emergency_description = Emergency_Description,
               triage_type = triage_colour,
               wait_time_secs,
               wait_time,
               people_n)

    } else if (data_type == "consultation") {

      # just renaming and dropping one var
      dta_cleaned <- dta_raw %>%
        mutate(id = hospital_id) %>%
        select(id,
               specialty = Speciality,
               priority = Priority,
               days = Time,
               people = Length)

    } else {

      dta_cleaned <- dta_raw %>%
        mutate(id = hospital_id) %>%
        select(id,
               specialty = Speciality,
               priority = Priority,
               days = Time,
               people = Length,
               is_surgery = Surgery)

    }

    ### finally, we add the metadata
    to_return <- suppressMessages(left_join(dta_cleaned, meta)) %>%
      as_tibble()

  } else {

    ## as json
    to_return <- content(resp, as = "text")

  }

  return(to_return)
}

#### Function for getting all the wait times-------------------------------

### get_wait_times_all()
get_wait_times_all <- function(output_format = c("json", "data_frame"),
                               request_headers = "",
                               data_type = c("emergency", "consultation", "surgery"),
                               sleep_time = 3) {

  # check data type
  check_data_type(data_type = tolower(data_type))

  # check output format
  check_output_format(output_format = output_format)

  ## Check sleep time
  to_sleep <- prep_sleep_time(sleep_time = sleep_time)

  # be nice
  if (to_sleep == 0) {

    base::message("\nI have seen you have added no sleep time.\nWhile there are no stated rate limits, this might lead to some server refusals as well as overload the server with requests.\nStrongly recommend adding some sleep_time.\n")
    Sys.sleep(0.5)

  }

  ## get all the hospital ids
  if (data_type == "emergency") {

    hospital_metadata <- get_hospital_metadata(output_format = "data_frame") %>%
      filter(shares_emergency_dta == TRUE)

  } else if (data_type == "consultation") {

    hospital_metadata <- get_hospital_metadata(output_format = "data_frame") %>%
      filter(shares_consultation_dta == TRUE)

  } else {

    hospital_metadata <- get_hospital_metadata(output_format = "data_frame") %>%
      filter(shares_surgery_dta == TRUE)

  }

  # set up the progress bar
  prog <- dplyr::progress_estimated(n = nrow(hospital_metadata))

  ## loop across the hospitals which share data, extract, and join
  output_list <- purrr::map(hospital_metadata$id, purrr::possibly(function(cur_id) {

    prog$tick()$print()

    ret <- try(get_wait_times(hospital_id = cur_id,
                              output_format = output_format,
                              request_headers = "",
                              data_type = data_type), silent = TRUE)

    Sys.sleep(to_sleep)

    return(ret)
  }, otherwise = NULL, quiet = FALSE))

  ## final tidying
  if (output_format == "data_frame") {

    ## bind cols and turn to tibble
    to_return <- output_list %>%
      bind_rows() %>%
      as_tibble()

  } else {

    ## as list
    to_return <- output_list

  }

  return(to_return)

}

### END
