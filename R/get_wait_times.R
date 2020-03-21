#' Get wait times for emergencies, consultations, and surgeries in the Hospitals covered by the "tempos" API
#'
#' This function provides a wrapper to the API endpoints for getting the live wait times for emergencies, consultations, and surgeries at the following domain: [http://tempos.min-saude.pt]http://tempos.min-saude.pt.
#' \code{\link{get_wait_times_all}} runs this function on all existing ids.
#'
#' @param hospital_id Integer containing the hospital id, get it through \code{\link{get_hospital_metadata}} under the collumn \code{id}
#' @param output_format defines the format of the final output resulting form the API call. Two options:
#' (1) \code{output_format = "data_frame"} returns a tibble object;
#' (2) \code{output_format = "json"} returns the data as a json file
#' @param request_headers character vector with the HTTP headers to be added to \code{\link[httr]{GET}} via \code{\link[httr]{add_headers()}}. Defaults to \code{NULL}
#' @param data_type Character string determining the type of data to request. Can be either (1) \code{"emergency"}, (2) \code{"consultation"}, or \code{"surgery"}
#' @return json string or tibble containing the relevant metadata. The relevant wait times for \code{"emergency"} are in both seconds as integer or \code{\link[lubridate]{ymd_hms}}. For \code{"consultation"} or \code{"surgery"} they unit of analysis is day as integer. All datasets also contain a variable measuring wait times by the number of people waiting.
#' @export
#' @examples
#'library(jsonlite)
#'library(esperaR)
#'
#'\dontrun{
#'library(ggplot2)
#'## get the metadata for extracting the ids
#'metadata <- get_hospital_metadata(output_format = "data_frame") %>%
#'  sample_n(., 1)
#'
#'## extract emergency data
#'wait_dta <- get_wait_times(hospital_id = metadata$id,
#'                           output_format = "data_frame",
#'                           request_headers = c(from = "fooh@baah.pt"),
#'                           data_type = "emergency")
#'
#'wait_dta %>%
#'  ggplot(aes(triage_type, wait_time_secs)) +
#'  geom_boxplot()
#'}

get_wait_times <- function(hospital_id = NULL,
                           output_format = c("json", "data_frame"),
                           request_headers = NULL,
                           data_type = c("emergency", "consultation", "surgery")){

  ## Prep the hospital id
  # hospital_id is required
  tryCatch(
    stopifnot(is_empty(hospital_id) == FALSE, is.na(hospital_id) == FALSE, grepl(pattern = "^(\\s+)?[0-9]+(\\s+)?$", x = as.character(hospital_id))),
    error=stop("Hospital ID is missing\n> To retrieve the hospital IDs, make an API call using this function and this endpoint 'api.php/institution'")
      )

  ## coerce to integer
  hospital_id <- as.integer(hospital_id)

  ### must select a data type
  if (!data_type %in% c("emergency", "consultation", "surgery")){

    stop("Please select on of the three relevant data_types:\n(i) \"emergency\"\n(ii) \"consultations\" \n(iii)\"surgery\"" )


  }

  ### Check if the hospital shares data, if not throw an error
  ## Get the metadata
  meta <- get_hospital_metadata(output_format = "data_frame")

  ## extract the relevant variables
  has_data <- pull(meta[meta$id == hospital_id, grepl(pattern = data_type, names(meta))])

  if (has_data == FALSE){

    stop("At the moment, I cannot find the requested data type for this hospital.\nData is not available for all hospitals.\nIf you think that this may be a mistake, please file an issue at\nhttps://github.com/josemreis/espera_urgencias/issues")

  }

  #### Prepare the endpoint
  endpoint <- case_when(
    data_type == "emergency" ~ "api.php/standbyTime",
    data_type == "consultation" ~ "api.php/standbyTimeCTH",
    data_type == "surgery" ~ "api.php/standbyTimeSIGLIC"
    ) %>%
    paste(., hospital_id, sep = "/")


  #### Send the HTTP GET request
  ### The request
  resp <- RETRY(verb = "GET",
                url = "http://tempos.min-saude.pt",
                path = endpoint,
                config = add_headers(request_headers),
                pause_base = 20,
                times = 5)


  #### Parse the response
  if (!output_format %in% c("json", "data_frame")){

    stop("Please select one of the following formats:\n> \'json\', or\n> \'data_frame\'")

  } else if (output_format == "data_frame") {

    ## parse the content
    content_raw <- content(resp, as = "text")

    # parse the json
    dta_raw <- try(jsonlite::fromJSON(content_raw, flatten = TRUE) %>%
                     .[["Result"]] %>%
                     set_names(.,
                               gsub(pattern = "\\.", replacement = "\\_", names(.))), silent = TRUE)

    if (class(dta_raw) == "try-error"){

      stop(paste0("Something went wrong when parsing the JSON file. Double check its format at: ", resp$url, "\nError message:\n", dta_raw))

    }

    #### Wrangle
    ### condition: data type
    if (data_type == "emergency"){

      ## wide to long format
      long_dta <- dta_raw %>% ## turn to wide format
        pivot_longer(.,
                     cols = matches("Time|Length"),
                     names_to = "triage_colour",
                     values_to = "value_raw") %>% ## separate the triage from the metric type
        separate(.,
                 col = "triage_colour",
                 sep = "_",
                 into = c("triage_colour", "metric_type")) %>% ## turn values into portuguese
        mutate(metric_type = case_when(
          metric_type == "Length" ~ "people_n",
          metric_type == "Time" ~ "wait_time_secs"
        )) %>% # turn the time/people var into two
        pivot_wider(.,
                    names_from = metric_type,
                    values_from = value_raw)

      ## Parse dates
      times_parsed <- long_dta %>%
        mutate(hours = hour(seconds_to_period(wait_time_secs)),
               minutes = minute(seconds_to_period(wait_time_secs)),
               wait_time = case_when(
                 hours == 0 & minutes == 0  ~ paste0("00:00:", wait_time_secs),
                 hours == 0 & minutes > 0 ~ paste0("00:", minutes, ":00"),
                 hours > 0 ~ paste0(hours, ":", minutes, ":00")
               ),
               wait_time = hms(wait_time)) %>%
        select(-c(hours, minutes))

      ## Add the last updated variable, remove some vars, and turn into final object
      dta_cleaned <- times_parsed %>%
        mutate(last_update = ymd_hms(LastUpdate),
               triage_colour = case_when(
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

      ## just renaming and dropping one var
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

#### Function for getting all the wait times
get_wait_times_all <- function(output_format = c("json", "data_frame"),
                               request_headers = "",
                               data_type = c("emergency", "consultation", "surgery"),
                               sleep_time = 3){

  ### must select a data type
  if (!data_type %in% c("emergency", "consultation", "surgery")){

    stop("Please select on of the three relevant data_types:\n(i) \"emergency\"\n(ii) \"consultations\" \n(iii)\"surgery\"" )

  }

  ## get all the hospital ids
  if (data_type == "emergency") {

    hospital_metadata <- get_hospital_metadata(output_format = "data_frame") %>%
      filter(shares_emergency_tems == TRUE)

  } else if (data_type == "consultation") {

    hospital_metadata <- get_hospital_metadata(output_format = "data_frame") %>%
      filter(shares_consultation_tems == TRUE)

  } else {

    hospital_metadata <- get_hospital_metadata(output_format = "data_frame") %>%
      filter(shares_surgery_tems == TRUE)

  }
  ## set up the progress bar
  prog <- progress_estimated(n = nrow(hospital_metadata))
  ## loop across the hospitals which share data, extract, and join
  to_return <- map(hospital_metadata$id, function(cur_id){

    prog$tick()$print()

    ret <- try(get_wait_times(hospital_id = cur_id,
                              output_format = "data_frame",
                              request_headers = "",
                              data_type = data_type), silent = TRUE)

    if (class(ret) == "try-error"){

      ret <- NULL
    }

    Sys.sleep(sleep_time)

    return(ret)
  }) %>%
    bind_rows() %>%
    as_tibble()

  return(to_return)
}

