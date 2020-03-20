#' Get metadata on all hospitals covered by the API
#'
#' This function provides a wrapper to the 'api.php/institution' endpoint from the following domain: http://tempos.min-saude.pt.
#' Crucial for extracting the wait times, see \code{\link{get_wait_times}} has it contains the "hospital id"s as well as logical variables for the data available per website
#'
#' @param output_format Character which defines the format of the final output resulting form the API call. Two options:
#' (1) \code{output_format = "data_frame"} returns a tibble object;
#' (2) \code{output_format = "json"} returns the data as a json file
#' @param request_headers character vector with the HTTP headers to be added to \code{\link[httr]{GET}} via \code{\link[httr]{add_headers()}}. Defaults to \code{NULL}
#' @return json string or tibble containing the relevant metadata.
#' @export
#' @examples
#'\dontrun{
#'library(esperaR)
#'library(jsonlite)
#'
#'## pull the metadata as a json file
#'hospital_meta_json <- get_hospital_metadata(output_format = "json")
#'
#'prettify(hospital_meta_json)
#'
#'## Adding a user-agent header to the get request and returning a tibble
#'hospital_meta_df <- get_hospital_metadata(output_format = "data_frame", request_headers = list("User-Agent" = "Mozilla/5.0 (X11; Linux x86_64; rv:74.0) Gecko/20100101 Firefox/74.0"))
#'
#'}



get_hospital_metadata <- function(output_format = c("json", "data_frame"), request_headers = ""){


  ### make the GET request
  resp <- RETRY(verb = "GET",
                url = "http://tempos.min-saude.pt",
                path = "api.php/institution",
                config = add_headers(request_headers),
                pause_base = 20,
                times = 10)

  # Check staus server response
  stop_for_status(resp)

  ### Parse the data
  if (!output_format %in% c("json", "data_frame")){

    stop("please select one of the following formats:\n> \'json\', or\n> \'data_frame\'")

  } else if (output_format == "data_frame") {

    ## parse the content
    content_raw <- content(resp, as = "text")

    # parse the json
    dta_raw <- try(jsonlite::fromJSON(content_raw, flatten = TRUE) %>%
                     .[["Result"]] %>%
                     set_names(.,
                               gsub(pattern = "\\.", replacement = "\\_", x = names(.))), silent = TRUE)

    if (class(dta_raw) == "try-error"){

      stop(paste0("Something went wrong when parsing the JSON file. Double check its format at: ", resp$url, "\nError message:\n", dta_raw))

    }

    ## wrangle and return
    to_return <- dta_raw %>%
      select(id = Id,
             hospital_name = Name,
             hospital_description = Description,
             longitude = Longitude,
             latitude = Latitude,
             address = Address,
             hospital_phone = Phone,
             hospital_email = Email,
             district = District,
             url_tems = StandbyTimesUrl,
             shares_emergency_dta = ShareStandbyTimes,
             shares_consultation_dta = HasCTH,
             shares_surgery_dta = HasSIGLIC,
             has_emergency = HasEmergency,
             hospital_url = InstitutionURL) %>%
      as_tibble()


  } else {

    ## as json
    to_return <- content(resp, as = "text")

  }

  return(to_return)

}
