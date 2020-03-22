#' Check data availability for a Hospital covered by the "tempos" API
#'
#' This function takes a hospital id and returns the data availability in said hospital. Makes use of the hospital metadata, either by resorting to the wrapper \code{\link{get_hospital_metadata}} or when supplied by the user,
#'
#' @param hospital_id Integer containing the hospital id, get it through \code{\link{get_hospital_metadata}} under the collumn \code{id}
#' @param hospital_metadata a "data.frame" or NULL (default). If NULL, the function makes an API call to extract the hospitals metadata
#' using \code{\link{get_hospital_metadata}}.
#' @return Returns a tibble containing three variables: "data_type", character referring to the type of data provided by the endpoint, "availability",
#' logical values providing information on the data availability in that hospital
#' @export
#' @importFrom dplyr select as_tibble filter contains mutate
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @examples
#' my_metadata <- get_hospital_metadata(output_format = "data_frame", request_headers = "")
#'
#' hospital_data_availability(hospital_id = sample(my_metadata$id, 1), hospital_metada = my_metadata)
#'

#### check_data_availability---------------------------------------------------------------
hospital_data_availability <- function(hospital_id = NULL, hospital_metadata = NULL){

  ## Prep the hospital_id -> coerces it to integer
  hospital_id <- prep_id(hospital_id = hospital_id)

  # Get the metadata if not passed as argument
  if (identical(hospital_metadata, NULL)) {

    hospital_metadata <- get_hospital_metadata(output_format = "data_frame",
                                               request_headers = "")

  }

  ## Filter the relevant rows, select the data availability vars, and turn into df
  to_return <- hospital_metadata %>%
    filter(id == hospital_id) %>%
    select(contains("shares")) %>%
    tidyr::pivot_longer(cols = c(1:3), names_to = "data_type", values_to = "availability") %>%
    mutate(id = hospital_id,
           data_type = gsub(pattern = "shares|\\_|dta", replacement = "", x = data_type))

  return(to_return)
}

### END
