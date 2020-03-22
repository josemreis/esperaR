# esperaR

An unoficial wraper to the [Tempos Medios de Espera](http://tempos.min-saude.pt/#/instituicoes) API for collecting wait time data aswell as hospital metadata

(work in progress)

### Instalation

To get the current development version from Github:

``` r
## install remotes package if it's not already
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

## install dev version 
remotes::install_github("josemreis/esperaR")

## load rtweet package
library(esperaR)
```

### Package Functionality

This package provides a couple wrapping functions to the [Portugal's Ministry of Health Tempos API](http://tempos.min-saude.pt) for collecting data on wait times (emergency, surgery or consultations) in Portuguese hospitals as well as several metadata features related with the  hospital, e.g. coordinates, phone number, website, among others.

### Responsible use

The current covid19 crisis motivated the creation of this package as this data may be of use now and for diagnostics after the crisis. In this time of resource constraints, please use the package responsibly with some request delays and in general go easy on the server.

### Overview of the package

The package has three main functions:

`get_hospital_metadata()` a wrapper to the 'api.php/institution' endpoint from the following. It makes an API call and returns metadata of all hospitals covered by the API - data as a raw JSON or data frame. Most workflows should start here as the output of this function yields the ``hospital_id``, crucial for extracting wait times data. For more details, check the docs at `?get_hospital_metadata`.

 ``` R
library(esperaR)
library(jsonlite)

## pull the metadata as a json file
hospital_meta_json <- get_hospital_metadata(output_format = "json")

prettify(hospital_meta_json)

## Adding a user-agent header to the get request and returning a tibble
hospital_meta_df <- get_hospital_metadata(output_format = "data_frame", request_headers = list("User-Agent" = "Mozilla/5.0 (X11; Linux x86_64; rv:74.0) Gecko/20100101 Firefox/74.0"))

 ```
 
 Output in more detail.
 
| Variables                	| Description                                            	| Type 	|
|--------------------------	|--------------------------------------------------------	|------	|
| id                       	| numerical id assigned to the hospital                  	| int  	|
| hospital_name            	| Hospital's name                                        	| chr  	|
| hospital_descriptio      	| Usually shorter version of the name and other info     	| chr  	|
| longitude                	| Hospital's logitude                                    	| dbl  	|
| latitude                 	| Hospital's latitude                                    	| dbl  	|
| address                  	| Hospital's address                                     	| chr  	|
| hospital_phone           	| Hospital's phone number                                	| int  	|
| hospital_email           	| Hospital's email address                               	| chr  	|
| district                 	| Hospital's district                                    	| chr  	|
| url_tems                 	| Hospital specific API endpoint for emergency data      	| chr  	|
| shares_emergency_data    	| does the hospital share emergency room wait times?     	| lgl  	|
| shares_consultation_data 	| does the hospital share doctor appointment wait times? 	| lgl  	|
| shares_surgery_data      	| does the hospital share surgery wait times?            	| lgl  	|
| has_emergency            	| does the hospital have an emergency room?              	| lgl  	|
| hospital_url             	| URL of the Hospital                                    	| chr  	|


`get_wait_times()` provides a wrapper to the API endpoints for the live wait times at emergency rooms, consultations, and surgeries in a given hospital. You select the hospital of interest by feeding the function its id `hospital_id`. All available `hospital_id`s can be retrieved from `get_hospital_metadata()`. You can select the data type endpoint by choosing one of the following options `data_type = c("emergency","consultation", "surgery")`. It returns the relevant wait times faceted by hospital service and triage. The triage for the emergency follows the "Manchester triage system". 
