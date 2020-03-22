# esperaR

An unoficial wrapper to the [Tempos Medios de Espera](http://tempos.min-saude.pt/#/instituicoes) API for collecting wait time data aswell as metadata from Portuguese Hospitals

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

## load the package 
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


`get_wait_times()` provides a wrapper to the API endpoints for the live wait times at emergency rooms, consultations, and surgeries in a given hospital as well as  metadata of the relevant hospital. Output: a raw JSON or data frame. 
You select the hospital of interest by feeding the function its id `hospital_id`. All available `hospital_id`s can be retrieved from `get_hospital_metadata()`. You can select the data type endpoint by choosing one of the following options `data_type = c("emergency","consultation", "surgery")`. The three more relevant varibles are: (i) last_update, which tells us the last time the data was updated  as datetime - YYYY-MM-DDTHH:MM:SS case of emergency, YYYY-MM-DD  for the other data; (ii) `wait_time_secs`, `wait_time_minutes`, `wait_time_hours` at the emergency room, and it returns the number of `days` of wait for surgeries or consultations; (iii) all three provide a variable with counts of the number of people waiting at the period of analysis `people`. Hospital metadata is also appended to the final dataset.

Example output for emergencies.

``` R
library(esperaR)

dplyr::glimpse(get_wait_times(hospital_id = 214, 
               output_format = "data_frame", 
               request_headers = "", 
               data_type = "emergency"))

Observations: 15
Variables: 23
$ id                      <int> 214, 214, 214, 214, 214, 214, 214, 214, 21…
$ last_update             <chr> "2020-03-22T19:00:00.357", "2020-03-22T19:…
$ emergency_code          <chr> "10011", "10011", "10011", "10011", "10011…
$ emergency_description   <chr> "Urgência Geral", "Urgência Geral", "Urgên…
$ triage_type             <chr> "Emergency\n(red)", "Very urgent\n(orange)…
$ wait_time_secs          <int> 0, 1914, 1542, 3652, 0, 0, 736, 35, 45, 0,…
$ wait_time_minutes       <dbl> 0, 31, 25, 0, 0, 0, 12, 0, 0, 0, 0, 24, 0,…
$ wait_time_hours         <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0
$ people_n                <int> 0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0
$ hospital_name           <chr> "Hospital Vila Franca de Xira", "Hospital …
$ hospital_description    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
$ longitude               <dbl> -8.985314, -8.985314, -8.985314, -8.985314…
$ latitude                <dbl> 38.97796, 38.97796, 38.97796, 38.97796, 38…
$ address                 <chr> "Estrada Nacional Nº1, Povos 2600-009 Vila…
$ hospital_phone          <int> 263006500, 263006500, 263006500, 263006500…
$ hospital_email          <chr> "hvfxira@hvfx.pt", "hvfxira@hvfx.pt", "hvf…
$ district                <chr> "Lisboa", "Lisboa", "Lisboa", "Lisboa", "L…
$ url_tems                <chr> "http://api.pds.min-saude.pt/api/Tems/Stan…
$ shares_emergency_dta    <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, …
$ shares_consultation_dta <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, …
$ shares_surgery_dta      <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, …
$ has_emergency           <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, …
$ hospital_url            <chr> "http://www.hospitalvilafrancadexira.pt", …

 ```

Example output for surgeries.

``` R
library(esperaR)

dplyr::glimpse(get_wait_times(hospital_id = 214, 
               output_format = "data_frame", 
               request_headers = "", 
               data_type = "surgery"))

Observations: 32
Variables: 21
$ id                      <int> 214, 214, 214, 214, 214, 214, 214, 214, 21…
$ last_update             <chr> "30/11/2019", "30/11/2019", "30/11/2019", …
$ specialty               <chr> "Cirurgia Geral", "Cirurgia Geral", "Cirur…
$ priority                <chr> "1 - Doença não oncológica", "1 - Doença o…
$ days                    <chr> "194", "28", "88", "30", "14", "120", "43"…
$ people                  <chr> "1118", "12", "50", "12", "2", "3", "13", …
$ is_surgery              <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, …
$ hospital_name           <chr> "Hospital Vila Franca de Xira", "Hospital …
$ hospital_description    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
$ longitude               <dbl> -8.985314, -8.985314, -8.985314, -8.985314…
$ latitude                <dbl> 38.97796, 38.97796, 38.97796, 38.97796, 38…
$ address                 <chr> "Estrada Nacional Nº1, Povos 2600-009 Vila…
$ hospital_phone          <int> 263006500, 263006500, 263006500, 263006500…
$ hospital_email          <chr> "hvfxira@hvfx.pt", "hvfxira@hvfx.pt", "hvf…
$ district                <chr> "Lisboa", "Lisboa", "Lisboa", "Lisboa", "L…
$ url_tems                <chr> "http://api.pds.min-saude.pt/api/Tems/Stan…
$ shares_emergency_dta    <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, …
$ shares_consultation_dta <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, …
$ shares_surgery_dta      <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, …
$ has_emergency           <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, …
$ hospital_url            <chr> "http://www.hospitalvilafrancadexira.pt", …
 ```
...
