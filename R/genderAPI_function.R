# gender API

# Function to use genderAPI
get_gender <- function(name, api_key, country_code = NA, last_name = NA){

  # Construct the query
  path <- paste0("https://gender-api.com/get?key=", api_key)

  if (is.na(last_name)) {
    if (is.na(country_code)) {
      request <- httr::GET(url = path,
                           query = list(
                             name = name))
    } else {
      request <- httr::GET(url = path,
                           query = list(
                             name = name,
                             country = country_code))
    }
  } else {
    if (is.na(country_code)){
      request <- httr::GET(url = path,
                           query = list(
                             split = paste0(name, " ", last_name)))
    } else {
      request <- httr::GET(url = path,
                           query = list(
                             split = paste0(name, " ", last_name),
                             country = country_code))
    }
  }

  response <- httr::content(request, as = "text", encoding = "UTF-8")
  response_df <- data.frame(jsonlite::fromJSON(response, flatten = TRUE))

  if (is.na(last_name)) {
    print(dplyr::select(response_df,name, country, gender, accuracy))
  } else {
    print(dplyr::select(response_df,first_name, last_name, country, gender, accuracy) )
  }
}
