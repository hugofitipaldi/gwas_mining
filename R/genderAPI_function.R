# gender API

# Function to use genderAPI
get_gender <- function(name, api_key, country_code = NA){

  # Construct the query
  path <- paste0("https://gender-api.com/get?key=", api_key)

  if (is.na(country_code)){
    request <- httr::GET(url = path,
                         query = list(
                           name = name))
  } else {
    request <- httr::GET(url = path,
                         query = list(
                           name = name,
                           country = country_code))
  }

  response <- httr::content(request, as = "text", encoding = "UTF-8")
  response_df <- data.frame(jsonlite::fromJSON(response, flatten = TRUE))
  print(dplyr::select(response_df,name, country, gender, accuracy) )
}
