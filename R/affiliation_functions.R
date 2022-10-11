# Affiliation functions

library(rio)
library(stringr)
library(stringi)
library(qdap)
library(utils)
library(tidyr)
library(dplyr)
library(data.table)
library(XML)
library(easyPubMed)
library(RCurl)
library(jsonlite)

## Auxiliary functions
detecting_country_country <- function(x) {

  text <- x
  text <- gsub(' USA',' United States', text)
  text <- gsub(' UK',' United Kingdom', text)
  text <- tolower(text)
  text <- stringi::stri_trans_general(str = text, id = "Latin-ASCII")
  text <- gsub('[[:digit:]]+', '', text)
  text <- qdap::rm_stopwords(text, separate = FALSE)
  text <- gsub('[[:punct:] ]+',' ', text)
  text <- gsub('centre','', text)
  text <- gsub('university','', text)

  countries <- rio::import("../data/function_data/countries.csv")

  countries$country_name <- countries$name
  countries$name <- tolower(countries$name)
  countries$name <- stringi::stri_trans_general(str = countries$name, id = "Latin-ASCII")
  countries$name <- paste0('\\b' ,countries$name, '\\b')

  return(toString(unique(countries[stringr::str_detect(text, countries$name) == TRUE,]$country_name)))

}

detecting_country_university <- function(x) {

  text <- x
  text <- tolower(text)
  text <- stringi::stri_trans_general(str = text, id = "Latin-ASCII")
  text <- gsub('[[:digit:]]+', '', text)
  text <- gsub('[[:punct:] ]+',' ', text)

  universities <- rio::import("../data/function_data/universities.csv")


  universities$university_name <- tolower(universities$university_name)
  universities$university_name <- stringi::stri_trans_general(str = universities$university_name, id = "Latin-ASCII")

  return(toString(unique(universities[stringr::str_detect(text, universities$university_name) == TRUE,]$country_name)))
}

detecting_country_state <- function(x) {

  text <- x
  text <- tolower(text)
  text <- stringi::stri_trans_general(str = text, id = "Latin-ASCII")
  text <- gsub('[[:digit:]]+', '', text)
  text <- qdap::rm_stopwords(text, separate = FALSE)
  text <- gsub('[[:punct:] ]+',' ', text)
  text <- gsub('centre','', text)
  text <- gsub('university','', text)

  states <- rio::import("../data/function_data/states.csv")

  states$name <- tolower(states$name)
  states$name <- stringi::stri_trans_general(str = states$name, id = "Latin-ASCII")
  states$name <- paste0('\\b' ,states$name, '\\b')

  return(toString(unique(states[stringr::str_detect(text, states$name) == TRUE,]$country_name)))

}

detecting_us_states <- function(x) {

  text <- x
  text <- stringi::stri_trans_general(str = text, id = "Latin-ASCII")
  text <- gsub('[[:digit:]]+', '', text)
  text <- gsub('[[:punct:] ]+',' ', text)
  text <- gsub('centre','', text)
  text <- gsub('university','', text)

  abbreviation    <- c("AL",
                       "AK", "AZ", "KS", "UT", "CO", "CT",
                       "DE", "FL", "GA", "HI", "ID", "IL",
                       "IN", "IA", "AR", "KY", "LA", "ME",
                       "MD", "MA", "MI", "MN", "MS", "MO",
                       "MT", "NE", "NV", "NH", "NJ", "NM",
                       "NY", "NC", "ND", "OH", "OK", "OR",
                       "PA", "RI", "SC", "SD", "TN", "TX",
                       "CA", "VT", "VA", "WA", "WV", "WI",
                       "WY", "DC")
  state_name    <- c("Alabama",
                     "Alaska", "Arizona", "Kansas",
                     "Utah", "Colorado", "Connecticut",
                     "Delaware", "Florida", "Georgia",
                     "Hawaii", "Idaho", "Illinois",
                     "Indiana", "Iowa", "Arkansas",
                     "Kentucky", "Louisiana", "Maine",
                     "Maryland", "Massachusetts", "Michigan",
                     "Minnesota", "Mississippi", "Missouri",
                     "Montana", "Nebraska", "Nevada",
                     "New Hampshire", "New Jersey", "New Mexico",
                     "New York", "North Carolina", "North Dakota",
                     "Ohio", "Oklahoma", "Oregon",
                     "Pennsylvania", "Rhode Island", "South Carolina",
                     "South Dakota", "Tennessee", "Texas",
                     "California", "Vermont", "Virginia",
                     "Washington", "West Virginia", "Wisconsin",
                     "Wyoming", "District of Columbia")

  US_states <- data.frame(abbreviation, state_name)

  US_states$abbreviation <- paste0('\\b' ,US_states$abbreviation, '\\b')

  return(toString(unique(US_states[stringr::str_detect(text, US_states$abbreviation) == TRUE,]$state_name)))

}

replace_us_state <- function(x) {
  text <- x
  text <- stringi::stri_trans_general(str = text, id = "Latin-ASCII")
  text <- gsub('[[:digit:]]+', '', text)
  #text <- qdap::rm_stopwords(text, separate = FALSE)
  text <- gsub('[[:punct:] ]+',' ', text)
  text <- gsub('centre','', text)
  text <- gsub('university','', text)

  text_df <- data.frame(stringr::str_split(text, " "))
  names(text_df) <- "splitted"
  for (i in 1:nrow(text_df)) {
    text_df$new_text[i] <- detecting_us_states(text_df$splitted[i])
  }
  text_df[text_df$new_text == "",]$new_text <- text_df[text_df$new_text == "",]$text_df$splitted

  return(paste(text_df$new_text, collapse = " "))

}

url_nominatim_search <- function(search_query_url, country_url,
                                 language_url, email_url) {
  # nominatim search api url
  url_nominatim_search_api <- "https://nominatim.openstreetmap.org/search/"
  # convert input into a list
  search_query_url <- sapply(search_query_url, as.list)
  # percent-encode search request
  search_query_url <- sapply(search_query_url, utils::URLencode)
  # parameters
  if (!is.null(country_url)) {
    country_url <- paste0("&countrycodes=", country_url)
  }
  parameters_url <- paste0("?format=json",
                           "&addressdetails=1&extratags=1&limit=1",
                           country_url, "&accept-language=", language_url,
                           "&email=", email_url)
  # construct search request for geocode
  url_nominatim_search_call <- paste0(url_nominatim_search_api,
                                      search_query_url, parameters_url)
  return(url_nominatim_search_call)
}

get_geodata_from_json_nominatim <- function(geodata_json) {

  # convert json output into r object
  geodata <- lapply(geodata_json, jsonlite::fromJSON, simplifyVector = FALSE)
  # extract coordinates, address and contacts
  lat_lng_a_c <- data.frame(lat = NA, lng = NA, address = NA, pub_name = NA,
                            street_name = NA, house_number = NA, suburb = NA,
                            postcode = NA, state_district = NA, website_1 = NA,
                            website_2 = NA, website_3 = NA, phone_1 = NA,
                            phone_2 = NA, email_1 = NA, email_2 = NA)
  for(i in 1:length(geodata)) {
    if(length(geodata[[i]]) != 0) {
      # get data
      lat <- geodata[[i]][[1]]$lat
      lng <- geodata[[i]][[1]]$lon
      address <- geodata[[i]][[1]]$display_name
      pub_name <- geodata[[i]][[1]]$address$pub
      street_name <- geodata[[i]][[1]]$address$road
      house_number <- geodata[[i]][[1]]$address$house_number
      suburb <- geodata[[i]][[1]]$address$suburb
      postcode <- geodata[[i]][[1]]$address$postcode
      state_district <- geodata[[i]][[1]]$address$state_district
      website_1 <- geodata[[i]][[1]]$extratags$website
      website_2 <- geodata[[i]][[1]]$extratags$url
      website_3 <- geodata[[i]][[1]]$extratags$`contact:website`
      phone_1 <- geodata[[i]][[1]]$extratags$phone
      phone_2 <- geodata[[i]][[1]]$extratags$`contact:phone`
      email_1 <- geodata[[i]][[1]]$extratags$email
      email_2 <- geodata[[i]][[1]]$extratags$`contact:website`
      # get rid of NULLs
      info <- list(lat, lng, address, pub_name, street_name,
                   house_number, suburb, postcode, state_district,
                   website_1, website_2, website_3,
                   phone_1, phone_2, email_1, email_2)
      for (j in 1:length(info)) {
        if (is.null(info[[j]])) info[[j]] <- NA
      }
      # create output data frame
      lat_lng_a_c[i, ] <- info
    } else {
      lat_lng_a_c[i, ] <- NA
    }
  }
  return(lat_lng_a_c)
}

geocode_nominatim <- function(search_query, country = NULL, language = "en",
                              fields = "coordinates", email) {

  search_query <- paste0(tail(unlist(strsplit(search_query, ",")), 2)[1], ",", tail(unlist(strsplit(search_query, ",")), 1))
  # construct url for geocoding
  url_geocode <- url_nominatim_search(search_query, country, language, email)
  # get data from nominatim
  # wait 3 seconds between each call
  geodata_json <- list()
  for (i in 1:length(url_geocode)) {
    geodata_json[i] <- RCurl::getURL(url_geocode[i])
    Sys.sleep(3)
  }

  pattern <- '\"country\":\"\\s*(.*?)\\s*\",\"country_code\"'
  result <- regmatches(geodata_json, regexec(pattern, geodata_json))
  country_match <- result[[1]][2]

  # get data from json output
  geodata_df <- as.data.frame(sapply(search_query, as.character),
                              stringsAsFactors = FALSE)
  names(geodata_df) <- "search query"
  rownames(geodata_df) <- NULL
  geodata_df[, 2:17] <- get_geodata_from_json_nominatim(geodata_json)
  geodata_df_query <- data.frame(search_query = geodata_df[, 1],
                                 stringsAsFactors = FALSE)
  geodata_df_coordinates <- geodata_df[, 2:3]
  geodata_df_address <- geodata_df[, 4:10]
  geodata_df_contacts <- geodata_df[, 11:17]
  # return dataframe with the geodata
  geodata_result <- geodata_df_query
  if("all" %in% fields) {
    geodata_result <- cbind(geodata_result, geodata_df[, 2:17])
  }
  if("coordinates" %in% fields) {
    geodata_result <- cbind(geodata_result, geodata_df_coordinates)
  }
  if("address" %in% fields) {
    geodata_result <- cbind(geodata_result, geodata_df_address)
  }
  if("contacts" %in% fields) {
    geodata_result <- cbind(geodata_result, geodata_df_contacts)
  }

  geodata_result <- cbind(geodata_result, country_match)

  return(geodata_result)
}

## Affiliation function

get_affiliations <- function(PMID, email = NULL, format.long = FALSE) {

  #get the xml of the articles
  doc <- easyPubMed::get_pubmed_ids(PMID)
  doc <- easyPubMed::fetch_pubmed_data(doc)
  doc <- XML::xmlParse(doc)
  #get the authors
  authors <- XML::getNodeSet(doc,"//Author")
  #turning the xml into a dataframe
  au <- list()
  for(i in 1:length(authors)){
    au[[i]] <- data.frame(XML::xmlToList(authors[[i]]))
  }
  author.aff <- data.table::rbindlist(au, fill = TRUE)

  # Coliding affiliations
  col_matches_aff <- dplyr::select(author.aff, starts_with('Affiliation'))
  pubmed_df <- cbind(author.aff[,1:3], col_matches_aff)
  pubmed_df <- pubmed_df[!duplicated(pubmed_df),]
  Position <- 1:nrow(pubmed_df)
  pubmed_df <- cbind(Position, pubmed_df)

  pubmed_df_original <- tidyr::unite(pubmed_df, col = 'Affiliations', 5:ncol(pubmed_df), sep = "_", remove = TRUE)

  Original_Affiliation <- pubmed_df_original$Affiliations

  author.aff2 <- pubmed_df
  data_long <- tidyr::gather(author.aff2, aff_number, Affiliation, 5:ncol(author.aff2), factor_key=TRUE)

  data_long <- data_long[!duplicated(data_long),]
  Original_Affiliation_long <- data_long$Affiliation

  if (!is.null(email)){

    for(i in 1:nrow(data_long)) {
      if (!is.na(data_long$Affiliation[i]) & !is.na(geocode_nominatim(data_long$Affiliation[i], email = email)$country_match)){
        data_long$Affiliation[i] <-  geocode_nominatim(data_long$Affiliation[i], email = email)$country_match}
    }

    for(i in 1:nrow(data_long)) {
      if (data_long$Affiliation[i] == "" & !is.na(data_long$Affiliation[i]) & detecting_country_country(data_long$Affiliation[i]) != ""){
        data_long$Affiliation[i] <-  detecting_country_country(data_long$Affiliation[i])}
    }

    for(i in 1:nrow(data_long)) {
      if (data_long$Affiliation[i] == "" & !is.na(data_long$Affiliation[i]) & detecting_country_university(data_long$Affiliation[i]) != ""){
        data_long$Affiliation[i] <-  detecting_country_university(data_long$Affiliation[i])}
    }

    for(i in 1:nrow(data_long)) {
      if (data_long$Affiliation[i] == "" & !is.na(data_long$Affiliation[i]) & detecting_country_state(data_long$Affiliation[i]) != ""){
        data_long$Affiliation[i] <-  detecting_country_state(data_long$Affiliation[i])}
    }

    data_wide <- tidyr::spread(data_long, aff_number, Affiliation)
    pubmed_df <- data_wide
    pubmed_df$Position <- NULL

    pubmed_df <- tidyr::unite(pubmed_df, col = 'Affiliations', 4:ncol(pubmed_df), sep = "_", remove = TRUE)
    pubmed_df$Original_Affiliation <- Original_Affiliation
    pubmed_df$Initials <- NULL
    names(pubmed_df) <- c("author_lastname", "author_firstname", "country_of_affiliation", "affiliation_freetext")

    data_long$Original_Affiliation <- Original_Affiliation_long
    data_long <- dplyr::filter(data_long, !is.na(Affiliation))
    data_long$Initials <- NULL
    data_long$aff_number <- NULL
    names(data_long) <- c("author_position", "author_lastname", "author_firstname", "country_of_affiliation", "affiliation_freetext")

    if (format.long == FALSE) {
      return(pubmed_df)
    } else {
      return(data_long)
    }

  } else {

    for(i in 1:nrow(data_long)) {
      if (!is.na(data_long$Affiliation[i]) & detecting_country_country(data_long$Affiliation[i]) != ""){
        data_long$Affiliation[i] <-  detecting_country_country(data_long$Affiliation[i])}
    }

    for(i in 1:nrow(data_long)) {
      if (data_long$Affiliation[i] == "" & !is.na(data_long$Affiliation[i]) & detecting_country_university(data_long$Affiliation[i]) != ""){
        data_long$Affiliation[i] <-  detecting_country_university(data_long$Affiliation[i])}
    }

    for(i in 1:nrow(data_long)) {
      if (data_long$Affiliation[i] == "" & !is.na(data_long$Affiliation[i]) & detecting_country_state(data_long$Affiliation[i]) != ""){
        data_long$Affiliation[i] <-  detecting_country_state(data_long$Affiliation[i])}
    }

    data_wide <- tidyr::spread(data_long, aff_number, Affiliation)
    pubmed_df <- data_wide
    pubmed_df$Position <- NULL

    pubmed_df <- tidyr::unite(pubmed_df, col = 'Affiliations', 4:ncol(pubmed_df), sep = "_", remove = TRUE)
    pubmed_df$Original_Affiliation <- Original_Affiliation
    pubmed_df$Initials <- NULL
    names(pubmed_df) <- c("author_lastname", "author_firstname", "country_of_affiliation", "affiliation_freetext")

    data_long$Original_Affiliation <- Original_Affiliation_long
    data_long <- dplyr::filter(data_long, !is.na(Affiliation))
    data_long$Initials <- NULL
    data_long$aff_number <- NULL
    names(data_long) <- c("author_position", "author_lastname", "author_firstname", "country_of_affiliation", "affiliation_freetext")

    if (format.long == FALSE) {
      return(pubmed_df)
    } else {
      return(data_long)
    }
  }
}

auth_aff_dict <- function (x,y) {

  authors_name <- x
  authors_name <- stringr::str_split(authors_name, ", ")
  authors_df <- data.frame(authors_name)
  names(authors_df) <- "Authors"
  authors_df$Affiliations <- stringr::str_match_all(authors_df$Authors,"[0-9]+")

  affiliation_dict <- y
  affiliation_dict <- stringr::str_split(affiliation_dict, "[.] [0-9]")
  affiliation_dict <- stringr::str_split(affiliation_dict, "\n[0-9]+ ")
  affiliation_dict <- data.frame(affiliation_dict)
  names(affiliation_dict) <- 'Affiliations'
  affiliation_dict$Keys <- 1:nrow(affiliation_dict)
  affiliation_dict <- affiliation_dict[,c(2,1)]
  affiliation_dict$Affiliations <- as.character(affiliation_dict$Affiliations)


  authors_df$Affiliations <- as.vector(authors_df$Affiliations)
  authors_df <- tidyr::separate(authors_df, col= Affiliations, sep = ',', into = toupper(letters[1:10]))
  authors_df[is.na(authors_df)] <- 0

  authors_df$A <- as.numeric(gsub("[^0-9]", "", authors_df$A))
  authors_df$B <- as.numeric(gsub("[^0-9]", "", authors_df$B))
  authors_df$C <- as.numeric(gsub("[^0-9]", "", authors_df$C))
  authors_df$D <- as.numeric(gsub("[^0-9]", "", authors_df$D))
  authors_df$E <- as.numeric(gsub("[^0-9]", "", authors_df$E))
  authors_df$F <- as.numeric(gsub("[^0-9]", "", authors_df$F))
  authors_df$G <- as.numeric(gsub("[^0-9]", "", authors_df$G))
  authors_df$H <- as.numeric(gsub("[^0-9]", "", authors_df$H))
  authors_df$I <- as.numeric(gsub("[^0-9]", "", authors_df$I))
  authors_df$J <- as.numeric(gsub("[^0-9]", "", authors_df$J))

  authors_df_sub <- authors_df[2:ncol(authors_df)]
  authors_df_sub <- data.frame(t(authors_df_sub))
  authors_df_sub$ROWSUM <- rowSums(authors_df_sub)
  authors_df_sub <- authors_df_sub[which(authors_df_sub$ROWSUM > 0),]
  authors_df_sub$ROWSUM <- NULL
  authors_df_sub <- data.frame(t(authors_df_sub))

  authors_df <- cbind(authors_df[,1], authors_df_sub)

  aff_hash <- hash::hash(keys = affiliation_dict$Keys, values = affiliation_dict$Affiliations)
  hash::.set(aff_hash, 0, 'NA')
  if("A" %in% colnames(authors_df)){
    authors_df$A <- values(aff_hash, keys = authors_df$A)
  }
  if("B" %in% colnames(authors_df)){
    authors_df$B <- values(aff_hash, keys = authors_df$B)
  }
  if("C" %in% colnames(authors_df)){
    authors_df$C <- values(aff_hash, keys = authors_df$C)
  }
  if("D" %in% colnames(authors_df)){
    authors_df$D <- values(aff_hash, keys = authors_df$D)
  }
  if("E" %in% colnames(authors_df)){
    authors_df$E <- values(aff_hash, keys = authors_df$E)
  }
  if("F" %in% colnames(authors_df)){
    authors_df$F <- values(aff_hash, keys = authors_df$F)
  }
  if("G" %in% colnames(authors_df)){
    authors_df$G <- values(aff_hash, keys = authors_df$G)
  }
  if("H" %in% colnames(authors_df)){
    authors_df$H <- values(aff_hash, keys = authors_df$H)
  }
  if("I" %in% colnames(authors_df)){
    authors_df$I <- values(aff_hash, keys = authors_df$I)
  }
  if("J" %in% colnames(authors_df)){
    authors_df$J <- values(aff_hash, keys = authors_df$J)
  }

  authors_df <- tidyr::unite(authors_df, col = 'Affiliations', 2:ncol(authors_df), sep = "_", remove = TRUE)
  names(authors_df) <- c('Authors', 'Affiliations')
  rownames(authors_df) <- 1:nrow(authors_df)

  authors_df$Affiliations <- gsub("_NA", "", authors_df$Affiliations)
  authors_df[authors_df$Affiliations == "NA",]$Affiliations <- NA

  authors_df$to_delete <- "do not delete"
  for (i in 1:nrow(authors_df)) (
    if (is.na(authors_df$Affiliations[i]))
      authors_df$to_delete[i + 1] <- "Delete this row"
  )

  authors_df <- authors_df %>%
    tidyr::fill(Affiliations, .direction = "up")

  authors_df <- dplyr::filter(authors_df, to_delete != "Delete this row")

  authors_df$to_delete <- NULL

  authors_df <- authors_df %>%
    mutate(Affiliations = strsplit(as.character(Affiliations), "_")) %>%
    unnest(Affiliations)

  authors_df$countries_of_affiliation <- ""

  for(i in 1:nrow(authors_df)) {
    if (!is.na(authors_df$Affiliations[i]) & detecting_country_country(authors_df$Affiliations[i]) != ""){
      authors_df$countries_of_affiliation[i] <-  detecting_country_country(authors_df$Affiliations[i])}
  }

  for(i in 1:nrow(authors_df)) {
    if (authors_df$countries_of_affiliation[i] == "" & !is.na(authors_df$Affiliations[i]) & detecting_country_university(authors_df$Affiliations[i]) != ""){
      authors_df$countries_of_affiliation[i] <-  detecting_country_university(authors_df$Affiliations[i])}
  }

  for(i in 1:nrow(authors_df)) {
    if (authors_df$countries_of_affiliation[i] == "" & !is.na(authors_df$Affiliations[i]) & detecting_country_state(authors_df$Affiliations[i]) != ""){
      authors_df$countries_of_affiliation[i] <-  detecting_country_state(authors_df$Affiliations[i])}
  }

  authors_df <- authors_df %>%
    group_by(Authors) %>%
    mutate(Affiliations = paste0(Affiliations, collapse = "_"), countries_of_affiliation = paste0(countries_of_affiliation, collapse = "_")) %>%
    slice(1L)

  names(authors_df) <- c("author_fullname", "affiliation_freetext", "country_of_affiliation")

  authors_df$author_fullname <- gsub("[0-9]+", "", authors_df$author_fullname)
  authors_df$author_fullname <- gsub(",", "", authors_df$author_fullname)

  authors_df <- authors_df[,c(1,3,2)]

  return(authors_df)
}


