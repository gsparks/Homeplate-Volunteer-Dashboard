library(httr)
library(jsonlite)
source('secrets.R')

# returns the check_vec if its length is not 0
# Otherwise return a vector of NAs the length of the reference_vec
fill_if0 <- function(reference_vec, check_vec) {
  if(length(check_vec) == 0) {
    return(rep(NA, length(reference_vec)))
  } else {
    return(check_vec)
  }
}

get_volunteer_data <- function() {
  # Get first page of data from Homeplate airtable
  response <- GET("https://api.airtable.com/v0/apppms4ZuxVLyJySd/Volunteers?maxRecords=10000&view=Master%20List",
                  query = list(api_key = token))

  text_json_response <- content(response, as="text")
  raw_data <- fromJSON(text_json_response)
  data <- raw_data[[1]][2][,1]
  volunteer_data <- data.frame(fill_if0(data[["Name"]], data[["Name"]]),
                               fill_if0(data[["Name"]], data[["Status"]]),
                               fill_if0(data[["Name"]], data[["Telephone Number"]]),
                               fill_if0(data[["Name"]], data[["Company/Organization"]]),
                               fill_if0(data[["Name"]], data[["Current Year Hours Count"]]),
                               fill_if0(data[["Name"]], data[["Total Hours Served"]]),
                               fill_if0(data[["Name"]], data[["2018 Hours"]]),
                               fill_if0(data[["Name"]], data[["Email Address"]]),
                               fill_if0(data[["Name"]], data[["Birthday"]]))

  columns <- c("name", "status", "phone_number",
               "org", "current_year_hours", "total_hours",
               "hours_2018", "email", "birthday")

  colnames(volunteer_data) <- columns

  # While the http headers contain a pagination offset,
  # continue to fetch and append the next page of data
  while(!is.null(raw_data[['offset']])) {
    ofst <- raw_data[['offset']]
    response <- GET("https://api.airtable.com/v0/apppms4ZuxVLyJySd/Volunteers?maxRecords=10000&view=Master%20List",
                    query = list(api_key = token,
                                 offset = ofst))

    text_json_response <- content(response, as="text")
    raw_data <- fromJSON(text_json_response)
    data <- raw_data[[1]][2][,1]
    volunteer_data_n <- data.frame(fill_if0(data[["Name"]], data[["Name"]]),
                                   fill_if0(data[["Name"]], data[["Status"]]),
                                   fill_if0(data[["Name"]], data[["Telephone Number"]]),
                                   fill_if0(data[["Name"]], data[["Company/Organization"]]),
                                   fill_if0(data[["Name"]], data[["Current Year Hours Count"]]),
                                   fill_if0(data[["Name"]], data[["Total Hours Served"]]),
                                   fill_if0(data[["Name"]], data[["2018 Hours"]]),
                                   fill_if0(data[["Name"]], data[["Email Address"]]),
                                   fill_if0(data[["Name"]], data[["Birthday"]]))
    colnames(volunteer_data_n) <- columns

    volunteer_data <- rbind(volunteer_data, volunteer_data_n)
  }

  return(volunteer_data)
}
