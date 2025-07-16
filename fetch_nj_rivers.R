# analyze nj river stream data
# load site number file from data folder
library(tidyverse)
library(lubridate)
library(here)
library(httr)
library(httr2)
library(xml2)

# load site numbers
 # read and parse "data/USGS_NJ_site_numbers.txt, space delimted, skip lines starting with #"
sites <- read_tsv(here("data", "USGS_NJ_site_numbers.txt"), 
                            skip = 19, 
                            col_names = c("agency","site_number", "station_name")) |> 
  mutate(site_number = str_trim(site_number),
         station_name = str_trim(station_name))


# Function to get site information from USGS API
parse_usgs_response <- function(response) {

  if (resp_status(response) == 200) {
    # Parse the response into a data frame
    content_text <- resp_body_string(response)
    
    # Split into lines and find data lines (skip comments starting with #)
    lines <- strsplit(content_text, "\n")[[1]]
    data_lines <- lines[!grepl("^#", lines)]
    
    if (length(data_lines) >= 3) {
      # Split by tabs
      col_names <- strsplit(data_lines[1], "\t")[[1]]
      fields <- strsplit(data_lines[3],"\t")[[1]]
      # pad fields with empty string if not enough fields to match col_names
      if (length(fields) < length(col_names)) {
        fields <- c(fields, rep("", length(col_names) - length(fields)))
      }
      # enframe fields with col_names as names
      fields <- setNames(fields, col_names)
      site_metadata <- enframe(fields) |>
        filter(name %in% c("station_nm", "site_no", "map_nm", "dec_lat_va", "dec_long_va", "alt_va","huc_cd")) %>%
        pivot_wider(names_from = name, values_from = value) |> 
        # simplify names
        rename(site_name = station_nm, site_number = site_no, 
               latitude = dec_lat_va, longitude = dec_long_va, 
               altitude = alt_va,map_name=map_nm,
               basin_code = huc_cd) |> 
        # convert latitude and longitude and altitude to numeric
        mutate(
          latitude = as.numeric(latitude),
          longitude = as.numeric(longitude),
          altitude = as.numeric(altitude)
        )
    }
    return(site_metadata)
  } else {
    warning(paste("No data found for site number:", site_number))
    return(tibble(site_name = NA, site_number = site_number, latitude = NA, longitude = NA, map_name = NA, 
                  altitude = NA))
  }
}

get_usgs_site_info <- function(site_numbers) {

  base_url <- "https://waterservices.usgs.gov/nwis/site/"
  reqs <- site_numbers |> 
    map(\(x) req_url_query(request(base_url),sites=x, format = "rdb", siteOutput = "expanded"))
  
  responses <- req_perform_sequential(reqs)

  sites_metadata <- responses |> 
    map(parse_usgs_response) |> 
    bind_rows()
  return(sites_metadata)
}

#' Download USGS Stream Gauge Data
#' 
#' Downloads highest available frequency time series data for USGS stream gauges
#' including stream height (gage height) and discharge rate
#' 
#' @param station_ids Character vector of USGS station IDs (e.g., "08167000")
#' @param start_date Start date in format "YYYY-MM-DD" 
#' @param end_date End date in format "YYYY-MM-DD"
#' @return Data frame with columns: site_number, datetime, discharge_cfs, gage_height_ft, plus quality flags
#' 
download_usgs_stream_data <- function(station_ids, start_date, end_date) {

  # Validate inputs
  if (length(station_ids) == 0) stop("At least one station ID required")
  
  # Convert dates to proper format
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  if (start_date > end_date) stop("Start date must be before end date")
  
  # USGS parameter codes
  # 00060 = Discharge, cubic feet per second
  # 00065 = Gage height, feet
  parameter_codes <- "00060,00065"
  
  # Create the API URL
  base_url <- "https://waterservices.usgs.gov/nwis/iv/"
  
  # Combine station IDs into comma-separated string
  sites_param <- paste(station_ids, collapse = ",")
  
  # Build query parameters
  query_params <- list(
    format = "rdb",
    sites = sites_param,
    parameterCd = parameter_codes,
    startDT = format(start_date, "%Y-%m-%d"),
    endDT = format(end_date, "%Y-%m-%d"),
    siteStatus = "all"
  )
  
  cat("Downloading data for", length(station_ids), "stations from", 
      format(start_date), "to", format(end_date), "...\n")
  
  # Make the API request
  response <- httr::GET(base_url, query = query_params)
  
  # Check if request was successful
  if (httr::status_code(response) != 200) {
    stop("API request failed with status code: ", httr::status_code(response))
  }
  
  # Get the content as text
  content_text <- httr::content(response, "text", encoding = "UTF-8")
  
  # Split into lines
  lines <- strsplit(content_text, "\n")[[1]]
  
  # Remove comment lines (lines starting with #)
  data_lines <- lines[!grepl("^#", lines)]
  
  # Remove empty lines
  data_lines <- data_lines[data_lines != ""]
  
  if (length(data_lines) < 2) {
    warning("No data returned for the specified parameters")
    return(data.frame())
  }
  
  # The first line contains column headers, second line contains data types
  # Skip the data types line and read from the third line onwards
  header_line <- data_lines[1]
  data_content <- data_lines[3:length(data_lines)]
  
  # Parse the header to get column names
  headers <- strsplit(header_line, "\t")[[1]]
  
  # Parse the data
  if (length(data_content) == 0) {
    warning("No data rows found")
    return(data.frame())
  }
  
  # Split each data line by tabs
  data_split <- strsplit(data_content, "\t")
  
  # Convert to matrix then data frame
  max_cols <- max(sapply(data_split, length))
  data_matrix <- matrix(NA, nrow = length(data_split), ncol = max_cols)
  
  for (i in seq_along(data_split)) {
    row_data <- data_split[[i]]
    data_matrix[i, 1:length(row_data)] <- row_data
  }
  
  # Create data frame with proper column names
  df <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
  
  # Set column names (truncate if we have more columns than headers)
  if (ncol(df) <= length(headers)) {
    names(df) <- headers[1:ncol(df)]
  } else {
    names(df) <- c(headers, paste0("col_", (length(headers)+1):ncol(df)))
  }
  
  # Clean and standardize the data frame
  # Find the essential columns we need
  site_col <- which(grepl("site_no", names(df), ignore.case = TRUE))[1]
  datetime_col <- which(grepl("datetime", names(df), ignore.case = TRUE))[1]
  
  # Find discharge and gage height columns (they often have parameter codes in names)
  discharge_cols <- which(grepl("00060|discharge", names(df), ignore.case = TRUE))
  gage_height_cols <- which(grepl("00065|gage.*height", names(df), ignore.case = TRUE))
  
  if (is.na(site_col) || is.na(datetime_col)) {
    stop("Could not identify required columns in the data")
  }
  
  # Select and rename columns
  result_df <- df %>%
    select(
      site_number = all_of(site_col),
      datetime = all_of(datetime_col),
      everything()
    ) %>%
    # Convert datetime
    mutate(
      datetime = ymd_hm(datetime, tz = "US/Eastern")
    )
  
  # Handle discharge columns
  if (length(discharge_cols) > 0) {
    discharge_col <- discharge_cols[1]  # Take first discharge column
    discharge_flag_col <- discharge_cols[2] # Take second as flag if available
    
    result_df <- result_df %>%
      mutate(
        discharge_cfs = as.numeric(.data[[names(df)[discharge_col]]])
      )
    
    if (length(discharge_cols) > 1) {
      result_df <- result_df %>%
        mutate(
          discharge_flag = .data[[names(df)[discharge_flag_col]]]
        )
    }
  } else {
    result_df$discharge_cfs <- NA_real_
    result_df$discharge_flag <- NA_character_
  }
  
  # Handle gage height columns  
  if (length(gage_height_cols) > 0) {
    gage_col <- gage_height_cols[1]  # Take first gage height column
    gage_flag_col <- gage_height_cols[2] # Take second as flag if available
    
    result_df <- result_df %>%
      mutate(
        gage_height_ft = as.numeric(.data[[names(df)[gage_col]]])
      )
    
    if (length(gage_height_cols) > 1) {
      result_df <- result_df %>%
        mutate(
          gage_height_flag = .data[[names(df)[gage_flag_col]]]
        )
    }
  } else {
    result_df$gage_height_ft <- NA_real_
    result_df$gage_height_flag <- NA_character_
  }
  
  # Select final columns and clean up
  final_df <- result_df %>%
    select(
      site_number,
      datetime,
      discharge_cfs,
      discharge_flag,
      gage_height_ft,
      gage_height_flag
    ) %>%
    # Remove rows where both discharge and gage height are missing
    filter(!(is.na(discharge_cfs) & is.na(gage_height_ft))) %>%
    # Arrange by site and datetime
    arrange(site_number, datetime)
  
  cat("Downloaded", nrow(final_df), "records for", 
      length(unique(final_df$site_number)), "stations\n")
  
  return(final_df)
}

# main body --------------------------------------------------------------------
sites_metadata <- sites$site_number |> 
  map(get_usgs_site_info) |> 
  bind_rows()

# add column for river drainage basin name
sites_metadata <- sites_metadata |> 
  mutate(basin_name = case_when(
    str_detect(basin_code, "0103") ~ "Passaic River Basin",
    str_detect(basin_code, "0105") ~ "Raritan River Basin",
    str_detect(basin_code, "0104") ~ "Rahway River Basin",
    TRUE ~ "Other"
  ))

# Save the metadata to a RData file
save(sites_metadata, file = here("data", "nj_river_sites_metadata.RData"))


stream_data <- download_usgs_stream_data(
  station_ids = sites_metadata$site_number,
  start_date = "2025-07-13",
  end_date = "2025-07-15"
)

stream_data <- stream_data |> 
  # remove columns with _flag suffix
  select(-ends_with("_flag")) |> 
  # add one second to datetimes at midnight
  mutate(datetime = as_datetime(ifelse(hour(datetime) == 0 & minute(datetime) == 0 & second(datetime) == 0,
                           datetime + seconds(1), datetime)),tz = "US/Eastern")

# Display the results
head(stream_data, 20)
# save streatm data as an R data file in the data folder
save(stream_data, file = "data/nj_river_data.RData")

