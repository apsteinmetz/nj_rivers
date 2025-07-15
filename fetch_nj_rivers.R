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

