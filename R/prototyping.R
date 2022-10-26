####  Web Scraping Code / ERDDAP Data Access

# Resource for setting up actions YAML:
# https://uncharteddata.netlify.app/posts/2022-10-07-automating-workflows-with-github-actions/


library(here)
library(rvest)
library(tidyverse)
library(boxr)
library(gmRi)


####  Common Resources  ####

# # Specify URL where file is stored
# base_url <-  "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/"

# # Specify destination where file should be saved
# save_destination <- here::here("testing_data")


# Date to test against
today <- Sys.Date()






####________________________________________#####


####  Check for Available/Missing Data  ####
check_sst_inventory <- function(access_date, save_destination = NULL, base_url = NULL){
  
  # Set base path for testing locally
  if(is.null(save_destination)){save_destination <- here::here("testing_data")}
  
  
  # Take inventory of what dates are in the save destination folder
  
  # Last month's folder
  prior_date <- access_date - 30
  prior_folder<- str_sub(str_remove_all(prior_date, "-"), 1, 6)
  
  # This month's folder
  access_folder <- str_sub(str_remove_all(access_date, "-"), 1, 6)
  
  # Check inventory
  file_inventory <- c(
    list.files(str_c(save_destination, "/", prior_folder)),
    list.files(str_c(save_destination, "/", access_folder)))
  
  
  # Flag any that are prelim and ignore them so new data comes in
  full_inventory <- file_inventory
  ignore_inventory <- file_inventory[!str_detect(file_inventory, "prelim")]

  # Return all the files, and the ones ignoring preliminary.
  return(
    list(
      "full" = file_inventory,
      "ignore" = ignore_inventory
    )
  )
  }





####__________________________________####


####  Rvest Web Scraping Approach  ####
# Finds all available links for a month
# 


####  Using rvest to parse the available links for the current month:
# Downloads all files for whatever month matches "access_date"
rvest_fresh_oisst <- function(access_date, base_url = NULL, save_destination = NULL, ignore_files = NULL){
  
  # Set the base paths
  if(is.null(base_url)){
    base_url <-  "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/"}
  # Set base path for testing locally
  if(is.null(save_destination)){save_destination <- here::here("testing_data/")}
  
  
  # Padded month strings
  year_num  <- str_sub(access_date, 1,4)
  month_num <- str_sub(access_date, 6,7)
  
  # Build the Paths to Download URL's & Where to Save the files
  month_link <- paste0(base_url, year_num, month_num,"/")
  save_dir  <- paste0(save_destination, year_num, month_num)
  
  
  # Connect to the web page:
  page <- read_html(month_link)
  
  # Get all the links that are for netcdf files:
  nc_names <- page %>%
    html_nodes("a") %>%       # find all links
    html_attr("href") %>%     # get the url
    str_subset("\\.nc") #%>% # find those that end in xlsx
  #.[[1]]                    # look at the first one
  
  
  # ---- NOTE:
  # This is the step where you should remove any files that have already been
  # downloaded. This eliminates the overwriting of up to 30 files every day
  # Then later, check for preliminary and finalized data copies
  message(paste(c("Ignoring Available Links for: ", ignore_files), collapse = "\n"))
  in_cache <- nc_names %in% ignore_files
  to_skip <- nc_names[in_cache]
  to_download <- nc_names[!in_cache]
  
  # ----
  
  # Build back the file list:
  link_list <- str_c(month_link, to_download)
  save_list <- str_c(save_dir, "/", to_download)
  
  
  # Double check the save link exists, create if not
  path_exist <- save_dir %in% list.dirs(save_destination)
  
  # If it doesn't 
  if(path_exist == FALSE){
    message(paste0("Creating new directory for downloads under: "), save_dir)
    dir.create(save_dir)
  }
  
  
  # Save all the daily files:
  walk2(link_list, save_list, function(x, y){download.file(url = x, destfile = y)})
  
  
}




####________________________________________#####


####  Preliminary File Management  ####

# Remove duplicate dates with final and preliminary data
retire_prelim_data <- function(access_date, save_destination = NULL, base_url = NULL){
  
  # Specify URL where file is stored
  if(is.null(base_url)){
    base_url <-  "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/"
  }
  
  # Set base path for testing locally
  if(is.null(save_destination)){save_destination <- here::here("testing_data")}
  
  
  # Build folder structures from date & save location
  
  # Last month's folder
  prior_date <- access_date - 30
  prior_folder <- str_sub(str_remove_all(prior_date, "-"), 1, 6)
  
  # This month's folder
  access_folder <- str_sub(str_remove_all(access_date, "-"), 1, 6)
  
  # Inventory of dates
  data_inventory <- bind_rows(
    list(
      data.frame("file" = list.files(str_c(save_destination, "/", prior_folder))),
      data.frame("file" = list.files(str_c(save_destination, "/", access_folder)))) %>% 
      setNames(c(prior_folder, access_folder)),
    .id = "folder")
    
  
  # Check if there are duplicates
  data_inventory <- data_inventory %>% 
    mutate(
      file_date = str_sub(file, 20, 27),
      prelim = ifelse(str_detect(file, "prelim"), T, F),
      full_path = str_c(save_destination, folder, file, sep = "/"))  %>% 
    group_by(file_date) %>% 
    mutate(duplicate = ifelse(n() > 1, TRUE, FALSE)) %>% 
    ungroup()
  
  
  # Flag any preliminary files that are also duplicate dates
  to_remove <- data_inventory %>% 
    filter(duplicate & prelim)
  
  # Grab File Names
  to_delete <- to_remove %>% pull(full_path)
  
  # Use to Remove Files
  walk(to_delete, function(x){
    # Message Explaining What Dates were flagged
    message(str_c("Retiring preliminary data for: ", str_sub(x, -23, -16)))
    file.remove(x)
  })
  
}


####________________________________________#####

#### Full Workflow  ####

# Step 1: Check Inventory for Files to Skip (Speeds it up)

## Check if it works locally
(local_inventory <- check_sst_inventory(access_date = Sys.Date(), save_destination = here::here("testing_data")))


# Does it match the GMRI folder structures? Not currently, just the month numbers
box_cache <- cs_path("res", "OISST/oisst_mainstays/update_caches")
(box_inventory <- check_sst_inventory(access_date = Sys.Date(), save_destination = box_cache))


# Step 2: Get Missing Files (Last Month)

# Run Last Month
last_mnth <- (Sys.Date()- 30)

# Local
rvest_fresh_oisst(access_date = last_mnth, 
                  base_url = NULL, 
                  save_destination = NULL, 
                  ignore_files = local_inventory$ignore)

# Box
rvest_fresh_oisst(access_date = last_mnth, 
                  base_url = NULL, 
                  save_destination = box_cache, 
                  ignore_files = box_inventory$ignore)



# Step 3: Get Missing Files (Current Month)


# Run this month locally
rvest_fresh_oisst(
  access_date = Sys.Date(), 
  base_url = NULL, 
  save_destination = NULL, 
  ignore_files = local_inventory$ignore)

# Run for Box
rvest_fresh_oisst(
  access_date = Sys.Date(), 
  #access_date = "2022-09-26", 
  base_url = NULL, 
  save_destination = box_cache, 
  ignore_files = box_inventory$ignore)




# Step 4:  Remove Duplicates and Preliminaries with

# Retire Files in testing Environment
retire_prelim_data(access_date = Sys.Date() - 30, base_url = NULL, save_destination = NULL)
retire_prelim_data(access_date = Sys.Date(), base_url = NULL, save_destination = NULL)

# Retire Files on Box
retire_prelim_data(access_date = Sys.Date() - 30, base_url = NULL, save_destination = box_cache) 
retire_prelim_data(access_date = Sys.Date(), base_url = NULL, save_destination = box_cache) 







####_______________________________####
####  Downloading Files Using Direct Access Links from NCEI  
# Shouldn't need any packages
# NCEI OISSTv2 AVHRR File Directory:


####  Download.file Single Date Approach  ####
# Needs destination folders to exist, Needs file names to not change
# Does not "find" links, but builds them from a known structure

# download_oisst_daily <- function(access_date, save_destination = NULL, base_url = NULL){
#   
#   # Set base url and destination folders:
#   # Specify URL where file is stored
#   if(is.null(base_url)){
#     base_url <-  "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/"
#   }
#   
#   # Set base path for testing locally
#   if(is.null(save_destination)){save_destination <- here::here("testing_data")}
#   
#   
#   #### Build the Date Dependent File Name Info:
#   #### ---- NOTE: Any date & path configurations should be iterable:
#   
#   # Padded month and day strings
#   year_num  <- str_sub(access_date, 1,4)
#   month_num <- str_sub(access_date, 6,7)
#   day_num   <- str_sub(access_date, 9,10)
#   
#   # Shorthand for year and month
#   yr_month <- paste0(year_num, month_num)
#   
#   # Target Folder containing daily files for that month
#   month_folder <- paste0(base_url, yr_month, "/")
#   
#   # Date filename structure for a daily file name:
#   date_full <- paste0(yr_month, day_num)
#   
#   # Download Cache Location:
#   dest_path <- str_c(save_destination, "/", yr_month, "/")
#   
#   #### ---- End Date and Path Configurations
#   
#   
#   #### ---- NOTE: a switch for  preliminary detection could go here
#   
#   # Finalized File Names:
#   daily_base <- paste0("oisst-avhrr-v02r01.", date_full, ".nc")
#   
#   # Preliminary Data Names:
#   daily_prelim <- paste0("oisst-avhrr-v02r01.", date_full, "_preliminary.nc")
#   
#   
#   # Check if the date is within 2 weeks of the system date:
#   is_prelim <-  as.numeric(str_sub(Sys.Date(), -2, -1)) - as.numeric(day_num) <= 14
#   
#   # Set the file target name
#   use_daily <- ifelse(is_prelim, daily_prelim, daily_base)
#   
#   #### ---- End Note on prelim check
#   
#   # Target URL for download link based on date:
#   daily_target_url <- str_c(month_folder, use_daily)
#   
#   # Destination File Name, Full Path to save
#   destfile <- str_c(dest_path, use_daily)
#   
#   # Print a message
#   message(str_c(
#     "Downloading daily file for: ", access_date, ". Using destination path: ", destfile
#   ))
#   
#   
#   # Apply download.file function in R
#   download.file(url = daily_target_url, destfile = destfile)
#   
#   
# }
# 
# 
# # Download a day
# download_oisst_daily(access_date = "2021-10-05")
