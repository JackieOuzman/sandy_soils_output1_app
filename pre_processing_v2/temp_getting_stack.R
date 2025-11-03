##I dont understand where the NDVI_stack.rds is getting saved / made.

#I think I found it

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(lubridate)
  library(ggtext)
  library(scales)
  library(ggplot2)
  library(fs)
})
rm(list=ls())
# ====================== Sites ======================
#site <- "1.Walpeup_MRS125"
#site <- "2.Crystal_Brook_Brians_House"
#site <- "3.Wynarka_Mervs_West"
#site <- "4.Wharminda"
#site <- "5.Walpeup_Gums"
site <- "6.Crystal_Brook_Randals"
# ====================== ratio to process ======================
ratio_name <- 
  "NDVI" 


# ====================== Year ======================
year_of_analysis <- 2025

# ====================== PATHS ======================
readDir <- paste0("//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}/work/Output-1/", site)
metadata_path <- "//fs1-cbr.nexus.csiro.au/{lw-soildatarepo}/work/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/"

Dir <- paste0("//fs1-cbr.nexus.csiro.au/{lw-soildatarepo}/work/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/Current/Files/",site)
Dir_year <- paste0(Dir, "/", as.character(year_of_analysis))

saveDir_year <- Dir_year

################################################################################
######################## 1) Read in Paddock maps ###############################
################################################################################
# ====================== list of bad dates for satellite ======================
bad_dates <- readxl::read_excel(
  paste0(file.path(readDir, "7.In_Season_data", 
                   "Sentinel_list_bad_dates.xlsx") ))
bad_dates$Dates <- as.character(bad_dates$Dates)

# Convert to a list where each column becomes a list element
bad_dates_list <- as.list(bad_dates)

# ====================== METADATA ======================
site.info <- readRDS(paste0(Dir, "/site_info.rds"))

file_path_details <- readxl::read_excel(
  paste0(metadata_path,"names of treatments per site 2025 metadata and other info.xlsx"),
  sheet = "location of file and details") %>% 
  filter(Site == site)

site_extension <- file_path_details$`sential file name extension`

# =================== 3.2 ) PROCESS SEASONS ===================
seasons <- site.info$seasons
seasons <- seasons %>%
  filter(year == year_of_analysis) %>%
  mutate(yr = as.numeric(year),
         plant_date = as.Date(plant_date)
  )

plant_date <- seasons$plant_date

################################################################################
############################  Sentinel-2   #############################
################################################################################

ratio_type <- paste0(ratio_name , "_Stack")

# Path to your precomputed Sentinel ratio stack
sen_path <- file.path(readDir, "7.In_Season_data", 
                      substr(year_of_analysis, 3, 4), 
                      "2.Satellite_Imagery",
                      "Sentinel", 
                      paste0(ratio_type, "_", site_extension, ".tif"))

if (!file.exists(sen_path)) stop("Sentinel stack not found: ", sen_path)

sen.dat <- terra::rast(sen_path)
sen.dat <- terra::project(sen.dat,'epsg:4326')
nm <- names(sen.dat)



##### REMOVE CLOUD IMAGES - Future task to automate this!!
# prefer 8-digit yyyymmdd anywhere in the name; fallback to yyyy-mm-dd
dates_8   <- stringr::str_extract(nm, "(?<!\\d)\\d{8}(?!\\d)")
dates_hy  <- stringr::str_extract(nm, "\\d{4}-\\d{2}-\\d{2}")
dates_chr <- ifelse(!is.na(dates_8), dates_8, gsub("-", "", dates_hy))
ndvi_list_dates <- as.Date(dates_chr, format = "%Y%m%d")

if (any(is.na(dates_chr))) {
  stop("Could not parse dates for layers: ", paste(nm[is.na(dates_chr)], collapse = ", "))
}

img_dates_sen <- as.Date(dates_chr, format = "%Y%m%d")

# ---- 2) Make order deterministic: sort by date (oldest â newest) ----
o <- order(img_dates_sen)
sen.dat        <- sen.dat[[o]]
img_dates_sen  <- img_dates_sen[o]

# Give layers clean, informative names
names(sen.dat) <- format(img_dates_sen, "%Y-%m-%d")



bad_dates_year <- bad_dates_list$Dates 
if (is.null(bad_dates_year)) bad_dates_year <- character(0)  
# Accept both "YYYY-mm-dd" and "YYYYmmdd"
bad_dates_year <- as.Date(bad_dates_year, tryFormats = c("%Y-%m-%d", "%Y%m%d"))
drop_idx <- which(img_dates_sen %in% bad_dates_year)  

if (length(drop_idx)) {
  message("Dropping ", length(drop_idx), " Sentinel layers by date: ",
          paste(format(img_dates_sen[drop_idx], "%Y-%m-%d"), collapse = ", "))
  sen.dat       <- sen.dat[[-drop_idx]]
  img_dates_sen <- img_dates_sen[-drop_idx]
  names(sen.dat) <- format(img_dates_sen, "%Y-%m-%d")
}  

## I am having trouble writing this new raster 
sen.dat

##---------------------------------###
# # #Note: Crystal Brook straddles 2x tiles, but the difference is not noticeable
# # #We can simply just drop one of the tile sets.
# nm <- names(sen.dat)
# 
# hqd_idx <- grep("HQD", nm, perl = TRUE)
# # If your names always have underscores around the tile, this also works:
#  hqd_idx <- grep("_T53HQD(_|$)", nm)
# 
# if (length(hqd_idx)) {
#   message("Dropping ", length(hqd_idx), " layers from tile T53HQD")
#   sen.dat <- sen.dat[[-hqd_idx]]
# } else {
#   message("No T53HQD layers present; nothing to drop.")
# }

##---------------------------------###
# This might also work

# Keep only the first occurrence of each unique name
sen.dat <- sen.dat[[!duplicated(nm)]]

#### how many dates did we retain? #recreate the steps above
nm <- names(sen.dat)
dates_8   <- stringr::str_extract(nm, "(?<!\\d)\\d{8}(?!\\d)")
dates_hy  <- stringr::str_extract(nm, "\\d{4}-\\d{2}-\\d{2}")
dates_chr <- ifelse(!is.na(dates_8), dates_8, gsub("-", "", dates_hy))
ndvi_list_dates <- as.Date(dates_chr, format = "%Y%m%d")
img_dates_sen <-   as.Date(dates_chr, format = "%Y%m%d")


# --- Read NDVI stack & dates ---

pl <- list(ndvi = sen.dat, dates = as.Date(img_dates_sen, format = "%Y%m%d"))

ndvi <- pl$ndvi
img_dates <- pl$dates

dap <- as.numeric(img_dates - plant_date)

names(ndvi) <- as.character(img_dates)


out_rds_sentiel      <- file.path(saveDir_year, "ndvi_stack_2025.rds")

saveRDS(ndvi, out_rds_sentiel)






#