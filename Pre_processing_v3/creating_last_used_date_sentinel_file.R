library(dplyr)
library(readr)
library(stringr)
library(purrr)


folder <- "H:/Output-1/shiny_app_data/temp_for_jackie"
files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
files   # check this prints all 8 files you expect



site_lookup <- tibble(
  file_prefix = c("WALMRS125", "BH",         "WYN_MER",       "WHA_WOO",
                  "WAL_GUM",   "CRY_RAN",    "WAH_BON",       "WYN_TAN"),
  site_name   = c("Walpeup MRS125", "Crystal Brook Brians House", "Wynarka Mervs West",
                  "Wharminda Woodys", "Walpeup Gums", "Crystal Brook Randals",
                  "Wharminda Bonanza", "Wynarka Tanks"),
  sowing_2026 = as.Date(c("2026-03-30", "2026-04-18", "2026-04-08", "2026-05-08",
                          "2025-05-01", "2026-05-09", "2026-05-13", "2026-05-06"))
)

site_lookup

get_prefix <- function(fname) {
  # strips the trailing _dd_mm_yyyy.csv date stamp, leaving the site prefix
  str_remove(basename(fname), "_\\d{2}_\\d{2}_\\d{4}\\.csv$")
}

# quick check
tibble(file = basename(files), prefix = map_chr(files, get_prefix))


combined <- files %>%
  set_names(get_prefix) %>%
  imap_dfr(~ read_csv(.x, show_col_types = FALSE) %>%
             mutate(file_prefix = .y, source_file = basename(.x))) %>%
  filter(`SC CLOUDY PROB PERCENT` <= 30) %>%
  left_join(site_lookup, by = "file_prefix") %>%
  mutate(date_downloaded = as.Date("2026-08-21"))

# quick check
combined %>% count(file_prefix, site_name)





combined_out <- combined %>%
  mutate(used_data = `SC CLOUDY PROB PERCENT` <= 30) %>%
  select(site_name, sowing_2026, TIME, `search date`,
         `SC CLOUDY PROB PERCENT`, used_data, date_downloaded, source_file)


last_used_date <- combined_out %>%
  filter(used_data == TRUE) %>%
  group_by(site_name) %>%
  summarise(last_date_used = max(TIME, na.rm = TRUE)) %>%
  ungroup()

last_used_date


write_csv(combined_out, file.path(folder, "combined_sentinel_dates_21_08_2026.csv"))
write_csv(last_used_date, file.path(folder, "last_used_date_sentinel_21_08_2026.csv"))


nrow(combined_out)