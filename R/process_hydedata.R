library(tidyverse)
library(readxl)
library(writexl)

# ---------------------------------------------------------------------------- #
# setup ####
# ---------------------------------------------------------------------------- #

project_dir <- "./"

# create regionalisation project directories
data_dir <- paste(project_dir, "data/raw/hyde", sep = "/")
write_data_dir <- paste(project_dir, "data/processed/hyde", sep = "/")
dir.create(write_data_dir, recursive = TRUE)

antnumber <- c(11:12, 21:24, 31:34, 41:43, 51:54, 61:63, 70)
antname <- c("11 Urban",
             "12 Dense settlements",
             "21 Village, Rice",
             "22 Village, Irrigated",
             "23 Village, Rainfed",
             "24 Village, Pastoral",
             "31 Croplands, residential irrigated",
             "32 Croplands, residential rainfed",
             "33 Croplands, populated",
             "34 Croplands, pastoral",
             "41 Rangeland, residential",
             "42 Rangeland, populated",
             "43 Rangeland, remote",
             "51 Semi-natural woodlands, residential",
             "52 Semi-natural woodlands, populated",
             "53 Semi-natural woodlands, remote",
             "54 Semi-natural treeless and barren lands",
             "61 Wild, remote - woodlands",
             "62 Wild, remote - treeless & barren",
             "63 Wild, remote - ice",
             "70 No definition")
years <- c(1980:2017)

old_growth_forest <- as.character(c(61, 53))
forest_in_use <- as.character(c(51, 52))
urban <- as.character(c(11, 12))
barren <- as.character(c(54, 62, 63))

# ---------------------------------------------------------------------------- #
# data ####
# ---------------------------------------------------------------------------- #

# hyde regions data
regions <- read_xlsx(paste(project_dir,
                           "data/regions_hyde.xlsx",
                           sep = "/")) %>%
  .[which(!is.na(.$Region)), ]

# loop over anthromes and extract required regions & years
for (ant in seq_len(length(antnumber))) {
  anthrome <- read_delim(paste0(data_dir,
                                "/anthromes/txt/anthr_",
                                antnumber[ant],
                                "_c.txt")) %>%
    .[which(.$region %in% regions$`ISO-CODE`), ] %>%
    .[, c(1, which(names(.) %in% years))] %>%
    # define anthrome be later able to differentiate
    mutate(Anthrome = antnumber[ant])
  if (ant == 1) {
    ant_data <- anthrome
  } else {
    ant_data <- ant_data %>%
      full_join(anthrome)
  }
}

# get actual years for which data is provided (for selection)
actual_years <- years[years %in% names(ant_data)]

region_data <- ant_data %>%
  gather("Year", "Value", as.character(actual_years)) %>%
  mutate(iso_code = as.integer(region),
         Unit = "Mha",
         # convert km2 in Mha
         Value = Value * 1e-4) %>%
  select(-region) %>%
  left_join(regions, by = c("iso_code" = "ISO-CODE")) %>%
  group_by(Region, Anthrome, Unit, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  spread(Anthrome, Value) %>%
  mutate(`Old growth forest` = rowSums(.[old_growth_forest], na.rm = TRUE),
         `Forest in use` = rowSums(.[forest_in_use], na.rm = TRUE),
         `Urban` = rowSums(.[urban], na.rm = TRUE),
         `Barren` = rowSums(.[barren], na.rm = TRUE)) %>%
  select(-as.character(antnumber)) %>%
  gather("Item", "Value", c("Old growth forest",
                            "Forest in use",
                            "Urban",
                            "Barren")) %>%
  spread(Year, Value)


# get availalbe regions
all_regions <- sort(unique(region_data$Region))

# create list of region sheets to save each region as sheet in excel file
regions_list <- list()
for (reg in all_regions) {
  regions_list[[reg]] <- filter(region_data, Region == reg)
}

# write excel file (requires writexl package)
write_xlsx(regions_list,
           path = paste(write_data_dir,
                        "hyde_landuse.xlsx",
                        sep = "/"))