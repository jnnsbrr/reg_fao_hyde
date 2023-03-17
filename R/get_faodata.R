library(FAOSTAT)


project_dir <- "./"

# create regionalisation project directories
data_dir <- paste(project_dir, "data/raw/fao", sep = "/")
dir.create(data_dir, recursive = TRUE)

# food balances
food_balances_past_dir <- paste(data_dir, "food_balances_past-2013", sep = "/")
dir.create(food_balances_past_dir)

food_balances_presence_dir <- paste(data_dir,
                                    "food_balances_2014-presence",
                                    sep = "/")
dir.create(food_balances_presence_dir)

# land use
landuse_dir <- paste(data_dir, "landuse", sep = "/")
dir.create(landuse_dir)

# emissions
emissions_dir <- paste(data_dir, "emissions", sep = "/")
dir.create(emissions_dir)

# fertilizer
fertilizer_dir <- paste(data_dir, "fertilizer", sep = "/")
dir.create(fertilizer_dir)

# manure
manure_dir <- paste(data_dir, "manure", sep = "/")
dir.create(manure_dir)

# ---------------------------------------------------------------------------- #
# 1. food balances -> meat demand & yield data ####
# ---------------------------------------------------------------------------- #
fao_metadata <- FAOsearch(dataset = "food balance", full = FALSE)

# past-2013 data set: FBSH
fao_metadata[fao_metadata$datasetcode == "FBSH", ]
# 2014-presence data set: FBS
fao_metadata[fao_metadata$datasetcode == "FBS", ]


food_balances_past <- get_faostat_bulk(
    code = "FBSH", data_folder = food_balances_past_dir)

zip_file <- list.files(food_balances_past_dir,  pattern = ".zip")

unzip(paste(food_balances_past_dir, zip_file, sep = "/"),
      exdir = food_balances_past_dir)

food_balances_presence <- get_faostat_bulk(
    code = "FBS", data_folder = food_balances_presence_dir)

zip_file <- list.files(food_balances_presence_dir,  pattern = ".zip")

unzip(paste(food_balances_presence_dir, zip_file, sep = "/"),
      exdir = food_balances_presence_dir)


# ---------------------------------------------------------------------------- #
# 2. land use -> crop and grazing land ####
# ---------------------------------------------------------------------------- #

fao_metadata <- FAOsearch(dataset = "land use", full = FALSE)
# land use data set: RL
fao_metadata[fao_metadata$datasetcode == "RL", ]

landuse <- get_faostat_bulk(
    code = "RL", data_folder = landuse_dir)

zip_file <- list.files(landuse_dir,  pattern = ".zip")

unzip(paste(landuse_dir, zip_file, sep = "/"), exdir = landuse_dir)


# ---------------------------------------------------------------------------- #
# 3. emissions total -> emission data ####
# ---------------------------------------------------------------------------- #

fao_metadata <- FAOsearch(dataset = "emissions", full = FALSE)
# emissions data: GT
fao_metadata[fao_metadata$datasetcode == "GT", ]

emissions <- get_faostat_bulk(
    code = "GT", data_folder = emissions_dir)

zip_file <- list.files(emissions_dir,  pattern = ".zip")

unzip(paste(emissions_dir, zip_file, sep = "/"), exdir = emissions_dir)


# ---------------------------------------------------------------------------- #
# 4. fertilizer by nutrient -> Fertilizer use ####
# ---------------------------------------------------------------------------- #

fao_metadata <- FAOsearch(dataset = "fertilizer", full = FALSE)
# fertilizer data: RFN
fao_metadata[fao_metadata$datasetcode == "RFN", ]

fertilizer <- get_faostat_bulk(
    code = "RFN", data_folder = fertilizer_dir)

zip_file <- list.files(fertilizer_dir,  pattern = ".zip")

unzip(paste(fertilizer_dir, zip_file, sep = "/"), exdir = fertilizer_dir)

# ---------------------------------------------------------------------------- #
# 5. livestock Manure -> Manure use ####
# ---------------------------------------------------------------------------- #

fao_metadata <- FAOsearch(dataset = "manure", full = FALSE)
# fertilizer data: RFN
fao_metadata[fao_metadata$datasetcode == "EMN", ]

manure <- get_faostat_bulk(
    code = "EMN", data_folder = manure_dir)

zip_file <- list.files(manure_dir,  pattern = ".zip")

unzip(paste(manure_dir, zip_file, sep = "/"), exdir = manure_dir)
