library(tidyverse)
library(readxl)
library(writexl)

# ---------------------------------------------------------------------------- #
# setup ####
# ---------------------------------------------------------------------------- #

project_dir <- "./"

# create regionalisation project directories
data_dir <- paste(project_dir, "data/raw/fao", sep = "/")
write_data_dir <- paste(project_dir, "data/processed/fao/Christophe", sep = "/")
dir.create(write_data_dir, recursive = TRUE)


# food balances
food_balances_past_dir <- paste(data_dir, "food_balances_past-2013", sep = "/")
food_balances_past_file <- list.files(path = food_balances_past_dir,
                                      pattern = "ormalized).csv",
                                      full.names = TRUE)

food_balances_presence_dir <- paste(data_dir,
                                    "food_balances_2014-presence",
                                    sep = "/")
food_balances_presence_file <- list.files(path = food_balances_presence_dir,
                                          pattern = "ormalized).csv",
                                          full.names = TRUE)
# land use
landuse_dir <- paste(data_dir, "landuse", sep = "/")
landuse_file <- list.files(path = landuse_dir,
                           pattern = "ormalized).csv",
                           full.names = TRUE)
# emissions
emissions_dir <- paste(data_dir, "emissions", sep = "/")
emissions_file <- list.files(path = emissions_dir,
                              pattern = "ormalized).csv",
                              full.names = TRUE)
# fertilizer
fertilizer_dir <- paste(data_dir, "fertilizer", sep = "/")
fertilizer_file <- list.files(path = fertilizer_dir,
                              pattern = "ormalized).csv",
                              full.names = TRUE)
# manure
manure_dir <- paste(data_dir, "manure", sep = "/")
manure_file <- list.files(path = manure_dir,
                           pattern = "ormalized).csv",
                           full.names = TRUE)

# regions data
regions <- read_xlsx(paste(project_dir,
                            "regions_fao_numbers.xlsx",
                            sep = "/"))

# ---------------------------------------------------------------------------- #
# 1. food balances -> meat demand & yield data (prodcution, food,
#   human consumption) ####
# ---------------------------------------------------------------------------- #
meat_items <- c("Mutton & Goat Meat",
                "Poultry Meat",
                "Pigmeat",
                "Bovine Meat")

crop_items <- c("Cereals - Excluding Beer",
                "Starchy Roots",
                "Sugar & Sweeteners",
                "Pulses",
                "Oilcrops",
                "Vegetables",
                "Fruits - Excluding Wine")

# past data (<=2013)
# locale encoding ASCII work around not perfect but solution for côte d'ivoire
food_balances_past <- readr::read_csv(food_balances_past_file)
# directly filter for unneeded data
food_consum_past <- food_balances_past %>%
  filter(Item %in% c("Population", meat_items, crop_items)) %>%
  filter(Element %in% c("Total Population - Both sexes",
                        "Food",
                        "Production"))

# present data (>2013)
food_balances_presence <- readr::read_csv(food_balances_presence_file)
food_consum_presence <- food_balances_presence %>%
  filter(Item %in% c("Population", meat_items, crop_items)) %>%
  filter(Element %in% c("Total Population - Both sexes",
                        "Food",
                        "Production"))

# join past and presence data (removing overlap period 2010-13)
country_consum <- food_consum_presence %>%
  full_join(food_consum_past %>% filter(Year < 2010))

# deal with special 90s cases
country_consum$Area[
  which(country_consum$Area %in% c("Czechia", "Czechoslovakia"))
] <- "Czech Republic"

# filter Christophe's countries of interest
relevent_area_codes <- c(2,3,4,7,9,1,10,11,52,13,16,14,57,255,23,18,19,80,20,21,26,27,233,29,115,32,33,37,39,40,41,44,250,46,48,107,98,49,50,51,54,72,56,58,59,60,63,209,238,66,67,68,74,75,73,79,81,84,89,90,91,93,95,97,99,100,101,102,103,104,105,106,109,110,112,108,114,117,118,113,120,119,121,122,123,126,256,129,130,131,132,133,134,136,137,138,146,141,273,143,144,28,147,149,150,156,157,158,159,154,162,221,165,166,168,169,170,171,173,174,179,183,185,184,193,194,195,272,197,199,198,202,277,203,38,276,207,210,211,212,208,215,216,217,220,222,223,213,226,230,225,229,231,234,235,236,237,249,251,181,5000, 53)
country_consum <- country_consum %>% filter(`Area Code` %in% relevent_area_codes | Area == "Czech Republic")

# process with regions and spread sheet for better calculations
country_consum_agg <- country_consum %>%
  filter(Element %in% c("Total Population - Both sexes",
                        "Food")) %>%
  group_by(Area, Item, Year) %>%
  summarise(Value = sum(Value), .groups = "drop") %>%
  spread(Item, Value) %>%
  mutate(`Red meat` = rowSums(.[c("Mutton & Goat Meat", "Bovine Meat")],
                                na.rm = TRUE),
         `White meat` = rowSums(.[c("Poultry Meat", "Pigmeat")], na.rm = TRUE),
         `All crops` = rowSums(.[crop_items], na.rm = TRUE)) %>%
  select(-any_of(c(meat_items, crop_items)))



# human consumptions requires to be divided by population
human_consumption <- country_consum_agg %>%
  # 1e6 1000 t -> kg, 1e3 Persons -> 1 person => .x * 1e3
  mutate(across(c("Red meat", "White meat", "All crops"),
                ~ (.x * 1e3) / Population)) %>%
  select(-Population) %>%
  gather("Item", "Value", c("Red meat", "White meat", "All crops")) %>%
  mutate(Element = "Human Consumption", Unit = "kg/person")

# food sums need to be multiplied to get Mt, join human consumptions and food
#   afterwards
food_consum <- country_consum_agg %>%
  select(-Population) %>%
  gather("Item", "Value", c("Red meat", "White meat", "All crops")) %>%
  mutate(Element = "Food",
         # 1000 tonnes -> Mt
         Value = Value * 1e-3,
         Unit = "Mt") %>%
  full_join(human_consumption)

production <- country_consum %>%
  filter(Element %in% c("Production")) %>%
  group_by(Area, Item, Year) %>%
  summarise(Value = sum(Value), .groups = "drop") %>%
  spread(Item, Value) %>%
  mutate(`Red meat` = rowSums(.[c("Mutton & Goat Meat", "Bovine Meat")],
                                na.rm = TRUE),
         `White meat` = rowSums(.[c("Poultry Meat", "Pigmeat")],
                                     na.rm = TRUE),
         `All crops` = rowSums(.[crop_items], na.rm = TRUE)) %>%
  select(-any_of(c(meat_items, crop_items))) %>%
  gather("Item", "Value", c("Red meat",
                            "White meat",
                            "All crops")) %>%
  mutate(Element = "Production",
         # 1000 tonnes -> Mt
         Value = Value * 1e-3,
         Unit = "Mt")

prod_food_consum <- food_consum %>%
  full_join(production) %>%
  mutate(vars = paste(Item, ", ", Element, " (", Unit, ")", sep = "")) %>% 
  select(Area, Year, Value, vars) %>% 
  filter(Year >= 2000 & Year <= 2020) %>% 
  spread(vars, Value)

# write excel file (requires writexl package)
write.csv(prod_food_consum,
           file = paste(write_data_dir,
                        "fao_whitemeat_redmeat_allcrops_christophe.csv",
                        sep = "/"))


# ---------------------------------------------------------------------------- #
# 2. land use -> crop and grazing land ####
# ---------------------------------------------------------------------------- #

all_landuse <- readr::read_csv(landuse_file) %>%
  filter(Item %in% c("Cropland",
                     "Land under perm. meadows and pastures"))

# deal with special 90s cases
all_landuse$Area[
  which(all_landuse$Area %in% c("Czechia", "Czechoslovakia"))
] <- "Czech Republic"

# filter Christophe's countries of interest
all_landuse <- all_landuse %>% filter(`Area Code` %in% relevent_area_codes | Area == "Czech Republic")

# label as Grazing land
all_landuse$Item[
  which(all_landuse$Item == "Land under perm. meadows and pastures")
] <- "Grazing land"

landuse <- all_landuse %>%
  group_by(Area, Item, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Value = Value * 1e-3,
         Unit = "Mha") %>%
  mutate(vars = paste(Item, " (", Unit, ")", sep = "")) %>% 
  select(Area, Year, Value, vars) %>% 
  filter(Year >= 2000 & Year <= 2020) %>% 
  spread(vars, Value)

write.csv(landuse,
          file = paste(write_data_dir,
                       "fao_cropland_grassland_christophe.csv",
                       sep = "/"))

# ---------------------------------------------------------------------------- #
# 3 emissions total -> emission data ####
# ---------------------------------------------------------------------------- #

ch4_items <- c("Enteric Fermentation",
               "Manure Management",
               "Rice Cultivation",
               "Burning - Crop residues")
ch4_elements <- c("Emissions (CH4)")

n2o_manure_items <- c("Manure Management",
                      "Manure applied to Soils",
                      "Manure left on Pasture",
                      "Crop Residues",
                      "Burning - Crop residues",
                      "Drained organic soils (N2O)",
                      "Savanna fires",
                      "Fires in humid tropical forests",
                      "Forest fires",
                      "On-farm energy use")

n2o_fert_items <- c("Synthetic Fertilizers")

n2o_elements <- c("Direct emissions (N2O)",
                  "Indirect emissions (N2O)")

# directly filter for unneeded data
all_emissions <- readr::read_csv(emissions_file) %>%
  filter((Item %in% ch4_items & Element %in% ch4_elements) |
         (Item %in% c(n2o_manure_items, n2o_fert_items) &
          Element %in% n2o_elements))

# deal with special 90s cases
all_emissions$Area[
  which(all_emissions$Area %in% c("Czechia", "Czechoslovakia"))
] <- "Czech Republic"

# filter Christophe's countries of interest
all_emissions <- all_emissions %>% filter(`Area Code` %in% relevent_area_codes | Area == "Czech Republic")

# process with regions and spread sheet for better calculations
regional_emissions <- all_emissions %>%
  group_by(Area, Item, Element, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Value = Value * 1e-3,
         Unit = "Mt")

# not all n2o_manure_items are occuring in combination with n2o_elements
#   therefore filter only both conditions are met
actual_items <- n2o_manure_items[
  n2o_manure_items %in% regional_emissions$Item[
    regional_emissions$Element %in% n2o_elements
  ]
]

# filter and calc ch4 emissions
ch4_emissions <- regional_emissions %>%
  filter(Item %in% ch4_items & Element %in% ch4_elements) %>%
  spread(Item, Value) %>%
  mutate(`CH4 agri emi` = rowSums(.[ch4_items], na.rm = TRUE)) %>%
  select(-all_of(ch4_items)) %>%
  select(-Element) %>% 
  gather("Item", "Value", `CH4 agri emi`)

# filter and calc no2 emissions (both for fertilizer and manure)
n2o_emissions <- regional_emissions %>%
  filter(Item %in% c(n2o_manure_items, n2o_fert_items) &
           Element %in% n2o_elements) %>%
  group_by(Area, Item, Year, Unit) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  spread(Item, Value) %>%
  mutate(`N2O syn fert` = rowSums(.[n2o_fert_items], na.rm = TRUE),
         `N2O emi manure` = rowSums(.[actual_items], na.rm = TRUE)) %>%
  select(-all_of(n2o_fert_items)) %>%
  select(-all_of(actual_items)) %>% 
  gather("Item", "Value", c("N2O syn fert", "N2O emi manure"))

# join all emissions data
emissions <- ch4_emissions %>%
  full_join(n2o_emissions) %>%
  mutate(vars = paste(Item, " (", Unit, ")", sep = "")) %>% 
  select(Area, Year, Value, vars) %>% 
  filter(Year >= 2000 & Year <= 2020) %>% 
  spread(vars, Value)

write.csv(emissions,
          file = paste(write_data_dir,
                       "fao_emissions_christophe.csv",
                       sep = "/"))

# ---------------------------------------------------------------------------- #
# 4. N: fertilizer & manure  by nutrient -> Fertilizer & Manure use ####
# ---------------------------------------------------------------------------- #

all_fertilizer <- readr::read_csv(fertilizer_file) %>%
  filter(Item == "Nutrient nitrogen N (total)" & Element == "Agricultural Use")

# deal with special 90s cases
all_fertilizer$Area[
  which(all_fertilizer$Area %in% c("Czechia", "Czechoslovakia"))
] <- "Czech Republic"

# filter Christophe's countries of interest
all_fertilizer <- all_fertilizer %>% filter(`Area Code` %in% relevent_area_codes | Area == "Czech Republic")

all_fertilizer$Item[
  which(all_fertilizer$Item == "Nutrient nitrogen N (total)")
] <- "Nitrogen use"


fertilizer <- all_fertilizer %>%
  group_by(Area, Item, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Value = Value * 1e-6,
         Unit = "Mt")

red_meat_items <- c("Camels",
                    "Cattle, dairy",
                    "Cattle, non-dairy",
                    "Goats",
                    "Sheep",
                    "Llamas")

white_meat_items <- c("Chickens, broilers",
                      "Chickens, layers",
                      "Ducks",
                      "Swine, breeding",
                      "Swine, market",
                      "Turkeys")

other_meat_items <- c("Horses",
                      "Asses")

all_manure <- readr::read_csv(manure_file) %>%
  filter(Item %in% c(red_meat_items, white_meat_items, other_meat_items) &
         Element == "Amount excreted in manure (N content)")

# deal with special 90s cases
all_manure$Area[
  which(all_manure$Area %in% c("Czechia", "Czechoslovakia"))
] <- "Czech Republic"

# filter Christophe's countries of interest
all_manure <- all_manure %>% filter(`Area Code` %in% relevent_area_codes | Area == "Czech Republic")

# process with regions and spread sheet for better calculations
manure <- all_manure %>%
  group_by(Area, Item, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Value = Value * 1e-9,
         Unit = "Mt") %>%
  spread(Item, Value) %>%
  mutate(`Excreted in manure (N content) Red meat` = rowSums(.[red_meat_items], na.rm = TRUE),
         `Excreted in manure (N content) White meat` = rowSums(.[white_meat_items], na.rm = TRUE),
         `Excreted in manure (N content) Other meat` = rowSums(.[other_meat_items], na.rm = TRUE),
         `All manure excretion` = rowSums(.[c(red_meat_items, white_meat_items, other_meat_items)], na.rm = TRUE)) %>%
  select(-all_of(red_meat_items)) %>%
  select(-all_of(white_meat_items)) %>%
  select(-all_of(other_meat_items)) %>%
  gather("Item", "Value", c("Excreted in manure (N content) Red meat",
                            "Excreted in manure (N content) White meat",
                            "Excreted in manure (N content) Other meat",
                            "All manure excretion"))

fertilizer_manure <- manure  %>%
  full_join(fertilizer) %>%
  mutate(vars = paste(Item, " (", Unit, ")", sep = "")) %>% 
  select(Area, Year, Value, vars) %>% 
  filter(Year >= 2000 & Year <= 2020) %>% 
  spread(vars, Value)

# write excel file (requires writexl package)
write.csv(fertilizer_manure,
          file = paste(write_data_dir,
                       "fao_N_fertilizer_manure_christophe.csv",
                       sep = "/"))

# ---------------------------------------------------------------------------- #
# 5. write all ####
# ---------------------------------------------------------------------------- #

all_together <- prod_food_consum %>%
  full_join(landuse) %>%
  full_join(emissions) %>%
  full_join(fertilizer_manure)

# create list of region sheets to save each region as sheet in excel file
regions_list <- list()
for (reg in all_regions) {
  regions_list[[reg]] <- filter(all_together, region == reg)
}

# write excel file (requires writexl package)
write_xlsx(regions_list,
           path = paste(write_data_dir,
                        "fao_all.xlsx",
                        sep = "/"))
