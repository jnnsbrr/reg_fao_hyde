# Regionalized Model Input Data

This repository contains the data and scripts necessary to generate input data
for a regionalized model. The input data is sourced from a variety of external
datasets, including FAOSTAT and HYDE.

1. `"get_faodata.R"`: downloads FAO data related to
food balances, land use, emissions, fertilizer, and manure from FAOSTAT
database.

2. `"get_hydedata.R"` downloads relevant data from HYDE database

3. `"process_faodata.R"` processes, and combines data from various sources related
to global agricultural production, land use, greenhouse gas emissions, and
fertilizer and manure use. The data is sourced from the FAOSTAT database and
is processed using the tidyverse package in R.It then combines this data and
calculates the net greenhouse gas emissions associated with agricultural
production. It also processes data on global fertilizer and manure use by
nutrient and livestock type. It combines this data and calculates the total
nitrogen fertilizer and manure use by region.
All of the processed data is then combined and write it to an Excel file with
separate sheets for each region.

4. `"process_faodata.R"` processes land use data from the HYDE database for
different regions and anthromes. The processed data is then saved as an Excel
file with each region as a separate sheet.

## Usage

1. Clone the repository
2. Set the project directory in the R script to the cloned repository
3. If not installed already, please install the following R packages:
    - `tidyverse`
    - `readxl`
    - `writexl`
    - `FAOSTAT`
4. Run the R scripts in the same order as above


## License

This project is licensed under the GNU General Public License v3.0. See the
LICENSE file for more details.

## Contact

If you have any questions or suggestions, please contact the author at
jannesbr@pik-potsdam.de
