# Dissertation
Ethnic Group, Country of Birth and Neighbourhood Settlement, 2011–2021: 

A reproducible 16 script workflow analysing ethnic group and country of birth across the 2011 and 2021 Censuses of England and Wales

The pipeline covers census harmonisation, separation indices, size-conditional inference,
Shapley decomposition, local spatial clustering, and neighbourhood classification,
distinguishing recent from longer established minority settlement.

The classification is independently validated against year of arrival, which is excluded from its construction.

## Running the analysis

1. Open `dissertation.Rproj` so that the project is set as the working directory.
2. Run `renv::restore()` to install the package versions specified in `renv.lock` (R 4.5.1).
3. Run `Rscript run_all.r`, or execute scripts `00`–`16` in order. Each script sources `00_environemnt_sourve` and reads only outputs saved by earlier scripts.
4. (So scripts 1-16 are sequential)
5. A full run typically takes 30 minutes. Most of the runtime is concentrated in scripts `05`, `06`, and `09`.

## Data Download Instructions 

Due to the size of some of the datasets, the raw data files are **not included in this GitHub repository**. To reproduce the analysis, download the required files from the sources listed below and place them in:

```text
data/raw/
```

The filenames must match those specified below exactly.

### Census data from Nomis

The following Census tables should be downloaded from [Nomis](https://www.nomisweb.co.uk):

* **QS203EW** – Country of birth (detailed), LSOA 2011
  `QS203EW_lsoa_2011.csv`

* **KS201EW** – Ethnic group, LSOA 2011
  `KS201EW_lsoa_2011.csv`

* **DC2205EW** – Country of birth by ethnic group by sex, MSOA 2011
  `DC2205EW_msoa_2011.csv`

* **TS004** – Country of birth, LSOA 2021
  `TS004_lsoa_2021.csv`

* **TS021** – Ethnic group, LSOA 2021
  `TS021_lsoa_2021.csv`

For each table, select the appropriate geography level and download the data as CSV. For the 2021 tables, the Nomis Census 2021 bulk download service can also be used.

### Census data from the ONS Custom Dataset Tool

The following datasets must be downloaded from the [ONS Census 2021 custom dataset tool](https://www.ons.gov.uk/datasets/create):

* **TS015** – Year of arrival in the UK (13 categories), LSOA 2021
  `TS015_lsoa_2021.csv`

* **RM010** – Country of birth by ethnic group, LSOA 2021
  `RM010_lsoa_2021.csv`

Select **Lower layer Super Output Areas** and **England and Wales** when creating the datasets.

### Geography and boundary files

The required geography files are available from the [ONS Open Geography Portal](https://geoportal.statistics.gov.uk):

* LSOA 2011 to LSOA 2021 to LAD 2022 lookup
  `lookup_lsoa11_lsoa21.csv`

* OA 2021 to LSOA to MSOA to LAD 2022 hierarchy lookup
  `lookup_lsoa21_hierachy.csv`

* OA 2011 to LSOA to MSOA to LAD 2011 hierarchy lookup
  `lookup_lsoa11_msoa11.csv`

* LAD 2022 to Region lookup
  `lookup_lad22_region22.csv`

* LSOA 2021 boundaries (generalised and clipped)
  `LSOA_2021_BGC.gpkg`

Search for the corresponding dataset title on the ONS Open Geography Portal and download the required CSV or GeoPackage format.

### Required files

After downloading the data, the `data/raw/` folder should contain:

```text
QS203EW_lsoa_2011.csv
KS201EW_lsoa_2011.csv
DC2205EW_msoa_2011.csv
TS004_lsoa_2021.csv
TS021_lsoa_2021.csv
TS015_lsoa_2021.csv
RM010_lsoa_2021.csv
lookup_lsoa11_lsoa21.csv
lookup_lsoa21_hierachy.csv
lookup_lsoa11_msoa11.csv
lookup_lad22_region22.csv
LSOA_2021_BGC.gpkg
```

**Note:** These files are not included in the GitHub repository because of their large file sizes. They must be downloaded separately before running the analysis.

Script `01_data_import.r` checks that the required files are present and that the relevant area counts match the published totals. The script will stop with an informative error if a required file is missing or a validation check fails.

### Using an existing data folder

If the required files are already available on your computer, you do not need to move or duplicate them. The `raw_path()` function in `00_environment_setup.r` can be edited to point to the existing `data/raw/` folder. No other scripts need to be changed. 



