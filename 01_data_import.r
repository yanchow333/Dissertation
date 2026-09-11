# 1. Data import
# Input:  raw Census tables and ONS geography files listed in script 00
# Output: 01_cob_raw, 01_eth_raw, 01_arrival_2021_raw, 01_eth_by_cob_2011_raw, 01_eth_by_cob_2021_raw,
#         01_lookup_lsoa_change, 01_lookup_hierarchy_2021, 01_lookup_region_2022, 01_lookup_msoa_change, 01_lsoa_boundaries_2021.gpkg

source("00_environment_setup.r")

# Readers

# nomis exports open with a title block, so the header row is found by its geography label
header_row <- function(path) {
  position <- grep("super output area", readLines(path, n = 40, warn = FALSE), ignore.case = TRUE)[1]
  if (is.na(position)) stop("header row not found in ", basename(path), call. = FALSE)
  position
}

read_census_table <- function(key) {
  path <- raw_path(key)
  read_csv(path, skip = header_row(path) - 1)
}

# DC2205EW is exported as one block per ethnic group
read_nomis_stacked <- function(key, marker = "Ethnic Group:") {
  lines <- readLines(raw_path(key), warn = FALSE)
  strata_at <- grep(marker, lines, fixed = TRUE)
  header_at <- grep("super output area", lines, ignore.case = TRUE)
  stopifnot("block markers and header rows do not pair" = length(strata_at) == length(header_at))
  ends <- c(strata_at[-1] - 1L, length(lines))

  map_dfr(seq_along(header_at), function(i) {
    stratum <- scan(text = lines[strata_at[i]], what = "", sep = ",", quote = "\"", quiet = TRUE)[2]
    block <- lines[header_at[i]:ends[i]]
    block <- block[c(TRUE, grepl('^"?[EW][0-9]{8}', block[-1]))]
    read_csv(I(block), col_types = cols(.default = col_character()), name_repair = "minimal") %>%
      rename(area_label = 1) %>%
      mutate(area_code = str_extract(area_label, "^[EW][0-9]{8}"), stratum = str_squish(stratum)) %>%
      select(-area_label)
  })
}

read_lookup <- function(key) clean_names(read_csv(raw_path(key)))

# wide census table to one row per area, category and year
census_to_long <- function(census_table, year) {
  census_table %>%
    rename(area_raw = 1) %>%
    mutate(area_code = str_trim(str_extract(area_raw, "^[^:]+"))) %>%
    filter(str_detect(area_code, "^[EW]01[0-9]{6}$")) %>%
    select(-area_raw) %>%
    pivot_longer(-area_code, names_to = "category", values_to = "people",
                 values_transform = list(people = as.character)) %>%
    mutate(people = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", people))), year = year) %>%
    filter(!is.na(people))
}

stacked_to_long <- function(census_table, year) {
  census_table %>%
    pivot_longer(-c(area_code, stratum), names_to = "category", values_to = "people") %>%
    mutate(category = str_squish(category),
           people = suppressWarnings(as.integer(str_remove_all(people, ","))),
           year = year) %>%
    filter(!is.na(people))
}

# RM010 column labels vary, each column is matched once and must be unique
rm010_column <- function(columns, pattern, exclude_codes = TRUE) {
  hit <- keep(columns, ~ str_detect(.x, regex(pattern, ignore_case = TRUE)) && !(exclude_codes && str_detect(.x, "Code$")))
  if (length(hit) != 1) stop("expected one RM010 column matching '", pattern, "', found ", length(hit), call. = FALSE)
  hit
}

# Census tables

cob_raw <- bind_rows(read_census_table("cob_2011") %>% census_to_long(2011),
                     read_census_table("cob_2021") %>% census_to_long(2021))

eth_raw <- bind_rows(read_census_table("eth_2011") %>% census_to_long(2011),
                     read_census_table("eth_2021") %>% census_to_long(2021))

arrival_2021_raw <- read_census_table("arrival_2021") %>%
  clean_names() %>%
  transmute(area_code = lower_layer_super_output_areas_code,
            category = year_of_arrival_in_the_uk_13_categories,
            people = observation,
            year = 2021) %>%
  filter(str_detect(area_code, "^[EW]01[0-9]{6}$"))

eth_by_cob_2011_raw <- read_nomis_stacked("eth_by_cob_2011") %>%
  stacked_to_long(2011) %>%
  rename(eth_category = stratum)

rm010 <- read_csv(raw_path("eth_by_cob_2021"))

eth_by_cob_2021_raw <- rm010 %>%
  transmute(year = 2021L,
            area_code = .data[[rm010_column(names(rm010), "Areas Code$", exclude_codes = FALSE)]],
            eth_category = str_squish(.data[[rm010_column(names(rm010), "Ethnic group")]]),
            cob_category = str_squish(.data[[rm010_column(names(rm010), "Country of birth")]]),
            people = .data[[rm010_column(names(rm010), "^Observation$|^Count$")]]) %>%
  filter(!str_detect(eth_category, "^Does not apply"), !str_detect(cob_category, "^Does not apply"))

# Lookup Geographys

lookup_lsoa_change <- read_lookup("lsoa_change")
lookup_hierarchy_2021 <- read_lookup("hierarchy_2021")
lookup_region_2022 <- read_lookup("region_2022")
lookup_lsoa_msoa_2011 <- read_lookup("hierarchy_2011") %>% distinct(lsoa11cd, msoa11cd)

lsoa_boundaries_2021 <- st_read(raw_path("lsoa_boundaries_2021"), quiet = TRUE)

# The ONS MSOA lookup omits splits, so the exact correspondence is built via LSOAs
lookup_msoa_change <- lookup_lsoa_change %>%
  distinct(lsoa11cd, lsoa21cd) %>%
  inner_join(lookup_lsoa_msoa_2011, by = "lsoa11cd") %>%
  inner_join(distinct(lookup_hierarchy_2021, lsoa21cd, msoa21cd), by = "lsoa21cd") %>%
  distinct(msoa11cd, msoa21cd) %>%
  drop_na()

# Coverage

count_coverage <- function(raw_counts, label) {
  raw_counts %>%
    filter(str_detect(category, "^Total|^All categories|^All usual residents")) %>%
    group_by(year) %>%
    summarise(population = sum(people), areas = n_distinct(area_code), .groups = "drop") %>%
    mutate(variable = label, .before = year)
}

coverage <- bind_rows(count_coverage(cob_raw, "Country of birth"), count_coverage(eth_raw, "Ethnic group"))

population_gap <- coverage %>%
  group_by(year) %>%
  summarise(gap = diff(range(population)), percent = round(100 * gap / max(population), 5), .groups = "drop")

report("Population and area counts", coverage)
report("Population gap between the two classifications", population_gap)
report("MSOA correspondence", tibble(pairs = nrow(lookup_msoa_change),
                                     msoa_2011 = n_distinct(lookup_msoa_change$msoa11cd),
                                     msoa_2021 = n_distinct(lookup_msoa_change$msoa21cd)))

# Checks

stopifnot(
  "an area count does not match the published LSOA total" = all(coverage$areas == lsoa_published[as.character(coverage$year)]),
  "TS015 does not cover every 2021 LSOA" = n_distinct(arrival_2021_raw$area_code) == lsoa_published[["2021"]],
  "the boundary layer does not cover every 2021 LSOA" = nrow(lsoa_boundaries_2021) == lsoa_published[["2021"]],
  "birthplace and ethnicity populations differ by more than disclosure control explains" = max(population_gap$gap) <= coverage_gap_max,
  "RM010 parsed to zero rows" = nrow(eth_by_cob_2021_raw) > 0,
  "an RM010 row has no area code" = !anyNA(eth_by_cob_2021_raw$area_code),
  "an RM010 cell has no count" = !anyNA(eth_by_cob_2021_raw$people),
  "the 2011 LSOA to MSOA lookup misses 2011 LSOAs" = n_distinct(lookup_lsoa_msoa_2011$lsoa11cd) == lsoa_published[["2011"]],
  "the MSOA correspondence misses 2011 MSOAs" = n_distinct(lookup_msoa_change$msoa11cd) == msoa_published[["2011"]],
  "the MSOA correspondence misses 2021 MSOAs" = n_distinct(lookup_msoa_change$msoa21cd) == msoa_published[["2021"]]
)

# Save

save_derived(cob_raw, "01_cob_raw")
save_derived(eth_raw, "01_eth_raw")
save_derived(arrival_2021_raw, "01_arrival_2021_raw")
save_derived(eth_by_cob_2011_raw, "01_eth_by_cob_2011_raw")
save_derived(eth_by_cob_2021_raw, "01_eth_by_cob_2021_raw")
save_derived(lookup_lsoa_change, "01_lookup_lsoa_change")
save_derived(lookup_hierarchy_2021, "01_lookup_hierarchy_2021")
save_derived(lookup_region_2022, "01_lookup_region_2022")
save_derived(lookup_msoa_change, "01_lookup_msoa_change")
save_geometry(lsoa_boundaries_2021, "01_lsoa_boundaries_2021")

report("Script 1 complete")
