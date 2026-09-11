# 12. Ethnic group by country of birth
# Input:  01_eth_by_cob_2011_raw, 01_eth_by_cob_2021_raw, 01_lookup_msoa_change, 01_lookup_hierarchy_2021, 04_panel_eth
# Output: 12_panel_generation, 12_generation_sparsity

# DC2205EW (2011) is published at MSOA and RM010 (2021) at LSOA, both are placed on harmonised MSOA units

source("00_environment_setup.r")

eth_by_cob_2011_raw <- read_derived("01_eth_by_cob_2011_raw")
eth_by_cob_2021_raw <- read_derived("01_eth_by_cob_2021_raw")
lookup_msoa_change <- read_derived("01_lookup_msoa_change")
lookup_hierarchy_2021 <- read_derived("01_lookup_hierarchy_2021")
panel_eth <- read_derived("04_panel_eth")

# Harmonised MSOA units

msoa_lookup <- harmonise_units(lookup_msoa_change, "msoa11cd", "msoa21cd", "M%05d") %>%
  rename(msoa = unit, msoa_code = area_code)

report("Harmonised MSOA units", tibble(units = n_distinct(msoa_lookup$msoa),
                                       msoa_2011 = sum(msoa_lookup$year == 2011L),
                                       msoa_2021 = sum(msoa_lookup$year == 2021L)))

stopifnot("a harmonised MSOA unit is missing from one census year" = all(count(distinct(msoa_lookup, msoa, year), msoa)$n == 2),
          "the units do not cover every 2011 MSOA" = sum(msoa_lookup$year == 2011L) == msoa_published[["2011"]],
          "the units do not cover every 2021 MSOA" = sum(msoa_lookup$year == 2021L) == msoa_published[["2021"]])

# 2011, DC2205EW

# DC2205EW labels ethnic groups differently from KS201EW
dc2205_groups <- tribble(
  ~eth_category, ~group,

  "White: English/Welsh/Scottish/Northern Irish/British", "White British",
  "White: Irish", "White Irish",
  "White: Gypsy or Irish Traveller", "Gypsy, Traveller or Roma",
  "White: Other White", "Other White",

  "Mixed/multiple ethnic group: White and Black Caribbean", "Mixed White and Black Caribbean",
  "Mixed/multiple ethnic group: White and Black African", "Mixed White and Black African",
  "Mixed/multiple ethnic group: White and Asian", "Mixed White and Asian",
  "Mixed/multiple ethnic group: Other Mixed", "Other Mixed",

  "Asian/Asian British: Indian", "Indian",
  "Asian/Asian British: Pakistani", "Pakistani",
  "Asian/Asian British: Bangladeshi", "Bangladeshi",
  "Asian/Asian British: Chinese", "Chinese",
  "Asian/Asian British: Other Asian", "Other Asian",

  "Black/African/Caribbean/Black British: African", "African",
  "Black/African/Caribbean/Black British: Caribbean", "Caribbean",
  "Black/African/Caribbean/Black British: Other Black", "Other Black",

  "Other ethnic group: Arab", "Arab",
  "Other ethnic group: Any other ethnic group", "Any other ethnic group"
)

# the continental totals partition the table and is the sum of group total
continent_totals <- c("Europe: Total", "Africa: Total", "Middle East and Asia: Total", "The Americas and the Caribbean: Total",
                      "Antarctica and Oceania (including Australasia)", "Other")

generation_2011 <- eth_by_cob_2011_raw %>%
  inner_join(dc2205_groups, by = "eth_category") %>%
  filter(category %in% c(continent_totals, "Europe: United Kingdom: Total")) %>%
  mutate(part = if_else(category == "Europe: United Kingdom: Total", "uk_born", "total")) %>%
  group_by(msoa_code = area_code, group, part) %>%
  summarise(people = as.numeric(sum(people)), .groups = "drop") %>%
  pivot_wider(names_from = part, values_from = people, values_fill = 0) %>%
  mutate(year = 2011L)

# 2021, RM010 aggregated from LSOA to MSOA

generation_2021 <- eth_by_cob_2021_raw %>%
  inner_join(distinct(lookup_hierarchy_2021, area_code = lsoa21cd, msoa21cd), by = "area_code") %>%
  mutate(uk = str_detect(cob_category, regex("United Kingdom", ignore_case = TRUE))) %>%
  group_by(msoa_code = msoa21cd, group = eth_category) %>%
  summarise(uk_born = as.numeric(sum(people[uk])), total = as.numeric(sum(people)), .groups = "drop") %>%
  mutate(year = 2021L)

report("Ethnic categories in RM010", distinct(generation_2021, group), rows = 20)

# Combined panel

panel_generation <- bind_rows(generation_2011, generation_2021) %>%
  inner_join(msoa_lookup, by = c("year", "msoa_code")) %>%
  group_by(year, msoa, group) %>%
  summarise(total = sum(total), uk_born = sum(uk_born), .groups = "drop") %>%
  mutate(foreign_born = pmax(total - uk_born, 0))

generation_sparsity <- panel_generation %>%
  group_by(year, group) %>%
  summarise(units = n(),
            units_no_uk_born = sum(uk_born == 0),
            units_no_foreign_born = sum(foreign_born == 0),
            foreign_share = round(100 * sum(foreign_born) / sum(total), 1),
            .groups = "drop")

report("Units with one generation empty, by group", generation_sparsity, rows = 40)

# Checks

coverage <- panel_generation %>%
  group_by(year) %>%
  summarise(joint_total = sum(total), .groups = "drop") %>%
  left_join(summarise(panel_eth, marginal_total = sum(people), .by = year), by = "year") %>%
  mutate(difference = joint_total - marginal_total)

group_coverage <- panel_generation %>%
  group_by(year, group) %>%
  summarise(joint = sum(total), .groups = "drop") %>%
  left_join(summarise(panel_eth, marginal = sum(people), .by = c(year, group)) %>% mutate(group = as.character(group)),
            by = c("year", "group")) %>%
  mutate(difference = joint - marginal)

report("Joint table against the ethnic group table", coverage)

stopifnot("a generational count is negative" = all(panel_generation$uk_born >= 0),
          "UK born exceeds the group total beyond perturbation" = all(panel_generation$uk_born <= panel_generation$total + perturbation_tolerance),
          "the joint and marginal totals differ beyond disclosure control" = all(abs(coverage$difference) < coverage_gap_max),
          "a group total differs from the ethnic group table beyond disclosure control" =all(abs(group_coverage$difference) < coverage_gap_max, na.rm = TRUE))

# Save

save_derived(panel_generation, "12_panel_generation")
save_derived(generation_sparsity, "12_generation_sparsity")

report("Script 12 complete")
