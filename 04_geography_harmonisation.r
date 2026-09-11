# 4. Geography harmonisation
# Input:  03_cob_grouped, 03_eth_grouped, 03_accession_grouped, 01_lookup_lsoa_change, 01_lookup_hierarchy_2021,
#         01_lookup_region_2022, 01_lsoa_boundaries_2021.gpkg
# Output: 04_zone_lookup, 04_zone_population_2021, 04_panel_cob, 04_panel_eth, 04_panel_accession,
#         04_boundary_change, 04_zone_construction, 04_zone_nesting, 04_zones.gpkg, 04_districts.gpkg, 04_outline.gpkg

# LSOAs that split or merged between 2011 and 2021 are combined into harmonised zones present in both censuses

source("00_environment_setup.r")

cob_grouped <- read_derived("03_cob_grouped")
eth_grouped <- read_derived("03_eth_grouped")
accession_grouped <- read_derived("03_accession_grouped")
lookup_lsoa_change <- read_derived("01_lookup_lsoa_change")
lookup_hierarchy_2021 <- read_derived("01_lookup_hierarchy_2021")
lookup_region_2022 <- read_derived("01_lookup_region_2022")
lsoa_boundaries_2021 <- read_geometry("01_lsoa_boundaries_2021")

# Harmonised zones

# ONS change indicator: U unchanged, S split, M merged, X irregular
boundary_change <- lookup_lsoa_change %>%
  count(chgind, name = "areas") %>%
  mutate(percent = round(100 * areas / sum(areas), 2)) %>%
  arrange(desc(areas))

zone_lookup <- harmonise_units(lookup_lsoa_change, "lsoa11cd", "lsoa21cd", "Z%06d") %>%
  rename(zone = unit)

zone_construction <- zone_lookup %>%
  count(zone, year) %>%
  pivot_wider(names_from = year, values_from = n, values_fill = 0, names_prefix = "lsoa_") %>%
  mutate(type = if_else(lsoa_2011 == 1 & lsoa_2021 == 1, "one to one", "aggregated")) %>%
  count(type, name = "zones") %>%
  mutate(percent = round(100 * zones / sum(zones), 2))

report("Boundary change between 2011 and 2021", boundary_change)
report(paste("Harmonised zones:", n_distinct(zone_lookup$zone)), zone_construction)

stopifnot("an area code appears twice in one census year" = !any(duplicated(zone_lookup[, c("year", "area_code")])),
          "a zone is missing from one census year" = all(count(distinct(zone_lookup, zone, year), zone)$n == 2),
          "2011 areas do not match the correspondence table" = sum(zone_lookup$year == 2011L) == n_distinct(lookup_lsoa_change$lsoa11cd),
          "2021 areas do not match the correspondence table" = sum(zone_lookup$year == 2021L) == n_distinct(lookup_lsoa_change$lsoa21cd))

# Higher geographies

#Join England only lookup with welsh authorities to form single England and Wales
lsoa_parents <- lookup_hierarchy_2021 %>%
  distinct(lsoa21cd, msoa21cd, lad22cd) %>%
  left_join(distinct(lookup_region_2022, lad22cd, region = rgn22nm), by = "lad22cd") %>%
  mutate(region = if_else(is.na(region) & str_starts(lad22cd, "W"), "Wales", region))

stopifnot("regions do not resolve to the ten ITL1 units of England and Wales" = n_distinct(lsoa_parents$region) == 10,
          "an authority has no region" = !anyNA(lsoa_parents$region))

# A zone crossing or (S) a boundary takes the parent that holds most of its LSOAs
modal <- function(values) names(sort(table(values), decreasing = TRUE))[1]

zone_parents <- zone_lookup %>%
  filter(year == 2021) %>%
  left_join(lsoa_parents, by = c("area_code" = "lsoa21cd"))

zone_geography <- zone_parents %>%
  group_by(zone) %>%
  summarise(msoa = modal(msoa21cd), lad = modal(lad22cd), region = modal(region), .groups = "drop")

zone_nesting <- zone_parents %>%
  group_by(zone) %>%
  summarise(n_msoa = n_distinct(msoa21cd), n_lad = n_distinct(lad22cd), .groups = "drop") %>%
  summarise(zones_crossing_msoa = sum(n_msoa > 1), zones_crossing_lad = sum(n_lad > 1))

report("Zones crossing an MSOA or authority boundary", zone_nesting)

# Panels

build_panel <- function(grouped_counts) {
  grouped_counts %>%
    inner_join(zone_lookup, by = c("year", "area_code")) %>%
    group_by(year, zone, group) %>%
    summarise(people = sum(people), .groups = "drop") %>%
    left_join(zone_geography, by = "zone")
}

panel_cob <- build_panel(cob_grouped)
panel_eth <- build_panel(eth_grouped)
panel_accession <- build_panel(accession_grouped)

zone_population_2021 <- panel_cob %>%
  filter(year == 2021) %>%
  group_by(zone) %>%
  summarise(population = sum(people), .groups = "drop") %>%
  left_join(zone_geography, by = "zone")

# Geometry

zones_sf <- lsoa_boundaries_2021 %>%
  inner_join(filter(zone_lookup, year == 2021), by = c("LSOA21CD" = "area_code")) %>%
  group_by(zone) %>%
  summarise(.groups = "drop")

# ONS district code and names retained for figures
districts_sf <- zones_sf %>%
  inner_join(select(zone_geography, zone, lad), by = "zone") %>%
  group_by(lad) %>%
  summarise(.groups = "drop") %>%
  left_join(distinct(lookup_hierarchy_2021, lad = lad22cd, district = lad22nm), by = "lad")

outline_sf <- zones_sf %>%
  st_geometry() %>%
  st_simplify(dTolerance = 250) %>%
  st_union() %>%
  st_sf(geometry = .)

stopifnot("geometry and panels disagree on the number of zones" = nrow(zones_sf) == n_distinct(zone_lookup$zone),
          "a district outline has no name" = !anyNA(districts_sf$district))

# Save

save_derived(zone_lookup, "04_zone_lookup")
save_derived(zone_population_2021, "04_zone_population_2021")
save_derived(panel_cob, "04_panel_cob")
save_derived(panel_eth, "04_panel_eth")
save_derived(panel_accession, "04_panel_accession")
save_derived(boundary_change, "04_boundary_change")
save_derived(zone_construction, "04_zone_construction")
save_derived(zone_nesting, "04_zone_nesting")
save_geometry(zones_sf, "04_zones")
save_geometry(districts_sf, "04_districts")
save_geometry(outline_sf, "04_outline")

report("Script 4 complete")
