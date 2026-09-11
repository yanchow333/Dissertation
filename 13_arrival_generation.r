# 13. Segregation by arrival cohort and by generation(2021)
# Input:  01_arrival_2021_raw, 04_zone_lookup, 12_panel_generation
# Output: 13_indices_arrival, 13_generation_indices, 13_generation_gap

# Duration of residence comes from year of arrival; generation from the ethnic group by birthplace table

source("00_environment_setup.r")

arrival_2021_raw <- read_derived("01_arrival_2021_raw")
zone_lookup <- read_derived("04_zone_lookup")
panel_generation <- read_derived("12_panel_generation")

# Arrival cohorts

cohort_levels <- c("UK born", "Before 1981", "1981 to 2000", "2001 or later")

# each TS015 band is assigned by its first year
# 'Does not apply' has no population and is dropped
classify_arrival <- function(category) {
  first_year <- as.integer(str_extract(category, "[0-9]{4}"))
  case_when(category == "Born in the UK" ~ "UK born",
            first_year < 1981 ~ "Before 1981",
            first_year < 2001 ~ "1981 to 2000",
            first_year >= 2001 ~ "2001 or later")}

arrival_bands <- arrival_2021_raw %>% mutate(cohort = classify_arrival(category))

report("Arrival bands by cohort", count(arrival_bands, category, cohort, wt = people, name = "people"), rows = 20)

panel_arrival <- arrival_bands %>%
  filter(!is.na(cohort)) %>%
  inner_join(filter(zone_lookup, year == 2021), by = c("year", "area_code")) %>%
  group_by(year, zone, group = cohort) %>%
  summarise(people = sum(people), .groups = "drop") %>%
  mutate(group = factor(group, levels = cohort_levels)) %>%
  complete(nesting(year, zone), group, fill = list(people = 0))

zone_population_arrival <- zone_totals(panel_arrival)

stopifnot("the arrival panel does not cover every zone" = n_distinct(panel_arrival$zone) == n_distinct(zone_lookup$zone),
          "an arrival band with population was not assigned a cohort" = sum(arrival_bands$people[is.na(arrival_bands$cohort)]) == 0)

indices_arrival <- pairwise_indices(panel_arrival, zone_population_arrival) %>%
  left_join(bootstrap_dissimilarity(panel_arrival, zone_population_arrival), by = c("year", "group")) %>%
  mutate(variable = "Arrival cohort", group = factor(group, levels = cohort_levels)) %>%
  arrange(group)

report("Dissimilarity and isolation by arrival cohort, 2021", indices_arrival)

# Generational split

# a group is dropped where one generation is empty in most units, since the index would rest on a few cells
sparse_groups <- panel_generation %>%
  group_by(year, group) %>%
  summarise(uk_born_zero = mean(uk_born == 0), foreign_zero = mean(foreign_born == 0), .groups = "drop") %>%
  filter(uk_born_zero > generation_zero_max | foreign_zero > generation_zero_max)

report("Groups excluded from the generational split", sparse_groups)

msoa_population <- panel_generation %>%
  group_by(year, msoa) %>%
  summarise(zone_total = sum(total), .groups = "drop")

generation_panel <- panel_generation %>%
  anti_join(sparse_groups, by = c("year", "group")) %>%
  select(year, msoa, group, `First generation` = foreign_born, `Later generation` = uk_born) %>%
  pivot_longer(c(`First generation`, `Later generation`), names_to = "generation", values_to = "people") %>%
  inner_join(msoa_population, by = c("year", "msoa"))

generation_indices <- generation_panel %>%
  group_by(year, group, generation) %>%
  summarise(group_total = sum(people),
            other_total = sum(zone_total - people),
            D = 0.5 * sum(abs(people / sum(people) - (zone_total - people) / other_total)),
            P = sum((people / sum(people)) * (people / zone_total), na.rm = TRUE),
            .groups = "drop")

foreign_share <- panel_generation %>%
  group_by(year, group) %>%
  summarise(foreign_share = sum(foreign_born) / sum(total), .groups = "drop")

generation_gap <- generation_indices %>%
  select(year, group, generation, D) %>%
  pivot_wider(names_from = generation, values_from = D) %>%
  mutate(gap = `Later generation` - `First generation`,
         direction = if_else(gap < 0, "First more uneven", "Later more uneven")) %>%
  left_join(foreign_share, by = c("year", "group")) %>%
  arrange(year, gap)

report("Groups where the first generation is the more uneven", filter(generation_gap, direction == "First more uneven"), rows = 40)

stopifnot("the generational panel is empty" = nrow(generation_panel) > 0,
          "a generational row has no MSOA population" = !anyNA(generation_panel$zone_total))

# Save

save_derived(indices_arrival, "13_indices_arrival")
save_derived(generation_indices, "13_generation_indices")
save_derived(generation_gap, "13_generation_gap")

report("Script 13 complete")
