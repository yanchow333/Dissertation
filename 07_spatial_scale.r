# 7. Spatial scale of separation
# Input:  04_panel_cob, 04_panel_eth
# Output: 07_scale_decomposition

# Mutual information at four nested levels; each increment is the separation added by the finer level

source("00_environment_setup.r")

panel_cob <- read_derived("04_panel_cob")
panel_eth <- read_derived("04_panel_eth")

# Decomposition

m_at_level <- function(panel, level_column) {
  panel %>%
    filter(!is.na(.data[[level_column]])) %>%
    group_by(year, level_unit = .data[[level_column]], group) %>%
    summarise(people = sum(people), .groups = "drop") %>%
    group_by(year) %>%
    group_modify(function(year_panel, ...) mutual_total(year_panel, group = "group", unit = "level_unit", weight = "people")) %>%
    ungroup() %>%
    filter(stat == "M") %>%
    select(year, M = est)
}

scale_profile <- function(panel, label) {
  imap_dfr(scale_levels, function(level_label, level_column) mutate(m_at_level(panel, level_column), level = level_column)) %>%
    mutate(level = factor(level, levels = names(scale_levels), labels = unname(scale_levels))) %>%
    arrange(year, level) %>%
    group_by(year) %>%
    mutate(increment = M - lag(M, default = 0), share = increment / max(M)) %>%
    ungroup() %>%
    mutate(variable = label)
}

# each group by the rest of the population, in the years the group is present
scale_profile_by_group <- function(panel, label) {
  map_dfr(unique(panel$group), function(target) {
    years_present <- unique(panel$year[panel$group == target & panel$people > 0])

    panel %>%
      filter(year %in% years_present) %>%
      mutate(binary_group = if_else(group == target, as.character(target), "Rest of population")) %>%
      group_by(year, zone, msoa, lad, region, group = binary_group) %>%
      summarise(people = sum(people), .groups = "drop") %>%
      scale_profile(label) %>%
      mutate(target_group = as.character(target))
  })
}

scale_decomposition <- bind_rows(
  scale_profile(panel_cob, "Country of birth") %>% mutate(target_group = "All groups"),
  scale_profile(panel_eth, "Ethnic group") %>% mutate(target_group = "All groups"),
  scale_profile_by_group(panel_cob, "Country of birth"),
  scale_profile_by_group(panel_eth, "Ethnic group")
)

report("Scale decomposition, whole population",
       scale_decomposition %>% filter(target_group == "All groups") %>% select(variable, year, level, M, increment, share),
       rows = 20)

# Checks

expected_rows <- bind_rows(mutate(panel_cob, variable = "Country of birth"), mutate(panel_eth, variable = "Ethnic group")) %>%
  filter(people > 0) %>%
  distinct(variable, target_group = as.character(group), year) %>%
  count(variable, target_group, name = "years") %>%
  mutate(expected = length(scale_levels) * years)

row_check <- scale_decomposition %>%
  filter(target_group != "All groups") %>%
  count(variable, target_group, name = "observed") %>%
  left_join(expected_rows, by = c("variable", "target_group"))

reconciliation <- scale_decomposition %>%
  group_by(variable, year, target_group) %>%
  summarise(difference = abs(sum(increment) - M[level == "Neighbourhood"]), .groups = "drop")

stopifnot("a group is missing a scale profile row" = all(row_check$observed == row_check$expected),
          "a scale increment is negative" = all(scale_decomposition$increment >= -tolerance),
          "increments do not sum to the neighbourhood total" = all(reconciliation$difference < tolerance))

# Save

save_derived(scale_decomposition, "07_scale_decomposition")

report("Script 7 complete")
