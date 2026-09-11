# 5. Segregation indices
# Input:  04_panel_cob, 04_panel_eth, 04_panel_accession, 01_cob_raw, 02_cob_tree, 04_zone_lookup, 01_lookup_hierarchy_2021
# Output: 05_indices, 05_information, 05_detailed_indices_2011, 05_district_indices_2021

# Dissimilarity and isolation for each group, mutual information and Theil's H for each classification

source("00_environment_setup.r")

panel_cob <- read_derived("04_panel_cob")
panel_eth <- read_derived("04_panel_eth")
panel_accession <- read_derived("04_panel_accession")
cob_raw <- read_derived("01_cob_raw")
cob_tree <- read_derived("02_cob_tree")
zone_lookup <- read_derived("04_zone_lookup")
lookup_hierarchy_2021 <- read_derived("01_lookup_hierarchy_2021")

cob_leaves <- filter(cob_tree, is_leaf, !is_total)

zone_population_cob <- zone_totals(panel_cob)
zone_population_eth <- zone_totals(panel_eth)
zone_population_2021 <- filter(zone_population_cob, year == 2021)

# Dissimilarity and isolation

indices_cob <- pairwise_indices(panel_cob, zone_population_cob)
indices_eth <- pairwise_indices(panel_eth, zone_population_eth)
indices_accession <- pairwise_indices(panel_accession, zone_population_2021)

report(paste("Bootstrapping dissimilarity,", n_bootstrap, "replications"))

bootstrap_cob <- bootstrap_dissimilarity(panel_cob, zone_population_cob)
bootstrap_eth <- bootstrap_dissimilarity(panel_eth, zone_population_eth)
bootstrap_accession <- bootstrap_dissimilarity(panel_accession, zone_population_2021)

# Mutual information and Theil's H

information_indices <- function(panel, label) {
  panel %>%
    group_by(year) %>%
    group_modify(function(year_panel, ...) mutual_total(year_panel, group = "group", unit = "zone", weight = "people",
                                                 se = TRUE, n_bootstrap = n_bootstrap)) %>%
    ungroup() %>%
    as_tibble() %>%
    mutate(variable = label)
}

# each group's share of mutual information
group_contributions <- function(panel) {
  panel %>%
    group_by(year) %>%
    group_modify(function(year_panel, ...) mutual_local(year_panel, group = "zone", unit = "group", weight = "people",
                                                 wide = TRUE, se = FALSE)) %>%
    ungroup() %>%
    as_tibble() %>%
    transmute(year, group, ls_group = ls, contribution = ls * p)
}

theil_index <- function(panel, information, label) {
  group_entropy <- panel %>%
    group_by(year) %>%
    group_modify(function(year_panel, ...) tibble(E = entropy(year_panel, group = "group", weight = "people"))) %>%
    ungroup()

  information %>%
    filter(stat == "M") %>%
    select(year, M = est) %>%
    left_join(group_entropy, by = "year") %>%
    transmute(variable = label, year, stat = "Theil H", est = M / E)
}

information_cob <- information_indices(panel_cob, "Country of birth")
information_eth <- information_indices(panel_eth, "Ethnic group")

contribution_cob <- group_contributions(panel_cob)
contribution_eth <- group_contributions(panel_eth)

theil <- bind_rows(theil_index(panel_cob, information_cob, "Country of birth"),
                   theil_index(panel_eth, information_eth, "Ethnic group"))

information <- bind_rows(information_cob, information_eth) %>%
  filter(stat %in% c("M", "H")) %>%
  select(variable, year, stat, est, se, bias) %>%
  bind_rows(theil)

indices <- bind_rows(
  indices_cob %>% left_join(bootstrap_cob, by = c("year", "group")) %>% mutate(variable = "Country of birth"),
  indices_eth %>% left_join(bootstrap_eth, by = c("year", "group")) %>% mutate(variable = "Ethnic group"),
  indices_accession %>% left_join(bootstrap_accession, by = c("year", "group")) %>% mutate(variable = "Accession cohort")
) %>%
  left_join(bind_rows(mutate(contribution_cob, variable = "Country of birth"),
                      mutate(contribution_eth, variable = "Ethnic group")),
            by = c("year", "group", "variable")) %>%
  arrange(variable, year, desc(D))

report("Dissimilarity and isolation", indices, rows = 60)
report("Mutual information and Theil's H", information)

# Detailed country of birth, 2011

detailed_2011 <- cob_raw %>%
  filter(year == 2011) %>%
  semi_join(cob_leaves, by = c("year", "category")) %>%
  inner_join(filter(zone_lookup, year == 2011), by = c("year", "area_code"))

detailed_population_2011 <- detailed_2011 %>%
  group_by(year, zone) %>%
  summarise(zone_total = sum(people), .groups = "drop")

# 20,000 residents keeps each country large enough for a stable index
detailed_indices_2011 <- detailed_2011 %>%
  mutate(country = str_squish(str_extract(category, "[^:]+$"))) %>%
  filter(!str_detect(str_to_lower(country), pattern_uk_born)) %>%
  group_by(year, zone, group = country) %>%
  summarise(people = sum(people), .groups = "drop") %>%
  pairwise_indices(detailed_population_2011) %>%
  filter(group_total >= 20000) %>%
  arrange(desc(D))

report("Detailed country of birth, 2011", detailed_indices_2011, rows = 60)

# Dissimilarity within each local authority, 2021

within_district <- function(panel, label) {
  panel %>%
    filter(year == 2021, !is.na(lad)) %>%
    group_by(lad) %>%
    group_modify(function(district_panel, ...) pairwise_indices(district_panel, zone_totals(district_panel))) %>%
    ungroup() %>%
    mutate(variable = label)
}

district_indices_2021 <- bind_rows(within_district(panel_cob, "Country of birth"),
                                   within_district(panel_eth, "Ethnic group")) %>%
  left_join(distinct(lookup_hierarchy_2021, lad = lad22cd, district = lad22nm), by = "lad") %>%
  filter(group_total > 0) %>%
  select(variable, lad, district, group, group_total, D, P, P_expected)

# Checks

reconciliation <- contribution_cob %>%
  group_by(year) %>%
  summarise(sum_contribution = sum(contribution), .groups = "drop") %>%
  left_join(information_cob %>% filter(stat == "M") %>% select(year, M = est, bias), by = "year") %>%
  mutate(difference = abs(sum_contribution - (M + bias)))

# 2021 publishes an 'All other EU countries' residual that 2011 does not
accession_residual <- cob_raw %>%
  filter(year == 2021) %>%
  inner_join(select(cob_leaves, year, category, key), by = c("year", "category")) %>%
  filter(str_detect(key, "all other eu countries"))

report("Group contributions against uncorrected mutual information", reconciliation)

stopifnot(
  "an observed index falls outside its bootstrap interval" = all(indices$D_lo <= indices$D & indices$D <= indices$D_hi, na.rm = TRUE),
  "group contributions do not reconcile with mutual information" = all(reconciliation$difference < tolerance),
  "Theil's H exceeds one" = all(theil$est <= 1 + tolerance),
  "a dissimilarity index falls outside the unit interval" = all(between(indices$D, 0, 1), na.rm = TRUE),
  "isolation falls below its expected value" = all(indices$P >= indices$P_expected - tolerance),
  "the 2021 EU residual is missing" = sum(accession_residual$people) > 0
)

# Save

save_derived(indices, "05_indices")
save_derived(information, "05_information")
save_derived(detailed_indices_2011, "05_detailed_indices_2011")
save_derived(district_indices_2021, "05_district_indices_2021")

report("Script 5 complete")
