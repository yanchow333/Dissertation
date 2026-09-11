# 8. Change in segregation, 2011 to 2021
# Input:  04_panel_cob, 04_panel_eth
# Output: 08_change_decomposition, 08_growth_redistribution, 08_quadrants

# The Shapley decomposition separates change in group size and zone composition from change in spatial structure
# mutual_difference returns uncorrected M, so M1 and M2 differ from the bias corrected values in 05_information

source("00_environment_setup.r")

panel_cob <- read_derived("04_panel_cob")
panel_eth <- read_derived("04_panel_eth")

# only groups present in both census years
comparable_groups <- function(panel) {
  panel %>%
    filter(people > 0) %>%
    distinct(year, group) %>%
    count(group) %>%
    filter(n == length(census_years)) %>%
    pull(group)
}

# Shapley decomposition

decompose_change <- function(panel, label) {
  panel_comparable <- filter(panel, group %in% comparable_groups(panel))

  mutual_difference(data1 = filter(panel_comparable, year == 2011), data2 = filter(panel_comparable, year == 2021),
                    group = "group", unit = "zone", weight = "people", method = "shapley") %>%
    as_tibble() %>%
    mutate(variable = label)
}

change_decomposition <- bind_rows(decompose_change(panel_cob, "Country of birth"),
                                  decompose_change(panel_eth, "Ethnic group"))

# Growth and redistribution

# redistribution is the dissimilarity between a group's own 2011 and 2021 distributions
growth_and_redistribution <- function(panel, label) {
  panel %>%
    filter(group %in% comparable_groups(panel)) %>%
    select(year, zone, group, people) %>%
    pivot_wider(names_from = year, values_from = people, names_prefix = "y", values_fill = 0) %>%
    group_by(group) %>%
    summarise(n_2011 = sum(y2011),
              n_2021 = sum(y2021),
              growth = round(100 * (n_2021 - n_2011) / n_2011, 1),
              redistribution = round(0.5 * sum(abs(y2021 / sum(y2021) - y2011 / sum(y2011))), 4),
              .groups = "drop") %>%
    mutate(variable = label)
}

growth_redistribution <- bind_rows(growth_and_redistribution(panel_cob, "Country of birth"),
                                   growth_and_redistribution(panel_eth, "Ethnic group"))

# Neighbourhood change

# each zone is placed by its change in diversity and its change in contribution to national separation
local_change <- function(panel, label) {
  contribution <- panel %>%
    group_by(year) %>%
    group_modify(function(year_panel, ...) mutual_local(year_panel, group = "group", unit = "zone", weight = "people", wide = TRUE)) %>%
    ungroup() %>%
    as_tibble() %>%
    select(year, zone, ls, p)

  diversity <- panel %>%
    group_by(year, zone) %>%
    mutate(share = people / sum(people)) %>%
    summarise(entropy = -sum(share * log(share), na.rm = TRUE), population = sum(people), .groups = "drop")

  contribution %>%
    left_join(diversity, by = c("year", "zone")) %>%
    pivot_wider(names_from = year, values_from = c(ls, p, entropy, population), names_sep = "_") %>%
    mutate(d_entropy = entropy_2021 - entropy_2011,
           d_contribution = (ls_2021 * p_2021) - (ls_2011 * p_2011),
           quadrant = case_when(d_entropy > 0 & d_contribution > 0 ~ "Diversifying, more separated",
                                d_entropy > 0 & d_contribution <= 0 ~ "Diversifying, less separated",
                                d_entropy <= 0 & d_contribution > 0 ~ "Homogenising, more separated",
                                TRUE ~ "Homogenising, less separated"),
           variable = label)
}

quadrants <- bind_rows(local_change(panel_cob, "Country of birth"), local_change(panel_eth, "Ethnic group")) %>%
  group_by(variable, quadrant) %>%
  summarise(zones = n(), population_2021 = sum(population_2021, na.rm = TRUE), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(percent_zones = round(100 * zones / sum(zones), 1)) %>%
  ungroup()

report("Shapley decomposition of the change in mutual information", change_decomposition)
report("Growth and redistribution", arrange(growth_redistribution, variable, desc(growth)), rows = 30)
report("Neighbourhood change quadrants", quadrants)

# Checks

decomposition_check <- change_decomposition %>%
  group_by(variable) %>%
  summarise(difference = abs((est[stat == "M2"] - est[stat == "M1"]) - sum(est[!stat %in% c("M1", "M2", "diff")])),
            .groups = "drop")

stopifnot("decomposition components do not sum to the observed change" = all(decomposition_check$difference < tolerance),
          "a redistribution index falls outside the unit interval" = all(between(growth_redistribution$redistribution, 0, 1)))

# Save

save_derived(change_decomposition, "08_change_decomposition")
save_derived(growth_redistribution, "08_growth_redistribution")
save_derived(quadrants, "08_quadrants")

report("Script 8 complete")
