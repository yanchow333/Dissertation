# 6. Size conditional null model
# Input:  04_panel_cob, 04_panel_eth, 04_panel_accession, 05_indices
# Output: 06_indices_null, 06_size_relationship

# Dissimilarity rises as group size falls even under random allocation, so each index is set against that expectation

source("00_environment_setup.r")

panel_cob <- read_derived("04_panel_cob")
panel_eth <- read_derived("04_panel_eth")
panel_accession <- read_derived("04_panel_accession")
indices <- read_derived("05_indices")

# Simulation

# a group of the observed size allocated at random across zones of the observed size
simulate_null <- function(group_total, zone_total, n_sim = n_permutations) {
  draws <- rmultinom(n_sim, size = round(group_total), prob = zone_total / sum(zone_total))

  list(D = apply(draws, 2, function(drawn) {
         other <- pmax(zone_total - drawn, 0)
         0.5 * sum(abs(drawn / sum(drawn) - other / sum(other)))
       }),
       P = apply(draws, 2, function(drawn) sum((drawn / sum(drawn)) * (drawn / zone_total), na.rm = TRUE)))
}

null_for_panel <- function(panel, label) {
  zone_population <- zone_totals(panel)

  group_population <- panel %>%
    group_by(year, group) %>%
    summarise(group_total = sum(people), .groups = "drop")

  future_pmap_dfr(
    list(group_population$year, group_population$group, group_population$group_total),
    function(census_year, group_name, group_total) {
      simulated <- simulate_null(group_total, zone_population$zone_total[zone_population$year == census_year])
      tibble(variable = label, year = census_year, group = group_name,
             D_expected = mean(simulated$D),
             D_null_sd = sd(simulated$D),
             D_null_lo = unname(quantile(simulated$D, 0.025)),
             D_null_hi = unname(quantile(simulated$D, 0.975)),
             P_expected_null = mean(simulated$P))
    },
    .options = furrr_options(seed = TRUE))
}

report(paste("Simulating", n_permutations, "random allocations per group"))

null_all <- bind_rows(null_for_panel(panel_cob, "Country of birth"),
                      null_for_panel(panel_eth, "Ethnic group"),
                      null_for_panel(panel_accession, "Accession cohort"))

# Adjusted indices

indices_null <- indices %>%
  inner_join(null_all, by = c("variable", "year", "group")) %>%
  mutate(D_corrected = (D - D_expected) / (1 - D_expected),
         z_score = (D - D_expected) / D_null_sd,
         expected_to_observed = if_else(D > 0, round(100 * D_expected / D, 1), NA_real_),
         exceeds_null = D > D_null_hi) %>%
  group_by(variable, year) %>%
  mutate(rank_raw = rank(-D, ties.method = "min"),
         rank_corrected = rank(-D_corrected, ties.method = "min"),
         rank_shift = rank_raw - rank_corrected) %>%
  ungroup() %>%
  arrange(variable, year, rank_raw)

size_relationship <- indices_null %>%
  filter(variable != "Accession cohort") %>%
  group_by(variable, year) %>%
  summarise(groups = n(),
            r_raw = round(cor(log(group_total), D, method = "spearman"), 3),
            r_corrected = round(cor(log(group_total), D_corrected, method = "spearman"), 3),
            r_expected = round(cor(log(group_total), D_expected, method = "spearman"), 3),
            .groups = "drop")

report("Groups within the null interval", filter(indices_null, !exceeds_null) %>% select(variable, year, group, D, D_null_hi))
report("Rank correlation of group size with dissimilarity", size_relationship)

# Checks

stopifnot("a size adjusted index exceeds one" = all(indices_null$D_corrected <= 1 + tolerance),
          "expected dissimilarity does not fall as group size rises" = all(size_relationship$r_expected < 0))

# Save

save_derived(indices_null, "06_indices_null")
save_derived(size_relationship, "06_size_relationship")

report("Script 6 complete")
