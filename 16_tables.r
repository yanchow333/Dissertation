# 16 Tables
# Input:  derived objects from scripts 2 to 14
# Output: outputs/tables, table_1 to table_8 for the main text and supplementary tables s01 to s22
# Main tables carry presentation headers; supplementary tables keep variable names so every value cited in the text can be traced

source("00_environment_setup.r")

write_table <- function(table, name, folder = "outputs/tables") write_csv(table, here(folder, paste0(name, ".csv")), na = "")
write_supplementary <- function(table, name) write_table(table, name, "outputs/tables/supplementary")

# Table 1, category resolution and population retention

table_1 <- read_derived("03_retention") %>%
  left_join(select(read_derived("02_cob_resolution"), variable, year, terminal_categories, foreign_born_mean_share, residual_percent),
            by = c("variable", "year")) %>%
  transmute(Variable = variable,
            Year = year,
            `Terminal categories` = terminal_categories,
            `Foreign born mean category share (%)` = foreign_born_mean_share,
            `Residual (%)` = residual_percent,
            `Source population` = source,
            `Grouped population` = grouped,
            `Lost (%)` = percent_lost) %>%
  arrange(Variable, Year)

# Table 2, boundary change and zone construction

change_labels <- c(U = "Unchanged", S = "Split", M = "Merged", X = "Irregularly revised")

table_2 <- bind_rows(
  read_derived("04_boundary_change") %>%
    transmute(Panel = "Boundary change, 2011 to 2021", Item = unname(change_labels[chgind]), Count = areas, `Percent (%)` = percent),
  read_derived("04_zone_construction") %>%
    transmute(Panel = "Harmonised zone construction", Item = str_to_sentence(type), Count = zones, `Percent (%)` = percent)
)

# Table 3, dissimilarity and isolation against the size conditional null

indices_null <- read_derived("06_indices_null") %>%
  filter(variable != "Accession cohort") %>%
  mutate(group = as.character(group))

table_3 <- indices_null %>%
  filter(year == 2021) %>%
  left_join(indices_null %>% filter(year == 2011) %>% select(variable, group, D_2011 = D), by = c("variable", "group")) %>%
  arrange(variable, desc(D)) %>%
  transmute(Variable = variable,
            Group = group,
            `Population 2021` = group_total,
            `D 2011` = round(D_2011, 3),
            `D 2021` = round(D, 3),
            `D expected` = round(D_expected, 3),
            `D corrected` = round(D_corrected, 3),
            Isolation = round(P, 3),
            `Isolation expected` = round(P_expected, 3))

# Table 4, scale decomposition, whole population

table_4 <- read_derived("07_scale_decomposition") %>%
  filter(target_group == "All groups") %>%
  select(variable, level, year, increment, share) %>%
  pivot_wider(names_from = year, values_from = c(increment, share)) %>%
  arrange(variable, level) %>%
  transmute(Variable = variable,
            Level = level,
            `Increment 2011` = round(increment_2011, 4),
            `Increment 2021` = round(increment_2021, 4),
            `Share 2011 (%)` = round(100 * share_2011, 1),
            `Share 2021 (%)` = round(100 * share_2021, 1))

# Table 5, Shapley decomposition of the change in mutual information, uncorrected

table_5 <- read_derived("08_change_decomposition") %>%
  mutate(est = round(est, 4)) %>%
  pivot_wider(names_from = variable, values_from = est) %>%
  rename(Component = stat)

# Table 6, pairing sensitivity

table_6 <- read_derived("09_pairing_sensitivity") %>%
  transmute(Pairing = pair,
            Scheme = scheme,
            `Clustered zones` = hotspot_zones,
            Both = dual,
            `Birthplace only` = birthplace_only,
            `Ethnicity only` = ethnicity_only,
            `Concordance (%)` = concordance_percent,
            `Correlation of Gi` = correlation,
            `Population ratio` = ratio)

# Table 7, settlement classes and their median indicator values

table_7 <- read_derived("10_class_summary") %>%
  left_join(read_derived("10_class_profile"), by = "class") %>%
  transmute(Class = class,
            Zones = zones,
            `Zones (%)` = percent_zones,
            `Population 2021` = population_2021,
            `Population (%)` = percent_population,
            `Foreign born share (%)` = fb_2021,
            `Minority ethnic share (%)` = me_2021,
            `First generation ratio` = fgr_2021,
            `Origin turnover` = turnover,
            `Change in foreign born share (pp)` = d_fb,
            `Change in minority ethnic share (pp)` = d_me)

# Table 8, change clusters, for the five categories that moved furthest

moving_categories <- c("White British", "Other White", "Pakistani", "African", "Caribbean")

table_8 <- read_derived("14_change_profile") %>%
  left_join(select(read_derived("14_change_arrival"), cluster, arrival_median = median), by = "cluster") %>%
  select(Cluster = cluster, Zones = zones, `Zones (%)` = percent_zones, `Population (%)` = percent_population,
         all_of(moving_categories), `Change in entropy` = d_entropy, `Change in foreign born share` = d_fb,
         `Median recent arrival share` = arrival_median)

write_table(table_1,"table_1_resolution_and_retention")
write_table(table_2,"table_2_geography_harmonisation")
write_table(table_3,"table_3_indices_and_null")
write_table(table_4,"table_4_scale_decomposition")
write_table(table_5,"table_5_change_decomposition")
write_table(table_6,"table_6_pairing_sensitivity")
write_table(table_7,"table_7_settlement_classes")
write_table(table_8,"table_8_change_clusters")

# Supplementary tables, in pipeline order

crosswalk <- read_derived("03_crosswalk") %>%
  select(variable, year, census_category = path, analysis_group = group, matching_rule = rule) %>%
  arrange(variable, year, analysis_group, census_category)

neighbourhood_share_by_group <- read_derived("07_scale_decomposition") %>%
  filter(level == "Neighbourhood", target_group != "All groups") %>%
  transmute(variable, year, group = target_group, neighbourhood_share_percent = round(100 * share, 1)) %>%
  arrange(variable, year, desc(neighbourhood_share_percent))

partition_agreement <- bind_rows(read_derived("11_correspondence"), read_derived("14_change_correspondence"))

cluster_diagnostics <- bind_rows(mutate(read_derived("11_cluster_diagnostics"), clustering = "Settlement indicators"),
                                 mutate(read_derived("14_change_diagnostics"), clustering = "Change vector")) %>%
  relocate(clustering)

write_supplementary(crosswalk,"table_s01_category_crosswalk")
write_supplementary(read_derived("03_composition"), "table_s02_group_composition")
write_supplementary(read_derived("04_zone_nesting"), "table_s03_zone_nesting")
write_supplementary(read_derived("05_information"), "table_s04_mutual_information")
write_supplementary(mutate(read_derived("05_detailed_indices_2011"), across(where(is.numeric), ~ round(.x, 4))),
                    "table_s05_detailed_birthplace_2011")
write_supplementary(read_derived("06_size_relationship"), "table_s06_size_relationship")
write_supplementary(neighbourhood_share_by_group, "table_s07_neighbourhood_share_by_group")
write_supplementary(read_derived("08_growth_redistribution"), "table_s08_growth_redistribution")
write_supplementary(read_derived("08_quadrants"), "table_s09_neighbourhood_quadrants")
write_supplementary(read_derived("10_thresholds"), "table_s10_classification_thresholds")
write_supplementary(read_derived("10_threshold_sensitivity"), "table_s11_threshold_sensitivity")
write_supplementary(read_derived("10_floor_sensitivity"), "table_s12_minority_floor_sensitivity")
write_supplementary(read_derived("10_indicator_correlations"), "table_s13_indicator_correlations")
write_supplementary(read_derived("11_ratio_diagnostics"), "table_s14_first_generation_ratio")
write_supplementary(read_derived("11_arrival_by_class"), "table_s15_arrival_by_class")
write_supplementary(cluster_diagnostics, "table_s16_kmeans_diagnostics")
write_supplementary(partition_agreement, "table_s17_partition_agreement")
write_supplementary(read_derived("12_generation_sparsity"), "table_s18_generation_sparsity")
write_supplementary(mutate(read_derived("13_indices_arrival"), across(where(is.numeric), ~ round(.x, 4))), "table_s19_arrival_cohorts")
write_supplementary(mutate(read_derived("13_generation_gap"), across(where(is.numeric), ~ round(.x, 4))), "table_s20_generation_gap")
write_supplementary(read_derived("14_change_coverage"), "table_s21_change_coverage")
write_supplementary(read_derived("14_class_by_change_cluster"), "table_s22_class_by_change_cluster")

report("Script 16 complete")
