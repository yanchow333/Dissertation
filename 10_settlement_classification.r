# 10. Settlement classification
# Input:  04_panel_cob, 04_panel_eth
# Output: 10_classification, 10_class_summary, 10_class_profile, 10_thresholds, 10_threshold_sensitivity,
#         10_floor_sensitivity, 10_indicator_correlations

# Six indicators and ordered quantile rules assign every zone to one of six classes

source("00_environment_setup.r")

panel_cob <- read_derived("04_panel_cob")
panel_eth <- read_derived("04_panel_eth")

# Indicators

foreign_born <- panel_cob %>%
  group_by(year, zone) %>%
  summarise(population = sum(people), fb_share = sum(people[group != "UK"]) / sum(people), .groups = "drop")

minority <- panel_eth %>%
  group_by(year, zone) %>%
  summarise(me_share = sum(people[group != "White British"]) / sum(people), .groups = "drop")

# the first generation ratio is undefined where the minority share is too small to divide by
composition <- foreign_born %>%
  left_join(minority, by = c("year", "zone")) %>%
  mutate(fgr = if_else(me_share >= minority_floor, fb_share / me_share, NA_real_))

# origin turnover is the dissimilarity between a zone's 2011 and 2021 foreign born composition
turnover <- panel_cob %>%
  filter(group != "UK") %>%
  group_by(year, zone) %>%
  mutate(share = people / sum(people)) %>%
  ungroup() %>%
  select(year, zone, group, share) %>%
  pivot_wider(names_from = year, values_from = share, names_prefix = "y", values_fill = 0) %>%
  group_by(zone) %>%
  summarise(turnover = 0.5 * sum(abs(y2021 - y2011)), .groups = "drop")

indicators <- composition %>%
  select(year, zone, fb_share, me_share, fgr, population) %>%
  pivot_wider(names_from = year, values_from = c(fb_share, me_share, fgr, population), names_sep = "_") %>%
  left_join(turnover, by = "zone") %>%
  mutate(d_fb = fb_share_2021 - fb_share_2011, d_me = me_share_2021 - me_share_2011)

# Rules

thresholds <- list(me_high = quantile(indicators$me_share_2021, 0.75, na.rm = TRUE),
                   me_low = quantile(indicators$me_share_2011, 0.50, na.rm = TRUE),
                   fgr_high = quantile(indicators$fgr_2021, 0.75, na.rm = TRUE),
                   fgr_low = quantile(indicators$fgr_2021, 0.25, na.rm = TRUE),
                   turn_high = quantile(indicators$turnover, 0.75, na.rm = TRUE),
                   d_me_high = quantile(indicators$d_me, 0.90, na.rm = TRUE),
                   d_fb_rise = quantile(indicators$d_fb, 0.75, na.rm = TRUE))

class_levels <- c("Recent migrant concentration", "Origin turnover concentration", "Settled minority concentration",
                  "Emergent migrant diversity", "Emergent settled diversity", "Low diversity")

# rules are tested in order, so a zone meeting the turnover rule is not tested for recent arrival
assign_class <- function(indicators, cut) {
  indicators %>%
    mutate(class = case_when(
      me_share_2021 >= cut$me_high & fgr_2021 >= cut$fgr_high & turnover >= cut$turn_high ~ "Origin turnover concentration",
      me_share_2021 >= cut$me_high & fgr_2021 >= cut$fgr_high & d_fb >= cut$d_fb_rise ~ "Recent migrant concentration",
      me_share_2021 >= cut$me_high & fgr_2021 <= cut$fgr_low ~ "Settled minority concentration",
      me_share_2011 < cut$me_low & d_me >= cut$d_me_high & fgr_2021 <= cut$fgr_low ~ "Emergent settled diversity",
      me_share_2011 < cut$me_low & d_me >= cut$d_me_high ~ "Emergent migrant diversity",
      TRUE ~ "Low diversity"),
      class = factor(class, levels = class_levels))
}

classified <- assign_class(indicators, thresholds)

rule_overlap <- classified %>%
  summarise(meets_both_concentration_rules = sum(me_share_2021 >= thresholds$me_high & fgr_2021 >= thresholds$fgr_high &
                                                   turnover >= thresholds$turn_high & d_fb >= thresholds$d_fb_rise, na.rm = TRUE))


# Class summaries

class_summary <- classified %>%
  group_by(class) %>%
  summarise(zones = n(), population_2021 = sum(population_2021, na.rm = TRUE), .groups = "drop") %>%
  mutate(percent_zones = round(100 * zones / sum(zones), 2),
         percent_population = round(100 * population_2021 / sum(population_2021), 2))

class_profile <- classified %>%
  group_by(class) %>%
  summarise(fb_2021 = round(100 * median(fb_share_2021, na.rm = TRUE), 1),
            me_2021 = round(100 * median(me_share_2021, na.rm = TRUE), 1),
            fgr_2021 = round(median(fgr_2021, na.rm = TRUE), 3),
            turnover = round(median(turnover, na.rm = TRUE), 3),
            d_fb = round(100 * median(d_fb, na.rm = TRUE), 1),
            d_me = round(100 * median(d_me, na.rm = TRUE), 1),
            .groups = "drop")

indicator_correlations <- classified %>%
  select(fb_share_2021, me_share_2021, d_fb, d_me, fgr_2021, turnover) %>%
  cor(use = "complete.obs", method = "pearson") %>%
  round(3) %>%
  as_tibble(rownames = "indicator")

report("Classification thresholds", round(unlist(thresholds), 4))
report("Zones meeting both concentration rules, assigned to origin turnover", rule_overlap)
report("Classification", class_summary)
report("Class profiles", class_profile)


# Sensitivity

# the four concentration cut points move together; the change thresholds stay fixed
threshold_sensitivity <- map_dfr(c(0.65, 0.70, 0.75, 0.80, 0.85), function(q) {
  cut <- modifyList(thresholds, list(me_high = quantile(indicators$me_share_2021, q, na.rm = TRUE),
                                     fgr_high = quantile(indicators$fgr_2021, q, na.rm = TRUE),
                                     fgr_low = quantile(indicators$fgr_2021, 1 - q, na.rm = TRUE),
                                     turn_high = quantile(indicators$turnover, q, na.rm = TRUE)))
  alternative <- assign_class(indicators, cut)$class

  tibble(quantile = q,
         retained = round(100 * mean(alternative == classified$class), 1),
         ari = round(adjustedRandIndex(alternative, classified$class), 3),
         non_residual = sum(alternative != "Low diversity"))
})

floor_sensitivity <- map_dfr(c(0.03, 0.05, 0.10), function(floor_value) {
  composition %>%
    filter(year == 2021) %>%
    mutate(fgr_alt = if_else(me_share >= floor_value, fb_share / me_share, NA_real_)) %>%
    summarise(floor = floor_value, defined = sum(!is.na(fgr_alt)), median = round(median(fgr_alt, na.rm = TRUE), 3))})

report("Stability under alternative thresholds", threshold_sensitivity)
report("Sensitivity to the minority share floor", floor_sensitivity)

# Checks

stopifnot("classification changed the number of zones" = nrow(classified) == nrow(indicators),
          "a zone has no class" = !anyNA(classified$class))

# Save

save_derived(classified, "10_classification")
save_derived(class_summary, "10_class_summary")
save_derived(class_profile, "10_class_profile")
save_derived(enframe(unlist(thresholds), name = "threshold", value = "value"), "10_thresholds")
save_derived(threshold_sensitivity, "10_threshold_sensitivity")
save_derived(floor_sensitivity, "10_floor_sensitivity")
save_derived(indicator_correlations, "10_indicator_correlations")

report("Script 10 complete")
