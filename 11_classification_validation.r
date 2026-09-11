# 11. Validation of the settlement classification
# Input:  10_classification, 04_zone_lookup, 01_arrival_2021_raw
# Output: 11_arrival_zone, 11_arrival_by_class, 11_cluster_diagnostics, 11_correspondence, 11_ratio_diagnostics

# The rule based classes are compared with k-means clusters derived from the same indicators to assess agreement,
# and with year of arrival as an external measure of validation

source("00_environment_setup.r")

classified <- read_derived("10_classification")
zone_lookup <- read_derived("04_zone_lookup")
arrival_2021_raw <- read_derived("01_arrival_2021_raw")

# K-means on the classification indicators

ratio_diagnostics <- classified %>%
  summarise(defined = sum(!is.na(fgr_2021)),
            undefined = sum(is.na(fgr_2021)),
            undefined_percent = round(100 * mean(is.na(fgr_2021)), 1),
            above_one = sum(fgr_2021 > 1, na.rm = TRUE),
            median = round(median(fgr_2021, na.rm = TRUE), 3))

report("First generation ratio: zones with an undefined ratio are excluded from clustering", ratio_diagnostics)

cluster_input <- classified %>%
  filter(!is.na(fgr_2021)) %>%
  select(zone, fb_share_2021, me_share_2021, fgr_2021, turnover, d_fb, d_me)

cluster_scaled <- cluster_input %>%
  select(-zone) %>%
  mutate(fgr_2021 = log1p(fgr_2021)) %>%
  scale()

# silhouettes have to use a sample because the full distance matrix does not fit in memory
silhouette_rows <- sample(nrow(cluster_scaled), min(silhouette_n, nrow(cluster_scaled)))
silhouette_dist <- dist(cluster_scaled[silhouette_rows, ])

cluster_diagnostics <- map_dfr(k_range, function(k) {
  fit <- kmeans(cluster_scaled, centers = k, nstart = 25, iter.max = 300, algorithm = "MacQueen")
  tibble(k = k,
         silhouette = round(mean(silhouette(fit$cluster[silhouette_rows], silhouette_dist)[, 3]), 4),
         within_ss = round(fit$tot.withinss, 1),
         explained = round(fit$betweenss / fit$totss, 4))
})

k_best <- cluster_diagnostics$k[which.max(cluster_diagnostics$silhouette)]
kmeans_fit <- kmeans(cluster_scaled, centers = k_best, nstart = 50, iter.max = 300, algorithm = "MacQueen")

class_vs_cluster <- classified %>%
  select(zone, class) %>%
  inner_join(mutate(cluster_input, cluster = factor(kmeans_fit$cluster)), by = "zone")

correspondence <- tibble(
  comparison = c("Rule classes against k-means, all zones", "Rule classes against k-means, excluding low diversity",
                 "Low diversity contrast against k-means"),
  ari = round(c(adjustedRandIndex(class_vs_cluster$class, class_vs_cluster$cluster),
                with(filter(class_vs_cluster, class != "Low diversity"), adjustedRandIndex(class, cluster)),
                adjustedRandIndex(class_vs_cluster$class != "Low diversity", class_vs_cluster$cluster)), 3))

report("Cluster diagnostics", cluster_diagnostics)
report(paste("Selected number of clusters:", k_best))
report("Rule classes against k-means clusters, row percentages",
       round(100 * prop.table(table(class_vs_cluster$class, class_vs_cluster$cluster), 1), 1))
report("Agreement between partitions", correspondence)

# Year of arrival

arrival_zone <- arrival_2021_raw %>%
  mutate(first_year = as.integer(str_extract(category, "[0-9]{4}"))) %>%
  filter(!is.na(first_year)) %>%
  group_by(area_code) %>%
  summarise(recent_n = sum(people[first_year >= recent_arrival_from]), foreign_n = sum(people), .groups = "drop") %>%
  inner_join(filter(zone_lookup, year == 2021), by = "area_code") %>%
  group_by(zone) %>%
  summarise(arrival = sum(recent_n) / sum(foreign_n), .groups = "drop")

arrival_validation <- classified %>%
  select(zone, class) %>%
  inner_join(arrival_zone, by = "zone") %>%
  drop_na()

arrival_by_class <- arrival_validation %>%
  group_by(class) %>%
  summarise(zones = n(),
            median = round(median(arrival), 3),
            q1 = round(quantile(arrival, 0.25), 3),
            q3 = round(quantile(arrival, 0.75), 3),
            .groups = "drop") %>%
  arrange(desc(median))

report("Recent arrival share by settlement class", arrival_by_class)

# Checks

stopifnot("no arrival band was classed as recent" = any(arrival_zone$arrival > 0, na.rm = TRUE),
          "arrival data covers fewer than 90 per cent of zones" = nrow(arrival_validation) > 0.9 * nrow(classified),
          "k-means did not converge" = kmeans_fit$iter < 300)

# Save

save_derived(arrival_zone, "11_arrival_zone")
save_derived(arrival_by_class, "11_arrival_by_class")
save_derived(cluster_diagnostics, "11_cluster_diagnostics")
save_derived(correspondence, "11_correspondence")
save_derived(ratio_diagnostics, "11_ratio_diagnostics")

report("Script 11 complete")
