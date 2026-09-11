# 14. Clustering neighbourhoods on change in composition
# Input:  04_panel_eth, 04_panel_cob, 04_zone_population_2021, 11_arrival_zone, 10_classification
# Output: 14_change_class, 14_change_profile, 14_change_arrival, 14_change_diagnostics, 14_change_coverage,
#         14_change_correspondence, 14_class_by_change_cluster

# Script 10 classifies what neighbourhoods are in 2021 and clusters what changed between 2011 and 2021

source("00_environment_setup.r")

panel_eth <- read_derived("04_panel_eth")
panel_cob <- read_derived("04_panel_cob")
zone_population_2021 <- read_derived("04_zone_population_2021")
arrival_zone <- read_derived("11_arrival_zone")
classified <- read_derived("10_classification")

# only categories defined the same way in both censuses are used
stable_categories <- c("White British", "White Irish", "Other White", "Mixed White and Black Caribbean",
                       "Mixed White and Black African", "Mixed White and Asian", "Indian", "Pakistani",
                       "Bangladeshi", "Chinese", "Caribbean", "African")

change_coverage <- panel_eth %>%
  group_by(year) %>%
  summarise(all_categories = sum(people), stable_categories = sum(people[group %in% stable_categories]), .groups = "drop") %>%
  mutate(percent_retained = round(100 * stable_categories / all_categories, 2))

report("Population in the comparable categories", change_coverage)

# Change vector

share_change <- function(panel) {
  panel %>%
    group_by(year, zone) %>%
    mutate(share = people / sum(people)) %>%
    ungroup() %>%
    select(year, zone, group, share) %>%
    pivot_wider(names_from = year, values_from = share, names_prefix = "y", values_fill = 0) %>%
    mutate(delta = y2021 - y2011) %>%
    select(zone, group, delta) %>%
    pivot_wider(names_from = group, values_from = delta, values_fill = 0)
}

entropy_eth <- panel_eth %>%
  filter(group %in% stable_categories) %>%
  group_by(year, zone) %>%
  mutate(share = people / sum(people)) %>%
  summarise(entropy = -sum(share * log(share), na.rm = TRUE), .groups = "drop")

fb_share <- panel_cob %>%
  group_by(year, zone) %>%
  summarise(fb = sum(people[group != "UK"]) / sum(people), .groups = "drop")

change_context <- entropy_eth %>%
  pivot_wider(names_from = year, values_from = entropy, names_prefix = "e") %>%
  left_join(pivot_wider(fb_share, names_from = year, values_from = fb, names_prefix = "f"), by = "zone") %>%
  transmute(zone, d_entropy = e2021 - e2011, d_fb = f2021 - f2011)

change_vector <- share_change(filter(panel_eth, group %in% stable_categories)) %>%
  inner_join(change_context, by = "zone") %>%
  inner_join(select(zone_population_2021, zone, population), by = "zone")

stopifnot("the change vector lost zones" = nrow(change_vector) == n_distinct(panel_eth$zone),
          "the change vector contains missing values" = !anyNA(select(change_vector, -zone)))

# Clustering

cluster_columns <- c(stable_categories, "d_entropy", "d_fb")

change_scaled <- scale(select(change_vector, all_of(cluster_columns)))

# silhouettes use a sample because the full distance matrix does not fit in memory
silhouette_rows <- sample(nrow(change_scaled), min(silhouette_n, nrow(change_scaled)))
silhouette_dist <- dist(change_scaled[silhouette_rows, ])

change_diagnostics <- map_dfr(k_range, function(k) {
  fit <- kmeans(change_scaled, centers = k, nstart = 25, iter.max = 300, algorithm = "MacQueen")
  tibble(k = k,
         silhouette = round(mean(silhouette(fit$cluster[silhouette_rows], silhouette_dist)[, 3]), 4),
         within_ss = round(fit$tot.withinss, 1),
         explained = round(fit$betweenss / fit$totss, 4),
         smallest = min(table(fit$cluster)))
})

k_change <- change_diagnostics$k[which.max(change_diagnostics$silhouette)]
change_fit <- kmeans(change_scaled, centers = k_change, nstart = 50, iter.max = 300, algorithm = "MacQueen")

report("Change cluster diagnostics", change_diagnostics)
report(paste("Selected number of change clusters:", k_change))

# each cluster is labelled by the two dimensions whose centres lie furthest from the national mean
signature <- as_tibble(change_fit$centers) %>%
  mutate(cluster = row_number()) %>%
  pivot_longer(-cluster, names_to = "dimension", values_to = "centre") %>%
  group_by(cluster) %>%
  slice_max(abs(centre), n = 2) %>%
  arrange(cluster, desc(abs(centre))) %>%
  summarise(signature = paste0(dimension, " ", if_else(centre > 0, "up", "down"), collapse = ", "), .groups = "drop")

change_class <- change_vector %>%
  mutate(cluster = change_fit$cluster) %>%
  left_join(signature, by = "cluster") %>%
  select(zone, cluster, signature, population, all_of(cluster_columns))

change_profile <- change_class %>%
  group_by(cluster, signature) %>%
  summarise(zones = n(),
            population_2021 = sum(population),
            across(all_of(cluster_columns), ~ round(100 * median(.x), 2)),
            .groups = "drop") %>%
  mutate(percent_zones = round(100 * zones / sum(zones), 2),
         percent_population = round(100 * population_2021 / sum(population_2021), 2)) %>%
  relocate(percent_zones, .after = zones) %>%
  relocate(percent_population, .after = population_2021)

report("Change cluster profiles, median change in percentage points", change_profile, width = Inf)

# Arrival recency, external to the change vector

change_arrival <- change_class %>%
  select(zone, cluster) %>%
  inner_join(arrival_zone, by = "zone") %>%
  drop_na() %>%
  group_by(cluster) %>%
  summarise(zones = n(),
            median = round(median(arrival), 3),
            q1 = round(quantile(arrival, 0.25), 3),
            q3 = round(quantile(arrival, 0.75), 3),
            .groups = "drop")

report("Recent arrival share by change cluster", change_arrival)

# Agreement with the settlement classification and sensitivity to excluded categories.

both_typologies <- classified %>%
  select(zone, class) %>%
  inner_join(select(change_class, zone, cluster), by = "zone")

class_by_change_cluster <- both_typologies %>%
  count(class, cluster) %>%
  pivot_wider(names_from = cluster, values_from = n, values_fill = 0, names_prefix = "cluster_")

# refit the same k across all 18 categories, preserving the zone order of the main fit
change_all_categories <- share_change(panel_eth) %>%
  inner_join(change_context, by = "zone") %>%
  arrange(match(zone, change_vector$zone))

fit_all_categories <- kmeans(scale(select(change_all_categories, -zone)), centers = k_change,
                             nstart = 50, iter.max = 300, algorithm = "MacQueen")

change_correspondence <- tibble(
  comparison = c("Settlement classes against change clusters, all zones",
                 "Settlement classes against change clusters, excluding low diversity",
                 "Change clusters on twelve against all eighteen categories"),

  ari = round(c(adjustedRandIndex(both_typologies$class, both_typologies$cluster),
                with(filter(both_typologies, class != "Low diversity"), adjustedRandIndex(class, cluster)),
                adjustedRandIndex(change_fit$cluster, fit_all_categories$cluster)), 3))

report("Settlement classes by change cluster", class_by_change_cluster)
report("Agreement between partitions", change_correspondence)

# Checks

stopifnot("k-means did not converge" = change_fit$iter < 300,
          "arrival data covers fewer than 90 per cent of zones" = sum(change_arrival$zones) > 0.9 * nrow(change_class))

# Save

save_derived(change_class,"14_change_class")
save_derived(change_profile,"14_change_profile")
save_derived(change_arrival,"14_change_arrival")
save_derived(change_diagnostics, "14_change_diagnostics")
save_derived(change_coverage, "14_change_coverage")
save_derived(change_correspondence,"14_change_correspondence")
save_derived(class_by_change_cluster, "14_class_by_change_cluster")

report("Script 14 complete")
