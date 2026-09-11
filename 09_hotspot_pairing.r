# 9. Local clustering and pairing sensitivity
# Input:  04_panel_cob, 04_panel_eth, 04_zones.gpkg
# Output: 09_pairing_sensitivity

# Birthplace and ethnic concentrations are identified separately using Getis Ord statistics, then cross classified.
# Each pairing uses matched spatial scope so population differences are not interpreted as geographic differences.

source("00_environment_setup.r")

panel_cob <- read_derived("04_panel_cob")
panel_eth <- read_derived("04_panel_eth")
zones_sf <- read_geometry("04_zones") %>% arrange(zone)

# Spatial weights

# queen contiguity: a zone with no contiguous neighbour is joined to its nearest zones by centroid distance
spatial_weights <- function(zones, k = island_k) {
  neighbours <- suppressWarnings(poly2nb(zones, queen = TRUE))
  islands_at <- which(card(neighbours) == 0)

  if (length(islands_at) > 0) {
    nearest <- knn2nb(knearneigh(st_coordinates(st_centroid(st_geometry(zones))), k = k))
    for (i in islands_at) neighbours[[i]] <- nearest[[i]]
    neighbours <- make.sym.nb(neighbours)
  }

  list(nb = neighbours,
       listw_gi = nb2listw(include.self(neighbours), style = "B", zero.policy = TRUE),
       islands = zones$zone[islands_at])
}

weights <- spatial_weights(zones_sf)

report(paste("Zones without a contiguous neighbour, joined to", island_k, "nearest zones:",
             paste(weights$islands, collapse = ", ")))

stopifnot("the contiguity relation is not symmetric" = is.symmetric.nb(weights$nb),
          "a zone has no neighbour after repair" = all(card(weights$nb) > 0),
          "repair changed the number of zones" = length(weights$nb) == nrow(zones_sf))

# Hotspot surfaces

# permutation p values are corrected for 34,630 simultaneous tests by the Benjamini and Hochberg method
hotspot_surface <- function(share, listw, alpha = 0.05) {
  set.seed(project_seed)
  getis <- localG_perm(share, listw, nsim = n_permutations, zero.policy = TRUE)
  tibble(gi = as.numeric(getis),
         gi_p = p.adjust(attr(getis, "internals")[, "Pr(z != E(Gi)) Sim"], method = "BH")) %>%
    mutate(hot = gi > 0 & gi_p < alpha)
}

group_surface <- function(panel, groups) {
  shares <- panel %>%
    filter(year == 2021) %>%
    group_by(zone) %>%
    summarise(group_people = sum(people[group %in% groups]), zone_total = sum(people), .groups = "drop") %>%
    right_join(tibble(zone = zones_sf$zone), by = "zone") %>%
    arrange(zone) %>%
    mutate(group_people = replace_na(group_people, 0),
           zone_total = replace_na(zone_total, 0),
           prop = if_else(zone_total > 0, group_people / zone_total, 0))

  bind_cols(select(shares, zone, people = group_people, prop), hotspot_surface(shares$prop, weights$listw_gi))
}

# Pairing schemes

# narrow pairs a birthplace region with its closest single ethnic group
# scope matched widens either side until the two surfaces describe comparable populations
asian_ethnicities <- c("Indian", "Pakistani", "Bangladeshi", "Chinese", "Other Asian", "Arab")

schemes <- tibble(
  scheme = rep(c("Narrow", "Scope matched"), each = 4),
  pair = rep(c("African", "Caribbean", "Asian and Middle Eastern", "EU accession and Other White"), 2),
  cob_groups = list("Africa", "Americas and Caribbean", "Middle East and Asia", "EU accession",
                    "Africa", "Americas and Caribbean", "Middle East and Asia", c("EU14", "EU accession", "Non EU Europe")),
  eth_groups = list("African", "Caribbean", "Pakistani", "Other White",
                    "African", c("Caribbean", "Other Black"), asian_ethnicities, c("Other White", "White Irish"))
)

stopifnot("a birthplace group in the schemes is absent from panel_cob" = all(unlist(schemes$cob_groups) %in% panel_cob$group),
          "an ethnic group in the schemes is absent from panel_eth" = all(unlist(schemes$eth_groups) %in% panel_eth$group))

# a group set used by more than one scheme is permuted once
surface_cache <- new.env(parent = emptyenv())

cached_surface <- function(panel, groups, side) {
  key <- paste(side, paste(sort(groups), collapse = "|"))
  if (!exists(key, envir = surface_cache)) {
    report(paste("Computing", side, "surface:", paste(groups, collapse = ", ")))
    assign(key, group_surface(panel, groups), envir = surface_cache)
  }
  get(key, envir = surface_cache)
}

cross_classify <- function(scheme, pair, cob_groups, eth_groups) {
  cached_surface(panel_cob, cob_groups, "birthplace") %>%
    select(zone, cob_hot = hot, cob_gi = gi, cob_n = people) %>%
    inner_join(cached_surface(panel_eth, eth_groups, "ethnicity") %>% select(zone, eth_hot = hot, eth_gi = gi, eth_n = people),
               by = "zone") %>%
    mutate(scheme = scheme,
           pair = pair,
           type = case_when(cob_hot & eth_hot ~ "Dual concentration",
                            cob_hot & !eth_hot ~ "Birthplace only concentration",
                            !cob_hot & eth_hot ~ "Ethnicity only concentration",
                            TRUE ~ "Neither"))
}

report(paste("Locating concentrations,", n_permutations, "permutations per surface"))

surfaces <- pmap_dfr(schemes, cross_classify)

stopifnot("surfaces do not hold one row per zone and scheme" = nrow(surfaces) == nrow(zones_sf) * nrow(schemes))

# Comparison

pairing_sensitivity <- surfaces %>%
  group_by(scheme, pair) %>%
  summarise(hotspot_zones = sum(type != "Neither"),
            dual = sum(type == "Dual concentration"),
            birthplace_only = sum(type == "Birthplace only concentration"),
            ethnicity_only = sum(type == "Ethnicity only concentration"),
            concordance_percent = round(100 * dual / pmax(hotspot_zones, 1), 1),
            correlation = round(cor(cob_gi, eth_gi, use = "complete.obs"), 3),
            ratio = round(sum(cob_n) / sum(eth_n), 3),
            .groups = "drop"
            ) %>%
  arrange(pair, scheme)

report("Cross classification under both pairing schemes", pairing_sensitivity)

# Save

save_derived(pairing_sensitivity, "09_pairing_sensitivity")

report("Script 9 complete")
