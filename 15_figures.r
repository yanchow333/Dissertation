# 15 Figures

source("00_environment_setup.r")

crosswalk <- read_derived("03_crosswalk")
indices <- read_derived("05_indices")
detailed_indices_2011 <- read_derived("05_detailed_indices_2011")
district_indices_2021 <- read_derived("05_district_indices_2021")
indices_null <- read_derived("06_indices_null")
scale_decomposition <- read_derived("07_scale_decomposition")
change_decomposition <- read_derived("08_change_decomposition")
pairing_sensitivity <- read_derived("09_pairing_sensitivity")
classified <- read_derived("10_classification")
class_profile <- read_derived("10_class_profile")
arrival_zone <- read_derived("11_arrival_zone")
indices_arrival <- read_derived("13_indices_arrival")
generation_indices <- read_derived("13_generation_indices")
change_class <- read_derived("14_change_class")

zones_sf <- read_geometry("04_zones")
districts_sf <- read_geometry("04_districts")
outline_sf <- read_geometry("04_outline")

concentration_classes <- c("Recent migrant concentration", "Settled minority concentration")
recent_arrival_label <- paste0("Foreign born arriving ", recent_arrival_from, " to 2021")

# Map layout
# the national map fills the centre and the four conurbations the corners, with one shared key beneath
# no map draws its own legend, because an outside legend takes panel width and crops England

conurbations <- names(conurbation_districts)

conurbation_outline <- function(place) {
  districts_sf %>%
    filter(district %in% conurbation_districts[[place]]) %>%
    st_union() %>%
    st_sf(geometry = .)
}

locator_boxes <- map_dfr(conurbations, function(place) st_sf(place = place, geometry = st_as_sfc(st_bbox(conurbation_outline(place))))) %>%
  st_buffer(4000)

locator_labels <- map_dfr(conurbations, function(place) {
  bounds <- st_bbox(conurbation_outline(place))
  st_sf(place = place, geometry = st_sfc(st_point(c(mean(c(bounds$xmin, bounds$xmax)), bounds$ymax + 18000)), crs = st_crs(zones_sf)))
})

# extends the plotted extent downward so the scale bar sits below the geography
extend_bbox_down <- function(shape, fraction = 0.16) {
  bounds <- st_bbox(shape)
  bounds[["ymin"]] <- bounds[["ymin"]] - fraction * (bounds[["ymax"]] - bounds[["ymin"]])
  bounds
}

scalebar_breaks <- function(shape) {
  span_km <- as.numeric(st_bbox(shape)$xmax - st_bbox(shape)$xmin) / 1000
  if (span_km > 300) c(0, 50, 100) else if (span_km > 80) c(0, 10, 20) else if (span_km > 30) c(0, 5, 10) else c(0, 2, 4)
}

map_conurbation <- function(data, column, palette, place) {
  shape <- conurbation_outline(place)
  base <- suppressWarnings(st_intersection(zones_sf, shape))
  overlay <- suppressWarnings(st_intersection(data, shape))

  panel <- tm_shape(base, bbox = extend_bbox_down(shape)) + tm_fill(fill = land)
  if (nrow(overlay) > 0) {
    panel <- panel +
      tm_shape(overlay) +
      tm_polygons(fill = column, fill.scale = tm_scale_categorical(values = unname(palette)), fill.legend = tm_legend_hide(), col_alpha = 0)
  }

  panel +
    tm_shape(shape) + tm_borders(col = ink, lwd = 1.1) +
    tm_scalebar(breaks = scalebar_breaks(shape), position = c("right", "bottom"), text.size = 0.45, color.dark = ink) +
    tm_title(place, size = 0.7, fontface = "bold") +
    tm_layout(frame = FALSE, bg.color = paper, asp = NA, inner.margins = rep(0.02, 4))
}

map_national <- function(data, column, palette) {
  tm_shape(zones_sf, bbox = extend_bbox_down(zones_sf, 0.10)) + tm_fill(fill = land) +
    tm_shape(data) +
    tm_polygons(fill = column, fill.scale = tm_scale_categorical(values = unname(palette)), fill.legend = tm_legend_hide(), col_alpha = 0) +
    tm_shape(outline_sf) + tm_borders(col = border, lwd = 0.3) +
    tm_shape(locator_boxes) + tm_borders(col = "#000000", lwd = 1.4) +
    tm_shape(locator_labels) + tm_text("place", size = 0.5, col = "#000000") +
    tm_scalebar(breaks = c(0, 50, 100), position = c("left", "bottom"), text.size = 0.45, color.dark = ink) +
    tm_compass(type = "arrow", size = 1.1, position = c("left", "top")) +
    tm_layout(frame = FALSE, bg.color = paper, asp = NA, inner.margins = rep(0.02, 4))
}

as_panel <- function(map) wrap_elements(full = tmap_grob(map))

# London sits bottom left
national_with_insets <- function(data, column, palette, key) {
  wrap_plots(A = as_panel(map_conurbation(data, column, palette, "West Midlands")),
             B = as_panel(map_conurbation(data, column, palette, "Greater Manchester")),
             C = as_panel(map_conurbation(data, column, palette, "London")),
             D = as_panel(map_conurbation(data, column, palette, "West Yorkshire")),
             X = as_panel(map_national(data, column, palette)),
             L = key,
             design = "AXXB\nCXXD\nLLLL",
             heights = c(1, 1, 0.42))
}

legend_key <- function(values, ncol = 2) {
  tibble(label = names(values), fill = unname(values)) %>%
    mutate(i = row_number() - 1, col = i %% ncol, row = -(i %/% ncol)) %>%
    ggplot() +
    geom_tile(aes(col, row, fill = fill), width = 0.10, height = 0.55) +
    geom_text(aes(col + 0.08, row, label = label), hjust = 0, size = 2.0, colour = ink) +
    scale_fill_identity() +
    scale_x_continuous(limits = c(-0.1, ncol - 0.05)) +
    coord_cartesian(clip = "off") +
    theme_void()
}
# Figure 3, dissimilarity 2011 against 2021

f3 <- indices %>%
  filter(variable != "Accession cohort") %>%
  select(variable, group, year, D) %>%
  pivot_wider(names_from = year, values_from = D, names_prefix = "D_") %>%
  mutate(revised = group %in% revised_categories) %>%
  ggplot(aes(D_2011, D_2021)) +
  geom_abline(slope = 1, intercept = 0, colour = rule, linetype = "dashed", linewidth = 0.4) +
  geom_point(aes(colour = variable, shape = revised), size = 2.3) +
  geom_text(data = ~ filter(.x, revised), aes(label = group), hjust = -0.12, size = 2.1, colour = rule) +
  scale_colour_manual(values = pal_variable) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), labels = c(`FALSE` = "Comparable", `TRUE` = "Revised in 2021")) +
  scale_x_continuous(limits = c(0.25, 0.75)) +
  scale_y_continuous(limits = c(0.25, 0.75)) +
  coord_equal() +
  labs(title = "Dissimilarity by group, 2011 and 2021", x = "Dissimilarity, 2011", y = "Dissimilarity, 2021")

save_figure(f3, "fig_3_dissimilarity_change", height = 180, width = 150)

# Figure 4, observed against size conditional expected dissimilarity

f4 <- indices_null %>%
  filter(year == 2021, variable != "Accession cohort") %>%
  mutate(group = fct_reorder(as.character(group), D)) %>%
  ggplot(aes(y = group)) +
  geom_segment(aes(x = D_expected, xend = D, yend = group), colour = rule, linewidth = 0.4) +
  geom_point(aes(x = D_expected, fill = "Expected (null model)"), colour = rule, shape = 21, size = 1.7) +
  geom_point(aes(x = D, colour = variable), size = 2.3) +
  scale_colour_manual(name = NULL, values = pal_variable) +
  scale_fill_manual(name = NULL, values = c("Expected (null model)" = rule),
                    guide = guide_legend(override.aes = list(shape = 21, size = 1.7, colour = rule, fill = rule))) +
  scale_x_continuous(limits = c(0, 0.78)) +
  labs(title = "Observed and expected dissimilarity, 2021", x = "Dissimilarity, 2021", y = NULL) +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 6.5))

save_figure(f4, "fig_4_dissimilarity_null", height = 160, width = 145)

# Figure 5, scale decomposition and Shapley components

f5_scale <- scale_decomposition %>%
  filter(target_group == "All groups") %>%
  mutate(year = factor(year)) %>%
  ggplot(aes(year, share, fill = level)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = percent(share, accuracy = 1)), position = position_stack(vjust = 0.5), size = 2.1, colour = paper) +
  facet_wrap(~ variable) +
  scale_fill_manual(values = pal_sequential[c(2, 4, 6, 8)]) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Share of separation added at each level")

shapley_labels <- c(group_marginal = "Population growth", unit_marginal = "Zone composition",
                    structural = "Spatial structure", additions = "Group additions", removals = "Group removals")

f5_shapley <- change_decomposition %>%
  filter(!stat %in% c("M1", "M2", "diff")) %>%
  mutate(stat = recode(stat, !!!shapley_labels),
         stat = fct_reorder(stat, est),
         direction = if_else(est >= 0, "Increase", "Decrease")) %>%
  ggplot(aes(est, stat, fill = direction)) +
  geom_col(width = 0.62) +
  geom_vline(xintercept = 0, colour = ink, linewidth = 0.3) +
  geom_text(aes(label = sprintf("%+.3f", est), hjust = if_else(est >= 0, -0.15, 1.15)), size = 2.1, colour = ink) +
  facet_wrap(~ variable, ncol = 1, scales = "free_x") +
  scale_fill_manual(values = pal_direction, guide = "none") +
  scale_x_continuous(expand = expansion(mult = 0.22)) +
  labs(x = "Contribution to the change in M", y = NULL) +
  theme(panel.grid.major.y = element_blank())

f5 <- (f5_scale | f5_shapley) +
  plot_annotation(title = "Spatial scale of separation and components of change",
                  tag_levels = "a", tag_prefix = "(", tag_suffix = ")", theme = theme_figure())

save_figure(f5, "fig_5_scale_and_change", height = 170, width = 195)

# Figure 6, pairing sensitivity
# each point carries its population ratio, the quantity that explains why concordance moves

f6 <- pairing_sensitivity %>%
  select(pair, scheme, concordance_percent, ratio) %>%
  pivot_wider(names_from = scheme, values_from = c(concordance_percent, ratio)) %>%
  transmute(pair,
            narrow = concordance_percent_Narrow,
            matched = `concordance_percent_Scope matched`,
            narrow_label = sprintf("%.1f%%\nratio %.2f", narrow, ratio_Narrow),
            matched_label = sprintf("%.1f%%\nratio %.2f", matched, `ratio_Scope matched`)) %>%
  mutate(pair = fct_reorder(pair, matched)) %>%
  ggplot(aes(y = pair)) +
  geom_segment(aes(x = narrow, xend = matched, yend = pair), colour = border, linewidth = 0.8,
               arrow = arrow(length = unit(4, "pt"), type = "closed")) +
  geom_point(aes(x = narrow, colour = "Narrow"), size = 2.4) +
  geom_point(aes(x = matched, colour = "Scope matched"), size = 2.4) +
  geom_text(aes(x = narrow, label = narrow_label), hjust = 1.2, size = 2.1, lineheight = 0.9, colour = rule) +
  geom_text(aes(x = matched, label = matched_label), hjust = -0.2, size = 2.1, lineheight = 0.9, colour = ink) +
  scale_colour_manual(values = c(Narrow = border, `Scope matched` = unname(pal_categorical[2]))) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0.02, 0.02))) +
  labs(title = "Effect of scope matching on pairing concordance",
       x = "Clustered zones identified by both surfaces (%)", y = NULL) +
  theme(panel.grid.major.y = element_blank())

save_figure(f6, "fig_6_pairing_sensitivity", height = 125, width = 165)

# Figure 7, settlement classes

f7_zones <- zones_sf %>%
  inner_join(select(classified, zone, class), by = "zone") %>%
  mutate(class = factor(class, levels = names(pal_class)))

f7 <- national_with_insets(f7_zones, "class", pal_class, key = legend_key(pal_class)) +
  plot_annotation(title = "Geographical distribution of the settlement classes (2021)", theme = theme_figure())

save_figure(f7, "fig_7_settlement_classes", height = 200, width = 295)

# Figure 8, class profiles

indicator_labels <- c(fb_2021 = "Foreign born share", me_2021 = "Minority ethnic share", d_fb = "Change in foreign born share",
                      d_me = "Change in minority ethnic share", fgr_2021 = "First generation ratio", turnover = "Origin turnover")

profile_long <- class_profile %>%
  pivot_longer(-class, names_to = "indicator", values_to = "value") %>%
  group_by(indicator) %>%
  mutate(z = as.numeric(scale(value))) %>%
  ungroup() %>%
  mutate(indicator = factor(unname(indicator_labels[indicator]), levels = unname(indicator_labels)),
         class = fct_reorder(class, z, .fun = max))

# the dotted outline marks the first generation ratio of the two concentration classes
profile_highlight <- filter(profile_long, indicator == "First generation ratio", class %in% concentration_classes)

f8 <- ggplot(profile_long, aes(indicator, class, fill = z)) +
  geom_tile(colour = paper, linewidth = 0.8) +
  geom_text(aes(label = sprintf("%+.1f", z)), size = 2.3, colour = ink) +
  geom_tile(data = profile_highlight, fill = NA, colour = "#000000", linetype = "dotted", linewidth = 0.8) +
  scale_fill_gradient2(low = pal_diverging[1], mid = pal_diverging[4], high = pal_diverging[7], midpoint = 0, name = "Standardised score") +
  labs(title = "Settlement classes on their constructing indicators", x = NULL, y = NULL) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1, size = 6.5),
        legend.position = "bottom", legend.title = element_text(size = 7, colour = rule))

save_figure(f8, "fig_8_class_profiles", height = 140, width = 190)

# Figure 9, minority ethnic share against recent arrival share

f9 <- classified %>%
  select(zone, class, me_share_2021) %>%
  inner_join(arrival_zone, by = "zone") %>%
  drop_na(arrival, me_share_2021) %>%
  mutate(highlight = class %in% concentration_classes) %>%
  arrange(highlight) %>%
  ggplot(aes(me_share_2021, arrival)) +
  geom_point(data = ~ filter(.x, !highlight), colour = border, size = 0.35, alpha = 0.25) +
  geom_point(data = ~ filter(.x, highlight), aes(colour = class), size = 0.7, alpha = 0.7) +
  scale_colour_manual(values = pal_class[concentration_classes]) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Minority ethnic share against recent arrival share (2021)",
       x = "Minority ethnic share of neighbourhood population", y = recent_arrival_label)

save_figure(f9, "fig_9_share_against_arrival", height = 190, width = 175)


# Figure 10, recent arrival share by settlement class

set.seed(project_seed)

f10_data <- arrival_zone %>%
  inner_join(select(classified, zone, class), by = "zone") %>%
  drop_na(arrival) %>%
  mutate(class = fct_reorder(class, arrival, .fun = median))

f10 <- ggplot(f10_data, aes(arrival, class)) +
  geom_jitter(data = slice_sample(f10_data, n = 400, by = class), aes(colour = class), height = 0.27, size = 0.65, alpha = 0.30) +
  geom_boxplot(fill = NA, colour = ink, linewidth = 0.35, width = 0.5, outlier.shape = NA) +
  stat_summary(fun = median, geom = "point", colour = ink, size = 2.1) +
  scale_colour_manual(values = pal_class, guide = "none") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Recent arrival share by settlement class (2021)", x = recent_arrival_label, y = NULL) +
  theme(panel.grid.major.y = element_blank())

save_figure(f10, "fig_10_arrival_by_class", height = 150, width = 175)

# Figure 11, change clusters in the four conurbations

f11_zones <- zones_sf %>%
  inner_join(select(change_class, zone, signature), by = "zone") %>%
  mutate(signature = signature %>% str_replace_all("d_entropy", "ethnic diversity") %>% str_replace_all("d_fb", "foreign born share"))

pal_change <- setNames(unname(pal_categorical[c(6, 2, 1)]), levels(factor(f11_zones$signature)))

f11 <- wrap_plots(A = as_panel(map_conurbation(f11_zones, "signature", pal_change, "London")),
                  B = as_panel(map_conurbation(f11_zones, "signature", pal_change, "West Midlands")),
                  C = as_panel(map_conurbation(f11_zones, "signature", pal_change, "Greater Manchester")),
                  D = as_panel(map_conurbation(f11_zones, "signature", pal_change, "West Yorkshire")),
                  L = legend_key(pal_change),
                  design = "AB\nCD\nLL",
                  heights = c(1, 1, 0.35)) +
  plot_annotation(title = "Neighbourhood change clusters, 2011 to 2021", theme = theme_figure())

save_figure(f11, "fig_11_change_clusters", height = 210, width = 200)

# Figure A1, category crosswalk
# categories identical in both censuses and in the analysis group are omitted

a1 <- crosswalk %>%
  filter(variable == "Ethnic group") %>%
  transmute(year, group, category = str_squish(str_extract(path, "[^:]+$"))) %>%
  pivot_wider(names_from = year, values_from = category, values_fn = list) %>%
  unnest(`2011`) %>%
  unnest(`2021`) %>%
  filter(`2011` != group | group != `2021`) %>%
  count(`2011`, group, `2021`, name = "freq") %>%
  ggplot(aes(axis1 = `2011`, axis2 = group, axis3 = `2021`, y = freq)) +
  geom_alluvium(aes(fill = group), alpha = 0.75, width = 0.28) +
  geom_stratum(fill = paper, colour = border, width = 0.28) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.1, colour = ink) +
  scale_x_discrete(limits = c("2011 census category", "Analysis group", "2021 census category"), expand = expansion(mult = 0.06)) +
  scale_fill_manual(values = colorRampPalette(pal_categorical)(18), guide = "none") +
  labs(title = "Census ethnic group categories and analysis groups, 2011 and 2021", x = NULL, y = NULL) +
  theme(panel.grid = element_blank(), axis.text.y = element_blank())

save_figure(a1, "fig_a1_category_crosswalk", height = 230, width = 200)

# Figure A2, change in foreign born and minority ethnic shares

tercile <- function(x) cut(x, quantile(x, c(0, 1/3, 2/3, 1), na.rm = TRUE), labels = 1:3, include.lowest = TRUE)

a2_zones <- zones_sf %>%
  inner_join(transmute(classified, zone, cell = paste(tercile(d_fb), tercile(d_me), sep = "-")), by = "zone") %>%
  filter(!str_detect(cell, "NA"))

bivariate_key <- expand_grid(fb = 1:3, me = 1:3) %>%
  mutate(fill = unname(pal_bivariate[paste(fb, me, sep = "-")])) %>%
  ggplot(aes(fb, me, fill = fill)) +
  geom_tile(colour = paper, linewidth = 0.9) +
  scale_fill_identity() +
  scale_x_continuous(breaks = 1:3, labels = c("Low", "Middle", "High")) +
  scale_y_continuous(breaks = 1:3, labels = c("Low", "Middle", "High")) +
  coord_equal(expand = FALSE) +
  labs(x = "Change in foreign born share", y = "Change in minority\nethnic share") +
  theme(panel.grid = element_blank(), axis.text = element_text(size = 5.5), axis.title = element_text(size = 6),
        plot.margin = margin(2, 2, 2, 2))

a2 <- national_with_insets(a2_zones, "cell", pal_bivariate,
                           key = wrap_plots(plot_spacer(), bivariate_key, plot_spacer(), widths = c(1, 0.8, 1))) +
  plot_annotation(title = "Change in the foreign born and minority ethnic shares, 2011 to 2021", theme = theme_figure())

save_figure(a2, "fig_a2_composition_change", height = 210, width = 295)

# Figure A3, detailed country of birth, 2011

a3 <- detailed_indices_2011 %>%
  slice_max(group_total, n = 30) %>%
  mutate(group = fct_reorder(group, D), thousands = group_total / 1000) %>%
  ggplot(aes(D, group)) +
  geom_segment(aes(x = 0, xend = D, yend = group), colour = border, linewidth = 0.3) +
  geom_point(aes(size = thousands), colour = unname(pal_categorical[1]), alpha = 0.85) +
  geom_text(aes(label = sprintf("%.2f", D)), hjust = -0.6, size = 2.1, colour = ink) +
  scale_size_continuous(range = c(1.2, 5), name = "Thousands of residents", breaks = c(50, 200, 500)) +
  scale_x_continuous(limits = c(0, 0.95), expand = expansion(mult = c(0, 0.06))) +
  labs(title = "Dissimilarity by country of birth, 2011", x = "Dissimilarity", y = NULL) +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 6.5), legend.position = "bottom",
        legend.title = element_text(size = 7, colour = rule))

save_figure(a3, "fig_a3_detailed_birthplace", height = 205, width = 160)


# Figure A4, arrival cohort and generation

a4_cohort <- indices_arrival %>%
  mutate(group = fct_rev(group)) %>%
  ggplot(aes(D, group)) +
  geom_segment(aes(x = 0, xend = D, yend = group), colour = border, linewidth = 0.4) +
  geom_point(size = 2.8, colour = unname(pal_categorical[1])) +
  geom_text(aes(label = sprintf("%.2f", D)), hjust = -0.55, size = 2.3, colour = ink) +
  scale_x_continuous(limits = c(0, 0.6), expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Dissimilarity, 2021", y = NULL) +
  theme(panel.grid.major.y = element_blank())

a4_generation <- generation_indices %>%
  filter(year == 2021) %>%
  select(group, generation, D) %>%
  pivot_wider(names_from = generation, values_from = D) %>%
  drop_na() %>%
  mutate(group = fct_reorder(group, `Later generation` - `First generation`)) %>%
  ggplot(aes(y = group)) +
  geom_segment(aes(x = `First generation`, xend = `Later generation`, yend = group), colour = border, linewidth = 0.7) +
  geom_point(aes(x = `First generation`, colour = "First generation"), size = 2.4) +
  geom_point(aes(x = `Later generation`, colour = "Later generation"), size = 2.4) +
  scale_colour_manual(values = c("First generation" = unname(pal_categorical[3]), "Later generation" = unname(pal_categorical[1]))) +
  labs(x = "Dissimilarity, 2021, harmonised MSOA", y = NULL) +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 6.5))

a4 <- (a4_cohort / a4_generation) +
  plot_layout(heights = c(1, 2.4)) +
  plot_annotation(title = "Dissimilarity by arrival cohort and by generation, 2021",
                  tag_levels = "a", tag_prefix = "(", tag_suffix = ")", theme = theme_figure())

save_figure(a4, "fig_a4_arrival_generation", height = 200, width = 165)

# Figure A5, dissimilarity within local authorities 2021

district_eth <- filter(district_indices_2021, variable == "Ethnic group")

top_districts <- district_eth %>%
  filter(group != "White British") %>%
  group_by(district) %>%
  summarise(minority_population = sum(group_total), .groups = "drop") %>%
  slice_max(minority_population, n = 30) %>%
  pull(district)

a5 <- district_eth %>%
  filter(district %in% top_districts) %>%
  mutate(district = fct_reorder(district, D, .fun = median), group = fct_reorder(as.character(group), D, .fun = median)) %>%
  ggplot(aes(group, district, fill = D)) +
  geom_tile(colour = paper, linewidth = 0.6) +
  scale_fill_viridis_c(option = "rocket", direction = -1, begin = 0.08, end = 0.95, name = "Dissimilarity",
                       limits = c(0, 0.7), oob = squish,
                       guide = guide_colourbar(direction = "horizontal", title.position = "top",
                                               barwidth = unit(80, "pt"), barheight = unit(6, "pt"))) +
  labs(title = "Dissimilarity within the 30 authorities with the largest minority ethnic populations, 2021", x = NULL, y = NULL) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5),
        axis.text.y = element_text(size = 6.5), legend.justification = "left", legend.title = element_text(size = 7, colour = rule))

save_figure(a5, "fig_a5_district_matrix", height = 250, width = 220)

report("Script 15 complete")

