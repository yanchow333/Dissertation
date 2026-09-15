# 0.Environment setup

# This file contains the shared packages, file paths, contstants, figure styles and functions

## run renv::restore() and state 'y' to intsall all packages missing, when complete remove line of code

# Runs scripts 00 to 16 in order
# Or Run all scripts 00 to 16 from this script run_all.r

#renv::restore()

packages <- c("here", "tidyverse", "janitor", "conflicted", "scales", "sf", "spdep", "tmap", "igraph",
              "segregation", "cluster", "mclust", "future", "furrr", "patchwork", "ggalluvial")

missing_packages <- setdiff(packages, rownames(installed.packages()))

# If error occurs, please run the packages from renv::restore(), to install
# run renv::restore() and state 'y' to intsall all packages missing, when complete remove line of code

if (length(missing_packages) > 0) stop("missing packages: please run the packages from renv::restore(), to install ",
                                       paste(missing_packages, collapse = ", "), call. = FALSE)

invisible(lapply(packages, library, character.only = TRUE))

conflicts_prefer(here::here, dplyr::select, dplyr::filter, dplyr::lag, dplyr::count, purrr::map, purrr::map_dfr,
                 stats::sd, stats::cor, stats::dist, .quiet = TRUE)

options(dplyr.summarise.inform = FALSE, readr.show_col_types = FALSE, scipen = 999)
plan(multisession, workers = max(1, availableCores() - 1))
tmap_mode("plot")

# Reproducibility

project_seed <- 1234
n_permutations <- 999
n_bootstrap <- 500

set.seed(project_seed)

# Folders

for (folder in c("data/derived", "outputs/figures", "outputs/tables/supplementary")) {
  dir.create(here(folder), recursive = TRUE, showWarnings = FALSE)
}

# Raw inputs found in data/raw

raw_files <- c(cob_2011 = "QS203EW_lsoa_2011.csv",
               cob_2021 = "TS004_lsoa_2021.csv",
               eth_2011 = "KS201EW_lsoa_2011.csv",
               eth_2021 = "TS021_lsoa_2021.csv",
               arrival_2021 = "TS015_lsoa_2021.csv",
               eth_by_cob_2011 = "DC2205EW_msoa_2011.csv",
               eth_by_cob_2021 = "RM010_lsoa_2021.csv",
               lsoa_change = "lookup_lsoa11_lsoa21.csv",
               hierarchy_2021 = "lookup_lsoa21_hierachy.csv",
               hierarchy_2011 = "lookup_lsoa11_msoa11.csv",
               region_2022 = "lookup_lad22_region22.csv",
               lsoa_boundaries_2021 = "LSOA_2021_BGC.gpkg"
)

raw_path <- function(key) here("data/raw", raw_files[[key]])

# published area counts for England and Wales, used to check every import is complete
lsoa_published <- c(`2011` = 34753L, `2021` = 35672L)
msoa_published <- c(`2011` = 7201L, `2021` = 7264L)

# Analysis constants

census_years <- c(2011, 2021)

tolerance <- 1e-6
leaf_tolerance <- 1e-3
retention_tolerance <- 0.001
perturbation_tolerance <- 5
coverage_gap_max <- 1000L

minority_floor <- 0.05
recent_arrival_from <- 2014
generation_zero_max <- 0.3
island_k <- 5
k_range <- 3:8
silhouette_n <- 5000

pattern_uk_born <- "united kingdom|england|scotland|wales|northern ireland|great britain"

# categories whose 2011 to 2021 change from new  response options
revised_categories <- c("Gypsy, Traveller or Roma", "Any other ethnic group")

# English regions coincide with ITL1; the region lookup covers England only, so Wales is added as one unit in script 04
scale_levels <- c(region = "Region and country (ITL1)", lad = "Local authority", msoa = "MSOA", zone = "Neighbourhood")

# Greater London and three metropolitan counties (Sourced from LAD22NM)
conurbation_districts <- list(
  "London" = c("Barking and Dagenham", "Barnet", "Bexley", "Brent", "Bromley", "Camden", "City of London",
               "Croydon", "Ealing", "Enfield", "Greenwich", "Hackney", "Hammersmith and Fulham", "Haringey",
               "Harrow", "Havering", "Hillingdon", "Hounslow", "Islington", "Kensington and Chelsea",
               "Kingston upon Thames", "Lambeth", "Lewisham", "Merton", "Newham", "Redbridge",
               "Richmond upon Thames", "Southwark", "Sutton", "Tower Hamlets", "Waltham Forest",
               "Wandsworth", "Westminster"),
  "West Midlands" = c("Birmingham", "Coventry", "Dudley", "Sandwell", "Solihull", "Walsall", "Wolverhampton"),
  "Greater Manchester" = c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport",
                           "Tameside", "Trafford", "Wigan"),
  "West Yorkshire" = c("Bradford", "Calderdale", "Kirklees", "Leeds", "Wakefield")
)

# Figure style

ink <- "#241A2E"
rule <- "#7A6E85"
land <- "#EFECF2"
border <- "#B7AEC0"
paper <- "#FFFFFF"
font_family <- "Helvetica"

pal_categorical <- c("#3B1D5E", "#B02A78", "#E07B23", "#0F6E68", "#2C6FB5", "#8A8195")
pal_sequential <- c("#F7F0F8", "#E2CBE8", "#C79BD3", "#A868BC", "#8A3FA0", "#6A2183", "#4A1160", "#2C0940")
pal_diverging <- c("#0F6E68", "#5FA79F", "#BBD6D2", "#EDE7EF", "#EBB4CE", "#C9629B", "#8E1B57")
pal_direction <- c(Decrease = "#2E6B45", Increase = "#B0295F")
pal_variable <- c("Country of birth" = "#6A2E8C", "Ethnic group" = "#C9629B")

pal_class <- c("Origin turnover concentration" = "#3B1D5E", "Recent migrant concentration" = "#B02A78",
               "Emergent migrant diversity" = "#E07B23", "Emergent settled diversity" = "#0F6E68",
               "Settled minority concentration" = "#2C6FB5", "Low diversity" = "#CFCAC4")

pal_bivariate <- c("1-1" = "#D6E4F0", "1-2" = "#7FB0D8", "1-3" = "#2C6FB5",
                   "2-1" = "#DCD3E8", "2-2" = "#A98FC4", "2-3" = "#6A3FA0",
                   "3-1" = "#F2CBD9", "3-2" = "#D178A4", "3-3" = "#A81E63")

theme_figure <- function(base = 9) {
  theme_minimal(base_size = base, base_family = font_family) +
    theme(plot.title = element_text(face = "bold", size = base + 1, colour = ink, hjust = 0, margin = margin(b = 10)),
          plot.title.position = "plot",
          plot.subtitle = element_text(size = base - 1, colour = rule, hjust = 0, margin = margin(b = 10)),
          plot.caption = element_text(size = base - 2, colour = rule, hjust = 0, margin = margin(t = 10)),
          plot.caption.position = "plot",
          axis.title = element_text(size = base - 1, colour = rule),
          axis.text = element_text(size = base - 1, colour = rule),
          strip.text = element_text(size = base - 1, colour = ink, hjust = 0, margin = margin(b = 4)),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#E8E4EC", linewidth = 0.3),
          legend.position = "top",
          legend.justification = "center",
          legend.title = element_blank(),
          legend.key.height = unit(9, "pt"),
          plot.margin = margin(10, 12, 8, 12))
}

theme_set(theme_figure())

# Saving and reading

save_derived <- function(object, name) saveRDS(object, here("data/derived", paste0(name, ".rds")))
read_derived <- function(name) readRDS(here("data/derived", paste0(name, ".rds")))

save_geometry <- function(object, name) st_write(object, here("data/derived", paste0(name, ".gpkg")), delete_dsn = TRUE, quiet = TRUE)
read_geometry <- function(name) st_read(here("data/derived", paste0(name, ".gpkg")), quiet = TRUE)

save_figure <- function(plot, name, height, width = 180) {
  path <- here("outputs/figures", paste0(name, ".png"))
  if (inherits(plot, c("tmap", "tmap_arrange"))) {
    tmap_save(plot, path, width = width, height = height, units = "mm", dpi = 300)
  } else {
    ggsave(path, plot, width = width, height = height, units = "mm", dpi = 300, bg = paper)
  }
  invisible(plot)
}

report <- function(title, object = NULL, rows = 10, width = NULL) {
  message("\n", title)
  if (inherits(object, "data.frame")) print(as_tibble(object), n = rows, width = width)
  else if (!is.null(object)) print(object)
  invisible(object)
}

# Shared functions

# areas connected through the 2011 to 2021 correspondence form one connected component, which becomes one unit
harmonise_units <- function(correspondence, code_2011, code_2021, id_format) {
  edges <- correspondence %>%
    drop_na(all_of(c(code_2011, code_2021))) %>%
    transmute(from = paste0("2011_", .data[[code_2011]]), to = paste0("2021_", .data[[code_2021]])) %>%
    distinct()

  membership <- components(graph_from_data_frame(edges, directed = FALSE))$membership

  tibble(node = names(membership), unit = sprintf(id_format, unname(membership))) %>%
    transmute(unit, year = as.integer(str_sub(node, 1, 4)), area_code = str_sub(node, 6))
}

zone_totals <- function(panel) {
  panel %>%
    group_by(year, zone) %>%
    summarise(zone_total = sum(people), .groups = "drop")
}

# dissimilarity and isolation of each group against the rest of the population
# expected isolation is the group's share of the population in the same census year
pairwise_indices <- function(panel, zone_population) {
  year_population <- summarise(zone_population, year_total = sum(zone_total), .by = year)

  panel %>%
    left_join(zone_population, by = c("year", "zone")) %>%
    group_by(year, group) %>%
    summarise(group_total = sum(people),
              other_total = sum(zone_total) - sum(people),
              D = 0.5 * sum(abs(people / sum(people) - (zone_total - people) / other_total)),
              P = sum((people / sum(people)) * (people / zone_total), na.rm = TRUE),
              .groups = "drop") %>%
    left_join(year_population, by = "year") %>%
    mutate(P_expected = group_total / year_total) %>%
    select(year, group, group_total, D, P, P_expected)
}

bootstrap_dissimilarity <- function(panel, zone_population, n_boot = n_bootstrap) {
  map_dfr(sort(unique(panel$year)), function(census_year) {
    counts <- panel %>%
      filter(year == census_year) %>%
      mutate(group = as.character(group)) %>%
      select(zone, group, people) %>%
      pivot_wider(names_from = group, values_from = people, values_fill = 0) %>%
      inner_join(filter(zone_population, year == census_year) %>% select(zone, zone_total), by = "zone") %>%
      arrange(zone)

    zone_total <- counts$zone_total
    count_matrix <- as.matrix(select(counts, -zone, -zone_total))
    n_zones <- nrow(count_matrix)

    replicates <- future_map(seq_len(n_boot), function(i) {
      rows <- sample.int(n_zones, n_zones, replace = TRUE)
      sampled <- count_matrix[rows, , drop = FALSE]
      sampled_total <- zone_total[rows]
      apply(sampled, 2, function(group_counts) {
        other <- sampled_total - group_counts
        if (sum(group_counts) == 0 || sum(other) == 0) return(NA_real_)
        0.5 * sum(abs(group_counts / sum(group_counts) - other / sum(other)))
      })
    }, .options = furrr_options(seed = TRUE))

    draws <- do.call(rbind, replicates)

    tibble(year = census_year,
           group = colnames(draws),
           D_lo = apply(draws, 2, quantile, 0.025, na.rm = TRUE),
           D_hi = apply(draws, 2, quantile, 0.975, na.rm = TRUE),
           D_se = apply(draws, 2, sd, na.rm = TRUE))
  })
}

report("Setup complete")
