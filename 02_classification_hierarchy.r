# 2. Classification hierarchy
# Input:  01_cob_raw, 01_eth_raw
# Output: 02_cob_tree, 02_eth_tree, 02_cob_resolution

# Census tables nest categories so terminal categories partition the population without double counting

source("00_environment_setup.r")

cob_raw <- read_derived("01_cob_raw")
eth_raw <- read_derived("01_eth_raw")

# Category trees

# labels use colon and dash separators
normalise_path <- function(label) {
  label %>%
    str_replace_all("\\s+-\\s+", ": ") %>%
    str_remove(regex(":\\s*Total$", ignore_case = TRUE)) %>%
    str_squish()
}

# a category is terminal by label when no other category extends its path
build_tree <- function(raw_counts) {
  raw_counts %>%
    distinct(year, category) %>%
    mutate(path = normalise_path(category),
           key = str_to_lower(path),
           depth = str_count(path, ":") + 1L,
           root = str_squish(str_split_i(path, ":", 1))) %>%
    group_by(year) %>%
    mutate(is_leaf = {
             parts <- str_split(key, ":\\s*")
             map_lgl(seq_along(parts), function(i) {
               this <- parts[[i]]
               !any(map_lgl(parts[-i], function(other) length(other) > length(this) && identical(other[seq_along(this)], this)))
             })
           },
           is_total = str_detect(key, "^total|^all categories|^all usual")) %>%
    ungroup()
}

# the arithmetic test overrides the label test: a category whose children sum to it is internal
confirm_leaves <- function(raw_counts, tree) {
  candidates <- filter(tree, !is_total)

  totals <- raw_counts %>%
    semi_join(candidates, by = c("year", "category")) %>%
    group_by(year, category) %>%
    summarise(total = sum(people), .groups = "drop") %>%
    left_join(select(candidates, year, category, key, depth), by = c("year", "category"))

  tested <- totals %>%
    group_by(year) %>%
    group_modify(function(year_totals, ...) {
      year_totals %>%
        mutate(children_sum = map2_dbl(key, depth, function(node_key, node_depth) {
                 children <- filter(year_totals, str_starts(key, fixed(paste0(node_key, ":"))), depth == node_depth + 1L)
                 if (nrow(children) == 0) NA_real_ else sum(children$total)
               }),
               is_internal = !is.na(children_sum) & abs(children_sum - total) <= leaf_tolerance * pmax(total, 1))
    }) %>%
    ungroup()

  reclassified <- tested %>%
    left_join(select(tree, year, category, is_leaf), by = c("year", "category")) %>%
    filter(is_internal, is_leaf)

  report(paste("Categories reclassified as internal by the arithmetic test:", nrow(reclassified)),
         select(reclassified, year, category, total, children_sum), rows = 30)

  tree %>%
    left_join(select(tested, year, category, is_internal), by = c("year", "category")) %>%
    mutate(is_leaf = is_leaf & !coalesce(is_internal, FALSE)) %>%
    select(-is_internal)
}

cob_tree <- confirm_leaves(cob_raw, build_tree(cob_raw))
eth_tree <- confirm_leaves(eth_raw, build_tree(eth_raw))

# Birthplace resolution

cob_leaf_counts <- cob_raw %>%
  semi_join(filter(cob_tree, is_leaf, !is_total), by = c("year", "category")) %>%
  group_by(year, category) %>%
  summarise(people = sum(people), .groups = "drop") %>%
  mutate(leaf = str_squish(str_extract(category, "[^:]+$")),
         residual = str_detect(str_to_lower(leaf), "^other|not otherwise specified"),
         uk_born = str_detect(str_to_lower(category), pattern_uk_born))

cob_resolution <- cob_leaf_counts %>%
  group_by(year) %>%
  summarise(terminal_categories = n(),
            residual_people = sum(people[residual]),
            residual_percent = round(100 * sum(people[residual]) / sum(people), 2),
            .groups = "drop") %>%
  left_join(cob_leaf_counts %>%
              filter(!uk_born) %>%
              group_by(year) %>%
              mutate(share = people / sum(people)) %>%
              summarise(foreign_born_categories = n(),
                        foreign_born_mean_share = round(100 * weighted.mean(share, people), 2),
                        .groups = "drop"),
            by = "year") %>%
  mutate(variable = "Country of birth", .before = year)

report("Birthplace category resolution", cob_resolution)

# Checks

# terminal categories must reconstruct the published total in every area
test_partition <- function(raw_counts, tree) {
  leaf_sum <- raw_counts %>%
    semi_join(filter(tree, is_leaf, !is_total), by = c("year", "category")) %>%
    group_by(year, area_code) %>%
    summarise(leaf_total = sum(people), .groups = "drop")

  published <- raw_counts %>%
    semi_join(filter(tree, is_total), by = c("year", "category")) %>%
    group_by(year, area_code) %>%
    summarise(published = sum(people), .groups = "drop")

  leaf_sum %>%
    inner_join(published, by = c("year", "area_code")) %>%
    group_by(year) %>%
    summarise(areas = n(), exact = sum(leaf_total == published), ratio = sum(leaf_total) / sum(published), .groups = "drop")
}

partition_cob <- test_partition(cob_raw, cob_tree)
partition_eth <- test_partition(eth_raw, eth_tree)

report("Partition test, country of birth", partition_cob)
report("Partition test, ethnic group", partition_eth)

stopifnot("birthplace terminal categories do not sum to the published total" = all(abs(partition_cob$ratio - 1) < leaf_tolerance),
          "ethnic terminal categories do not sum to the published total" = all(abs(partition_eth$ratio - 1) < leaf_tolerance))

# Save

save_derived(cob_tree, "02_cob_tree")
save_derived(eth_tree, "02_eth_tree")
save_derived(cob_resolution, "02_cob_resolution")

report("Script 2 complete")
