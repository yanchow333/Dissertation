# 3. Category harmonisation
# Input:  01_cob_raw, 01_eth_raw, 02_cob_tree, 02_eth_tree
# Output: 03_cob_grouped, 03_eth_grouped, 03_accession_grouped, 03_crosswalk, 03_retention, 03_composition

# Terminal categories are assigned to analysis groups that exist in both censuses

source("00_environment_setup.r")

cob_raw <- read_derived("01_cob_raw")
eth_raw <- read_derived("01_eth_raw")
cob_tree <- read_derived("02_cob_tree")
eth_tree <- read_derived("02_eth_tree")

cob_leaves <- cob_tree %>% filter(is_leaf, !is_total) %>% select(year, category, path, key, root)
eth_leaves <- eth_tree %>% filter(is_leaf, !is_total) %>% select(year, category, path, key, root)

# Assignment rules

group_levels_cob <- c("UK", "EU14", "EU accession", "Non EU Europe", "Africa", "Middle East and Asia",
                      "Americas and Caribbean", "Oceania and Other")

group_levels_eth <- c("White British", "White Irish", "Gypsy, Traveller or Roma", "Other White",
                      "Mixed White and Black Caribbean", "Mixed White and Black African", "Mixed White and Asian",
                      "Other Mixed", "Indian", "Pakistani", "Bangladeshi", "Chinese", "Other Asian", "African",
                      "Caribbean", "Other Black", "Arab", "Any other ethnic group")

# rules are ordered
# 2021 splits accession states into EU8, EU2 and other EU; all three are recombined to match 2011
classify_birthplace <- function(key) {
  in_eu_branch <- str_detect(key, "eu countries") & !str_detect(key, "non.?eu")

  rule <- case_when(
    str_detect(key, pattern_uk_born) ~ "uk pattern",
    str_detect(key, "guernsey|jersey|alderney|sark") ~ "crown dependency",
    str_detect(key, "^europe: ireland|republic of ireland") ~ "ireland",
    in_eu_branch & str_detect(key, "accession|eu8|eu2") ~ "accession label",
    in_eu_branch & str_detect(key, "member countries in march 2001|eu14") ~ "eu14 label",
    in_eu_branch & str_detect(key, "all other eu countries") ~ "other eu accession",
    str_detect(key, "^europe") ~ "europe residual",
    str_starts(key, "africa") ~ "africa branch",
    str_starts(key, "middle east and asia") ~ "asia branch",
    str_starts(key, "the americas and the caribbean|americas") ~ "americas branch",
    str_starts(key, "antarctica and oceania|oceania|other") ~ "oceania branch",
    str_detect(key, "british overseas") ~ "overseas territory"
  )

  group <- c(`uk pattern` = "UK", `crown dependency` = "Oceania and Other", ireland = "EU14",
             `accession label` = "EU accession", `other eu accession` = "EU accession", `eu14 label` = "EU14",
             `europe residual` = "Non EU Europe", `africa branch` = "Africa", `asia branch` = "Middle East and Asia",
             `americas branch` = "Americas and Caribbean", `oceania branch` = "Oceania and Other",
             `overseas territory` = "Oceania and Other")

  tibble(rule = rule, group = unname(group[rule]))
}

# mixed categories are tested before the single ethnicities they contain
classify_ethnicity <- function(key) {
  group <- case_when(
    str_detect(key, "^english|^white british$|welsh, scottish, northern irish or british") ~ "White British",
    str_detect(key, "^irish$|white irish") ~ "White Irish",
    str_detect(key, "gypsy|traveller|roma") ~ "Gypsy, Traveller or Roma",
    str_detect(key, "^other white|^any other white") ~ "Other White",
    str_detect(key, "white and black caribbean") ~ "Mixed White and Black Caribbean",
    str_detect(key, "white and black african") ~ "Mixed White and Black African",
    str_detect(key, "white and asian") ~ "Mixed White and Asian",
    str_detect(key, "mixed.*other|other mixed") ~ "Other Mixed",
    str_detect(key, "indian") ~ "Indian",
    str_detect(key, "pakistani") ~ "Pakistani",
    str_detect(key, "bangladeshi") ~ "Bangladeshi",
    str_detect(key, "chinese") ~ "Chinese",
    str_detect(key, "asian.*other|other asian") ~ "Other Asian",
    str_detect(key, "african") ~ "African",
    str_detect(key, "caribbean") ~ "Caribbean",
    str_detect(key, "black.*other|other black") ~ "Other Black",
    str_detect(key, "arab") ~ "Arab",
    str_detect(key, "any other ethnic group|other ethnic group: any other") ~ "Any other ethnic group"
  )

  tibble(rule = group, group = group)
}

leaf_key <- function(key) str_squish(str_extract(key, "[^:]+$"))

crosswalk_cob <- cob_leaves %>% bind_cols(classify_birthplace(.$key))
crosswalk_eth <- eth_leaves %>% bind_cols(classify_ethnicity(leaf_key(.$key)))

unmapped_cob <- filter(crosswalk_cob, is.na(group))
unmapped_eth <- filter(crosswalk_eth, is.na(group))

report(paste("Unmapped birthplace categories:", nrow(unmapped_cob)), select(unmapped_cob, year, path), rows = 40)
report(paste("Unmapped ethnic categories:", nrow(unmapped_eth)), select(unmapped_eth, year, path), rows = 40)

# an unmapped terminal category can drop its population without warning
stopifnot(
  "a birthplace terminal category has no analysis group" = nrow(unmapped_cob) == 0,
  "an ethnic terminal category has no analysis group" = nrow(unmapped_eth) == 0,
  "a non White category was assigned to White British" =
    nrow(filter(crosswalk_eth, group == "White British", str_detect(key, "black|asian|mixed|arab|other"))) == 0,
  "a UK country was assigned to a foreign born group" =
    nrow(filter(crosswalk_cob, group != "UK", str_detect(key, pattern_uk_born))) == 0
)

crosswalk <- bind_rows(mutate(crosswalk_cob, variable = "Country of birth"),
                       mutate(crosswalk_eth, variable = "Ethnic group")) %>%
  select(variable, year, category, path, group, rule)

# Aggregation

aggregate_groups <- function(raw_counts, crosswalk, group_levels) {
  raw_counts %>%
    inner_join(select(crosswalk, year, category, group), by = c("year", "category")) %>%
    group_by(year, area_code, group) %>%
    summarise(people = sum(people), .groups = "drop") %>%
    complete(nesting(year, area_code), group = group_levels, fill = list(people = 0)) %>%
    mutate(group = factor(group, levels = group_levels)) %>%
    arrange(year, area_code, group)
}

cob_grouped <- aggregate_groups(cob_raw, crosswalk_cob, group_levels_cob)
eth_grouped <- aggregate_groups(eth_raw, crosswalk_eth, group_levels_eth)

# EU8 and EU2 are published separately in 2021 only, so they are analysed for 2021 alone
classify_accession <- function(key) {
  case_when(str_detect(key, "eu8|poland|lithuania|latvia|estonia|czech|slovak|slovenia|hungary") ~ "EU8",
            str_detect(key, "eu2|romania|bulgaria") ~ "EU2")
}

accession_grouped <- cob_raw %>%
  filter(year == 2021) %>%
  inner_join(select(cob_leaves, year, category, key), by = c("year", "category")) %>%
  mutate(group = classify_accession(key)) %>%
  filter(!is.na(group)) %>%
  group_by(year, area_code, group) %>%
  summarise(people = sum(people), .groups = "drop") %>%
  complete(nesting(year, area_code), group = c("EU2", "EU8"), fill = list(people = 0))

# Population accounting

compare_with_published <- function(raw_counts, tree, grouped, label) {
  published <- raw_counts %>%
    semi_join(filter(tree, is_total), by = c("year", "category")) %>%
    group_by(year, area_code) %>%
    summarise(published = sum(people), .groups = "drop")

  grouped %>%
    group_by(year, area_code) %>%
    summarise(grouped = sum(people), .groups = "drop") %>%
    full_join(published, by = c("year", "area_code")) %>%
    mutate(difference = grouped - published, variable = label)
}

area_accounting <- bind_rows(compare_with_published(cob_raw, cob_tree, cob_grouped, "Country of birth"),
                             compare_with_published(eth_raw, eth_tree, eth_grouped, "Ethnic group"))

national_total <- function(counts, name) {
  counts %>%
    group_by(year) %>%
    summarise("{name}" := sum(people), .groups = "drop")
}

retention <- bind_rows(
  inner_join(national_total(semi_join(cob_raw, cob_leaves, by = c("year", "category")), "source"),
             national_total(cob_grouped, "grouped"), by = "year") %>%
    mutate(variable = "Country of birth"),
  inner_join(national_total(semi_join(eth_raw, eth_leaves, by = c("year", "category")), "source"),
             national_total(eth_grouped, "grouped"), by = "year") %>%
    mutate(variable = "Ethnic group")
) %>%
  mutate(difference = source - grouped, percent_lost = round(100 * difference / source, 5)) %>%
  select(variable, year, source, grouped, difference, percent_lost)

report("Population retained through harmonisation", retention)

# Composition

composition <- bind_rows(mutate(cob_grouped, variable = "Country of birth"),
                         mutate(eth_grouped, variable = "Ethnic group")) %>%
  group_by(variable, year, group) %>%
  summarise(people = sum(people), .groups = "drop") %>%
  group_by(variable, year) %>%
  mutate(percent = round(100 * people / sum(people), 2)) %>%
  ungroup()

report("National composition by analysis group", composition, rows = 60)

# Checks

group_sets <- composition %>%
  group_by(variable) %>%
  summarise(identical = setequal(as.character(group[year == 2011]), as.character(group[year == 2021])), .groups = "drop")

stopifnot("population is not conserved at area level" = all(area_accounting$difference == 0, na.rm = TRUE),
          "an area appears in only one of the published and grouped tables" = !anyNA(area_accounting$difference),
          "population was lost in harmonisation" = all(retention$percent_lost < retention_tolerance),
          "group sets differ between census years" = all(group_sets$identical))

# Save

save_derived(cob_grouped, "03_cob_grouped")
save_derived(eth_grouped, "03_eth_grouped")
save_derived(accession_grouped, "03_accession_grouped")
save_derived(crosswalk, "03_crosswalk")
save_derived(retention, "03_retention")
save_derived(composition, "03_composition")

report("Script 3 complete")
