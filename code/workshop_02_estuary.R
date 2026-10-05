# Load required packages
library(tidyverse)
library(r4ds.tutorials)

# Pivoting data to be longer in order to tidy it
df1 <- tribble(
  ~id,  ~bp1, ~bp2,
  "A",  100,  120,
  "B",  140,  115,
  "C",  120,  125
)

# But we want three variables: id, bp, and value. We can use pivot_longer() to achieve this.

df1 |> 
  pivot_longer(
    cols = bp1:bp2,
    names_to = "measurement",
    values_to = "value"
  )

# Widening datasets if an observation is scattered across multiple rows. We can use pivot_wider() to achieve this.

cms_patient_experience

cms_patient_experience |> 
  distinct(measure_cd, measure_title)

cms_patient_experience |> 
  pivot_wider(
    id_cols = starts_with("org"),
    names_from = measure_cd,
    values_from = prf_rate
  )

# A simpler example

df2 <- tribble(
  ~id, ~measurement, ~value,
  "A",        "bp1",    100,
  "B",        "bp1",    140,
  "B",        "bp2",    115, 
  "A",        "bp2",    120,
  "A",        "bp3",    105
)

# We’ll take the names from the measurement column using the names_from() argument and the values from the value column using the values_from() argument:

df2 |> 
  pivot_wider(
    names_from = measurement,
    values_from = value
  )

df2 |> 
  distinct(measurement) |> 
  pull()

df2 |> 
  select(-measurement, -value) |> 
  distinct()

# pivot_wider() then combines these results to generate an empty dataframe

df2 |> 
  select(-measurement, -value) |> 
  distinct() |> 
  mutate(x = NA, y = NA, z = NA)

# Pivoting Exercises using Palmer Penguins
# We want to create a single plot that shows the distribution of all morphometric measurements (bill length, bill depth, flipper length, and body mass) for our penguins

# Open palmer penguins package
penguins <- read.csv("data/penguins.csv")

# Create a long version of the penguins dataset

penguins_long <- penguins |>
  pivot_longer(
    cols = c(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g),
    names_to = "measurement_type",
    values_to = "value"
  )
# View the result
head(penguins_long)

# This longer format is more suitable for plotting with ggplot2, as it allows us to easily facet the data by measurement type and visualize the distributions of each measurement.
penguins_long |>
  drop_na(value) |>
  ggplot(aes(x = value, fill = species)) +                    
  geom_histogram(bins = 30, alpha = 0.7, colour = "black") +
  facet_wrap(~ measurement_type, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Morphometric distributions across penguin species",
    x = "Measurement value",
    y = "Frequency"
  )

# Calculate summary statistics for each measurement type and species combination
mass_summary <- penguins |>
  drop_na(body_mass_g) |>
  group_by(species, island) |>
  summarise(mean_mass = mean(body_mass_g))

head(mass_summary)

# For publication, we can pull the island names up for headers
mass_matrix <- mass_summary |>
  pivot_wider(
    names_from = island,
    values_from = mean_mass
  )

head(mass_matrix)

# Can split rate column into to variables: 1) cases and 2) population in table3
table3 %>% 
  separate(rate, into = c("cases", "population"))

# This makes cases and population "character" data but they are actually numbers so we must change this
table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE)
# This makes cases and population "numeric" data, which is what we want. 
# You can also use seperate() to split a column into more than two columns. 
# This arrangement can, for example, separate the last two digits of each year.

table3 %>% 
  separate(year, into = c("century", "year"), sep = 2)

# Unit() is the opposite of seperate() and combines multiple columns into one.
table5 %>% 
  unite(new, century, year, sep = "")

# Wrangling strings and dates
# Standardising text with stringr

# Example of messy data
messy_sites <- tibble(
  site_id = c("Nelly Bay", "nelly_bay", "NELLY BAY", "Geoffrey_Bay ", "geoffrey bay")
)

messy_sites

# Using stringr within mutate to standardize the text
clean_sites <- messy_sites |>
  mutate(
    # 1. Convert everything to lowercase
    site_clean = str_to_lower(site_id),
    # 2. Replace any spaces with underscores
    site_clean = str_replace_all(site_clean, pattern = " ", replacement = "_"),
    # 3. Trim any leading or trailing whitespace (invisible spaces at the ends)
    site_clean = str_trim(site_clean)
  )

print(clean_sites)

# Mastering time with lubridate

library(lubridate)

# Parsing different date formats
date_1 <- dmy("25/12/2026")
date_2 <- ymd("2026-12-25")

# R now recognizes these as identical Date objects
date_1 == date_2


# A tibble of raw sensor data with a messy character timestamp
sensor_data <- tibble(
  raw_time = c("14-05-2026 08:30:00", "14-05-2026 08:45:00", "14-05-2026 09:00:00"),
  temperature = c(24.5, 24.6, 24.4)
)

# Converting character strings to true POSIXct datetime objects
sensor_clean <- sensor_data |>
  mutate(
    true_time = dmy_hms(raw_time)
  )

print(sensor_clean)

# Joining tables (using two tables to demonstrate)

# Table 1: Biological observation data
observations <- tibble(
  site_code = c("NB", "GB", "MI", "NB", "HB"),
  species = c("Trout", "Snapper", "Trout", "Cod", "Trout"),
  count = c(5, 2, 1, 3, 8)
)

# Table 2: Spatial metadata
site_metadata <- tibble(
  site_code = c("NB", "GB", "MI", "RP", "WP"),
  zone = c("Marine National Park", "Conservation Park", "Habitat Protection", "General Use", "Other Use"),
  lat = c(-19.16, -19.15, -19.14, -19.12, -19.11)
)

# Must use left_ and right_join()
# left_join()keeps all the rows from your left table (observations) and matches data from your right table (site_metadata) wherever the key matches.
# Joining metadata to our observations
joined_data <- observations |>
  left_join(site_metadata, by = join_by(site_code))

print(joined_data)

# strict filter: inner_join() keeps only the rows that have matching keys in both tables.
matched_data <- observations |>
  inner_join(site_metadata, by = join_by(site_code))
glimpse(matched_data)

# Anti_join does the opposite of inner_join() and returns only the rows from your left table that do not have a match in your right table.

# Which observations are missing from our metadata dictionary?
missing_context <- observations |>
  anti_join(site_metadata, by = join_by(site_code))
missing_context

# Handling missing values
# Raw data from an old temperature logger
logger_data <- tibble(
  depth_m = c(10, 20, 30, 40),
  temp_c = c(24.5, 24.1, -999, 23.5)) # -999 is a known sensor error code

# Convert the -999 error codes to true NA values
fixed_logger <- logger_data |>
  mutate(temp_c = na_if(temp_c, -999))

print(fixed_logger)

# Replace missing values with coaesce()

# Simulate some count data
shark_counts <- tibble(
  site = c("Reef_A", "Reef_B", "Reef_C"),
  shark_count = c(3, NA, 5)) # The NA here actually means 0 sharks were seen

# Replace NA with 0 
shark_fixed <- shark_counts |>
  mutate(shark_count = coalesce(shark_count, 0))

print(shark_fixed)

# NaN means Not a Number where a value is mathematically undefined, such as 0/0. NA means Not Available, which is used to represent missing data.

cpue_data <- tibble(
  site = c("Bay_1", "Bay_2"),
  catch = c(10, 0),
  effort_hours = c(2, 0))

# Calculating CPUE (catch / effort)
cpue_calc <- cpue_data |>
  mutate(
    cpue = catch / effort_hours)

print(cpue_calc)
# R treats NaN as NA (can be removed with na.omit() or drop_na()) but does not treat NA as NaN.)


# Implicit missing values and the zero-data framework: complete()
raw_catch <- tibble(
  site = c("Reef_1", "Reef_1", "Reef_2"),
  species = c("Pmaculatus", "Pleopardus", "Pmaculatus"),
  count = c(5, 2, 8))
raw_catch
# We can use tidyr::complete() to build a zero-data framework
# Force inclusion of hidden zero-catch data
full_catch_matrix <- raw_catch |>
  complete(site, species, fill = list(count = 0))

print(full_catch_matrix)

# The nuclear option: drop_na() if you want to remove the entire row
sensor_log <- tibble(
  day = 1:4,
  salinity = c(35.2, 35.1, NA, 35.3)
)

# Remove any row where salinity is missing
clean_log <- sensor_log |>
  drop_na(salinity)
clean_log

## Practice Exercises: PENGUINS, PIVOTS, AND JOINING DATA
# Make sure packages are loaded
library(tidyverse)
library(palmerpenguins)

# The messy metadata provided by a colleague
island_metadata <- tibble(
  island_name = c(" biscoe", "Dream ", "Torgersen"),
  station_install = c("15/01/2003", "22-03-2004", "05/11/2001"),
  latitude = c(-64.81, -64.73, -64.76)
)

print(island_metadata)

# Create a new object called clean_metadata. Using mutate(), stringr, and lubridate, fix the island_name column so that it perfectly matches the island column in the penguins dataset (hint: watch out for those hidden spaces!). Then, convert the station_install column into a proper Date object.

clean_metadata <- island_metadata |>
  mutate(
    island_name = str_trim(island_name),
    island_name = str_to_title(island_name),
    island_name = str_replace_all(island_name, pattern = " ", replacement = ""),
    station_install = dmy(station_install), # Use dmy() to parse dates in day-month-year format
  )

clean_metadata

# Exercise 2: Relational join
penguins_spatial <- penguins |>
  left_join(clean_metadata, 
            by = join_by(island == island_name))

head(penguins_spatial)

# Exercise 3: Wide summary matrix
# A collaborator has asked for a clean, publication-ready table showing the maximum body mass recorded for each species, separated by island.

# Create the wide summary matrix and drop missing values
summary_matrix <- (penguins_spatial |>
  drop_na(body_mass_g) |>
  group_by(species, island) |>
  summarise(
    max_body_mass = max(body_mass_g), 
    .groups = "drop")
)

summary_matrix

# Pivot the dataset wider so that the species form the rows, and the islands form the columns.
pivot_summary <- summary_matrix |>
  pivot_wider(
    names_from = island, 
    values_from = max_body_mass
    )
pivot_summary


## Keystone exercise: estuary fish survey ##
# estuary_catch_log.xlsx: Handwritten field counts of fish species (using common names), spread across multiple spreadsheet tabs.
# estuary_metadata.csv: The spatial coordinates and estuary zones for each site.
# estuary_sonde_data.csv: High-frequency water quality sensor readings.
# species_dictionary.csv: A taxonomic lookup table linking local common names to their accepted scientific names

# Task: write a single, reproducible R script (or .qmd) that ingests this mess, cleans it, joins it into a single master dataset, and produces a publication-ready visualisation of the estuary's ecology.









