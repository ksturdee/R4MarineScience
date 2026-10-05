
# Load the primary data science framework and Excel import library
library(tidyverse)
library(readxl)
library(here)

# Read in mangrove_data
# mangrove_data <- read_csv(file = here::here("data/workshop1/mangrove_survey_raw.csv"))
# Data contains messy column names which corrupted the data frame.

# Use args within read_csv to skip headers and declare missing flags
mangrove_data <- read_csv(
  here::here("data/workshop1/mangrove_survey_raw.csv"),
  skip = 5,   # Skip the first 5 lines of field notes
  na = c(".", "NA", "9999", "ND", "blank"))  # Convert known text alts to true NA

### Data frame architectures: Tibbles versus legacy tables
# Force a modern tibble to degrade into a legacy base R data frame structure
# reef_cover_log_df <- as.data.frame(reef_cover_log)

# Print the old-style dataframe structure to view
# print(reef_cover_log_df)

# And compare with tibble alternative
# print(reef_cover_log)


###  Wrangling out ecological signals using Palmer Penguins
library(palmerpenguins)
data(penguins)

# Convert the penguin tibble to a legacy base R data frame
penguins_df <- as.data.frame(penguins)

# Compare the base data frame with the original tibble
print(penguins_df)
print(penguins)

# Examine the structure of the dataset - always do this when loading a new dataset!
str(penguins)
glimpse(penguins) # tidyverse version (from dplyr package); maps out the entire architectural anatomy of your dataset

# Generate an exploratory summary matrix
summary(penguins)

# Vertically slice specific morphometric variables by explicit name
morphology_metrics <- select(penguins, species, bill_length_mm, bill_depth_mm, body_mass_g)
glimpse(morphology_metrics)

# Retain a continuous block of attributes using the colon operator
spatial_block <- select(penguins, species:island)

# Discard logistics tracking attributes while preserving everything else using the minus sign
clean_scientific_fields <- select(penguins, -year)

# Isolate observations belonging to a single categorical target group
adelie_cohort <- filter(penguins, species == "Adelie")

# Sift out individuals using continuous numerical boundary thresholds
# Preserves only large penguins whose mass exceeds 4500 grams
heavy_penguins <- filter(penguins, body_mass_g > 4500)

# Combine multiple conditional parameters across separate attributes
# Preserves records matching Gentoo penguins sampled explicitly on Biscoe Island
biscoe_gentoo <- filter(penguins, species == "Gentoo" & island == "Biscoe")

# Sift records matching multiple targeting flags within an explicit set
sub_islands <- filter(penguins, island %in% c("Dream", "Torgersen"))

### Ordering sequences with arrange()
# Sort penguins by ascending body mass (Default setting: Smallest mass first)
lightest_first <- arrange(penguins, body_mass_g)

# Sort penguins in descending sequence using the desc() layout wrapper
heaviest_first <- arrange(penguins, desc(body_mass_g))

# Execute nested sorting criteria: Group by species, then sort by descending bill length
stratified_morphology <- arrange(penguins, species, desc(bill_length_mm))

### The Pipe

penguins_final <- penguins |>
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) |>
  filter(species == "Adelie")
head(penguins_final)

### New attributes with mutate()
# Allows us to calculate new variables based on ones that exist in our raw data

# Calculate a new morphological ratio in our environment
penguin_ratios <- penguins  |> 
  mutate(body_mass_kg = body_mass_g / 1000,   # Convert grams to kilograms
         bill_ratio = bill_length_mm / bill_depth_mm  # Bill ratio
  )
# View your newly engineered variables appended to the far-right columns
glimpse(penguin_ratios)

### Data aggregation and ecological summarisation 
# group_by(), R does not alter the physical appearance of the table. Instead, it
# alters how it interacts with subsequent functions by creating hidden, virtual 
# data buckets based on categorical factor levels.

# Grouping our active memory penguins by species
grouped_penguins <- group_by(penguins, species)
# Notice that the table looks identical, but metadata notes 'Groups: species [3]'
print(grouped_penguins)

# Collapsing the buckets into explicit summary metrics
species_mass_summary <- summarise(grouped_penguins,
                                  mean_mass_g = mean(body_mass_g)
)

print(species_mass_summary)
# However this returns some NA values... must overcome using pipe

# Overcoming the missing value trap using na.rm = TRUE
biological_signal <- penguins %>%
  group_by(species, sex) %>%
  summarise(
    sample_size = n(),                                     # Count total individuals per category
    mean_mass_g = mean(body_mass_g, na.rm = TRUE),         # Calculate mean ignoring missing cells
    sd_mass_g   = sd(body_mass_g, na.rm = TRUE)            # Standard deviation calculation
  )

print(biological_signal)


# 1. Exporting our collapsed summary table as a universal flat text file
write_csv(
  biological_signal,
  here::here("outputs", "penguin_species_mass_summary.csv")
)

# 2. Saving our cleaned morphological cohort table as a native R binary file
saveRDS(morphology_metrics, here::here("outputs", "clean_penguin_morphology_cohort.rds"))

mass_compare_plot <- penguins |>
  group_by(species, island) |>
  summarise(
    mean_mass = mean(body_mass_g, na.rm = TRUE),
    sd_mass = sd(body_mass_g, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  ggplot(aes(x = species, y = mean_mass, colour = island)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_mass - sd_mass, 
                    ymax = mean_mass + sd_mass), 
                width = 0.2) +
  labs(
    title = "Mean Body Mass by Species and Island",
    subtitle = "Error bars represent standard deviation",
    y = "Mean Body Mass (g)",
    x = "Species",
    colour = "Island"
  ) +
  scale_colour_manual(
    values = c(
      "darkgreen",
      "saddlebrown",
      "goldenrod"
    )
  ) + 
  theme_minimal() +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.background = element_rect(
      fill = "grey95",
      color = "grey70"
    ),
    legend.key = element_rect(
      fill = "grey95",
      color = NA
    ),
    legend.position = "right",
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      color = "black"
    )
  )

mass_compare_plot

ggsave("outputs/mass_compare_plot.png", 
       plot = mass_compare_plot, 
       width = 120, height = 120, 
       units = "mm", dpi = 300)




