# Government data tables frequently export with complex metadata headers, unformatted text strings, and trailing summary rows. Use your growing data wrangling skills to tidy the data into usable format.

# Load packages
library(tidyverse)

# Load data using relative path using "here" package

CatchDataQLDGov <- read_csv(
  here::here("data", "workshop3", "CatchDataQLDGov.csv")
)

# Inspect data

glimpse(CatchDataQLDGov) 
names(CatchDataQLDGov)
head(CatchDataQLDGov)
dim(CatchDataQLDGov)

# This told us that the data has 1,000 rows and 7 columns. The column names are 
# not very informative, and there are some missing values in the data. We must
# wrangle the data to make it more usable.

# Time: Year, month
# Space: Area, BeachName
# Species: CommonName, ScientificName, SpeciesGroup
# Catch records: each row is an individual capture
# Additional ecology: Length(m), Sex, Alive/Deceased, Fate

# How have shark catches changed through time across Queensland regions?

CatchDataQLDGov |> 
  count(Year)

CatchDataQLDGov |>
  count(Area, sort = TRUE)

CatchDataQLDGov |> 
  count(CommonName, sort = TRUE)

# Build annual catch totals

annual_catch <- CatchDataQLDGov |>
  group_by(Year, Area) |>
  summarise(
    catches = n(),
    .groups = "drop"
  )  

# Exploratory plot of annual catches by area

ggplot(
  annual_catch,
  aes(
    x = Year,
    y = catches,
    color = Area
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Annual shark catches by area",
    x = "Year",
    y = "Number of catches"
  ) +
  theme_minimal()

# This plot shows that shark catches have generally declined over time, with 
# some areas showing more pronounced declines than others. Further analysis 
# could explore the reasons behind these trends, such as changes in fishing 
# regulations, environmental factors, or shifts in shark populations.
# Interestingly, there was a spike in catches more recently, especially in the area
# Mackay

# Make a bar plot of total catches by Area

ggplot(
  annual_catch,
  aes(
    x = Area,
    y = catches,
    fill = Area
  )
) +
  geom_bar(stat = "identity") +
  labs(
    title = "Total shark catches by area",
    x = "Area",
    y = "Number of catches"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
 
CatchDataQLDGov |>
  count(Year)

CatchDataQLDGov |>
  count(CommonName, sort = TRUE)

# The species trend is more pronounced, with the top 5 species accounting for 
# a large proportion of the total catches. The regional trend is less pronounced, 
# with catches spread across multiple areas. Therefore, the species trend will 
# likely make the strongest Workshop 3 report.

# Refine my analysis to focus on the species and their trends over time. 
# I will create a new data frame that summarizes the total catches by species 
# and year.

species_year_catches <- CatchDataQLDGov |>
  group_by(CommonName, Year) |>
  summarise(
    catches = n(),
    .groups = "drop"
  )


# Find the top 5 species by total catches and make a new data frame that only includes those species.
top_caught_species <- species_year_catches |>
  group_by(CommonName) |>
  summarise(
    total_catches = sum(catches),
    .groups = "drop"
  ) |>
  arrange(desc(total_catches)) |>
  slice_head(n = 5)


# Exploratory plot of annual catches by species accounting for the top 5 species
ggplot(
  species_year_catches |>
    filter(CommonName %in% top_caught_species$CommonName),
  aes(
    x = Year,
    y = catches,
    color = CommonName
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Annual shark catches by species (top 5)",
    x = "Year",
    y = "Number of catches"
  ) +
  theme_minimal()

# What year had the highest number of catches for each of the top 5 species?
species_year_catches |>
  filter(CommonName %in% top_caught_species$CommonName) |>
  group_by(CommonName) |>
  slice_max(
    order_by = catches,
    n = 1,
    with_ties = FALSE
  ) |>
  arrange(desc(catches))

# What happened in 2025? Why did the catches spike for the top 5 species? 
# Further analysis could explore the reasons behind this spike, such as changes 
# in fishing regulations, environmental factors, or shifts in shark populations.

# Are there any seasonal trends in shark catches? I will create a new data frame 
# that summarizes the total catches by month and year.

seasonal_catches <- CatchDataQLDGov |>
  group_by(Year, MonthName) |>
  summarise(
    catches = n(),
    .groups = "drop"
  )
# Assess seasonal trends in shark catches
ggplot(
  seasonal_catches,
  aes(
    x = MonthName,
    y = catches,
    fill = as.factor(Year)
  )
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Seasonal shark catches by year",
    x = "Month",
    y = "Number of catches"
  ) +
  theme_minimal() +
  scale_fill_discrete(name = "Year")

# Does gear catch different-sized animals?

ggplot(
  CatchDataQLDGov,
  aes(
    x = Gear,
    y = `Length(m)`,
    fill = Gear
  )
) +
  geom_boxplot() +
  theme_classic() +
  labs(
    x = "Gear type",
    y = "Length (m)"
  )

# Does size differ among species?

top_species <- CatchDataQLDGov |>
  count(CommonName, sort = TRUE) |>
  slice_head(n = 5)

CatchDataQLDGov |>
  filter(CommonName %in% top_species$CommonName) |>
  ggplot(
    aes(
      x = reorder(CommonName, `Length(m)`, median),
      y = `Length(m)`,
      fill = CommonName
    )
  ) +
  geom_boxplot() +
  coord_flip() +
  theme_classic() +
  theme(
    legend.position = "none"
  )

# Do males and females differ in size?
CatchDataQLDGov |>
  filter(Sex != "U") |>
  ggplot(
    aes(
      x = Sex,
      y = `Length(m)`,
      fill = Sex
    )
  ) +
  geom_boxplot() +
  theme_classic()

# Is there an interaction between Gear and Species?
CatchDataQLDGov |>
  count(Gear, CommonName) |>
  group_by(Gear) |>
  slice_max(n, n = 10) |>
  ggplot(
    aes(
      x = reorder(CommonName, n),
      y = n,
      fill = Gear
    )
  ) +
  geom_col() +
  coord_flip() +
  facet_wrap(~Gear, scales = "free_y") +
  theme_classic()

# Numerical summary
CatchDataQLDGov |>
  group_by(Gear) |>
  summarise(
    mean_length = mean(`Length(m)`, na.rm = TRUE),
    median_length = median(`Length(m)`, na.rm = TRUE),
    n = n()
  )

CatchDataQLDGov |>
  group_by(CommonName, Sex) |>
  summarise(
    mean_length = mean(`Length(m)`, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
  
# ------------------------------------------------------------------------------
# Research question: Which Queensland regions have experienced the greatest 
# changes in shark catches over time?

# Build the analysis dataset

annual_catch |> 
  distinct(Area)

# There are 12 distinct areas in the dataset. Plotting all of them
# would be messy, so let's find the most active regions.

top_areas <- annual_catch |> 
  group_by(Area) |> 
  summarise(
    total_catches = sum(catches),
    .groups = "drop"
  ) |> 
  arrange(desc(total_catches)) |> 
  slice_head(n = 6)

# Create final plotting dataset
top_area_trends <- annual_catch |> 
  filter(Area %in% top_areas$Area)

# Plot
# Temporal Trends in Shark Catch Records Across Queensland Regions
# Six regions with the highest total catches
ggplot(
  top_area_trends,
  aes(
    x = Year,
    y = catches,
    color = Area
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1) +
  labs(
    x = "Year",
    y = "Number of Recorded Catches",
    colour = "Region"
  ) +
  theme_classic()



library(scales)

theme_marine <- function() {
  
  theme_classic(base_size = 12) +
    
    theme(
      
      plot.title = element_text(
        face = "bold",
        size = 18,
        colour = "#425547",
        hjust = 0.5
      ),
      
      plot.subtitle = element_text(
        colour = "#5C6A54",
        hjust = 0.5
      ),
      
      axis.title = element_text(
        colour = "black",
        size = 14
      ),
      
      axis.text = element_text(
        colour = "black"
      ),
      
      strip.background = element_rect(
        fill = "white",
        colour = "black",
        linewidth = 1
      ),
      
      strip.text = element_text(
        face = "bold",
        colour = "black",
        size = 12
      ),
      
      panel.spacing = unit(1, "lines")
    )
}

# This plot uses facetting to show the trends in each of the top 6 regions 
# separately, making it easier to compare the trends across regions. 

final_plot <- ggplot(
  top_area_trends,
  aes(
    x = Year,
    y = catches
  )
) +
  geom_line(
    colour = "black",
    linewidth = 2
  ) +
  geom_point(
    shape = 24, # open triangle
    size = 1.2,
    stroke = 1,
    fill = "white",
    colour = "black"
  ) +
  facet_wrap(~ Area) +
  labs(
    x = "Year",
    y = "Number of Recorded Catches"
  ) +
  theme_marine()
final_plot


ggsave(
  "outputs/workshop3/qfish_regional_trends.png",
  final_plot,
  width = 12,
  height = 8,
  dpi = 300
)



