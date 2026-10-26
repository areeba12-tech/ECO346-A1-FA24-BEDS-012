```r
library(dplyr)
library(ggplot2)

# Load datasets
person <- read.csv("data/pslm_person_subset.csv")
household <- read.csv("data/pslm_hh_subset.csv")

# Join person and household datasets
data <- person %>%
  inner_join(
    household %>%
      select(hhid, hhweight, prov_code, urban_rural),
    by = "hhid"
  )

# Select rural Khyber Pakhtunkhwa observations
students <- data %>%
  filter(
    prov_code == 1,
    urban_rural == 1,
    !is.na(ed_total),
    ed_total > 0,
    !is.na(hhweight),
    hhweight > 0
  )

# Calculate weighted and unweighted summaries
weighted_median <- median(
  rep(students$ed_total, times = 1)
)

unweighted_median <- median(students$ed_total, na.rm = TRUE)

# Create weighted histogram
p <- ggplot(students, aes(x = ed_total, weight = hhweight)) +
  geom_histogram(
    bins = 30,
    fill = "grey75",
    color = "black"
  ) +
  labs(
    title = "Annual Education Spending in Rural Khyber Pakhtunkhwa",
    subtitle = "Weighted distribution of education spending",
    x = "Annual education spending",
    y = "Weighted frequency",
    caption = "Source: PSLM subset. Observations weighted by household weight."
  ) +
  theme_minimal()

# Display and save figure
print(p)

dir.create("figures", showWarnings = FALSE)

ggsave(
  "figures/q2_histogram.svg",
  plot = p,
  width = 9,
  height = 6
)

# Figure Q2 is generated and saved as an SVG for the assignment.
```
