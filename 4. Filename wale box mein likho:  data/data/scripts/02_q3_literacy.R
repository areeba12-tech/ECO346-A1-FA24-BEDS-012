

library(dplyr)
library(ggplot2)
person <- read.csv("data/pslm_person_subset.csv")
household <- read.csv("data/pslm_hh_subset.csv")
data <- person %>%
  inner_join(
    household %>%
      select(hhid, hhweight, prov_code, urban_rural),
    by = "hhid"
  )
students <- data %>%
  filter(
    prov_code == 1,
    urban_rural == 1,
    age >= 10,
    age <= 64,
    !is.na(literate),
    !is.na(sex),
    !is.na(hhweight),
    hhweight > 0,
    sex %in% c(1, 2)
  ) %>%
  mutate(
    age_group = cut(
      age,
      breaks = seq(10, 65, by = 5),
      right = FALSE,
      labels = seq(10, 60, by = 5)
    ),
    age_midpoint = as.numeric(as.character(age_group)) + 2,
    gender = ifelse(sex == 1, "Men", "Women")
  )
literacy <- students %>%
  group_by(age_group, age_midpoint, gender) %>%
  summarise(
    literacy_rate =
      sum(hhweight * literate) / sum(hhweight) * 100,
    observations = n(),
    .groups = "drop"
  ) %>%
  mutate(
    small_group = observations < 30
  )
gender_gap <- literacy %>%
  select(age_midpoint, gender, literacy_rate) %>%
  tidyr::pivot_wider(
    names_from = gender,
    values_from = literacy_rate
  ) %>%
  mutate(gap = abs(Women - Men))

largest_gap <- gender_gap %>%
  filter(!is.na(gap)) %>%
  slice_max(gap, n = 1, with_ties = FALSE)
p <- ggplot(
  literacy,
  aes(
    x = age_midpoint,
    y = literacy_rate,
    color = gender,
    group = gender
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(
    aes(shape = small_group),
    size = 2.8,
    stroke = 1
  ) +
  scale_shape_manual(
    values = c("FALSE" = 16, "TRUE" = 1),
    guide = "none"
  ) +
  geom_text(
    data = literacy %>%
      filter(age_midpoint == 62),
    aes(label = gender),
    hjust = -0.15,
    show.legend = FALSE
  ) +
  annotate(
    "text",
    x = largest_gap$age_midpoint,
    y = max(literacy$literacy_rate, na.rm = TRUE),
    label = paste0(
      "Largest gap: ",
      round(largest_gap$gap, 1),
      " percentage points"
    ),
    hjust = 0.5,
    vjust = -0.5,
    color = "black",
    size = 3.5
  ) +
  scale_x_continuous(
    breaks = seq(12, 62, by = 5),
    limits = c(12, 65)
  ) +
  scale_y_continuous(
    limits = c(0, 100)
  ) +
  labs(
    title = "Literacy rates differ by age and sex in rural KP",
    x = "Age-group midpoint (years)",
    y = "Weighted literacy rate (%)",
    caption = paste(
      "Source: PSLM 2019-20.",
      "Sample: Rural Khyber Pakhtunkhwa, ages 10-64.",
      "Weighted by household hhweight.",
      "Hollow points indicate fewer than 30 observations."
    )
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p)
ggsave(
  "figures/q3_literacy.svg",
  plot = p,
  width = 10,
  height = 6
)
