
library(dplyr)
library(ggplot2)

person <- read.csv("data/pslm_person_subset.csv")
household <- read.csv("data/pslm_hh_subset.csv")

data <- person %>%
  inner_join(
    household %>% select(hhid, hhweight, prov_code, urban_rural),
    by = "hhid"
  )

students <- data %>%
  filter(
    prov_code == 1,
    urban_rural == 1,
    !is.na(age),
    !is.na(ed_total),
    age >= 10,
    ed_total >= 0
  )

p <- ggplot(students, aes(x = age, y = ed_total)) +
  geom_point(alpha = 0.15, color = "steelblue") +
  geom_smooth(method = "loess", se = TRUE, color = "darkred") +
  coord_cartesian(ylim = c(0, quantile(students$ed_total, 0.95))) +
  labs(
    title = "Age and Education Spending in Rural KP",
    subtitle = "Points show individuals; curve shows the overall trend",
    x = "Age (years)",
    y = "Annual education spending",
    caption = "Source: PSLM subset. Values above the 95th percentile are hidden for readability."
  ) +
  theme_minimal()

dir.create("figures", showWarnings = FALSE)
ggsave("figures/q4_redesigned.svg", p, width = 8, height = 5)
print(p)
