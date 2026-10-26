
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
  geom_point(alpha = 0.3) +
  labs(
    title = "Age and Annual Education Spending in Rural KP",
    x = "Age (years)",
    y = "Annual education spending"
  ) +
  theme_minimal()

dir.create("figures", showWarnings = FALSE)

ggsave("figures/q4_default.svg", p, width = 8, height = 5)

print(p)
