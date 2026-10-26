library(dplyr)
library(ggplot2)
person <- read.csv("data/pslm_person_subset.csv")
data <- person %>%
  inner_join(
    household %>% select(hhid, hhweight, prov_code, urban_rural),
    by = "hhid"
  )
students <- data %>%
  filter(
    prov_code == 1,
    urban_rural == 1,
    !is.na(ed_total),
    ed_total > 0,
    !is.na(hhweight),
    hhweight > 0
  )
weighted_quantile <- function(x, w, p) {
  valid <- !is.na(x) & !is.na(w) & w > 0
  x <- x[valid]
  w <- w[valid]

  ord <- order(x)
  x <- x[ord]
  w <- w[ord]

  x[which(cumsum(w) >= p * sum(w))[1]]
}
w_median <- weighted_quantile(
  students$ed_total, students$hhweight, 0.50
)

w_p10 <- weighted_quantile(
  students$ed_total, students$hhweight, 0.10
)

w_p90 <- weighted_quantile(
  students$ed_total, students$hhweight, 0.90
)

u_median <- median(students$ed_total)
plot_data <- students %>%
  filter(ed_total >= 100, ed_total <= 1000000) %>%
  mutate(log_spending = log10(ed_total))

excluded <- nrow(students) - nrow(plot_data)
cat("Number of students:", nrow(students), "\n")
cat("Weighted median (Rs):", round(w_median), "\n")
cat("Unweighted median (Rs):", round(u_median), "\n")
cat("Weighted 10th percentile (Rs):", round(w_p10), "\n")
cat("Weighted 90th percentile (Rs):", round(w_p90), "\n")
cat("P90/P10 ratio:", round(w_p90 / w_p10, 2), "\n")
cat("Students excluded from histogram:", excluded, "\n")

p <- ggplot(
  plot_data,
  aes(x = log_spending, weight = hhweight)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    breaks = seq(2, 6, by = 0.1),
    closed = "left",
    fill = "grey75",
    color = "white"
  ) +
  geom_vline(
    xintercept = log10(w_median),
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = log10(w_p90),
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = log10(w_median),
    y = Inf,
    label = paste("Weighted median: Rs", round(w_median)),
    angle = 90,
    vjust = 1.3,
    hjust = 1,
    size = 3
  ) +
  annotate(
    "text",
    x = log10(w_p90),
    y = Inf,
    label = paste("Weighted P90: Rs", round(w_p90)),
    angle = 90,
    vjust = 1.3,
    hjust = 0,
    size = 3
  ) +
  scale_x_continuous(
    breaks = 2:6,
    labels = c("100", "1,000", "10,000",
               "100,000", "1,000,000"),
    limits = c(2, 6)
  ) +
  labs(
    title = "Education spending varies across rural KP students",
    x = "Annual education spending (rupees)",
    y = "Density per log10 unit",
    caption = paste0(
      "Source: PSLM 2019-20. Rural students in Khyber Pakhtunkhwa; ",
      "ed_total > 0. Weighted by household hhweight. ",
      excluded, " students outside the histogram range excluded."
    )
  ) +
  theme_minimal()

print(p)

ggsave(
  "figures/q2_histogram.svg",
  plot = p,
  width = 10,
  height = 6
)
 Figure Q2 is generated and saved as an SVG for the assignment.
