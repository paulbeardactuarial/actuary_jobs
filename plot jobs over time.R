
library(tidyverse)
library(zoo)
library(lubridate)
library(patchwork)

jobs_data_raw <- readr::read_csv("./wayback_job_stats_summary.csv")
retro_jobs_data_raw <- readr::read_csv("./legacy_wayback_job_stats_summary.csv")

# some of the datapoints are bunched together. The code applies a cooling period of `x` days 
# to prevent counting too many similar snapshots. 
# Note actuaryjobs posts expire after 28 days by default
days_cool_off_after_data <- 45

# manually create a scrape of the latest job posting numbers
latest_data <-
  tibble::tibble(
    date = as.Date("2025-06-07"), 
    permanent_jobs = 371, 
    interim_jobs = 19
  )


raw_fields_req <- c("date", "permanent_jobs", "interim_jobs")

data_raw <-
  rbind(
    jobs_data_raw |> dplyr::select(all_of(raw_fields_req)), 
    retro_jobs_data_raw |> dplyr::select(dplyr::all_of(raw_fields_req)),
    latest_data
    )

jobs_data_clean <- 
data_raw |> 
  janitor::clean_names() |> 
  dplyr::filter(!is.na(permanent_jobs)) |> 
  dplyr::select(date, permanent_jobs, interim_jobs) |> 
  tidyr::pivot_longer(cols = c("permanent_jobs", "interim_jobs"), names_to = "type") |> 
  dplyr::mutate(
    type = stringr::str_remove(type, "_jobs"),
    date = as.Date(date, tryFormats = "%d/%m/%Y")
    ) |> 
  unique() |> 
  # thin out overly dense sections by having `days_cool_off_after_data` cooling period after each date point
  dplyr::arrange(type, date) |> 
  dplyr::mutate(
    fresh_date = purrr::accumulate(
      .x = date, 
      .f = function(x, y) {
        if(x %m+% days(days_cool_off_after_data) > y) {x} else {y}
      }),
    .by = type
  ) |>
  dplyr::filter(date == fresh_date) |> 
  dplyr::select(-fresh_date)

# interim plot

plot_type <- "interim, contract & temp"

arrow_y_vals <- c(90, 110)
sii_date_line <- c("2016-01-01", "2018-01-01")
covid_date_line <- c("2020-03-23", "2022-03-23")
data_color <- "forestgreen"

p_interim <-
jobs_data_clean |> 
  filter(type == "interim") |> 
  # get rolling mean
  mutate(mean_postings = rollmedian(value, k = 5, align = "center", fill = NA)) |> 
  ggplot() +
  aes(x = date, y = value, color = type) +
  geom_point(color = data_color, size = 2) +
  geom_line(mapping = aes(y = mean_postings), color = data_color) +
  scale_y_continuous( 
    name = "No. of Job Postings",
    limits = c(0, NA)
    ) +
  theme_bw() +
  geom_vline(
    linetype = 2,
    xintercept = as.Date("2020-03-23")
    ) +
  geom_vline(
    linetype = 2,
    xintercept = as.Date("2016-01-01")
    ) +
  annotate(
    "line",
    x = as.Date(covid_date_line),
    y = arrow_y_vals,
    arrow = arrow(length = unit(0.1, "inches"), ends = "first")
  ) +
  annotate(
    "text",
    x = max(as.Date(covid_date_line)),
    y = arrow_y_vals[2] * 1.1,
    size = 3,
    label = "First Covid 
lockdown"
  ) +
  annotate(
    "line",
    x = as.Date(sii_date_line),
    y = arrow_y_vals,
    arrow = arrow(length = unit(0.1, "inches"), ends = "first")
  ) +
  annotate(
    "text",
    x = max(as.Date(sii_date_line)) %m+% months(2),
    y = arrow_y_vals[2] * 1.1,
    size = 3,
    label = glue::glue(
      "Solvency II 
      comes into effect"
    )
  ) +
  scale_x_date(
    name = "",
    date_breaks = "1 year", 
    limits = as.Date(c(NA, "2026-01-01"))) +
  labs(
    title = glue::glue(tools::toTitleCase(plot_type), " job postings"),
    subtitle = "As found on https://www.theactuaryjobs.com/jobs/"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = data_color, face = "bold", size = 20),
    plot.subtitle = element_text(color = data_color, face = "bold", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_blank()
  )  

        

# permanent plot

plot_type <- "permanent"

arrow_y_vals <- c(350, 200)
sii_date_line <- c("2016-01-01", "2018-01-01")
covid_date_line <- c("2020-03-23", "2022-03-23")
data_color <- "violet"

p_perm <-
jobs_data_clean |> 
  filter(type == plot_type) |> 
  # get rolling mean
  mutate(mean_postings = rollmedian(value, k = 5, align = "center", fill = NA)) |> 
  ggplot() +
  aes(x = date, y = value) +
  geom_point(color = data_color, size = 2) +
  geom_line(mapping = aes(y = mean_postings), color = data_color) +
  scale_y_continuous( 
    name = "No. of Job Postings",
    limits = c(0, NA)
  ) +
  theme_bw() +
  geom_vline(
    linetype = 2,
    xintercept = as.Date("2020-03-23")
  ) +
  geom_vline(
    linetype = 2,
    xintercept = as.Date("2016-01-01")
  ) +
  annotate(
    "line",
    x = as.Date(covid_date_line),
    y = arrow_y_vals,
    arrow = arrow(length = unit(0.1, "inches"), ends = "first")
  ) +
  annotate(
    "text",
    x = max(as.Date(covid_date_line)),
    y = arrow_y_vals[2] * 0.64,
    size = 3,
    label = "First Covid 
lockdown"
  ) +
  annotate(
    "line",
    x = as.Date(sii_date_line),
    y = arrow_y_vals,
    arrow = arrow(length = unit(0.1, "inches"), ends = "first")
  ) +
  annotate(
    "text",
    x = max(as.Date(sii_date_line)) %m+% months(2),
    y = arrow_y_vals[2] * 0.64,
    size = 3,
    label = glue::glue(
      "Solvency II 
      comes into effect"
    )
  ) +
  scale_x_date(
    name = "",
    date_breaks = "1 year", 
    limits = as.Date(c(NA, "2026-01-01"))) +
  labs(
    title = tools::toTitleCase(plot_type)
  ) +
  labs(
    title = glue::glue(tools::toTitleCase(plot_type), " job postings"),
    subtitle = "As found on https://www.theactuaryjobs.com/jobs/"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = data_color, face = "bold", size = 20),
    plot.subtitle = element_text(color = data_color, face = "bold", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_blank()
  )  
p_perm

ggsave("permanent.png", p_perm, dpi = 1000, width = 6, height = 3.75, units = "in")
ggsave("interim.png", p_interim, dpi = 1000, width = 6, height = 3.75, units = "in")


