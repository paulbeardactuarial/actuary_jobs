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
        if (x %m+% days(days_cool_off_after_data) > y) {
          x
        } else {
          y
        }
      }
    ),
    .by = type
  ) |>
  dplyr::filter(date == fresh_date) |>
  dplyr::select(-fresh_date) |> 
  dplyr::rename(job_count = value)


# =====================================================================
# ======================== construct a rolling mean ===================
# =====================================================================

job_types <- c("interim", "permanent")

mean_jobs_df <-
  tidyr::expand_grid(
    date = seq(
      from = min(jobs_data_clean$date),
      to = max(jobs_data_clean$date),
      by = "1 month"
    ) |> floor_date(unit = "months"),
    type = job_types
  )

# adding a rolling mean for `job_count`. 
# The value will be the mean of `jobs_data_clean$job_count` where the date is within +-6 months of that point
# In cases where there are less than 3 data points, the value of `job_count` is converted to NA
mean_jobs_df <-
  mean_jobs_df |>
  dplyr::mutate(
    job_count_stats = purrr::map2(
      .x = mean_jobs_df$date,
      .y = mean_jobs_df$type,
      .f = function(x, y) {
        jobs_data_clean |>
          dplyr::filter(type == y) |>
          dplyr::filter(date |> dplyr::between(x %m+% months(-6), x %m+% months(6))) |>
          dplyr::summarise(
            job_count = mean(job_count),
            data_points = dplyr::n()
          )
      }
    )
  ) |>
  tidyr::unnest(job_count_stats) |>
  dplyr::mutate(
    job_count = dplyr::if_else(data_points < 3, NA, job_count)
  )

# pull a range (should be vector of 2) for the limited data bit (is arouund 2018/2019)
limited_data <-
  mean_jobs_df |>
  dplyr::filter(type %in% job_types) |>
  dplyr::filter(is.na(job_count)) |>
  dplyr::pull(date) |>
  range()

# =====================================================================
# ================== construct vertical event lines ===================
# =====================================================================

add_vertical_dashed_lines_for_events <- function(arrow_y_vals, wt = 1.1) {
  
  # dates of dashed lines
  sii_date_line <- c("2016-01-01", "2014-01-01")
  covid_date_line <- c("2020-03-23", "2021-09-23")
  ifrs17_date_line <- c("2023-01-01", "2025-01-01")
  
  # create list, for adding layers to plot
  plot_layers <-
  list(
    geom_vline(
      linetype = 2,
      xintercept = as.Date("2023-01-01")
    ),
    geom_vline(
      linetype = 2,
      xintercept = as.Date("2020-03-23")
    ),
    geom_vline(
      linetype = 2,
      xintercept = as.Date("2016-01-01")
    ),
    annotate(
      "line",
      x = as.Date(covid_date_line),
      y = arrow_y_vals,
      arrow = arrow(length = unit(0.1, "inches"), ends = "first")
    ),
    annotate(
      "text",
      x = max(as.Date(covid_date_line)),
      y = arrow_y_vals[2] * wt,
      size = 3,
      label = "First Covid
lockdown"
    ),
    annotate(
      "line",
      x = as.Date(sii_date_line),
      y = arrow_y_vals,
      arrow = arrow(length = unit(0.1, "inches"), ends = "last")
    ),
    annotate(
      "text",
      x = min(as.Date(sii_date_line)) %m+% months(2),
      y = arrow_y_vals[2] * wt,
      size = 3,
      label = glue::glue(
        "Solvency II
      comes into effect"
      )
    ),
    annotate(
      "line",
      x = as.Date(ifrs17_date_line),
      y = arrow_y_vals,
      arrow = arrow(length = unit(0.1, "inches"), ends = "first")
    ),
    annotate(
      "text",
      x = max(as.Date(ifrs17_date_line)) %m+% months(2),
      y = arrow_y_vals[2] * wt,
      size = 3,
      label = glue::glue(
        "IFRS17
      comes into effect"
      )
    )
  )
  
  return(plot_layers)
}


# =====================================================================
# ================== construct layer about low data ===================
# =====================================================================

low_data_rectangle <- function(limited_data, height = 50) {
list(
geom_rect(
  aes(xmin = limited_data[1], xmax = limited_data[2], ymin = -Inf, ymax = Inf),
  fill = "grey85",
  color = "grey85",
  alpha = 0.25
),
  annotate(
    "text",
    x = mean(limited_data),
    y = height,
    angle = 90,
    label = "Limited Data",
    color = "white",
    alpha = 1
  )
)
}

# =====================================================================
# ============================= interim plot ==========================
# =====================================================================

plot_of_type <- "interim"
arrow_y_vals_interim <- c(90, 110)
plot_title <- glue::glue(tools::toTitleCase("interim, contract & temp"), " job postings")
data_color <- "forestgreen"

p_interim <-
  jobs_data_clean |>  
  filter(type == plot_of_type) |>
  ggplot() +
  low_data_rectangle(limited_data, height = 65) +
  aes(x = date, y = job_count) +
  geom_point(color = data_color, size = 2) +
  geom_line(data = mean_jobs_df |> filter(type == plot_of_type), color = data_color) +
  scale_y_continuous(
    name = "No. of Job Postings",
    limits = c(0, NA)
  ) +
  theme_bw() +
  geom_vline(
    linetype = 2,
    xintercept = as.Date("2023-01-01")
  ) +
  add_vertical_dashed_lines_for_events(arrow_y_vals_interim, wt = 1.1) +
  scale_x_date(
    name = "",
    date_breaks = "1 year",
    limits = as.Date(c(NA, "2026-01-01"))
  ) +
  labs(
    title = plot_title,
    subtitle = "As found on https://www.theactuaryjobs.com/jobs/"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(color = data_color, face = "bold", size = 20),
    plot.subtitle = element_text(color = data_color, face = "bold", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_blank()
  )


# =====================================================================
# =========================== permanent plot ==========================
# =====================================================================

plot_of_type <- "permanent"
plot_title <- glue::glue(tools::toTitleCase(plot_of_type), " job postings")
arrow_y_vals_perm <- c(350, 200)
data_color <- "violet"

p_perm <-
  jobs_data_clean |>
  filter(type == plot_of_type) |>
  ggplot() +
  aes(x = date, y = job_count) +
  low_data_rectangle(limited_data, height = 410) +
  geom_point(size = 2, color = data_color) +
  geom_line(data = mean_jobs_df |> filter(type == plot_of_type), color = data_color) +
  scale_y_continuous(
    name = "No. of Job Postings",
    limits = c(0, NA)
  ) +
  theme_bw() +
  add_vertical_dashed_lines_for_events(arrow_y_vals_perm, wt = 0.65) +
  scale_x_date(
    name = "",
    date_breaks = "1 year",
    limits = as.Date(c(NA, "2026-01-01"))
  ) +
  labs(
    title = plot_title,
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
