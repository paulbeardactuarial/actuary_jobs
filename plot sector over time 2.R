library(tidyverse)
library(lubridate)

# variables
days_cool_off_after_data <- 45
categories_to_other <- c("Banking and finance", "Health", "Hedge funds", "IT")
plot_color <- "bisque3"

# ===================================================================
# ====================== collect data ===============================
# ===================================================================

jobs_data_raw <- readr::read_csv("./wayback_job_stats_sectors.csv")
retro_jobs_data_raw <- readr::read_csv("./legacy_wayback_job_stats_sectors.csv")

# manually create a scrape of the latest job posting numbers
latest_data <-
  tibble::tribble(
    ~sector, ~job_count,
    "Banking and finance", 13,
    "General insurance", 193,
    "Health", 9,
    "Hedge funds", 4,
    "Investment", 25,
    "IT", 10,
    "Life insurance", 116,
    "Management consultancy", 18,
    "Pensions", 113,
    "Reinsurance", 57,
    "Risk management", 44,
    "Solvency II", 36,
    "Systems", 21,
    "Other", 19
  ) |>
  dplyr::mutate(date = as.Date("2025-06-07"), .before = 1)

raw_fields_req <- c("date", "sector", "job_count")

data_raw <-
  rbind(
    jobs_data_raw |> dplyr::select(all_of(raw_fields_req)),
    retro_jobs_data_raw |> dplyr::select(dplyr::all_of(raw_fields_req)),
    latest_data
  )

# ===================================================================
# ====================== clean data =================================
# ===================================================================

# get dates that we are going to retain
# thin out by having `days_cool_off_after_data` cooling period after each date point
dates_used <-
  data_raw |>
  janitor::clean_names() |>
  dplyr::mutate(
    date = as.Date(date, tryFormats = "%d/%m/%Y")
  ) |>
  dplyr::pull(date) |>
  unique() |>
  sort() |>
  purrr::accumulate(
    .f = function(x, y) {
      if (x %m+% days(days_cool_off_after_data) > y) {
        x
      } else {
        y
      }
    }
  ) |>
  unique()


jobs_data_clean <-
  data_raw |>
  janitor::clean_names() |>
  dplyr::mutate(
    date = as.Date(date, tryFormats = "%d/%m/%Y")
  ) |>
  unique() |>
  dplyr::arrange(date) |>
  dplyr::mutate(
    sector = dplyr::if_else(
      sector %in% categories_to_other,
      "Other",
      sector
    )
  ) |>
  dplyr::filter(date %in% dates_used) |>
  tidyr::complete(
    date,
    sector,
    fill = list(job_count = 0)
  ) |>
  dplyr::summarise(job_count = sum(job_count), .by = c("date", "sector"))



# =====================================================================
# ============================= plot ==================================
# =====================================================================

plot_data <-
  jobs_data_clean |>
  dplyr::summarise(job_count = sum(job_count), .by = c("date", "sector")) |>
  dplyr::mutate(year = lubridate::year(date)) |>
  dplyr::summarise(
    job_count = mean(job_count, na.rm = T),
    data_points = dplyr::n(),
    .by = c("year", "sector")
  ) |>
  dplyr::mutate(sector = forcats::fct_reorder2(sector, year, job_count))

sectors_to_plot <- c("General insurance", "Life insurance", "Pensions", "Investment")

mean_jobs_df <-
  tidyr::expand_grid(
    date = seq(
      from = min(jobs_data_clean$date),
      to = max(jobs_data_clean$date),
      by = "1 month"
    ) |> floor_date(unit = "months"),
    sector = sectors_to_plot
  )

mean_jobs_df <-
  mean_jobs_df |>
  dplyr::mutate(
    job_count_stats = purrr::map2(
      .x = mean_jobs_df$date,
      .y = mean_jobs_df$sector,
      .f = function(x, y) {
        jobs_data_clean |>
          dplyr::filter(sector == y) |>
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

limited_data <-
  mean_jobs_df |>
  dplyr::filter(sector %in% sectors_to_plot) |>
  dplyr::filter(is.na(job_count)) |>
  dplyr::pull(date) |>
  range()

p <-
  mean_jobs_df |>
  dplyr::filter(sector %in% sectors_to_plot) |>
  dplyr::mutate(sector = factor(sector, levels = sectors_to_plot)) |>
  ggplot() +
  geom_rect(
    aes(xmin = limited_data[1], xmax = limited_data[2], ymin = -Inf, ymax = Inf),
    fill = "grey85",
    color = "grey85",
    alpha = 0.25
  ) +
  annotate(
    "text",
    x = mean(limited_data),
    y = 250,
    angle = 90,
    label = "Limited Data",
    color = "white",
    alpha = 1
  ) +
  aes(
    x = date,
    y = job_count,
    color = sector
  ) +
  geom_line() +
  geom_point(
    data =
      jobs_data_clean |>
        dplyr::filter(sector %in% sectors_to_plot) |>
        dplyr::mutate(sector = factor(sector, levels = sectors_to_plot)),
    size = 1
  ) +
  facet_wrap(vars(sector)) +
  scale_y_continuous(
    name = "No. of Jobs On-site"
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_x_date(
    name = "",
    date_breaks = "2 year",
    limits = as.Date(c(NA, "2026-01-01"))
  ) +
  labs(caption = "** Note jobs posted can be assigned multiple sector types") +
  theme_bw() +
  labs(
    title = "Job postings by Sector*",
    subtitle = "As found on https://www.theactuaryjobs.com/jobs/"
  ) +
  theme(
    plot.title = element_text(color = plot_color, face = "bold", size = 20),
    strip.background = element_rect(fill = plot_color),
    plot.subtitle = element_text(color = plot_color, face = "bold", size = 10),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "none"
  )


ggsave("sector.png", p, dpi = 1000, width = 6, height = 4.75, units = "in")
