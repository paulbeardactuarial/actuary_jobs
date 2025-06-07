
library(tidyverse)
library(zoo)
library(lubridate)
library(patchwork)
library(ggiraph)

jobs_data_raw <- readr::read_csv("./wayback_job_stats_sectors.csv")
retro_jobs_data_raw <- readr::read_csv("./legacy_wayback_job_stats_sectors.csv")

plot_color <- "violet"

# manually create a scrape of the latest job posting numbers
latest_data <-
  tibble::tribble(
    ~sector, ~job_count,
    "Banking and finance", 13,
    "General insurance", 193,
    "Health",  9,
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

categories_to_other <- c("Banking and finance", "Health", "Hedge funds", "IT", "Investment")

raw_fields_req <- c("date", "sector", "job_count")

data_raw <-
  rbind(
    jobs_data_raw |> dplyr::select(all_of(raw_fields_req)), 
    retro_jobs_data_raw |> dplyr::select(dplyr::all_of(raw_fields_req)),
    latest_data
  )

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
  dplyr::summarise(job_count = sum(job_count), .by = c("date", "sector"))


bump_plot_data <-
jobs_data_clean |> 
  # thin out overly dense sections by only having 1 date point per 2 month window
  dplyr::mutate(floored_date = lubridate::floor_date(date, unit = "bimonth")) |> 
  dplyr::slice_head(by = c("floored_date", "sector")) |>
  dplyr::mutate(year = lubridate::year(floored_date)) |> 
  dplyr::summarise(job_count = sum(job_count), .by = c("year", "sector")) |> 
  dplyr::mutate(sector_rank = rank(job_count, ties.method = "first"), .by = year) |> 
  dplyr::mutate(sector = forcats::fct_reorder2(sector, year, job_count))



bump_plot_data |> 
  dplyr::mutate(prop = job_count/sum(job_count), .by = year) |> 
  ggplot() +
  aes(x = year, 
      y = prop, 
      color = forcats::fct_rev(sector)) +
  geom_line() +
  facet_wrap(vars(sector)) +
  scale_x_continuous(
    breaks = seq(from=2013, 2025, by = 2),
    labels = seq(from=2013, 2025, by = 2),
    name = "") +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    name = "Proprtion of Sector Type** Posted (%)") +
  labs(caption = "** Note jobs posted can be assigned multiple sector types") + 
  theme_bw() +
  theme(
    plot.title = element_text(color = plot_color, face = "bold", size = 20),
    strip.background = element_rect(fill = plot_color),
    plot.subtitle = element_text(color = plot_color, face = "bold", size = 10),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "none"
  ) 
