
library(tidyverse)
library(zoo)
library(lubridate)
library(patchwork)
library(ggiraph)

jobs_data_raw <- readr::read_csv("./wayback_job_stats_sectors.csv")
retro_jobs_data_raw <- readr::read_csv("./legacy_wayback_job_stats_sectors.csv")

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


p <-
jobs_data_clean |> 
  # thin out overly dense sections by only having 1 date point per 2 month window
  dplyr::mutate(floored_date = lubridate::floor_date(date, unit = "bimonth")) |> 
  dplyr::slice_head(by = c("floored_date", "sector")) |>
  ggplot() +
  aes(x = date, 
      y = job_count, 
      fill = forcats::fct_reorder2(sector, date, job_count) |> forcats::fct_rev(), 
      data_id = sector) +
  geom_area_interactive(position = "fill") +
  scale_fill_discrete(name = "")


library(ggbump)


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
  ggplot() +
  aes(x = year, 
      y = sector_rank, 
      color = sector) +
  geom_bump(size = 1) +
  geom_text(
    data = bump_plot_data |> dplyr::filter(year == 2025),
    mapping = aes(label = sector),
    hjust = 0,
    nudge_x = 0.25,
    size = 4
  ) +
  scale_color_discrete(name = "") +
  scale_x_continuous(
    limits = c(NA, 2032), 
    breaks = seq(from=2013, 2025, by = 2),
    labels = seq(from=2013, 2025, by = 2),
    name = "") +
  scale_y_continuous(name = "Rank") +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(color = data_color, face = "bold", size = 20),
    plot.subtitle = element_text(color = data_color, face = "bold", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank()
  ) 



bump_plot_data |> 
  ggplot() +
  aes(x = year, 
      y = job_count, 
      fill = forcats::fct_rev(sector)) +
  geom_col(
    position = "fill",
    width = 0.9,
    alpha = 1) +
  scale_fill_discrete(name = "") +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(from=2013, 2025, by = 1),
    labels = seq(from=2013, 2025, by = 1),
    name = "") +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(from=0, 1, by = 0.2),
    labels = scales::label_percent(accuracy = 1),
    name = "Proprtion of Jobs Posted (%)") +
  theme_bw() +
  theme(
    plot.title = element_text(color = data_color, face = "bold", size = 20),
    plot.subtitle = element_text(color = data_color, face = "bold", size = 10),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    panel.grid.major.x  = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y  = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank()
  ) 


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
    plot.title = element_text(color = data_color, face = "bold", size = 20),
    plot.subtitle = element_text(color = data_color, face = "bold", size = 10),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    # panel.grid.major.x  = element_blank(),
    # panel.grid.major.y = element_blank(),
    # panel.grid.minor.y  = element_blank(),
    # panel.grid.minor.x = element_blank(),
    # panel.border = element_blank(),
    legend.position = "none"
  ) 
