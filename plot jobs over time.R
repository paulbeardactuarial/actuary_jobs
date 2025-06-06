
library(tidyverse)
library(zoo)
library(lubridate)
library(patchwork)

jobs_data_raw <- readr::read_csv("./wayback_job_stats.csv")

jobs_data_clean <- 
jobs_data_raw |> 
  janitor::clean_names() |> 
  dplyr::filter(!is.na(permanent_jobs)) |> 
  dplyr::select(date, permanent_jobs, interim_jobs) |> 
  tidyr::pivot_longer(cols = c("permanent_jobs", "interim_jobs"), names_to = "type") |> 
  dplyr::mutate(
    type = stringr::str_remove(type, "_jobs"),
    date = as.Date(date, tryFormats = "%d/%m/%Y")
    )

# interim plot

plot_type <- "interim, contract & temp"

arrow_y_vals <- c(80, 90)
llm_date_line <- c("2022-12-31", "2023-12-01")
covid_date_line <- c("2020-03-1", "2018-03-23")
data_color <- "forestgreen"

p1 <-
jobs_data_clean |> 
  filter(type == "interim") |> 
  # thin out overly dense sections by only having 1 date point per 2 month window
  dplyr::mutate(floored_date = lubridate::floor_date(date, unit = "bimonth")) |> 
  dplyr::slice_head(by = floored_date) |>
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
    xintercept = as.Date("2022-11-30")
    ) +
  annotate(
    "line",
    x = as.Date(covid_date_line),
    y = arrow_y_vals,
    arrow = arrow(length = unit(0.1, "inches"))
  ) +
  annotate(
    "text",
    x = min(as.Date(covid_date_line)),
    size = 3.5,
    y = arrow_y_vals[2] * 1.05,
    label = "first Covid lockdown"
  ) +
  annotate(
    "line",
    x = as.Date(llm_date_line),
    y = arrow_y_vals,
    arrow = arrow(length = unit(0.1, "inches"), ends = "first")
  ) +
  annotate(
    "text",
    x = max(as.Date(llm_date_line)),
    y = arrow_y_vals[2] * 1.05,
    size = 3.5,
    label = glue::glue(
      "ChatGPT first 
      released to public"
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

arrow_y_vals <- c(300, 250)
llm_date_line <- c("2022-12-31", "2023-12-01")
covid_date_line <- c("2020-03-1", "2018-03-23")
data_color <- "violet"

p2 <-
jobs_data_clean |> 
  filter(type == plot_type) |> 
  # thin out overly dense sections by only having 1 date point per 2 month window
  dplyr::mutate(floored_date = lubridate::floor_date(date, unit = "bimonth")) |> 
  dplyr::slice_head(by = floored_date) |>
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
    xintercept = as.Date("2022-11-30")
  ) +
  annotate(
    "line",
    x = as.Date(covid_date_line),
    y = arrow_y_vals,
    arrow = arrow(length = unit(0.1, "inches"))
  ) +
  annotate(
    "text",
    x = min(as.Date(covid_date_line)),
    y = arrow_y_vals[2] * 0.9,
    size = 3.5,
    label = "first Covid lockdown"
  ) +
  annotate(
    "line",
    x = as.Date(llm_date_line),
    y = arrow_y_vals,
    arrow = arrow(length = unit(0.1, "inches"), ends = "first")
  ) +
  annotate(
    "text",
    x = max(as.Date(llm_date_line)) %m+% months(5),
    y = arrow_y_vals[2] * 0.75,
    size = 3.5,
    label = glue::glue(
      "ChatGPT first 
      released to public"
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


p2

p1

