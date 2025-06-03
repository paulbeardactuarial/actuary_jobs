
library(tidyverse)

jobs_data_raw <- readr::read_csv("./wayback_job_stats.csv")

jobs_data_clean <- 
jobs_data_raw |> 
  janitor::clean_names() |> 
  dplyr::filter(!is.na(permanent_jobs)) |> 
  dplyr::select(date, permanent_jobs, interim_jobs) |> 
  tidyr::pivot_longer(cols = c("permanent_jobs", "interim_jobs"), names_to = "type") |> 
  dplyr::mutate(
    type = stringr::str_remove(type, "_jobs"),
    date = as.Date(date)
    )

plot_type <- "permanent"

jobs_data_clean |> 
  filter(type == plot_type) |> 
  ggplot() +
  aes(x = date, y = value, color = type) +
  geom_point() +
  scale_y_continuous( limits = c(0, NA)) +
  theme_bw() +
  scale_x_date(date_breaks = "1 year") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1))  
        