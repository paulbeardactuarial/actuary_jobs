
library(tidyverse)
library(zoo)
library(lubridate)
library(patchwork)
library(ggiraph)

jobs_data_raw <- readr::read_csv("./wayback_job_stats_locations.csv")
retro_jobs_data_raw <- readr::read_csv("./legacy_wayback_job_stats_locations.csv")

plot_color <- "violet"

# manually create a scrape of the latest job posting numbers
latest_data <-
  tibble::tribble(
    ~location, ~job_count,
    "East of England", 1,
    "London (Greater)", 270,
    "North West England", 24,
    "Northern Ireland", 1,
    "Scotland", 23,
    "South East England", 288,
    "South West England", 11,
    "West Midlands", 20,
    "Yorkshire and the Humber", 14,
    "Crown Dependencies", 1,
    "Ireland", 12,
    "Europe", 349,
    "North America", 15,
    "Homeworking", 105,
    "Nationwide", 33
  ) |> 
  dplyr::mutate(date = as.Date("2025-06-07"), .before = 1)


categories_to_england <- c("Nationwide", "England", "Europe")

categories_to_abroad <- c(
"Africa",
"Australasia",
"Asia",
"North America",
"Caribbean",                    
"Channel Islands & Isle of Man", 
"Asia & Australasia",
"Central America",                      
"Crown Dependencies",                               
"Asia Pacific",
"Middle East",
"Oceania",                                    
"Offshore",
"Ireland",
"Republic of Ireland"
)

raw_fields_req <- c("date", "location", "job_count")

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
    location = dplyr::case_when(
      location %in% categories_to_england ~ "England",
      location %in% categories_to_abroad ~ "Abroad",
      location %in% "Yorkshire and Humber" ~ "Yorkshire and the Humber",
      location %in% c("East Midlands", "East Midlands Region") ~ "East Midlands England",
      location %in% "West Midlands" ~ "West Midlands England",
      location |> stringr::str_detect("London") ~ "London",
      .default = location
    )
  ) |> 
  dplyr::summarise(job_count = sum(job_count), .by = c("date", "location")) |> 
  dplyr::filter(!location %in% c("England", "Abroad"))



plot_data <-
  jobs_data_clean |> 
  # thin out overly dense sections by having 30 day cooling period after each date point
  dplyr::arrange(location, date) |> 
  dplyr::mutate(
    fresh_date = purrr::accumulate(
      .x = date, 
      .f = function(x, y) {
        if(x %m+% days(30) > y) {x} else {y}
      }),
    .by = location
  ) |>
  dplyr::filter(date == fresh_date) |> 
  dplyr::mutate(year = lubridate::year(date)) |> 
  dplyr::summarise(
    job_count = mean(job_count, na.rm = T), 
    data_points = dplyr::n(),
    .by = c("year", "location")
    ) |> 
  dplyr::mutate(location = forcats::fct_reorder2(location, year, job_count)) |> 
  dplyr::mutate(NUTS_NAME = 
                  location |> 
                  stringr::str_remove("\\(") |> 
                  stringr::str_remove("\\)") |> 
                  toupper()
                  )



uk_map_data <- 
giscoR::gisco_get_nuts(country = "UK", nuts_level = 1) |> 
  dplyr::mutate(NUTS_NAME = 
                  NUTS_NAME |> 
                  stringr::str_remove("\\(") |> 
                  stringr::str_remove("\\)") |> 
                  toupper()
  ) |> 
  dplyr::right_join(plot_data, by = "NUTS_NAME") 

uk_map_data_homeworking <- 
  plot_data |> 
  dplyr::filter(location == "Homeworking") |> 
  dplyr::mutate(x = 1.5, y = 55)

uk_map_p <-
uk_map_data |> 
  ggplot() +
  aes(fill = job_count) +
  geom_sf_interactive() + 
  scale_fill_continuous() +
  theme_void() + 
  geom_point_interactive(
    data = uk_map_data_homeworking,
    shape = 1,
    mapping = aes(x = x, y = y), size = 15
    ) +
  scale_fill_gradient(low = "white", high = "blue") +
  theme(
    legend.position = "none"
  )

girafe(ggobj = uk_map_p)

plot_data |> 
  ggplot() +
  aes(x = year, 
      y = job_count, 
      fill = forcats::fct_rev(location)) +
  geom_col() +
  facet_wrap(vars(location)) +
  scale_x_continuous(
    breaks = seq(from=2013, 2025, by = 2),
    labels = seq(from=2013, 2025, by = 2),
    name = "") +
  labs(caption = "** Note jobs posted can be assigned multiple location types") + 
  theme_bw() +
  theme(
    plot.title = element_text(color = plot_color, face = "bold", size = 20),
    strip.background = element_rect(fill = plot_color),
    plot.subtitle = element_text(color = plot_color, face = "bold", size = 10),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "none"
  ) 
