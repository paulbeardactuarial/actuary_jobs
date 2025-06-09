
library(tidyverse)
library(zoo)
library(lubridate)
library(patchwork)
library(ggiraph)

# ===================================================================
# ====================== collect data ===============================
# ===================================================================

jobs_data_raw <- readr::read_csv("./wayback_job_stats_locations.csv")
retro_jobs_data_raw <- readr::read_csv("./legacy_wayback_job_stats_locations.csv")

days_cool_off_after_data <- 45

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

raw_fields_req <- c("date", "location", "job_count")

data_raw <-
  rbind(
    jobs_data_raw |> dplyr::select(all_of(raw_fields_req)), 
    retro_jobs_data_raw |> dplyr::select(dplyr::all_of(raw_fields_req)),
    latest_data
  )



# ===================================================================
# ====================== clean data =================================
# ===================================================================

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
      if(x %m+% days(days_cool_off_after_data) > y) {x} else {y}
    }) |> 
  unique() 

# clean the data by:
# 1. consolidating some of the locations
# 2. pruning dates to space out
# 3. adding in rows with zero job_count when they are missing (needed for averages later)
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
  dplyr::filter(!location %in% c("England", "Abroad")) |>  
  dplyr::filter(date %in% dates_used) |> 
  tidyr::complete(
    date, 
    location,
    fill = list(job_count = 0)
  )
    
plot_data <- 
  jobs_data_clean |> 
  dplyr::mutate(year = lubridate::year(date)) |> 
  dplyr::mutate(
    year_band = dplyr::case_when(
      dplyr::between(date, as.Date("2014-01-01"), as.Date("2015-12-31")) ~ "2014/15",
      dplyr::between(date, as.Date("2023-01-01"), as.Date("2024-12-31")) ~ "2023/24",
      .default = "Other"
    )
  ) |> 
  dplyr::summarise(
    job_count = mean(job_count, na.rm = T), 
    data_points = dplyr::n(),
    .by = c("year_band", "location")
    ) |> 
  dplyr::mutate(location = forcats::fct_reorder2(location, year_band, job_count)) |> 
  dplyr::mutate(NUTS_NAME = 
                  location |> 
                  stringr::str_remove("\\(") |> 
                  stringr::str_remove("\\)") |> 
                  toupper()
                  ) 




# =====================================================================
# ================= get UK geometry map data and join =================
# =====================================================================

uk_nuts_data <- giscoR::gisco_get_nuts(country = "UK", nuts_level = 1)

create_circle <- function(center_x, center_y, radius, n_points = 50) {
  angles <- seq(0, 2*pi, length.out = n_points + 1)
  x_coords <- center_x + radius * 1.65 * cos(angles) # 1.65 to counteract distortion of map at UK latitude
  y_coords <- center_y + radius * sin(angles)
  
  # Return as matrix (required for sf polygons)
  cbind(x_coords, y_coords)
}

# Position to the right of UK
uk_bbox <- sf::st_bbox(uk_nuts_data)
center_x <- uk_bbox[3]  
center_y <- mean(c(uk_bbox[2], uk_bbox[4]))
radius <- 0.5 

# Create the circle coordinates
circle_coords <- create_circle(center_x, center_y, radius)

# Create the sf geometry
hw_geometry <- sf::st_sfc(
  sf::st_multipolygon(
    list(list(circle_coords))  
  ),
  crs = sf::st_crs(uk_nuts_data)
)


uk_map_data <- uk_nuts_data |> 
  dplyr::add_row(
    NAME_LATN = "Homeworking",
    NUTS_NAME = "Homeworking",
    geometry = hw_geometry
  ) |> 
  dplyr::mutate(NUTS_NAME = 
                  NUTS_NAME |> 
                  stringr::str_remove("\\(") |> 
                  stringr::str_remove("\\)") |> 
                  toupper()
  ) |> 
  dplyr::right_join(plot_data, by = "NUTS_NAME") 





# =====================================================================
# ========================= plot UK map data =========================
# =====================================================================
p_uk <-
uk_map_data |> 
  dplyr::filter(!year_band %in% "Other") |> 
  ggplot() +
  aes(fill = job_count) +
  geom_sf_interactive() + 
  theme_void() + 
  scale_fill_gradient(
    name = "Average No. of Job Postings up at a Time",
    low = "white", 
    high = "darkblue", 
    transform = "sqrt",
    guide = guide_colorbar(barheight = 0.6),
    breaks = c(0, 100, 400)) +
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5, size = 9),
    panel.spacing = unit(2, "cm"),
    plot.title = element_text(color = "darkblue", face = "bold", size = 20),
    plot.subtitle = element_text(color = "darkblue", face = "bold", size = 10, margin = margin(t = 8, b = 14)),
    plot.caption = element_text(hjust = 0),
    strip.text = element_text(size = rel(1.1), face = "bold"),
    plot.margin = margin(t = 6, r = 5, b = 10, l = 5, unit = "pt"),
    plot.background = element_rect(fill = "white", colour = "white"),
    legend.margin = margin(b = 15)  # Gap below legend
  ) +
  annotate(
    "text",
    x = center_x,
    y = center_y - 0.8,
    label = "Remote",
    size = 2.5
  ) + 
  facet_wrap(vars(year_band)) +
  labs(
    title = "Location* of Job Postings in UK",
    subtitle = "As found on https://www.theactuaryjobs.com/jobs/",
    caption = "* Note that multiple locations can be assigned to the same job posting"
  )


ggsave("location.png", p_uk, dpi = 800, width = 5, height = 5, units = "in")




