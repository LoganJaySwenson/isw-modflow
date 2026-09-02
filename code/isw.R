# Estimate aquifer drawdown and streamflow depletion due to pumping using isw package.
library(isw)
library(sf)
library(tidyverse)

source(file.path("code", "theme.R"))

pumping_wells <- example_pumping_wells 
observation_wells <- example_observation_wells
streams <- example_stream_reaches

crs <- st_crs(streams)

stream_segments <- get_stream_segments(
  streams,
  reach_spacing = set_units(400, "m")
  )

pumping_schedules <- tibble(
  t = as.Date(c("2025-01-01", "2025-02-01", "2025-03-01", "2025-04-01")),
  pump_1 = set_units(c(500, 500, 300, 0), "m^3/day"),
  pump_2 = set_units(c(0, 250, 250, 0), "m^3/day")
  )

evaluation_times <- seq.Date(
  from = as.Date("2025-01-01"),
  to = as.Date("2026-01-01"),
  by = "day"
  )

injection_times <- NULL

stream_apportionment <- get_adf_stream_apportionment(
  pumping_wells,
  stream_segments,
  sample_spacing = set_units(100, "m"),
  method = "web_squared"
  )

adf_schedule <- get_stream_injection_schedule(
  pumping_wells = pumping_wells,
  pumping_schedules = pumping_schedules,
  stream_segments = stream_segments,
  evaluation_times = evaluation_times,
  injection_times = injection_times,
  method = "adf",
  stream_apportionment = stream_apportionment
  ) 

constant_head_schedule <- get_stream_injection_schedule(
  pumping_wells = pumping_wells,
  pumping_schedules = pumping_schedules,
  stream_segments = stream_segments,
  evaluation_times = evaluation_times,
  injection_times = injection_times,
  method = "constant_head"
  )

adf_stream_depletion <- adf_schedule |> 
  mutate(
    method =  "ADF"
  ) |> 
  group_by(method, pump_id, reach_id, interval_start, interval_end) |> 
  summarise(
    stream_depletion = -sum(injection_rate),
    .groups = "drop"
  )

constant_head_stream_depletion <- constant_head_schedule |> 
  mutate(
    method = "constant_head"
  ) |> 
  group_by(method, pump_id, reach_id, interval_start, interval_end) |>
  summarise(
    stream_depletion = -sum(injection_rate),
    rmse = sqrt(mean(boundary_residual**2)),
    .groups = "drop"
  )

stream_depletion <- bind_rows(adf_stream_depletion, constant_head_stream_depletion)

adf_water_levels <- get_aquifer_water_level_change(
  pumping_wells = pumping_wells,
  pumping_schedules = pumping_schedules,
  observation_wells = observation_wells,
  stream_segments = stream_segments,
  evaluation_times = evaluation_times,
  stream_injection_schedule = adf_schedule) |>
  mutate(method = "ADF") |> 
  select(method, pump_id, observation_id, evaluation_time, pumping_drawdown, stream_recovery, water_level_change)

constant_head_water_levels <- get_aquifer_water_level_change(
  pumping_wells = pumping_wells,
  pumping_schedules = pumping_schedules,
  observation_wells = observation_wells,
  stream_segments = stream_segments,
  evaluation_times = evaluation_times,
  stream_injection_schedule = constant_head_schedule) |>
  mutate(method = "constant_head") |> 
  select(method, pump_id, observation_id, evaluation_time, pumping_drawdown, stream_recovery, water_level_change)

water_levels <- bind_rows(adf_water_levels, constant_head_water_levels)

water_levels |>   
  group_by(method, pump_id, observation_id) |>   
  mutate(time = time_length(evaluation_time - min(evaluation_time), unit = "days")+1) |>   
  ungroup() |>    
  select(method, time, pump_id, observation_id, pumping_drawdown, stream_recovery, water_level_change) |> 
  write_csv(file.path("data", "isw-vignette-aquifer-depletion.csv"))

stream_depletion |> 
  group_by(method, pump_id, reach_id) |>
  mutate(time = time_length(interval_end - min(interval_start), unit = "days")) |>
  ungroup() |>
  select(method, time, pump_id, reach_id, stream_depletion, rmse) |> 
  write_csv(file.path("data", "isw-vignette-stream-depletion.csv"))

st_write(pumping_wells, file.path("data", "isw-vignette.gpkg"), layer="pumping_wells", delete_layer=T) 
st_write(observation_wells, file.path("data", "isw-vignette.gpkg"), layer="observation_wells", delete_layer=T) 
st_write(streams, file.path("data", "isw-vignette.gpkg"), layer="streams", delete_layer=T) 