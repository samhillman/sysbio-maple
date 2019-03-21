library(tidyverse)

real_infected <- read_csv("data/realinfected.csv")
dd            <- read_csv("data/Hillman_S_BI3156_Assessment1modeldd.csv")
dd_scaled     <- read_csv("data/Hillman_S_BI3156_Assessment1_modeldd_scaled_data.csv")
ff            <- read_csv("data/Hillman_S_BI3156_Assessment1modelfd.csv")
ff_scaled     <- read_csv("data/Hillman_S_BI3156_Assessment1_modelfd_scaled_data.csv")

real_infected <- real_infected %>%
  mutate(real_infected = infected,
         time_point = time) %>%
  select(time_point, real_infected)

dd <- mutate(dd, dd_infected = I) %>% select(time_point, dd_infected)
dd_scaled <- mutate(dd_scaled, dd_scaled_infected = I) %>% select(time_point, dd_scaled_infected)
ff <- mutate(ff, ff_infected = I) %>% select(time_point, ff_infected)
ff_scaled <- mutate(ff_scaled, ff_scaled_infected = I) %>% select(time_point, ff_scaled_infected)

infected_data <- left_join(real_infected, dd, by = "time_point") %>%
  left_join(., dd_scaled, by = "time_point") %>%
  left_join(., ff, by = "time_point") %>%
  left_join(., ff_scaled, by = "time_point")

infected_data <- infected_data %>%
  mutate(dd_error = real_infected - dd_infected,
         dd_scaled_error = real_infected - dd_scaled_infected,
         ff_error = real_infected - ff_infected,
         ff_scaled_error = real_infected - ff_scaled_infected,
         dd_error_sq = dd_error^2,
         dd_scaled_error_sq = dd_scaled_error^2,
         ff_error_sq = ff_error^2,
         ff_scaled_error_sq = ff_scaled_error^2) 

infected_data_summary <- infected_data %>%
  summarise(dd_sum = sum(dd_error_sq),
            dd_scaled_sum = sum(dd_scaled_error_sq),
            ff_sum = sum(ff_error_sq),
            ff_scaled_sum = sum(ff_scaled_error_sq))

write_csv(infected_data, "data/infected_data.csv")
write_csv(infected_data_summary, "data/infected_data_summary.csv")
