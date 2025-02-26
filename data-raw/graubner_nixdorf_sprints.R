## code to prepare `graubner_nixdorf_sprints`
# based on available data in Graubner and Nixdorf article (2011)
# Graubner, R., & Nixdorf, E. (2009). Biomechanical analysis of the sprint and hurdles events at the 2009 IAAF World Championships in Athletics. Positions, 1(10).

library(dplyr)

# Usain Bolt 100 m -----

distance <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
splits <- c(0, 1.88, 2.88, 3.78, 4.64, 5.47, 6.29, 7.10, 7.92, 8.74, 9.58)

velocity<- c(0, 5.77, 9.99, 11.11, 11.63, 12.08, 12.20, 12.29, 12.17, 12.17, 11.96)

reaction_time <- 0.146

maximal_velocity <-  12.34

athlete <- "Bolt"

event <- "Men's 100 m"

# put the information into a single data frame in long format

bolt_100m <- dplyr::tibble(distance = distance, splits = splits, velocity = velocity, reaction_time = reaction_time, maximal_velocity = maximal_velocity, athlete = athlete, event = event)

# Usain bolt 200 m ----

distance <- c(0, 50, 100, 150, 200)
splits <- c(0, 5.60, 5.40+4.32, 5.60+4.32+4.52, 5.60+4.32+4.52+4.75)

velocity <- c(0, 8.93, 11.57, 11.06, 10.53)

# reaction time
reaction_time <- 0.133

maximal_velocity <-  NA

athlete <- "Bolt"

event <- "Men's 200 m"

bolt_200m <- dplyr::tibble(distance = distance, splits = splits,velocity = velocity, reaction_time = reaction_time, maximal_velocity = maximal_velocity, athlete = athlete, event = event)


# LeShawn Merritt 400 m ----

velocity_data <- c(0,8.22, 9.88, 9.78, 9.54, 9.26, 9.24, 8.80, 8.22)

# calculate time_intervals
time_intervals <- 50/velocity_data

# time splits
splits <- c(0, cumsum(time_intervals[-1]))
distance <- c(0, 50, 100, 150, 200, 250, 300, 350, 400)

velocity <- velocity_data

reaction_time <- 0.161

maximal_velocity <-  NA

athlete <- "Merritt"

event <- "Men's 400 m"

merritt_400m <- dplyr::tibble(distance = distance, splits = splits, reaction_time = reaction_time, maximal_velocity = maximal_velocity, athlete = athlete, event = event)


# Fraser-Price 100 m -----

distance <- c(0, 20, 40, 60, 80, 100)
time <- c(0, 3.03, 4.98, 6.88, 8.77, 10.73)

time_intervals <- diff(time)

velocity <- c( 0, 20/time_intervals)

reaction_time <- 0.146

maximal_velocity <- 10.58

athlete <- "Fraser-Price"

event <- "Women's 100 m"

fraser_price_100m <- dplyr::tibble(distance = distance, splits = time, velocity = velocity, reaction_time = reaction_time, maximal_velocity = maximal_velocity, athlete = athlete, event = event)

# Felix 200 m ------

distance <- c(0, 50, 100, 150, 200)
time <- c(0, 6.25, 6.25+4.91, 6.25+4.91+5.22, 6.25+4.91+5.22+5.64)

velocity <- c(0,8.00, 10.18, 9.60, 8.85)

reaction_time <- 0.173

maximal_velocity <-  NA

athlete <- "Felix"

event <- "Women's 200 m"

felix_200m <- dplyr::tibble(distance = distance, splits = time, velocity = velocity, reaction_time = reaction_time, maximal_velocity = maximal_velocity, athlete = athlete, event = event)

# Williams 400 m ------

velocity_data <- c(0, 7.59, 9.12, 8.77, 8.35, 8.25, 8.09, 7.84, 7.20)

# calculate time_intervals
time_intervals <- 50/velocity_data

# time splits
splits <- c(0, cumsum(time_intervals[-1]))
distance <- c(0, 50, 100, 150, 200, 250, 300, 350, 400)

velocity <- velocity_data

# reaction time

reaction_time <- 0.194

maximal_velocity <-  NA

athlete <- "Williams"

event <- "Women's 400 m"

williams_400m <- dplyr::tibble(distance = distance, splits = splits, velocity = velocity, reaction_time = reaction_time, maximal_velocity = maximal_velocity, athlete = athlete, event = event)


# combine all data

graubner_nixdorf_sprints <- dplyr::bind_rows(bolt_100m, bolt_200m, merritt_400m, fraser_price_100m, felix_200m, williams_400m)



usethis::use_data(graubner_nixdorf_sprints, overwrite = TRUE)
