

#' Sprint Performance from Time Splits
#'
#' Computes the sprint performance (time to cover the total distance) from time splits and
#' associated distance at which the time splits were measured.
#'
#' @param time_splits A vector with time splits over each distance interval (s)
#' @param distance A vector with the distances at which time splits were measured (m)
#' @param reaction_time A double with the reaction time measured on the starting blocks (s)
#'
#' @returns A tibble with the following columns: distance (m), performance (s)
#' @export
#'
#' @examples
#'
#' # extract men's 100 m data from graubner-nixdorf_sprints dataset
#'
#' data <- graubner_nixdorf_sprints |>
#'           dplyr::filter(event == "Men's 100 m")
#'
#'  sprint_perf_from_split(time_splits = data$splits,
#'  distance = data$distance,
#'  reaction_time = data$reaction_time[1])
#'
sprint_perf_from_split <- function(time_splits, distance, reaction_time = 0){

  # get time split associated with the last element in the distance vector.

  perf <- time_splits[length(time_splits)]

  # adjust for reaction_time

  adjusted_perf <- perf - reaction_time

  # return a data frame with the adjusted perf and the associated distance

  final_dist <- distance[length(distance)]

  result <- tibble::tibble(
    distance = final_dist,
    performance = adjusted_perf
  )

  return(result)

}


#' Extract Sprint Performance for Graubner and Nixdorf (2011) Time Splits Data
#'
#' Extracts the sprint performance (time to cover the total distance) for each event in the Graubner and Nixdorf (2011) dataset.
#'
#' @returns A tibble with the time performance, over each distance of the graubner_nixdorf_sprints dataset, with associated event and athlete sex.
#' @export
#'
#' @examples
#' #' graubner_nixdorf_perf_from_splits()
#'
graubner_nixdorf_perf_from_splits <- function(){

  #prepare table

  perf_table <- tibble::tibble(
    distance = numeric(),
    performance = numeric(),
    sex = character(),
    event = character()
  )


  # create an event list

  event_list <- unique(graubner_nixdorf_sprints$event)

  for(i in event_list){

    if(i == "Men's 100 m"| i == "Men's 200 m"| i == "Men's 400 m"){
      sex <- "male"
    }
    if(i == "Women's 100 m"| i == "Women's 200 m"| i == "Women's 400 m"){
      sex <- "female"
    }

     race <- i

    # get the perf

    dat <- graubner_nixdorf_sprints |>
      dplyr::filter(event == i)

    perf <- sprint_perf_from_split(
      time_splits = dat$splits,
      distance = dat$distance,
      reaction_time = dat$reaction_time[1]
    )


    sub_perf_tab <- perf |>
      dplyr::mutate(sex = sex,
                    event = race)

    # append table

    perf_table <- dplyr::bind_rows(perf_table, sub_perf_tab)

  }

  return(perf_table)

}



