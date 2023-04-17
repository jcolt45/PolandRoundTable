#' Adjusts list of affiliations based on how we want to treat "one-off" connections
#' like participation in "Events" (protests, letters signing, etc.)
#'
#'
#'

#' @export
adj_affil_list <- function(affils_by_date,
                           lifelong = "none",
                           event_length = 30,
                           decay = "none",
                           days_to_zero = 0) {


  # Option 1: All connections are lifelong

  if (lifelong == "all") {
    affils_by_date <- affils_by_date %>%
      mutate(
        End.Date = lubridate::ymd("1989-02-06")
      )
  }


  # Option 2: Only Events are lifelong

  if (lifelong == "events") {
    affils_by_date <- affils_by_date %>%
      mutate(
        End.Date = case_when(
          Category == "Event" ~ lubridate::ymd("1989-02-06"),
          TRUE ~ End.Date
        )
      )
  }


  # Option 3: Events last X days after

  if (lifelong != "events") {
    affils_by_date <- affils_by_date %>%
      mutate(
        End.Date = case_when(
          Category == "Event" ~ End.Date + lubridate::days(event_length),
          TRUE ~ End.Date
        )
      )
  }


  ## Not a priority  now, but would be cool to have decays
  ## We'd have to add rows to the affils list with different weights.
  # # Add a decay from the end
  #
  # if (decay == "all") {
  #   affils_by_date_add <- affils_by_date %>%
  #     mutate(
  #       Start.Date = End.Date
  #       End.Date = End.Date + lubridate::days(days_to_zero),
  #       weight =
  #       )
  #     )
  # }
  #
  #
  # if (decay == "all") {
  #   affils_by_date <- affils_by_date %>%
  #     mutate(
  #       End.Date = case_when(
  #         Category == "Event" ~ End.Date + lubridate::days(event_length),
  #         TRUE ~ End.Date
  #       )
  #     )
  # }

  return(affils_by_date)

}

#' @export
add_mem_cats <- function(edgelist,
                         mem_meta,
                         cross_cat_col,
                         id_col) {
  edgelist %>%
    left_join(mem_meta %>% select(id_col, cross_cat_col), by = c(from = id_col)) %>%
    rename(from_cat = cross_cat_col) %>%
    left_join(mem_meta%>% select(id_col, cross_cat_col), by = c(to = id_col)) %>%
    rename(to_cat = cross_cat_col)

}

#' @export
only_cross_cons <- function(edgelist,
                            mem_meta,
                            cross_cat_col,
                            id_col) {

  edgelist %>%
    add_mem_cats(mem_meta,
                 cross_cat_col,
                 id_col) %>%
    filter(to_cat != from_cat)

}
