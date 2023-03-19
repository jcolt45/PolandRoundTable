#------- By Organization ------#

#' Find the betweenness measures for a set of organizations over a time period
#'
#' @param orgs A character vector of organization IDs.
#' @param start A start of date range, in DD-MM-YYYY string format.  Defaults to beginning of dates in data.
#' @param end An end of date range, in DD-MM-YYYY string format. Defaults to end of dates in data.
#'
#' @return A tibble with pairs of organizations and their number of shared members in that range.
#'
#' @import dplyr
#' @import lubridate
#' @export
get_betweenness_orgs <- function(orgs, start = NULL, end = NULL, timesteps = "months") {

  if (is.null(start)) {
    start <- min(affiliation_dates$Start.Date)
  }

  if (is.null(end)) {
    end <- max(affiliation_dates$Start.Date)
  }

  start <- lubridate::ymd(start)
  end <- lubridate::ymd(end)
  range <- lubridate::interval(start, end)

  if (timesteps == "months") {

    n_steps <- range %/% months(1) - 1
    starts <- start + months(0:n_steps)
    ends <- starts + months(1)

  } else if (timesteps == "days") {

    n_steps <- range %/% days(1) - 1
    starts <- start + days(0:n_steps)
    ends <- starts + days(1)

  } else if (timesteps == "years") {

    n_steps <- range %/% years(1) - 1
    starts <- start + years(0:n_steps)
    ends <- starts + years(1)
  }

  res <- purrr::map2_dfr(starts, ends,
              ~get_one_betweenness_orgs(orgs,
                                    start = .x,
                                    end = .y))

  res$Start.Date = starts
  res$End.Date = ends


  res <- res %>%
    tidyr::pivot_longer(all_of(orgs),
                        names_to = "Org.ID",
                        values_to = "Betweenness") %>%
    filter(!(is.na(Betweenness)))

  return(res)

}

#' Find the betweenness measures for a set of organizations over a time period
#'
#' @param ... A set of organization IDs.
#' @param start A start of date range, in DD-MM-YYYY string format.  Defaults to beginning of dates in data.
#' @param end An end of date range, in DD-MM-YYYY string format. Defaults to end of dates in data.
#'
#' @return A tibble with pairs of organizations and their number of shared members in that range.
#' @import dplyr
#' @export
get_one_betweenness_orgs <- function(orgs, start = NULL, end = NULL) {

  if (is.null(start)) {
    start <- min(affiliation_dates$Start.Date)
  }

  if (is.null(end)) {
    end <- max(affiliation_dates$Start.Date)
  }


  start <- lubridate::ymd(start)
  end <- lubridate::ymd(end)

  edgelist <- get_edgelist_orgs(start, end)

  if (is.null(edgelist)) {
    res <- rep(NA, length(orgs))
    names(res) <- orgs
    return(res)
  }


  graph <- igraph::graph_from_data_frame(edgelist, directed = FALSE)

  res <- purrr::map_dbl(orgs,
                 ~betweenness_checked(graph, edgelist, .x))

  names(res) <- orgs

  return(res)

}

#------- By Individual ------#

#' Find the betweenness measures for a set of individuals over a time period
#'
#' @param network The data frame containing edge connections.  Format must match
#' built-in datasets `affiliation_dates`, `affiliation_dates_opp`, or `affiliation_dates_gov`.
#' @param members A character vector of member IDs.
#' @param start A start of date range, in DD-MM-YYYY string format.  Defaults to beginning of dates in data.
#' @param end An end of date range, in DD-MM-YYYY string format. Defaults to end of dates in data.
#' @param timesteps A string ("days", "months", "years") for the level of aggregation
#' of the network before computing betweenness.
#'
#' @return A tibble with pairs of organizations and their number of shared members in that range.
#'
#' @import dplyr
#' @import lubridate
#' @export
get_betweenness_members <- function(network,
                                    members = NULL,
                                    start = NULL, end = NULL,
                                    timesteps = "months") {

  if(is.null(members)) {
    members <- unique(network$Member.ID)
  }

  if (is.null(start)) {
    start <- min(network$Start.Date, na.rm = TRUE)
  }

  if (is.null(end)) {
    end <- max(network$Start.Date, na.rm = TRUE)
  }

  start <- lubridate::ymd(start)
  end <- lubridate::ymd(end)
  range <- lubridate::interval(start, end)

  if (timesteps == "months") {

    n_steps <- range %/% months(1) - 1
    starts <- start + months(0:n_steps)
    ends <- starts + months(1)

  } else if (timesteps == "days") {

    n_steps <- range %/% days(1) - 1
    starts <- start + days(0:n_steps)
    ends <- starts + days(1)

  } else if (timesteps == "years") {

    n_steps <- range %/% years(1) - 1
    starts <- start + years(0:n_steps)
    ends <- starts + years(1)
  }

  res <- purrr::map2_dfr(starts, ends,
                         ~get_one_betweenness_members(network,
                                                      members,
                                                   start = .x,
                                                   end = .y))

  res$Start.Date = starts
  res$End.Date = ends


  res <- res %>%
    tidyr::pivot_longer(all_of(members),
                        names_to = "Member.ID",
                        values_to = "Betweenness") %>%
    filter(!(is.na(Betweenness)))

  return(res)

}

#' Find the betweenness measures for one individual in a time period
#'
#' @param network The data frame containing edge connections.  Format must match
#' built-in datasets `affiliation_dates`, `affiliation_dates_opp`, or `affiliation_dates_gov`.
#' @param members A character vector of member IDs.
#' @param start A start of date range, in DD-MM-YYYY string format.  Defaults to beginning of dates in data.
#' @param end An end of date range, in DD-MM-YYYY string format. Defaults to end of dates in data.
#' @param timesteps A string ("days", "months", "years") for the level of aggregation
#' of the network before computing betweenness.
#'
#' @return A tibble with the betweenness of all invididuals in one time window.
#'
#' @import dplyr
#' @import lubridate
#' @export
get_one_betweenness_members <- function(network,
                                        members = NULL,
                                        start = NULL,
                                        end = NULL) {

  if(is.null(members)) {
    members <- unique(network$Member.ID)
  }

  if (is.null(start)) {
    start <- min(network$Start.Date)
  }

  if (is.null(end)) {
    end <- max(network$Start.Date)
  }


  start <- lubridate::ymd(start)
  end <- lubridate::ymd(end)

  edgelist <- get_edgelist_members(network, start, end)

  if (is.null(edgelist)) {
    res <- rep(NA, length(members))
    names(res) <- members
    return(res)
  }

  graph <- igraph::graph_from_data_frame(edgelist, directed = FALSE)

  res <- purrr::map_dbl(members,
                        ~betweenness_checked(graph, edgelist, .x))

  names(res) <- members

  return(res)

}




#------- Helpers -------#

#' Calculates betweenness but first checks if vertex is valid
#'
#' @param graph An igraph object
#' @param edgelist A tibble of edges
#' @param vertex The vertex to calculate betweenness of
#'
#' @return A double
#'
betweenness_checked <- function(graph, edgelist, vertex) {

  if (vertex %in% c(edgelist$from, edgelist$to)) {

    igraph::betweenness(graph, vertex)

  } else {

    NA

  }

}
