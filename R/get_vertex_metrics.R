#------- By Organization ------#

#' Find the vertex_metrics measures for a set of organizations over a time period
#'
#' @param orgs A character vector of organization IDs.
#' @param metrics A named list of metric computing functions, as from `igraph`.  Must take arguments of (`graph`, `vertex`).
#' @param start A start of date range, in DD-MM-YYYY string format.  Defaults to beginning of dates in data.
#' @param end An end of date range, in DD-MM-YYYY string format. Defaults to end of dates in data.
#'
#' @return A tibble with pairs of organizations and their number of shared members in that range.
#'
#' @import dplyr
#' @import lubridate
#' @export
get_vertex_metrics_orgs <- function(orgs,
                                    metrics,
                                    start = NULL,
                                    end = NULL,
                                    timesteps = "months") {

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

  names(starts) <- 1:length(starts)
  names(ends) <- 1:length(ends)

  res <- purrr::map2_dfr(starts, ends,
              ~get_one_vertex_metrics_orgs(orgs,
                                           metrics,
                                    start = .x,
                                    end = .y))


  return(res)

}

#' Find the vertex_metrics measures for a set of organizations over a time period
#'
#' @param ... A set of organization IDs.
#' @param start A start of date range, in DD-MM-YYYY string format.  Defaults to beginning of dates in data.
#' @param end An end of date range, in DD-MM-YYYY string format. Defaults to end of dates in data.
#'
#' @return A tibble with pairs of organizations and their number of shared members in that range.
#' @import dplyr
#' @export
get_one_vertex_metrics_orgs <- function(orgs, metrics, start = NULL, end = NULL) {

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
  names(orgs) <- orgs

  res <- purrr::map_dfr(orgs,
                        ~vertex_metrics_checked(graph, metrics, edgelist, .x))

  res <- res %>%
    mutate(
      Start.Date = start,
      End.Date = end,
      Org.ID = orgs
    )

  return(res)

}

#------- By Individual ------#

#' Find the vertex_metrics measures for a set of individuals over a time period
#'
#' @param members A character vector of member IDs.
#' @param start A start of date range, in DD-MM-YYYY string format.  Defaults to beginning of dates in data.
#' @param end An end of date range, in DD-MM-YYYY string format. Defaults to end of dates in data.
#'
#' @return A tibble with pairs of organizations and their number of shared members in that range.
#'
#' @import dplyr
#' @import lubridate
#' @export
get_vertex_metrics_members <- function(members, metrics, start = NULL, end = NULL, timesteps = "months") {

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
                         ~get_one_vertex_metrics_members(members,
                                                         metrics,
                                                   start = .x,
                                                   end = .y))

  return(res)

}

#' Find the vertex_metrics measures for a set of organizations over a time period
#'
#' @param members A vector of member IDs.
#' @param start A start of date range, in DD-MM-YYYY string format.  Defaults to beginning of dates in data.
#' @param end An end of date range, in DD-MM-YYYY string format. Defaults to end of dates in data.
#'
#' @return A tibble with pairs of organizations and their number of shared members in that range.
#' @import dplyr
#' @export
get_one_vertex_metrics_members <- function(members, metrics, start = NULL, end = NULL) {

  if (is.null(start)) {
    start <- min(affiliation_dates$Start.Date)
  }

  if (is.null(end)) {
    end <- max(affiliation_dates$Start.Date)
  }


  start <- lubridate::ymd(start)
  end <- lubridate::ymd(end)

  edgelist <- get_edgelist_members(start, end)

  if (is.null(edgelist)) {
    res <- rep(NA, length(members))
    names(res) <- members
    return(res)
  }

  graph <- igraph::graph_from_data_frame(edgelist, directed = FALSE)

  res <- purrr::map_dfr(members,
                        ~vertex_metrics_checked(graph, metrics, edgelist, .x))

  res <- res %>%
    mutate(
      Start.Date = start,
      End.Date = end,
      Member.ID = members
    )

  return(res)

}




#------- Helpers -------#

#' Calculates vertex_metrics but first checks if vertex is valid
#'
#' @param graph An igraph object
#' @param metrics A named list of metric functions
#' @param edgelist A tibble of edges
#' @param vertex The vertex to calculate vertex_metrics of
#'
#' @import igraph
#'
#' @return A double
#'
vertex_metrics_checked <- function(graph, metrics, edgelist, vertex) {

  if (vertex %in% c(edgelist$from, edgelist$to)) {

    purrr::map_dbl(metrics, ~do.call(.x, args = list(graph, vertex)))

  } else {

    rep(NA, length(metrics))

  }

}
