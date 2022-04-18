#' Make an edgelist of organizations with member overlap in given date range
#'
#' @param start A start of date range, in YYYY-MM-DD string format.
#' @param end An end of date range, in YYYY-MM-DD string format.
#'
#' @return A tibble with pairs of organizations and their number of shared members in that range.
#' @import dplyr
#' @export
get_edgelist_orgs <- function(start, end = NULL) {

  if (is.null(end)) {
    end <- start
  }

  start <- lubridate::ymd(start)
  end <- lubridate::ymd(end)

  affil_mat <- affiliation_dates %>%
    filter(Start.Date <= end &
             End.Date >= start) %>%
    select(Member.ID, Org.ID) %>%
    distinct() %>%
    mutate(
      match = 1
    ) %>%
    tidyr::pivot_wider(names_from = Org.ID,
                values_from = match,
                values_fill = 0) %>%
    select(-Member.ID) %>%
    as.matrix() %>%
    crossprod()

   orgs <- rownames(affil_mat)

   if (length(orgs) == 0) {
     return(NULL)
   }


   edgelist <- affil_mat %>%
     as_tibble() %>%
     mutate(
       from = orgs
     ) %>%
     tidyr::pivot_longer(-from,
                  names_to = "to",
                  values_to = "num_members") %>%
     filter(num_members > 0)

    return(edgelist)

}


#' Make an edgelist of members with organizational overlap in given date range
#'
#' @param start A start of date range, in DD-MM-YYYY string format.
#' @param end An end of date range, in DD-MM-YYYY string format.
#'
#' @return A tibble with pairs of members who were in the same institution or participated in the same event in the date range.
#' @import dplyr
#' @export
get_edgelist_members <- function(start, end = NULL) {

  if (is.null(end)) {
    end <- start
  }

  start <- lubridate::ymd(start)
  end <- lubridate::ymd(end)

  affil_mat <- affiliation_dates %>%
    filter(Start.Date <= end
           & End.Date >= start) %>%
    select(Member.ID, Org.ID) %>%
    distinct() %>%
    mutate(
      match = 1
    ) %>%
    tidyr::pivot_wider(names_from = Member.ID,
                values_from = match,
                values_fill = 0) %>%
    select(-Org.ID) %>%
    as.matrix() %>%
    crossprod()

  mems <- rownames(affil_mat)

  if (length(mems) == 0) {
    return(NULL)
  }

  edgelist <- affil_mat %>%
    as_tibble() %>%
    mutate(
      from = mems
    ) %>%
    tidyr::pivot_longer(-from,
                 names_to = "to",
                 values_to = "num_orgs") %>%
    filter(num_orgs > 0)


  return(edgelist)

}
