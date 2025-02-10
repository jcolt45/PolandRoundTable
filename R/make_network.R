#' Make an edgelist of organizations with member overlap in given date range
#' This function should be updated to take a dataset piped in, and to calculate
#' edgeweight in the newer way.
#'
#' @param affils_by_date Data frame of affiliations with start and end dates
#' @param start A start of date range, in YYYY-MM-DD string format.
#' @param end An end of date range, in YYYY-MM-DD string format.
#' @return A tibble with pairs of organizations and their number of shared members in that range.
#' @import dplyr readr
#' @export
get_edgelist_orgs <- function(affils_by_date, start, end = NULL) {

  if (is.null(end)) {
    end <- start
  }

  start <- lubridate::ymd(start)
  end <- lubridate::ymd(end)

  affil_mat <- affils_by_date %>%
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
     mutate(weight = 1) %>%
     filter(
       weight > 0
     ) %>%
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
#' @param affils_by_date Data frame of affiliations with start and end dates
#' @param start A start of date range, in YYYY-MM-DD string format.
#' @param end An end of date range, in YYYY-MM-DD string format.
#' @param get_edge_names Boolean; should we label the edges by name?
#' @return A tibble with pairs of members who were in the same institution or participated in the same event in the date range.
#' @import dplyr
#' @export
get_edgelist_members <- function(affils_by_date,
                                 on_cols,
                                 start,
                                 get_edge_names = TRUE,
                                 end = NULL,
                                 weight_col = NULL) {

  ## No end date supplied, we snapshot at start date

  if (is.null(end)) {
    end <- start
  }

  start <- lubridate::ymd(start)
  end <- lubridate::ymd(end)

  #### Filter by date range

  affils_by_date <- affils_by_date %>%
    filter(Start.Date <= end
           & End.Date >= start)

  ## No weight col supplied, we make one to weight everything as 1

  if (is.null(weight_col)) {

    affils_by_date <- affils_by_date %>%
      mutate(
        weight = 1
      )

    weight_col = "weight"

  }


  tmp <-  affils_by_date %>%
    select(Member.ID, on_cols[1], weight_col) %>%
    drop_na(on_cols[1])


    bad <- is.na(tmp[[on_cols[1]]]) | tmp[[on_cols[1]]] == ""
    tmp[bad, weight_col] <- 0

    affil_mat <- tmp %>%
      distinct() %>%
      tidyr::pivot_wider(names_from = Member.ID,
                values_from = weight_col,
                values_fill = 0) %>%
    select(-on_cols[1]) %>%
    as.matrix() %>%
    crossprod()

  for(i in 2:length(on_cols)) {

     tmp <- affils_by_date  %>%
      select(Member.ID, on_cols[1:i], weight_col) %>%
      drop_na(on_cols[1])

      bad <- bad | is.na(tmp[[on_cols[i]]]) | tmp[[on_cols[i]]] == ""
      tmp[bad, weight_col] <- 0

      affil_mat_2 <- tmp %>%
        distinct() %>%
        tidyr::pivot_wider(names_from = Member.ID,
                         values_from = weight_col,
                         values_fill = 0) %>%
      select(-on_cols[1:i]) %>%
      as.matrix() %>%
      crossprod()


    affil_mat <- affil_mat + affil_mat_2


  }

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
                 values_to = "weight") %>%
    filter(parse_number(from) < parse_number(to)) %>%
    filter(weight > 0)


  # # drop duplicates
  # edgelist <- edgelist %>%
  #   mutate(
  #     c1 = map2_chr(from, to, ~c(.x, .y) %>% min()),
  #     c2 = map2_chr(to, from, ~c(.x, .y) %>% max())
  #   ) %>%
  #   distinct(c1, c2, .keep_all = TRUE)

  if (get_edge_names) {

  edgelist <- edgelist %>%
    mutate(
      #edge_members = "OOPS"
      edge_orgs = map2_chr(to, from, ~find_edge_members(affils_by_date, "Member.ID", "Umbrella", "Umbrella.Name", .x, .y))
    )
  }

  return(edgelist)

}
