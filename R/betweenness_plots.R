#------- By Individual ------#

#' Find the betweenness measures for a set of individuals over a time period
#'
#' @param betweenness_list A dataframe of betweenness measures by date, in the
#' format of built-in dataset `all_betweenness_opp`
#' @param group_col An unquoted variable name specifying a column to group on.
#' @param rolling_avg An integer.  Number of months before and after target
#' date to compute a rolling average of, for smoothing.  Use `0` for no smoothing.
#' @param start A start of date range, in DD-MM-YYYY string format.  Defaults to beginning of dates in data.
#' @param end An end of date range, in DD-MM-YYYY string format. Defaults to end of dates in data.
#'
#' @return A ggplot object showing the centrality (betweenness) of individuals
#' over time.
#'
#' @import dplyr
#' @import lubridate
#' @import ggplot2
#' @export
plot_betweenness_mem <- function(betweenness_list,
                                 group_col = Member.ID,
                                 rolling_avg = 6,
                                 start = NULL, end = NULL) {

  bet_by_group <- betweenness_list %>%
    group_by({{group_col}}, Start.Date) %>%
    summarize(
      avg_betweenness = mean(Betweenness),
      num_people = n()
    ) %>%
    mutate(
      betweenness = slider::slide_dbl(avg_betweenness,
                                      ~mean(.x),
                                      .before = rolling_avg,
                                      .after = rolling_avg)
    )

  bet_by_group %>%
    ggplot(aes(x = Start.Date,
               y = betweenness,
               color = {{group_col}})) +
    geom_line()


}
