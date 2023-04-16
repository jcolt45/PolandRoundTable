#------- By Individual ------#

#' Find the betweenness measures for a set of individuals over a time period
#'
#' @param metric_df A dataframe of betweenness measures by date, in the
#' format of built-in dataset `all_metrics`
#' @param metric_col An unquoted variable name specifying a column with the metric
#' of interest.
#' @param group_col (Optional) An unquoted variable name specifying a column to group by.
#' @param rolling_avg An integer.  Number of months before and after target
#' date to compute a rolling average of, for smoothing.  Use `0` for no smoothing.
#' @param start A start of date range, in DD-MM-YYYY string format.  Defaults to beginning of dates in data.
#' @param end An end of date range, in DD-MM-YYYY string format. Defaults to end of dates in data.
#'
#' @return A ggplot object showing the metric of individuals over time.
#'
#' @import dplyr
#' @import lubridate
#' @import ggplot2
#' @export
plot_metric <- function(metric_df,
                            metric_col,
                            group_col = Member.ID,
                            rolling_avg = 6,
                            start = NULL, end = NULL) {

  metric_by_group <- metric_df %>%
    group_by({{group_col}}, Start.Date) %>%
    summarize(
      avg_metric = mean({{metric_col}}),
      num_people = n()
    ) %>%
    mutate(
      betweenness = slider::slide_dbl(avg_metric,
                                      ~mean(.x),
                                      .before = rolling_avg,
                                      .after = rolling_avg)
    )

  metric_by_group %>%
    ggplot(aes(x = Start.Date,
               y = avg_metric,
               color = {{group_col}})) +
    geom_line()


}
