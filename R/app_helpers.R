#' @export
find_edge_members <- function(graph_info,
                              node_var,
                              edge_var,
                              edge_labels = edge_var,
                              node_1, node_2) {


  node_1_sub <- graph_info[graph_info[[node_var]] == node_1,][[edge_var]]
  node_2_sub <- graph_info[graph_info[[node_var]] == node_2,][[edge_var]]

  edge_mems <- intersect(node_1_sub, node_2_sub)

  relevant <- graph_info[[node_var]] == node_1 & graph_info[[edge_var]] %in% edge_mems

  edge_mems_names <- graph_info[relevant, ][[edge_labels]]

  return(paste(edge_mems_names, collapse = ", "))

}

#' @import ggraph igraph
#' @import dplyr
#' @export
get_layout_df <- function(graph_df,
                          node_meta,
                          node_var,
                          prev_layout = NULL,
                          algorithm = "kk") {


  if (!is.null(prev_layout)) {

    my_layout <- create_layout(graph_from_data_frame(graph_df, directed = FALSE),
                               layout = "igraph",
                               algorithm = algorithm,
                               coords = layout_from_previous(graph_df, prev_layout)
    )

  } else {

    my_layout <- create_layout(graph_from_data_frame(graph_df, directed = FALSE),
                               layout = "igraph",
                               algorithm = algorithm)

    # my_layout <- layout_with_stress(graph_from_data_frame(graph_df)) %>%
    #   cbind(graph_df)

  }

  return(my_layout)
}

#' @export
layout_from_previous <- function(new_graph, prev_layout){

  prev_df <- cbind(prev_layout$x, prev_layout$y, prev_layout$name) %>%
    as.data.frame()

  names(prev_df) = c("x", "y", "name")

  new_node_names = data.frame(
    name = unique(c(new_graph$from, new_graph$to))
  )

  match_layout <-  prev_df %>%
    right_join(new_node_names)


  nas <- is.na(match_layout$x)

  match_layout$x[nas] = runif(sum(nas), min(prev_layout$x), max(prev_layout$y))
  match_layout$y[nas] = runif(sum(nas), min(prev_layout$y), max(prev_layout$y))

  return(as.matrix(match_layout[, c("x", "y")]))

}


# Function to generate ggplot colors (Original hcl: hues, 65 ,100)
#' @export
ggcolors <- function(n){
  hues = seq(15, 375, length = n+2)
  hcl(h = hues, l = 65, c = 100)[2:(n+1)]
}

# Function to get a Date value

#' @export
get_date <- function(year, month, day){
  lubridate::ymd(paste(year, month, day, sep = "-"))
}


# Get unique non NA-values from a column
#' @import dplyr
#' @export
get_opts_list <- function(dat, col) {

  dat %>%
    drop_na({{col}}) %>%
    distinct({{col}}) %>%
    pull({{col}}) %>%
    sort()

}
