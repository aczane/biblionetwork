#' Creates a (distinct) nodelist
#'
#' \code{make_nodelist} generates a nodelist from relational data for network
#' graphs
#'
#' This function, by default, gives a list of \strong{distinct} nodes only.
#' There is an option for preserving node attributions (i.e., keeping additional
#' columns in the dataframe), or dropping them.


#' @return The default output is one column with distinct nodes only, keeping no
#'   additional attributes/columns.
#'
#'   If keep_col = TRUE, keeps all additional attributes in the dataframe.
#'
#'   If extra arguments (...) are provided, these are additional arguments to
#'   distinct(); i.e., distinct(node_col, ..., .keep_all = keep_col).

#' @param df dataframe containing the relevant columns
#' @param node_col column containing concatenated string values of co-occurring
#'   node identifiers, e.g., 1 row = nodeID1, nodeID2, nodeID3
#' @param separator = separator character in \code{nodes_col}, e.g., " , "
#' @param keep_col logical, becomes the logical argument for distinct(.keep_all
#'   = )
#' @param ... other columns to use in distinct() to retain distinct combinations
#'   rather than simply distinct values of \code{node_col}


#' @export
make_nodelist <- function(df, node_col, separator, keep_col = FALSE, ...){
  node_col <- enquo(node_col)
  dist_col <- quos(...)

  node_list <-
    df %>%
    separate_rows(!!node_col, sep = separator) %>%
    dplyr::distinct(!!node_col, !!!dist_col, .keep_all = keep_col) %>%
    select(!!node_col, !!!dist_col) %>%
    arrange(!!node_col)
}



