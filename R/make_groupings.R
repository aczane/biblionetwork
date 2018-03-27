#' Prepares (untidies) tidy data for edgelist creation
#'
#' \code{make_groupings} untidies data. It groups column node values by some
#' shared grouping variable, and concatenates these nodes into one list of strings per
#' grouping variable.
#'
#' The resulting dataframe can then be used as input for \code{make_edgelist}
#' and \code{make_nodelist}.

#' @return The output is a df with at least two columns:
#' the grouping variable, and the concatenated nodes.

#' @param df dataframe containing the relevant columns
#' @param node_col column containing node IDs in separate rows
#' @param grouping column containing the grouping variable - the variable that certain nodes have in common
#' @param sep desired separator character in \code{node_col}, e.g., " , "

#' @export
make_groupings <- function(df, node_col, grouping, separator){
  node_col <- enquo(node_col)
  grouping <- enquo(grouping)

  untidied <-
    df %>%
    pull(!!node_col) %>%
    str_split(separator) %>%
    map_dfr(function(x) {expand.grid(x, x, stringsAsFactors = FALSE)}) %>%
    filter(!Var1 == Var2) %>%
    mutate(X1 = ifelse(Var1 < Var2, Var1, Var2),
           X2 = ifelse(X1 == Var1, Var2, Var1)) %>%
    select(X1, X2) %>%
    mutate(w = 1) %>%
    group_by(X1, X2) %>%
    summarise(weight = sum(w)/2) %>%
    ungroup()
}

