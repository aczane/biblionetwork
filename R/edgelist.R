#' Creates an edgelist
#'
#' \code{make_edgelist} generates an edgelist (a co-occurrence list) from
#' relational data
#'
#' This generates edgelists for \strong{undirected graphs} from (typically)
#' bibliometric data It is built off of work by
#' \href{https://www.r-bloggers.com/turning-keywords-into-a-co-occurrence-network/}{Francois
#' Briatte}. Input data are usually of two common forms:
#'
#' 1. A co-citation network: The nodes are publications (one node = one
#' publication), and the edges connect publications that share references, i.e.,
#' if publication 1 and publication 2 both cite reference A, they are connected
#' by an edge.
#'
#' 2. A co-author network: The nodes are authors (one node = one author), and
#' the edges connect authors who wrote a publication together.


#' @return The output is a df with three columns:
#' X1 = head node, X2 = end node, weight = numeric weight of edge
#' (these are undirected networks, so edge pairs (i,j) and (j,i) are equivalent).

#' @param df dataframe containing the relevant columns
#' @param node_col column containing concatenated string values of co-occuring
#'   node identifiers, e.g., 1 row = nodeID1, nodeID2, nodeID3
#' @param sep separator character in \code{node_col}, e.g., " , "

#' @export
make_edgelist <- function(df, node_col, separator){
  node_col <- enquo(node_col)

  edge_list <-
    df %>%
    pull(!!node_col) %>%
    stringr::str_split(separator) %>%
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

