####################                                      ####################
####################  Funtions for ontology manipulation  ####################
####################                                      ####################




#' @title Get a directed adjacency matrix from `ontologyIndex` object
#'
#' @param ontology `ontologyIndex` object
#' @param terms named ontology term list
#' @description Adapted from package `ontologyPlot` so entire package doesn't have to be installed, as it requires Rgraphviz.
#' @return sqaure directed adjacency matrix
#' @export
#'
#' @examples
#' get_adjacency_matrix(hpo, ontologyIndex::get_descendants(hpo, "HP:0000118"))
#'
get_adjacency_matrix <- function(ontology, terms) {
  names(terms) <- terms
  adj.mat <- sapply(
    terms,
    function(term) terms %in% ontology$parents[[term]]
  )
  rownames(adj.mat) <- colnames(adj.mat)
  t(adj.mat)
}


#-----------------------------------------------------------------------------------------------

#' @title Get an undirected adjacency matrix from `ontologyIndex` object
#'
#' @param ontology `ontologyIndex` object
#' @param terms named ontology term list
#' @description Difers from `get_adjacency_matrix()` in that it returns an undirected matrix. Good starting point for enrichment.
#' @return sqaure undirected adjacency matrix
#' @export
#'
#' @examples
#' get_undirected_matrix(hpo, ontologyIndex::get_descendants(hpo, "HP:0000118"))
#'
get_undirected_matrix <- function(ontology, terms) {
  names(terms) <- terms
  adj.mat <- sapply(
    terms,
    function(term) terms %in% ontology$parents[[term]] | terms %in% ontology$children[[term]]
  )
  rownames(adj.mat) <- colnames(adj.mat)
  t(adj.mat)
}


#-----------------------------------------------------------------------------------------------


#' @title Enrich an adjacency matrix by linking pairs if they share a given annotation
#' @description A clunky way to draw new edges between unlinked terms if they may be linked by a given annotation. E.g. if two HPO terms have a shared Orphanet annotation, draw a new edge.
#' @param adj_matrix A HPO adjacency matrix, see `get_adjacency_matrix()`.
#' @param term_pair_df Two column dataframe: 'hpo_id' & 'diseases' (a character list of disease annotations). Will make more generic.
#'
#' @return An enriched adjacency matrix.
#' @export
#'
enrich_set <- function(adj_matrix, term_pair_df) {

  row_list <- rownames(adj_matrix)
  col_list <- colnames(adj_matrix)

  for (row in 1:nrow(adj_matrix)) {
    for (col in 1:ncol(adj_matrix)){

      hpo1 <- row_list[row]
      hpo2 <- col_list[col]

      disease_ind1 <- which(term_pair_df$hpo_id == hpo1)
      disease_ind2 <- which(term_pair_df$hpo_id == hpo2)

      diseases1 <- unlist(term_pair_df$diseases[disease_ind1])
      diseases2 <- unlist(term_pair_df$diseases[disease_ind2])

      if (adj_matrix[row, col] != TRUE & hpo1 != hpo2) {
        adj_matrix[row,col] <- length(intersect(diseases1, diseases2)) > 0
      }
    }
  }
  return(adj_matrix)
}


#-------------------------------------------------------------------------------------------------


#' @title Get HPO categories for a given HPO ID.
#' @description HPO Categories are defined as children terms of "Phenotypic Abnormality", "HP:0000118". E.g. "Abnormality of Connective Tissue". If no applicable category, returns HPO root HP:0000001.
#' @param hpo `ontologyIndex` hpo object (also loaded with `PhenoClustR`)
#' @param term_id single HPO ID
#'
#' @return Named character list of HPO IDs
#' @export
#'
#' @examples
#' get_categories(hpo, "HP:0001025") # Urticaria
get_categories <- function(hpo, term_id) {

  categories <- ontologyIndex::get_term_property(hpo, "children", "HP:0000118")

  ancestors <- ontologyIndex::get_ancestors(hpo, term_id)

  term_catgry <- ancestors[which(ancestors %in% categories)]

  if (length(term_catgry) > 0) {
    return(term_catgry)
  }

  else {
    return(list("HP:0000001"))
  }
}
