#' Pairwise P Values from Similarity Matrix
#'
#' @param sim_matx Similarity matrix, output from ontologySimilarity::get_sim_grid()
#'
#' @return Matrix of P values
#' @export

pairwise_p <- function(sim_matx) {

    # Create list of pair indices:

    pair_list <- expand.grid(1:nrow(sim_matx), 1:ncol(sim_matx)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(pair = list(c(Var1, Var2))) %>%
        .$pair



    # Apply `get_sim_p()` to each pair, diagonal is auto-zero

    p_values <- purrr::map_dbl(pair_list, ~ {

        if (.[1] == .[2]) {
            return(0)
        }

        else {
            return(ontologySimilarity::get_sim_p(sim_matx, group = .))
        }
    }
    )

    return(
        matrix(p_values,
               nrow = nrow(sim_matx),
               ncol = ncol(sim_matx)))

}

#------------------------------------------------------------

#' Pairwise P-Values Calculated from Ontology & Term Sets
#'
#' @param term_set list of character vectors contain ontology term IDs
#' @param ontology `ontology_index` object
#' @param ic information content, named numeric list
#' @param method "lin" or "resnik
#'
#' @return matrix of p-values for each pair in term set (by index)
#' @export

pairwise_p_ontology <- function(term_set, ontology, ic, method) {
    # Create list of pair indices:

    n_obs <- length(term_set)

    pair_list <- expand.grid(1:n_obs, 1:n_obs) %>%
        rowwise() %>%
        mutate(pair = list(c(Var1, Var2))) %>%
        .$pair

    p_values <- purrr::map_dbl(pair_list, ~ {
        if (.[1] == .[2]) {
            return(0)
        }

        else {
            return(ontologySimilarity::get_sim_p_from_ontology(ontology = ontology,
                                           information_content = ic,
                                           term_sets = term_set,
                                           method = method,
                                           group = .))
        }
    })

    return(
        matrix(p_values,
               nrow = n_obs,
               ncol = n_obs))

}
