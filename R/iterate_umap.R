#' Run UMAP with many combinations of hyperparameters.
#'
#' @param dat Unlabeled dataframe for UMAP input
#' @param dat.labels Labels for dat
#' @param params Sqaure dataframe of UMAP hyperparameter values (all required):
#' - n_neighbors
#' - min_dist
#' - n_components
#' - metric
#' - method
#' - seed
#'
#' @return Nested tibble::tibble() containing UMAP objs, output layout, labeled output, and 2D/3D plot obj.
#' @export
#'
iterate_umap <- function(dat, dat.labels, params) {
    umap_all <- list()
    dat_all <- list()
    labels_all <- list()


    for (i in 1:nrow(params)) {
        umap_obj <- umap::umap(dat,
                               n_neighbors = params$n_neighbors[i],
                               min_dist = params$min_dist[i],
                               n_components = params$n_components[i],
                               metric = params$metric[i],
                               method = params$method[i],
                               seed = params$seed[i])

        umap_all[[i]] <- umap_obj
    }

    df <- tibble::tibble(umap_obj = umap_all)

    df %>%

        mutate(layout = purrr::map(umap_obj, function(.) {as.data.frame(.$layout)}),
               data = purrr::map(layout, function(.) {cbind(., dat.labels)})) %>%

        mutate(umap_plot = purrr::map2(umap_obj, data, plot_umap_bulk))
}
