#' Quick 2D/3D Plot from UMAP Output
#'
#' @param umap_obj Object returned from `umap::umap()`
#' @param label_df Dataframe with labels for umap obj layout
#' @param interact Use plotly to create interactive plot
#' @description Currently only supports dataframe with 'mrn' column for labeling. Will make more general.
#'
#' @return ggplot2 / plotly obj.
#' @export
#'
plot_umap <- function(umap_obj, label_df, interact = TRUE) {

    theme_set(theme_minimal())

    dat <- umap_obj$layout %>% as.data.frame() %>% cbind(label_df)

    title <- deparse(substitute(umap_obj))

    conf <- umap_obj$config

    subtitle <- glue_data(conf,
                          "n neighbors: {n_neighbors}  |  min dist.: {min_dist}  |  metric: {metric}  |  method: {method}")

    p <- dat %>%

        ggplot(aes(V1, V2, text = mrn)) +
        geom_point(alpha = 0.2) +

        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              panel.grid = element_blank())

    if (interact == TRUE) {

        if (conf$n_components == 2) {

            ggplotly(p, tooltip = "text") %>%
                layout(title =
                           list(text =
                                    paste0(title,
                                           '<br>',
                                           '<sup>',
                                           subtitle,
                                           '</sup>'),
                                x = 0))
        }

        else {

            dat %>% plot_ly(x = ~V1, y = ~V2, z = ~V3,
                            type = "scatter3d", mode = "markers", text = ~mrn,
                            size = 1, alpha = .2) %>%

                layout(title =
                           list(text =
                                    paste0(title,
                                           '<br>',
                                           '<sup>',
                                           subtitle,
                                           '</sup>'),
                                x = 0))
        }
    }

    else {
        p + labs(title = title, subtitle = subtitle)
    }
}



#' Iterable quick 2D/3D Plot from UMAP Output, for use with `PhenoClustR::iterate_umap()`
#'
#' @param umap_obj Object returned from `umap::umap()`
#' @param label_df Dataframe with labels for umap obj layout
#' @param interact Use plotly to create interactive plot
#' @description Currently only supports dataframe with 'mrn' column for labeling. Will make more general.
#'
#' @return ggplot2 / plotly obj.
#'
plot_umap_bulk <-  function(umap_obj, label_df, interact = FALSE) {

    theme_set(theme_minimal())

    dat <- label_df

    conf <- umap_obj$config

    title <- glue_data(conf,
                       "n neighbors: {n_neighbors}  |  min dist.: {min_dist}
                        metric: {metric}  |  method: {method}")

    p <- dat %>%

        ggplot(aes(V1, V2, text = mrn)) +
        geom_point(alpha = 0.2) +

        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              panel.grid = element_blank())

    if (conf$n_components == 2) {

        if (interact == TRUE) {

            ggplotly(p, tooltip = "text") %>%
                layout(title =
                           list(text = title,
                                x = 0))
        }
        else {
            p + labs(subtitle = title)
        }

    }


    else {

        dat %>% plot_ly(x = ~V1, y = ~V2, z = ~V3,
                        type = "scatter3d", mode = "markers", text = ~mrn,
                        size = 1, alpha = .2) %>%

            layout(title =
                       list(text = title,
                            x = 0))
    }
}
