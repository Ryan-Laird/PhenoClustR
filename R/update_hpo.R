#' @title Update Version of Human Phenotype Ontology
#'
#' @description Internal function to fetch latest version of the Human Phenotype Ontology and save it as an R object, `hpo`, availble upon package load.
#'
#' @examples
#' update_hpo()
update_hpo <- function() {
    hpo <- ontologyIndex::get_ontology(curl::curl(
        url = 'http://purl.obolibrary.org/obo/hp.obo'
    ))


    usethis::use_data(hpo,
                      overwrite = TRUE,
                      version = 3)
}
