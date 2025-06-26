#' @export
#' @import ggplot2
#' @import dplyr
hm_plot <- function(swm_base) {
    ggplot(swm_base, aes(x = hopital_manager)) +
        geom_bar() +
        theme_minimal() +
        labs(
            title = "Présence de HM",
            x = "solution",
            y = "Nombre d'établissements"
        )
}

#' @export
#' @import ggplot2
#' @import dplyr
forme_juridique_plot <- function(swm_base) {
    ggplot(swm_base, aes(x = forme_juridique, fill = forme_juridique)) +
        geom_bar() +
        scale_fill_viridis_d(name = "Forme juridique") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(
            title = "Forme juridique",
            x = "Forme juridique",
            y = "Nombre d'établissements"
        )
}

#' @export
#' @import ggplot2
#' @import dplyr
nombre_lits_plot <- function(swm_base) {
    nb_etablissements_swm <- nrow(swm_base)
    summary_lits_swm <- summarize_lits_swm(swm_base) %>%
        mutate(nb_etablissements_total = nb_etablissements_swm)

    summary_lits_swm %>%
        ggplot(aes(x = type_lit, y = nb_etablissements, fill = type_lit)) +
        geom_col() +
        scale_fill_viridis_d(name = "Type de lit") +
        theme_minimal() +
        labs(
            title = "Nombre d'établissements par type de lit",
            x = "Type de lit",
            y = "Nombre d'établissements"
        )
}

#' @export
#' @import ggplot2
#' @import dplyr
nombre_lits_par_type_lit <- function(swm_base_summarized) {
    swm_base_summarized %>%
        filter(indicator == "nb_lits") %>%
        ggplot(aes(x = type_lit, y = value, fill = type_lit)) +
        geom_col() +
        scale_fill_viridis_d(name = "Type de lit") +
        scale_y_continuous(
            labels = scales::comma,
            breaks = seq(0, max(swm_base_summarized$value), 25000)
        ) +
        theme_minimal() +
        labs(
            title = "Nombre de lits par type de lit",
            x = "Type de lit",
            y = "Nombre de lits"
        )
}
