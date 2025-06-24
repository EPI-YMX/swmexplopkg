#' @noRd
#' @import dplyr
#' @import tidyr
summarize_lits_swm <- function(swm_base_hm) {
    swm_base_hm %>%
        summarise(
            mco = sum(has_mco, na.rm = TRUE),
            ssr = sum(has_ssr, na.rm = TRUE),
            psy = sum(has_psy, na.rm = TRUE)
        ) %>%
        pivot_longer(
            cols = c(mco, ssr, psy),
            names_to = "type_lit",
            values_to = "nb_etablissements"
        ) %>%
        mutate(source = "swm")
}

# Function to reshape SWM base for long format
reshape_swm_base_long <- function(swm_base_hm) {
    swm_base_long <- swm_base_hm %>%
        pivot_longer(
            cols = c(
                nombre_de_lits_et_places_mco,
                nombre_de_lits_et_places_ssr,
                nombre_de_lits_et_places_psy,
                nombre_de_lits_et_places_total
            ),
            names_to = "type_lit",
            values_to = "nb_lits"
        ) %>%
        mutate(
            type_lit = case_when(
                type_lit == "nombre_de_lits_et_places_mco" ~ "MCO",
                type_lit == "nombre_de_lits_et_places_ssr" ~ "SSR",
                type_lit == "nombre_de_lits_et_places_psy" ~ "PSY",
                type_lit == "nombre_de_lits_et_places_total" ~ "MCO+SSR+PSY",
                TRUE ~ NA_character_
            )
        ) %>%
        mutate(
            type_lit = factor(
                type_lit,
                levels = c("MCO", "SSR", "PSY", "MCO+SSR+PSY")
            )
        ) %>%
        mutate(indicator = "nb_lits", value = nb_lits) %>%
        select(-nb_lits)

    numeric_indicator_nb_etablissements <- swm_base_long %>%
        mutate(indicator = "nb_etablissements") %>%
        mutate(
            value = case_when(
                type_lit == "MCO" ~ has_mco,
                type_lit == "SSR" ~ has_ssr,
                type_lit == "PSY" ~ has_psy,
                type_lit == "MCO+SSR+PSY" ~ has_mco | has_ssr | has_psy,
                TRUE ~ NA
            )
        )

    bind_rows(swm_base_long, numeric_indicator_nb_etablissements) %>%
        select(
            finess_geographique,
            forme_juridique,
            dept,
            type_lit,
            indicator,
            value
        )
}

#' @noRd
count_swm_base_long <- function(swm_base_long) {
    swm_base_long %>%
        group_by(type_lit, indicator) %>%
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
}
