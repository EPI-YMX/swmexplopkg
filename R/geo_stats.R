#' @noRd
#' @import dplyr
#' @import sf
stats_by_dept <- function(dept_path, swm_base_long) {
    dept <- sf::read_sf(dept_path) %>%
        st_transform(4326)

    counts <- swm_base_long %>%
        group_by(dept, type_lit, indicator) %>%
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

    swm_base_long_with_dept <- dept %>%
        st_drop_geometry() %>%
        left_join(counts, by = c("code" = "dept")) %>%
        complete(
            code = dept$code,
            type_lit = unique(swm_base_long$type_lit),
            indicator = unique(swm_base_long$indicator),
            fill = list(value = 0)
        ) %>%
        select(-nom)

    dept %>%
        left_join(swm_base_long_with_dept, by = c("code" = "code")) %>%
        filter(!is.na(type_lit))
}


stats_by_dept_finess <- function(t_finess_4326) {
    finess_long <- st_drop_geometry(t_finess_4326) %>%
        pivot_longer(
            cols = c(mco, ssr, psy),
            names_to = "type_lit",
            values_to = "nb_etablissements"
        )

    stats_finess <- finess_long %>%
        group_by(dept, type_lit) %>%
        summarise(
            value = sum(nb_etablissements, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        mutate(type_lit = toupper(type_lit)) %>%
        mutate(indicator = "nb_etablissements")

    stats_globales_finess <- finess_long %>%
        group_by(dept) %>%
        summarise(value = n_distinct(finess, na.rm = TRUE)) %>%
        mutate(type_lit = "MCO+SSR+PSY") %>%
        distinct() %>%
        mutate(indicator = "nb_etablissements") %>%
        select(dept, type_lit, value, indicator)

    stats_dept_finess <- bind_rows(
        stats_finess,
        stats_globales_finess
    )
    stats_dept_finess
}
