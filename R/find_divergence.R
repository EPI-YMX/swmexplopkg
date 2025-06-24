#' @noRd
find_divergence <- function(
    swm_base_hm,
    t_finess_4326,
    col_name_swm,
    col_name_finess
) {
    swm <- swm_base_hm %>%
        filter(!!sym(col_name_swm) == TRUE) %>%
        select(finess_geographique, nom_du_compte, !!sym(col_name_swm))

    finess <- t_finess_4326 %>%
        st_drop_geometry() %>%
        filter(!!sym(col_name_finess) == FALSE) %>%
        select(finess, !!sym(col_name_finess))

    join1 <- inner_join(
        swm,
        finess,
        by = c("finess_geographique" = "finess"),
        suffix = c("_swm", "_finess")
    )

    swm <- swm_base_hm %>%
        filter(!!sym(col_name_swm) == FALSE) %>%
        select(finess_geographique, nom_du_compte, !!sym(col_name_swm))

    finess <- t_finess_4326 %>%
        st_drop_geometry() %>%
        filter(!!sym(col_name_finess) == TRUE) %>%
        select(finess, !!sym(col_name_finess))

    join2 <- inner_join(
        swm,
        finess,
        by = c("finess_geographique" = "finess"),
        suffix = c("_swm", "_finess")
    )

    bind_rows(join1, join2) %>%
        rename(
            valeur_swm = !!sym(col_name_swm),
            valeur_finess = !!sym(col_name_finess)
        )
}
