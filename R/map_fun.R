#' @export
#' @import dplyr
#' @import sf
add_coordinates_in_swm <- function(swm_base, finess_raw) {
    finess_raw_as_sf <- finess_raw %>%
        st_as_sf(coords = c("geoloc_4326_long", "geoloc_4326_lat")) %>%
        st_set_crs(4326)

    finess_raw_as_sf %>%
        select(finess) %>%
        inner_join(
            swm_base,
            by = c("finess" = "finess_geographique"),
            suffix = c("_swm", "_finess")
        )
}


#' @export
#' @import leaflet
ratio_map <- function(data_map) {
    pal <- colorNumeric(
        palette = "inferno",
        domain = c(0, 100),
        reverse = TRUE
    )

    leaflet(data_map) %>%
        addTiles() %>%
        addPolygons(
            fillColor = ~ pal(ratio_etablissements),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE
            ),
            label = ~ paste0(nom, ": ", round(ratio_etablissements, 1), "%"),
            labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                direction = "auto"
            )
        ) %>%
        addLegend(
            "bottomright",
            pal = pal,
            values = ~ratio_etablissements,
            title = "Ratio établissements SWM/FINESS",
            labFormat = labelFormat(prefix = ""),
            opacity = 1
        )
}

#' @export
#' @import leaflet
#' @import dplyr
#' @import sf
stats_by_dept_map <- function(stats_dept_swm, indicator_used, title) {
    stats_by_dept_all <- stats_dept_swm %>%
        filter(type_lit == "MCO+SSR+PSY") %>%
        filter(indicator == indicator_used)

    tooltip <- stats_dept_swm %>%
        st_drop_geometry() %>%
        filter(indicator == indicator_used) %>%
        group_by(code, nom, indicator) %>%
        summarise(
            tooltip_detail = paste(
                paste0(type_lit, ": ", value),
                collapse = "<br>"
            ),
            .groups = "drop"
        )

    stats_by_dept_all <- stats_by_dept_all %>%
        left_join(tooltip, by = c("code", "nom", "indicator"))

    pal <- colorNumeric(
        palette = "inferno",
        domain = stats_by_dept_all$value,
        reverse = TRUE
    )

    leaflet(stats_by_dept_all) %>%
        addTiles() %>%
        addPolygons(
            fillColor = ~ pal(value),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE
            ),
            label = ~ lapply(tooltip_detail, HTML),
            labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                direction = "auto"
            )
        ) %>%
        addLegend(
            "bottomright",
            pal = pal,
            values = ~value,
            title = title,
            labFormat = labelFormat(prefix = ""),
            opacity = 1
        )
}
