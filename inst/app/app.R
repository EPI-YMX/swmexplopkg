library(shiny)
library(swmexplopkg)
library(DT)
library(ggplot2)
library(leaflet)
library(dplyr)
library(sf)
library(scales)
library(tidyr)

# Load and process data using package functions
swm_base <- data_clean_swm_base(
    system.file(
        "data",
        "Base clients HM sur SMS avec statut et activité.xlsx",
        package = "swmexplopkg"
    )
)

t_finess_4326 <- data_clean_finess(
    system.file("data", "t-finess.csv", package = "swmexplopkg"),
    keep_only_mco_ssr_psy = TRUE
)

# Filter SWM data to only include active establishments
finess_raw <- data.table::fread(system.file(
    "data",
    "t-finess.csv",
    package = "swmexplopkg"
))

swm_base <- keep_only_active_only_on_finess(swm_base, finess_raw)

# Filter to HM establishments only
swm_base_hm <- swm_base %>%
    filter(hopital_manager == "HM")

# Prepare data for visualizations
swm_base_long <- reshape_swm_base_long(swm_base_hm)
swm_base_summarized <- count_swm_base_long(swm_base_long)

# Prepare FINESS data
finess_long <- st_drop_geometry(t_finess_4326) %>%
    pivot_longer(
        cols = c(mco, ssr, psy),
        names_to = "type_lit",
        values_to = "nb_etablissements"
    )

# Prepare comparison data
stats_dept_swm <- stats_by_dept(
    system.file("data", "departements.geojson", package = "swmexplopkg"),
    swm_base_long
)

stats_dept_finess <- stats_by_dept_finess(t_finess_4326)

join_swm_finess <- inner_join(
    stats_dept_swm,
    stats_dept_finess,
    by = c("code" = "dept", "type_lit" = "type_lit", "indicator" = "indicator"),
    suffix = c("_swm", "_finess")
) %>%
    mutate(ratio_etablissements = value_swm / value_finess * 100) %>%
    mutate(
        ratio_etablissements = case_when(
            ratio_etablissements > 100 ~ 100,
            ratio_etablissements < 0 ~ 0,
            TRUE ~ ratio_etablissements
        )
    )

# Calculate total beds
total_beds <- swm_base_hm %>%
    summarise(
        total_beds = sum(
            nombre_de_lits_et_places_mco,
            nombre_de_lits_et_places_ssr,
            nombre_de_lits_et_places_psy,
            na.rm = TRUE
        )
    ) %>%
    pull(total_beds)

# Custom CSS for value boxes
value_box_css <- "
.value-box {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    border-radius: 10px;
    padding: 20px;
    margin: 10px;
    color: white;
    text-align: center;
    box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
}
.value-box .value {
    font-size: 2.5em;
    font-weight: bold;
    margin-bottom: 5px;
}
.value-box .label {
    font-size: 1.1em;
    opacity: 0.9;
}
.value-box.blue { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); }
.value-box.green { background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%); }
.value-box.purple { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); }
.value-box.orange { background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); }
.value-box.red { background: linear-gradient(135deg, #ffecd2 0%, #fcb69f 100%); color: #333; }
"

# UI
ui <- fluidPage(
    tags$head(
        tags$style(HTML(value_box_css))
    ),

    titlePanel("Présence de Hopital Manager en France"),

    tabsetPanel(
        # Overview tab
        tabPanel(
            "Vue d'ensemble",
            fluidRow(
                column(
                    6,
                    div(
                        class = "value-box blue",
                        div(class = "value", nrow(swm_base_hm)),
                        div(
                            class = "label",
                            "Établissements avec Hopital Manager"
                        )
                    )
                ),
                column(
                    6,
                    div(
                        class = "value-box orange",
                        div(
                            class = "value",
                            format(total_beds, big.mark = " ")
                        ),
                        div(class = "label", "Total de lits")
                    )
                )
            ),
            fluidRow(
                column(
                    12,
                    wellPanel(
                        h4("Localisation des établissements"),
                        leafletOutput("establishments_map", height = 500)
                    )
                )
            )
        ),

        # Description générale tab
        tabPanel(
            "Description générale",
            fluidRow(
                column(
                    4,
                    div(
                        class = "value-box blue",
                        div(
                            class = "value",
                            sum(swm_base_hm$has_mco, na.rm = TRUE)
                        ),
                        div(class = "label", "Établissements MCO")
                    )
                ),
                column(
                    4,
                    div(
                        class = "value-box green",
                        div(
                            class = "value",
                            sum(swm_base_hm$has_ssr, na.rm = TRUE)
                        ),
                        div(class = "label", "Établissements SSR")
                    )
                ),
                column(
                    4,
                    div(
                        class = "value-box purple",
                        div(
                            class = "value",
                            sum(swm_base_hm$has_psy, na.rm = TRUE)
                        ),
                        div(class = "label", "Établissements PSY")
                    )
                )
            ),
            fluidRow(
                column(
                    6,
                    wellPanel(
                        h4("Nombre de lits"),
                        plotOutput("nombre_lits_par_type_lit")
                    )
                ),
                column(
                    6,
                    wellPanel(
                        h4(
                            "Répartition par forme juridique"
                        ),
                        plotOutput("forme_juridique_plot")
                    )
                )
            ),
            fluidRow(
                column(
                    6,
                    wellPanel(
                        h4("Établissements par département"),
                        leafletOutput("dept_etablissements_map", height = 500)
                    )
                ),
                column(
                    6,
                    wellPanel(
                        h4("Lits par département"),
                        leafletOutput("dept_lits_map", height = 500)
                    )
                )
            )
        ),

        # Comparisons tab
        tabPanel(
            "Comparaisons par département",
            fluidRow(
                column(
                    12,
                    wellPanel(
                        h4("Comparaison avec le fichier FINESS"),
                        p(
                            "Le fichier FINESS (Fichier National des Établissements Sanitaires et Sociaux) est le fichier de référence sur les établissements de santé en France. Il est géré par l'Agence Technique de l'Information sur l'Hospitalisation (ATIH) et contient les informations officielles sur tous les établissements de santé français."
                        )
                    )
                )
            ),
            fluidRow(
                column(
                    6,
                    wellPanel(
                        h4(
                            "Ratio nombre d'établissements SWM/FINESS - MCO/SSR/PSY confondus"
                        ),
                        leafletOutput("ratio_combined_map", height = 500)
                    )
                ),
                column(
                    6,
                    wellPanel(
                        h4(
                            "Ratio nombre d'établissements SWM/FINESS - MCO seul"
                        ),
                        leafletOutput("ratio_mco_map", height = 500)
                    )
                )
            ),
            fluidRow(
                column(
                    6,
                    wellPanel(
                        h4(
                            "Ratio nombre d'établissements SWM/FINESS - SSR seul"
                        ),
                        leafletOutput("ratio_ssr_map", height = 500)
                    )
                ),
                column(
                    6,
                    wellPanel(
                        h4(
                            "Ratio nombre d'établissements SWM/FINESS - PSY seul"
                        ),
                        leafletOutput("ratio_psy_map", height = 500)
                    )
                )
            )
        )
    )
)

# Server
server <- function(input, output) {
    # Overview plots
    output$hm_presence_plot <- renderPlot({
        hm_plot(swm_base)
    })

    output$forme_juridique_plot <- renderPlot({
        forme_juridique_plot(swm_base_hm)
    })

    output$establishments_map <- renderLeaflet({
        swm_with_coordinates <- add_coordinates_in_swm(swm_base_hm, finess_raw)

        leaflet(swm_with_coordinates) %>%
            addTiles() %>%
            addMarkers(
                label = ~nom_du_compte,
                clusterOptions = markerClusterOptions()
            )
    })

    # Description générale plots
    output$nombre_lits_par_type_lit <- renderPlot({
        nombre_lits_par_type_lit(swm_base_summarized)
    })

    output$dept_etablissements_map <- renderLeaflet({
        stats_by_dept_map(
            stats_dept_swm,
            "nb_etablissements",
            "Nombre d'établissements"
        )
    })

    output$dept_lits_map <- renderLeaflet({
        stats_by_dept_map(stats_dept_swm, "nb_lits", "Nombre de lits")
    })

    # Comparison maps
    output$ratio_combined_map <- renderLeaflet({
        data_map <- join_swm_finess %>%
            filter(type_lit == "MCO+SSR+PSY")
        ratio_map(data_map)
    })

    output$ratio_mco_map <- renderLeaflet({
        data_map <- join_swm_finess %>%
            filter(type_lit == "MCO")
        ratio_map(data_map)
    })

    output$ratio_ssr_map <- renderLeaflet({
        data_map <- join_swm_finess %>%
            filter(type_lit == "SSR")
        ratio_map(data_map)
    })

    output$ratio_psy_map <- renderLeaflet({
        data_map <- join_swm_finess %>%
            filter(type_lit == "PSY")
        ratio_map(data_map)
    })
}

# Run the app
shinyApp(ui = ui, server = server)
