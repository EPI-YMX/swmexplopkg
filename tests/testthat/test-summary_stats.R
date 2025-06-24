swm_base <- data_clean_swm_base(
  system.file(
    "data",
    "Base clients HM sur SMS avec statut et activité.xlsx",
    package = "swmexplopkg"
  )
)

finess_raw <- data.table::fread(system.file(
  "data",
  "t-finess.csv",
  package = "swmexplopkg"
))
swm_base <- keep_only_active_only_on_finess(swm_base, finess_raw)

swm_base_hm <- swm_base %>%
  filter(hopital_manager == "HM")

swm_base_long <- reshape_swm_base_long(swm_base_hm)
swm_base_summarized <- count_swm_base_long(swm_base_long)
t_finess_4326 <- data_clean_finess(
  system.file("data", "t-finess.csv", package = "swmexplopkg"),
  keep_only_mco_ssr_psy = TRUE
)

test_that("reshape_swm_base_long works", {
  expect_equal(nrow(swm_base_long), nrow(swm_base_hm) * 8)

  lits <- dplyr::filter(swm_base_long, indicator == "nb_lits")

  expect_equal(
    lits %>%
      dplyr::filter(type_lit == "MCO+SSR+PSY") %>%
      dplyr::pull(value) %>%
      sum(),
    swm_base_hm %>% dplyr::pull(nombre_de_lits_et_places_total) %>% sum()
  )

  etablissements <- dplyr::filter(
    swm_base_long,
    indicator == "nb_etablissements"
  )

  etablissements_mco <- dplyr::filter(etablissements, type_lit == "MCO") %>%
    pull(value) %>%
    sum()
  swm_base_hm_mco <- swm_base_hm %>% pull(has_mco) %>% sum()

  expect_equal(etablissements_mco, swm_base_hm_mco)

  etablissements_ssr <- dplyr::filter(etablissements, type_lit == "SSR") %>%
    pull(value) %>%
    sum()
  swm_base_hm_ssr <- swm_base_hm %>% pull(has_ssr) %>% sum()

  expect_equal(etablissements_ssr, swm_base_hm_ssr)

  etablissements_psy <- dplyr::filter(etablissements, type_lit == "PSY") %>%
    pull(value) %>%
    sum()
  swm_base_hm_psy <- swm_base_hm %>% pull(has_psy) %>% sum()

  expect_equal(etablissements_psy, swm_base_hm_psy)
})


stats_dept_swm <- stats_by_dept(
  system.file("data", "departements.geojson", package = "swmexplopkg"),
  swm_base_long
)
test_that("stats_by_dept works", {
  dept02 <- stats_dept_swm %>%
    filter(code == "02")

  nb_etablissements_dept02 <- dept02 %>%
    filter(indicator == "nb_etablissements") %>%
    pull(value) %>%
    sum()

  expect_equal(
    nb_etablissements_dept02,
    swm_base_hm %>% filter(dept == "02") %>% nrow()
  )

  dept33 <- stats_dept_swm %>%
    filter(code == "33")

  nb_etablissements_dept33 <- dept33 %>%
    filter(indicator == "nb_etablissements")

  expect_equal(
    nb_etablissements_dept33 %>%
      filter(type_lit == "MCO") %>%
      pull(value) %>%
      sum(),
    swm_base_hm %>% filter(dept == "33") %>% filter(has_mco) %>% nrow()
  )

  expect_equal(
    nb_etablissements_dept33 %>%
      filter(type_lit == "SSR") %>%
      pull(value) %>%
      sum(),
    swm_base_hm %>% filter(dept == "33") %>% filter(has_ssr) %>% nrow()
  )

  expect_equal(
    nb_etablissements_dept33 %>%
      filter(type_lit == "PSY") %>%
      pull(value) %>%
      sum(),
    swm_base_hm %>% filter(dept == "33") %>% filter(has_psy) %>% nrow()
  )

  expect_equal(
    nb_etablissements_dept33 %>%
      filter(type_lit == "MCO+SSR+PSY") %>%
      pull(value) %>%
      sum(),
    swm_base_hm %>%
      filter(dept == "33") %>%
      filter(has_mco | has_ssr | has_psy) %>%
      nrow()
  )

  nb_lits_dept33 <- dept33 %>%
    filter(indicator == "nb_lits")

  expect_equal(
    nb_lits_dept33 %>%
      filter(type_lit == "MCO+SSR+PSY") %>%
      pull(value) %>%
      sum(),
    swm_base_hm %>%
      filter(dept == "33") %>%
      pull(nombre_de_lits_et_places_total) %>%
      sum()
  )

  expect_equal(
    nb_lits_dept33 %>% filter(type_lit == "MCO") %>% pull(value) %>% sum(),
    swm_base_hm %>%
      filter(dept == "33") %>%
      pull(nombre_de_lits_et_places_mco) %>%
      sum()
  )

  expect_equal(
    nb_lits_dept33 %>% filter(type_lit == "SSR") %>% pull(value) %>% sum(),
    swm_base_hm %>%
      filter(dept == "33") %>%
      pull(nombre_de_lits_et_places_ssr) %>%
      sum()
  )

  expect_equal(
    nb_lits_dept33 %>% filter(type_lit == "PSY") %>% pull(value) %>% sum(),
    swm_base_hm %>%
      filter(dept == "33") %>%
      pull(nombre_de_lits_et_places_psy) %>%
      sum()
  )

  expect_equal(
    nb_lits_dept33 %>%
      filter(type_lit == "MCO+SSR+PSY") %>%
      pull(value) %>%
      sum(),
    swm_base_hm %>%
      filter(dept == "33") %>%
      pull(nombre_de_lits_et_places_total) %>%
      sum()
  )
})

stats_dept_finess <- stats_by_dept_finess(t_finess_4326)
test_that("stats_by_dept_finess works", {
  dept02 <- stats_dept_finess %>%
    filter(dept == "02")

  t_finess_4326_dept02 <- t_finess_4326 %>%
    filter(dept == "02")

  dept02_mco <- dept02 %>%
    filter(type_lit == "MCO") %>%
    pull(value) %>%
    sum()

  t_finess_4326_dept02_mco <- t_finess_4326_dept02 %>%
    filter(mco) %>%
    nrow()

  expect_equal(dept02_mco, t_finess_4326_dept02_mco)

  dept02_ssr <- dept02 %>%
    filter(type_lit == "SSR") %>%
    pull(value) %>%
    sum()

  t_finess_4326_dept02_ssr <- t_finess_4326_dept02 %>%
    filter(ssr) %>%
    nrow()
  expect_equal(dept02_ssr, t_finess_4326_dept02_ssr)

  dept02_psy <- dept02 %>%
    filter(type_lit == "PSY") %>%
    pull(value) %>%
    sum()

  t_finess_4326_dept02_psy <- t_finess_4326_dept02 %>%
    filter(psy) %>%
    nrow()

  expect_equal(dept02_psy, t_finess_4326_dept02_psy)

  dept02_mco_ssr_psy <- dept02 %>%
    filter(type_lit == "MCO+SSR+PSY") %>%
    pull(value) %>%
    sum()

  t_finess_4326_dept02_mco_ssr_psy <- t_finess_4326_dept02 %>%
    filter(mco | ssr | psy) %>%
    pull(finess) %>%
    unique() %>%
    length()

  expect_equal(dept02_mco_ssr_psy, t_finess_4326_dept02_mco_ssr_psy)
})
