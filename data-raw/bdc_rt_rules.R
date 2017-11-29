library(biodatacoreUtils)

bdc_rt_intervals <- c('0.0-3.5', '3.5-6.0', '6.0-6.5', '6.5-7.0')

bdc_rt_tol_rules <-
  expand.grid(
    purrr::flatten_chr(unique(bdc_set_rules)),
    purrr::flatten_chr(unique(bdc_set_rules)),
    bdc_rt_intervals
  ) %>%
  dplyr::as_data_frame() %>%
  dplyr::mutate_all(as.character) %>%
  purrr::set_names('dset_1', 'dset_2', 'rt_interval') %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    tolerance = dplyr::case_when(
      # set_1 - set_1
      setequal(c(dset_1, dset_2), c('set_1')) & rt_interval == '0.0-3.5' ~ 0.05,
      setequal(c(dset_1, dset_2), c('set_1')) & rt_interval == '3.5-6.0' ~ 0.10,
      setequal(c(dset_1, dset_2), c('set_1')) & rt_interval == '6.0-6.5' ~ 0.05,
      setequal(c(dset_1, dset_2), c('set_1')) & rt_interval == '6.5-7.0' ~ 0.03,

      # set_2 - set_2
      setequal(c(dset_1, dset_2), c('set_2')) & rt_interval == '0.0-3.5' ~ 0.10,
      setequal(c(dset_1, dset_2), c('set_2')) & rt_interval == '3.5-6.0' ~ 0.20,
      setequal(c(dset_1, dset_2), c('set_2')) & rt_interval == '6.0-6.5' ~ 0.15,
      setequal(c(dset_1, dset_2), c('set_2')) & rt_interval == '6.5-7.0' ~ 0.05,

      # set1 - set2
      setequal(c(dset_1, dset_2), c('set_1', 'set_2')) & rt_interval == '0.0-3.5' ~ 0.20,
      setequal(c(dset_1, dset_2), c('set_1', 'set_2')) & rt_interval == '3.5-6.0' ~ 0.30,
      setequal(c(dset_1, dset_2), c('set_1', 'set_2')) & rt_interval == '6.0-6.5' ~ 0.20,
      setequal(c(dset_1, dset_2), c('set_1', 'set_2')) & rt_interval == '6.5-7.0' ~ 0.05,
      TRUE ~ rlang::na_dbl
    )
  ) %>%
  dplyr::arrange(.data$dset_1, .data$dset_2)

devtools::use_data(bdc_rt_tol_rules)
