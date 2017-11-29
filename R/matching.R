# Helpers -----------------------------------------------------------------

#' Title
#'
#' @family bdc functions
#'
#' @return vector character: Names of valid bdc datasets
#' @export
#'
bdc_dsets <- function() {
  names(get('bdc_set_rules'))
}



#' Calculates differences in absolute parts per million
#'
#' @keywords internal
#'
#' @param x,y vector numeric
#'
#' @return vector double
#' @export
#'
abs_ppm_diff <- function(x, y) {
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(y))
  stopifnot(length(x) == length(y) || length(x) == 1 || length(y) == 1)
  stopifnot(!(x == 0))
  abs((x - y) / x) * 1e6
}

#' Title
#'
#' @keywords internal
#'
#' @family bdc functions
bdc_pick_rt_interval <- function(mzid) {
  stopifnot(biodatacoreUtils::is_mzid(mzid))
  rt <- attr(mzid, 'rt')

  interval <-
    dplyr::case_when(
      dplyr::between(rt, 0, 3.5) ~ '0.0-3.5',
      dplyr::between(rt, 3.5, 6) ~ '3.5-6.0',
      dplyr::between(rt, 6, 6.5) ~ '6.0-6.5',
      dplyr::between(rt, 6.5, 7) ~ '6.5-7.0',
      TRUE ~ NA_character_
    )

  if (rlang::is_na(interval)) {
    rlang::abort('Bad interval')
  } else {
    interval
  }
}


# Filters -----------------------------------------------------------------

#' Title
#'
#' @note TRUE means to filter out
#' @keywords internal
#'
#' @family bdc functions
#' @export
bdc_filter_mz <- function(x, y, ppm_thresh = 10) {
  stopifnot(biodatacoreUtils::is_mzid(x) && biodatacoreUtils::is_mzid(y))

  x_src <- attr(x, 'source')
  y_src <- attr(y, 'source')

  stopifnot(all(c(x_src, y_src) %in% c(bdc_dsets())))

  x_mz <- attr(x, 'mz')
  y_mz <- attr(y, 'mz')

  same_source <-
    x_src == y_src

  # `NA == anything` is NA. If either source is NA, a comparison should go
  # occur.
  if (rlang::is_na(same_source) || !same_source) {

    # floating point comparison silliness
    # abs_ppm_diff(x_mz, y_mz) > ppm_thresh
    (abs_ppm_diff(x_mz, y_mz) - ppm_thresh) > .Machine$double.eps
  } else {
    TRUE
  }
}

#' Title
#'
#' @note TRUE means to filter out
#' @keywords internal
#'
#' @family bdc functions
#' @export
bdc_filter_rt <- function(x, y) {
  stopifnot(biodatacoreUtils::is_mzid(x) && biodatacoreUtils::is_mzid(y))

  bdc_set_rules <- get('bdc_set_rules')
  x_src <- attr(x, 'source')
  y_src <- attr(y, 'source')

  stopifnot(all(c(x_src, y_src) %in% c(bdc_dsets())))

  x_set <- bdc_set_rules[[x_src]]
  y_set <- bdc_set_rules[[y_src]]

  x_rt <- attr(x, 'rt')
  y_rt <- attr(y, 'rt')


  rt_intervals <- c(bdc_pick_rt_interval(x), bdc_pick_rt_interval(y))
  same_source <-
    x_src == y_src

  # `NA == anything` is NA. If either source is NA, a comparison should go
  # occur.
  if (rlang::is_na(same_source) || !same_source) {
    thresh <-
      get('bdc_rt_tol_rules') %>%
      dplyr::filter(.data$dset_1 == !!x_set) %>%
      dplyr::filter(.data$dset_2 == !!y_set) %>%
      dplyr::filter(.data$rt_interval %in% !!rt_intervals) %>%
      dplyr::pull(.data$tolerance) %>%
      min()

    abs_diff <-  abs(x_rt - y_rt)

    # floating point comparison silliness. There is this crazy example. Try
    # (3.5-3.3) == 0.2. :::diff > thresh
    (abs_diff - thresh) > .Machine$double.eps
  } else {
    TRUE
  }
}



# matching ----------------------------------------------------------------


#' Title
#'
#'
#' @param x,table list: Each element should be an mzid object.
#' @param ppm_thresh scalar numeric: ppm threshold to filter mz differences
#'
#' @family bdc functions
#' @export
bdc_mzid_match <- function(x, table, ppm_thresh = 10) {
  # stopifnot(biodatacoreUtils::is_mzids(x) && biodatacoreUtils::is_mzids(table))

  matches <-
    purrr::cross2(x, table, .filter = function(.x, .y) {
      bdc_filter_mz(.x, .y, ppm_thresh = ppm_thresh) || bdc_filter_rt(.x, .y)
    })

  #as.data.frame method for mzids from biodatacoreUtils
  matches %>%
    purrr::map_dfr(as.data.frame) %>%
    rlang::set_names(function(nm) {
      dplyr::if_else(grepl('\\.[0-9]$', nm), gsub('\\.[0-9]$', '_match', nm), nm)
    })

}

