# TODO Tests currently don't run on 'check. It has something to do with loading
# of package data.

# Helpers -----------------------------------------------------------------
context('matching helpers')

test_that('returns absolute difference', {
  expect_equal(abs_ppm_diff(10, 9), 1e5)
  expect_equal(abs_ppm_diff(-10, -9), 1e5)
})

test_that('errors when x is 0', {
  expect_error(abs_ppm_diff(0, rnorm(1)))
})


# Filters -----------------------------------------------------------------
context('matching filters')

test_that('filters returns TRUE when source is the same', {
  ids <- biodatacoreUtils::mzids(c(1,1), c(1,1), source = c('fhs', 'fg500'))

  expect_true(bdc_filter_mz(ids[[1]], ids[[1]]))
  expect_true(bdc_filter_rt(ids[[1]], ids[[1]]))

  expect_false(bdc_filter_mz(ids[[1]], ids[[2]]))
  expect_false(bdc_filter_rt(ids[[1]], ids[[2]]))

})

test_that('filters returns FALSE when difference equals threshold', {
  ids <- biodatacoreUtils::mzids(c(1e6, 1e6 - 10, 1, 1), c(1, 1, 3.5, 3.3), source = c('fhs', 'fg500', 'fhs', 'fg500'))

  expect_false(bdc_filter_mz(ids[[1]], ids[[2]]))
  # cant just swtich order of mzs because there is a divison by x

  expect_false(bdc_filter_rt(ids[[3]], ids[[4]]))
  expect_false(bdc_filter_rt(ids[[4]], ids[[3]]))

  ids <- biodatacoreUtils::mzids(c(1e6, 1e6 - 11, 1, 1), c(1, 1, 3.5, 3.2), source = c('fhs', 'fg500', 'fhs', 'fg500'))

  expect_true(bdc_filter_mz(ids[[1]], ids[[2]]))

  expect_true(bdc_filter_rt(ids[[3]], ids[[4]]))
  expect_true(bdc_filter_rt(ids[[4]], ids[[3]]))


})

test_that('filters error when source is NA', {

  ids <- biodatacoreUtils::mzids(c(1,1,2), c(1,1,1), source = c('fhs', 'NA', 'NA'))

  expect_error(bdc_filter_mz(ids[[1]], ids[[3]]))
  expect_error(bdc_filter_mz(ids[[3]], ids[[1]]))
  expect_error(bdc_filter_mz(ids[[2]], ids[[3]]))

  expect_error(bdc_filter_rt(ids[[1]], ids[[3]]))
  expect_error(bdc_filter_rt(ids[[3]], ids[[1]]))
  expect_error(bdc_filter_rt(ids[[2]], ids[[3]]))

})


test_that('filters error when source is NA', {

  ids <- biodatacoreUtils::mzids(c(1,1,2), c(1,1,1), source = c('fhs', 'NA', 'NA'))

  expect_error(bdc_filter_mz(ids[[1]], ids[[3]]))
  expect_error(bdc_filter_mz(ids[[3]], ids[[1]]))
  expect_error(bdc_filter_mz(ids[[2]], ids[[3]]))

  expect_error(bdc_filter_rt(ids[[1]], ids[[3]]))
  expect_error(bdc_filter_rt(ids[[3]], ids[[1]]))
  expect_error(bdc_filter_rt(ids[[2]], ids[[3]]))

})

test_that('rt filters does not error when time is on [0, 7]', {

  ids <- biodatacoreUtils::mzids(c(1, 1, 1, 1), c(0, 7, 0, 7), source = c('fhs', 'fg500', 'klein', 'fr02'))

  expect_false(bdc_filter_rt(ids[[1]], ids[[3]]))
  expect_false(bdc_filter_rt(ids[[2]], ids[[4]]))

})
