library(biodatacoreUtils)

bdc_set_rules <-
  list(
    set_1 = c(
      'fhs',
      'vital400'
    ),
    set_2 = c(
      'fr02',
      'klein',
      'fg500'
    )
  ) %>%
  reverse_split()

devtools::use_data(bdc_set_rules)
