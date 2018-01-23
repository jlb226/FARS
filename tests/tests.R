library(testthat)

expect_warning(fars_read_years(2000))

expect_error(fars_map_state(51, 2014))
