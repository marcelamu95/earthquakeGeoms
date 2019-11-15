
filename <- system.file("extdata", "signif.txt", package="earthquakeGeoms")
data <- readr::read_delim(filename, delim = '\t')
clean_data <- eq_clean_data(data)

## Test eq_clean_data
testthat::expect_that(clean_data$date, testthat::is_a('Date'))
testthat::expect_that(clean_data$LATITUDE, testthat::is_a('numeric'))
testthat::expect_that(clean_data$LONGITUDE, testthat::is_a('numeric'))

## Test eq_location_clean
clean_data <- eq_location_clean(clean_data, "LOCATION_NAME")

testthat::expect_that(clean_data$LOCATION_NAME, testthat::is_a("character"))


