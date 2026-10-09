# Tests adapted from https://www.nobelprize.org/about/linked-data-examples/

test_that("Rejects erroneous input (filter_laureates)",{
    laureates_test <- get_laureates(year=1901)
    laureates_test <- parse_to_shiny(laureates_test)

    expect_error(filter_laureates(awardYear=1899))
    expect_error(filter_laureates(gender="plant"))
    expect_error(filter_laureates(category="Philosophy"))
    expect_error(filter_laureates(limit=-1))
    expect_error(filter_laureates(awardYear=1950, awardYearTo=1930))
    expect_error(filter_laureates(continent="Eurasia"))
    expect_error(filter_laureates(age=-1))
})

test_that("Filtering by gategory works (filter_laureates)",{
    laureates_test <- get_laureates(gender="female")
    laureates_test <- parse_to_shiny(laureates_test)
    laureates_test <- laureates_test[order(as.numeric(laureates_test$awardYear)), ]
    laureates_test <- filter_laureates(laureates_test, category="phy", limit=4)

    prize_year <- c(2018, 2020, 1903, 1963)
    name_list <- c("Marie Curie", "Maria Goeppert Mayer", "Donna Strickland", "Andrea Ghez")

    expect_true(all(laureates_test$category=="Physics"))
    expect_true(all(laureates_test$name %in% name_list))
})