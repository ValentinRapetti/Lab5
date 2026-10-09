# Tests adapted from https://www.nobelprize.org/about/linked-data-examples/

test_that("Rejects erroneous input (get_laureates)",{
    expect_error(get_laureates(year=1899))
    expect_error(get_laureates(gender="plant"))
    expect_error(get_laureates(category="Philosophy"))
    expect_error(get_laureates(limit=-1))
    expect_error(get_laureates(year=1950, yearTo=1930))
})

test_that("Filter by year works (get_laureates)",{
    laureates_test <- get_laureates(year=1901)

    prize_list <- c("The Nobel Prize in Physics", "The Nobel Prize in Literature", "The Nobel Peace Prize",
        "The Nobel Peace Prize", "The Nobel Prize in Physiology or Medicine", "The Nobel Prize in Chemistry")
    name_list <- c("Wilhelm Conrad Röntgen", "Sully Prudhomme", "Jean Henry Dunant", 
        "Frédéric Passy", "Emil Adolf von Behring", "Jacobus Henricus van 't Hoff")
    
    out_category_table <- table(sapply(laureates_test$nobelPrizes, function(x) x$categoryFullName.en))

    expect_equal(unname(out_category_table), unname(table(prize_list)))
    expect_true(all(sapply(laureates_test$nobelPrizes, function(x) x$awardYear)==1901))
    expect_true(all(laureates_test$fullName.en %in% name_list))
})