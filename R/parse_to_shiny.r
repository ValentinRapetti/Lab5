#' Modify laureate database for shiny app
#'
#' This function selects some columns from the laureates' database
#' and adds others that are needed in the shiny app.
#' The columns added are age, awardYear, category and prizeAmount.
#'
#' @param dframe data frame
#' @return dframe data frame
#' @export
parse_to_shiny <- function(dframe){

    # Add four columns: Age, AwardYear, Category, and PrizeAmount
    awardYears <- as.integer(sapply(dframe$nobelPrizes, function(x) x$awardYear[[1]]))
    birthYears <- as.integer(dframe$birth.year)
    age_list <- awardYears - birthYears
    dframe["age"] <- age_list
    dframe["awardYear"] <- awardYears

    category <- as.character(sapply(dframe$nobelPrizes, function(x) x$category.en[[1]]))
    dframe["category"] <- category

    prizeAmount <- sapply(dframe$nobelPrizes, function(x) x$prizeAmountAdjusted[[1]])
    dframe["prizeAmount"] <- prizeAmount

    # Join different names' fields (to get the organizations' names)
    dframe$name <- Reduce(
            function(x, y) ifelse(is.na(x), y, x),
            dframe[c("knownName.en", "acronym", "nativeName")])

    dframe <- dframe[, c("id", "name", "gender", "birth.year", "birth.place.countryNow.en", "birth.place.continent.en",
                         "age", "awardYear", "category", "prizeAmount")]

    names(dframe)[names(dframe)=="birth.place.countryNow.en"] <- "country"
    names(dframe)[names(dframe)=="birth.place.continent.en"] <- "continent"
    
    dframe["gender"][is.na(dframe["gender"])] <- "org"

    return(dframe)
}