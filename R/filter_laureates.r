#' Filter the laureates' data frame
#'
#' This function filters the database of Nobel Laureates.
#' The input data frame must be the result of parse_to_shiny.
#'
#' @param dframe data frame
#' @param id integer
#' @param gender "male", "female", "org"
#' @param awardYear integer
#' @param awardYearTo integer
#' @param name string
#' @param birthYear integer (YYYY)
#' @param birthYearTo integer (YYYY)
#' @param country string
#' @param continent string ['Africa', 'Asia', 'Europe', 'North America', 'Oceania', 'South America']
#' @param category string ['che', 'eco', 'lit', 'pea', 'phy', 'med']
#' @param age integer
#' @param limit integer
#' @return dframe data frame
#' @export
filter_laureates <- function(dframe, id="", gender="", awardYear = "", 
                         awardYearTo="", name="", birthYear="", birthYearTo="",
                         country="", continent="", category="", age="", limit=2000){

    # Check input for each variable
    category <- switch(category,
        "Chemistry" = "che",
        "Economic Sciences" = "eco",
        "Literature" = "lit",
        "Peace" = "pea",
        "Physics" = "phy",
        "Physiology or Medicine" = "med",
        category)
    presentYear <- as.integer(format(Sys.Date(),"%Y"))
    cat_list <- c('che', 'eco', 'lit', 'pea', 'phy', 'med')
    cont_list <- c('Africa', 'Asia', 'Europe', 'North America', 'Oceania', 'South America')

    stopifnot("Id must be a positive integer"=((is.numeric(id) & length(id)==1 & id>0) | id==""),
        "Gender must be \"male\", \"female\", or \"org\""=(gender=="female" | gender=="male" | gender=="org" | gender==""),
        "AwardYear must be an integer"=((is.numeric(awardYear) & length(awardYear)==1) | awardYear==""), 
        "AwardYearTo must be an integer"=((is.numeric(awardYearTo) & length(awardYearTo)==1) | awardYearTo==""),
        "AwardYear must be greater than 1900"=(awardYear>1900 | awardYear==""),
        "AwardYear must be smaller than present year"=(awardYear<=presentYear | awardYear==""),
        "AwardYearTo must be smaller than present year"=(awardYearTo<=presentYear | awardYearTo==""),
        "AwardYearTo must be greater or equal than AwardYear"=(awardYearTo>=awardYear | awardYearTo==""),
        "Name must be a string"=is.character(name),
        "BirthYear is not an integer"=((is.numeric(birthYear) & length(birthYear)==1) | birthYear==""),
        "BirthYearTo is not an integer"=((is.numeric(birthYearTo) & length(birthYearTo)==1) | birthYearTo==""),
        "BirthYearTo must be greater or equal than BirthYear"=(birthYearTo>=birthYear | birthYearTo==""),
        "Country must be a string"=is.character(country),
        "Continent must be a string"=(continent %in% cont_list | continent==""),
        "Category must be ['che', 'eco', 'lit', 'pea', 'phy', 'med']"=(category %in% cat_list | category==""),
        "Age is not a positive integer"=((is.numeric(age) & length(age)==1 & age>0) | age==""),
        "Limit must be an integer greater than 0"=(is.numeric(limit) & length(limit)==1 & limit>0)
    )

    # Filters
    if(awardYearTo=="") awardYearTo <- awardYear
    if(birthYearTo=="") birthYearTo <- birthYear

    category <- switch(category,
        che = "Chemistry",
        eco = "Economic Sciences",
        lit = "Literature",
        pea = "Peace",
        phy = "Physics",
        med = "Physiology or Medicine",
        category)

    dframe <- head(dframe[(dframe$id == id | id == "") &
                          (dframe$gender == gender | gender == "") &
                          (dframe$awardYear <= awardYearTo | awardYearTo == "") &
                          (dframe$awardYear >= awardYear | awardYear == "") &
                          (dframe$name == name | name == "") &
                          ((!is.na(dframe$birth.year) & dframe$birth.year >= birthYear) | birthYear == "") &
                          ((!is.na(dframe$birth.year) & dframe$birth.year <= birthYearTo) | birthYearTo == "") &
                          ((!is.na(dframe$country) & dframe$country == country) | country == "") &
                          ((!is.na(dframe$continent) & dframe$continent == continent) | continent == "") &
                          (dframe$category == category | category == "") &
                          ((!is.na(dframe$age) & dframe$age == age) | age == ""), ],
              limit)

    return(dframe)
}