filter_laureates <- function(dframe, gender="", awardYear = "",  awardYearTo="", 
                         name="", birthYear="", birthYearTo="",
                         country="", continent="", category="", age="", limit=2000){

    # Check input for each variable

    if(awardYearTo=="") awardYearTo <- awardYear
    if(birthYearTo=="") birthYearTo <- birthYearTo

    dframe <- dframe[(dframe$gender == gender | gender == "") &
                     (dframe$awardYear >= awardYear | awardYear == "") &
                     (dframe$awardYear <= awardYearTo | awardYearTo == "") &
                     (dframe$name == name | name == "") &
                     (dframe$birth.year >= birthYear | birthYear == "") &
                     (dframe$birth.year <= birthYearTo | birthYearTo == "") &
                     (dframe$birth.place.countryNow.en == country | country == "") &
                     (dframe$birth.place.continent.en == continent | continent == "") &
                     (dframe$category == category | category == "") &
                     ((!is.na(dframe$age) & dframe$age == age) | age == ""), ]

    return(dframe)
}