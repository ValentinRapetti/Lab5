library(httr2)
library(jsonlite)


get_laureates <- function(gender="", year = "",  yearTo="", 
                         name="", birthDate="", birthDateTo="",
                        birthCountry="", category="", age="", limit=2000){

    # Check input for each variable 

    # --------
    #  Query
    # --------
    response <- request("https://api.nobelprize.org") |>
        req_url_path("2.1", "laureates") |>
        req_url_query(gender=gender, 
                    nobelPrizeYear = year, 
                    yearTo=yearTo, 
                    name=name,
                    birthDate=birthDate,
                    birthDateTo=birthDateTo,
                    birthCountry=birthCountry,
                    nobelPrizeCategory=category,
                    limit = limit) |>
        req_perform()

    data_list <- resp_body_string(response)
    data_list <- fromJSON(data_list, flatten=TRUE)
    dframe <- as.data.frame(data_list$laureates)

    awardYears <- as.integer(sapply(dframe$nobelPrizes, function(x) x$awardYear[[1]]))
    birthYears <- as.integer(dframe$birth.year)
    age_list = awardYears - birthYears
    dframe["age"] <- age_list
    dframe["awardYear"] <- awardYears
    if(age!="")
        dframe <- dframe[!is.na(dframe$age) & dframe$age==age ,]

    category <- as.character(sapply(dframe$nobelPrizes, function(x) x$category.en[[1]]))
    dframe["category"] <- category

    prizeAmount <- sapply(dframe$nobelPrizes, function(x) x$prizeAmountAdjusted[[1]])
    print(prizeAmount)
    print(awardYears)
    # dframe["prizeAmount"] <- prizeAmount

    #dframe <- dframe[, c("id", "knownName.en", "gender", "birth.year", "birth.place.country.en", "birth.place.continent.en",
                         #"age", "awardYear", "category")]#, "prizeAmount")]

    # ---------
    #  \Query
    # ---------

    return(dframe)
}

#df <- get_laureates(category="eco", age=58)

# df <- get_laureates(gender="female", category="phy")

df <- get_laureates(category = "pea")



a <- data.frame(
    id = df$id[1],
    name = df$knownName.en[1],
    category = df$nobelPrizes[[1]]$category.en,
    awardYear = df$nobelPrizes[[1]]$awardYear
)