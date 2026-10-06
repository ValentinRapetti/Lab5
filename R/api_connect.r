library(httr2)
library(jsonlite)


get_laureates <- function(gender="", year = "",  yearTo="", 
                         name="", birthDate="", birthDateTo="",
                        birthCountry="", category="", limit=2000){

# Check input for each variable 
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

    return(dframe)
}

df <- get_laureates(category="eco")



# -------------------------------------------------------------------
# Get some interesting information
# df$nobelPrizes[[i]]$awardYear
# df$nobelPrizes[[i]]$category.en
# df$nobelPrizes[[i]]$prizeAmountAdjusted