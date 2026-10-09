#' Get list of laureates using API
#'
#' This funcction downloads the 
#'
#' @param formula formula
#' @param data data.frame
#' @return linreg class object
#' @export
get_laureates <- function(gender="", year = "",  yearTo="", 
                         name="", birthDate="", birthDateTo="",
                         birthCountry="", category="", limit=2000){

    # Check input for each variable 

    # --------
    #  Query
    # --------
    response <- httr2::request("https://api.nobelprize.org") |>
        httr2::req_url_path("2.1", "laureates") |>
        httr2::req_url_query(gender=gender, 
                    nobelPrizeYear = year, 
                    yearTo=yearTo, 
                    name=name,
                    birthDate=birthDate,
                    birthDateTo=birthDateTo,
                    birthCountry=birthCountry,
                    nobelPrizeCategory=category,
                    limit = limit) |>
        httr2::req_perform()

    data_list <- httr2::resp_body_string(response)
    data_list <- jsonlite::fromJSON(data_list, flatten=TRUE)
    dframe <- as.data.frame(data_list$laureates)

    # ---------
    #  \Query
    # ---------

    return(dframe)
}