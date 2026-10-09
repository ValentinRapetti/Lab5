#' Get list of laureates using API calls
#'
#' This function downloads the database of Nobel Laureates from nobelprize.org.
#' Several filters can be applied on the query to reduce the database size.
#'
#' @param gender "male", "female", "other"
#' @param year integer
#' @param yearTo integer
#' @param name string
#' @param birthDate date format (YYYY, -MM-, -DD or YYYY-MM-DD)
#' @param birthDateTo date format (YYYY, -MM-, -DD or YYYY-MM-DD)
#' @param birthCountry string
#' @param category string ['che', 'eco', 'lit', 'pea', 'phy', 'med']
#' @param limit integer
#' @return dframe data frame
#' @references https://www.nobelprize.org/about/developer-zone-2/
#' @export
get_laureates <- function(gender="", year = "",  yearTo="", 
                         name="", birthDate="", birthDateTo="",
                         birthCountry="", category="", limit=2000){
                        
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

    stopifnot("Gender must be \"male\", \"female\", or \"other\""=(gender=="female" | gender=="male" | gender=="other" | gender==""),
        "Year must be an integer"=((is.numeric(year) & length(year)==1) | year==""), 
        "YearTo must be an integer"=((is.numeric(yearTo) & length(yearTo)==1) | yearTo==""),
        "Year must be greater than 1900"=(year>1900 | year==""),
        "Year must be smaller than present year"=(year<=presentYear | year==""),
        "YearTo must be smaller than present year"=(yearTo<=presentYear | yearTo==""),
        "YearTo must be greater or equal than Year"=(yearTo>=year | yearTo==""),
        "Name must be a string"=is.character(name),
        "BirthDate is not a character"=is.character(birthDate), #Need to check for date format
        "BirthDateTo is not a character"=is.character(birthDateTo),
        "BirthCountry must be a string"=is.character(birthCountry),
        "Category must be ['che', 'eco', 'lit', 'pea', 'phy', 'med']"=(category %in% cat_list | category==""),
        "Limit must be an integer greater than 0"=(is.numeric(limit) & length(limit)==1 & limit>0)
    )

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

    # We expand the list for those who won more than one Nobel Prize
    n_prizes <- sapply(dframe$nobelPrizes, function(x) length(x$awardYear))
    prizes <- dframe$nobelPrizes

    dframe <- dframe[rep(seq_len(nrow(dframe)), times = n_prizes), ]

    # Fix the contents of the duplication
    dframe$nobelPrizes <- unlist(
    lapply(prizes, function(x) {
        lapply(seq_len(nrow(x)), function(i) x[i, , drop = FALSE])
    }),
    recursive = FALSE
    )

    ## MISSING: CHECK IF YEAR AND CATEGORY MATCH
    # dframe <- head(dframe[
    #             sapply(dframe$nobelPrizes, function(x)
    #                 (x$awardYear <= yearTo | yearTo == "") &
    #                 (x$awardYear >= year | year == "") &
    #                 (x$category.en == category | category == "")), ], limit)

    return(dframe)
}