# shiny_app <- function(){

    
# }


library(shiny)

#df <- get_laureates(category="eco", age=58)
# df <- get_laureates(gender="female", category="phy")
#df <- get_laureates(category = "pea")
df <- get_laureates(category = "pea")
df_shiny <- parse_to_shiny(df)


ui <- fluidPage(
  
  numericInput(inputId = "Year", 
               label = "Award Year", 
               value = 1950, min = 1900, max = 2026, step = 1),
  plotOutput(outputId = "probs")
  
)

server <- function(input, output) {

  output$probs <- renderPlot({
    df_filter <- filter_laureates(df_shiny, awardYear = input$Year)
    award_continents <- table(df_filter["birth.place.continent.en"], useNA="ifany")
    names(award_continents)[is.na(names(award_continents))] <- "Organizations"

    base_continents <- as.table(1:7*0)
    names(base_continents) <- c("Africa","Asia","Europe","North America", "Oceania", "South America", "Organizations")
    base_continents[names(award_continents)] <- base_continents[names(award_continents)] + award_continents

    barplot(height = base_continents,
            names = names(base_continents),
            ylab = "# of laureates",
            main = "Distribution of laureates per continent")
  })
  
}

shinyApp(ui, server)