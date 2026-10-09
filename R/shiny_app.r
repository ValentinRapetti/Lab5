library(shiny)
library(dplyr)
library(leaflet)
library(countrycode)
library(rnaturalearth)
library(sf)

# ------------------
# Data
# ------------------

df <- get_laureates(category = "pea")
df_shiny <- parse_to_shiny(df)

continent_colors <- c(
  "Africa" = "black",
  "Asia" = "yellow",
  "Europe" = "blue",
  "North America" = "red",
  "South America" = "orange",
  "Oceania" = "green",
  "Organizations" = "gray"
)

continent_colors <- colorspace::desaturate(
  continent_colors,
  amount = 0.2
)

# ------------------
# UI
# ------------------

ui <- fluidPage(
  sliderInput(
    "Year",
    "Award Year Range",
    min = 1900,
    max = 2026,
    value = c(1950, 2020),
    step = 1,
    sep = ""
  ),
  tabsetPanel(
    tabPanel(
      "Map",
      leafletOutput("map", height = "700px")
    ),
    tabPanel(
      "Continents",
      plotOutput("probs", height = "600px")
    ),
    tabPanel(
      "Gender",
      plotOutput("gender_plot", height = "600px")
    ),
    tabPanel(
      "Mean age" ,
      plotOutput("age_plot", height = "600px")
    )
  )
)

# ------------------
# SERVER
# ------------------

server <- function(input, output) {
  # Reactive filtered dataset
  laureates_filtered <- reactive({
    filter_laureates(
      df_shiny,
      awardYear = input$Year[1],
      awardYearTo = input$Year[2]
    )
  })
  # ------------------
  # Continent barplot
  # ------------------
  output$probs <- renderPlot({
    df_filter <- laureates_filtered()
    counts <- table(
      df_filter$birth.place.continent.en,
      useNA = "ifany"
    )
    names(counts)[is.na(names(counts))] <- "Organizations"
    graphics::barplot(
      counts,
      names.arg = names(counts),
      col = continent_colors[names(counts)],
      ylab = "Number of laureates",
      main = "Distribution of laureates by continent"
    )
  })
  # ------------------
  # Map
  # ------------------
  output$map <- renderLeaflet({
    df_filter <- laureates_filtered()
    df_map <- df_filter %>%
      select(
        birth.place.countryNow.en
      )
    df_map$iso3 <- countrycode(
      df_map$birth.place.countryNow.en,
      origin = "country.name",
      destination = "iso3c",
      custom_match = c(
        "France" = "FRA",
        "Guadeloupe, France" = "FRA",
        "Faroe Islands (Denmark)" = "FRO",
        "Scotland" = "GBR"
      )
    )
    country_counts <- df_map %>%
      count(iso3, name = "n")
    world <- ne_countries(
      scale = "medium",
      returnclass = "sf"
    )
    world_data <- world %>%
      left_join(
        country_counts,
        by = c("adm0_a3" = "iso3")
      )
    world_data$n[is.na(world_data$n)] <- 0
    pal <- colorBin(
      palette = "YlOrRd",
      domain = world_data$n,
      bins = c(0, 1, 2, 5, 10, 20, 50, 100, 200)
    )
    leaflet(world_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(n),
        fillOpacity = 0.9,
        color = "black",
        weight = 0.5,
        popup = ~paste0(
          "<b>", name, "</b><br>",
          "Count: ", n
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~n,
        title = "Laureates"
      )
  })
  output$gender_plot <- renderPlot({
    df_filter <- laureates_filtered()
    gender_counts <- table(
      df_filter$gender,
      useNA = "no"
    )
    gender_colors <- c(
      "male" = "blue",
      "female" = "red",
      "org" = "green"
    )
    output$gender_plot <- renderPlot({
      gender_counts <- table(
        laureates_filtered()$gender,
        useNA = "no"
      )
      pie(
        gender_counts,
        col = gender_colors[names(gender_counts)],
        main = "Nobel Laureates by Gender"
      )
    })
  })
  output$age_plot <- renderPlot({
    
    df_filter <- laureates_filtered()
    
    age_by_year <- df_filter %>%
      group_by(awardYear) %>%
      summarise(
        mean_age = mean(age, na.rm = TRUE)
      )
    
    plot(
      age_by_year$awardYear,
      age_by_year$mean_age,
      type = "p",
      pch = 16,
      col = "black",
      xlab = "Award Year",
      ylab = "Mean Age",
      main = "Mean Age of Nobel Laureates by Award Year"
    )
  })  
}

shinyApp(ui, server)

