
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

ui <- shiny::fluidPage(
  shiny::sliderInput(
    "Year",
    "Award Year Range",
    min = 1900,
    max = 2026,
    value = c(1950, 2020),
    step = 1,
    sep = ""
  ),
  shiny::tabsetPanel(
    shiny::tabPanel(
      "Map",
      leaflet::leafletOutput("map", height = "700px")
    ),
    shiny::tabPanel(
      "Continents",
      shiny::plotOutput("probs", height = "600px")
    ),
    shiny::tabPanel(
      "Gender",
      shiny::plotOutput("gender_plot", height = "600px")
    ),
    shiny::tabPanel(
      "Mean age" ,
      shiny::plotOutput("age_plot", height = "600px")
    )
  )
)

# ------------------
# SERVER
# ------------------

server <- function(input, output) {
  # Reactive filtered dataset
  laureates_filtered <- shiny::reactive({
    filter_laureates(
      df_shiny,
      awardYear = input$Year[1],
      awardYearTo = input$Year[2]
    )
  })
  # ------------------
  # Continent barplot
  # ------------------
  output$probs <- shiny::renderPlot({
    df_filter <- laureates_filtered()
    counts <- table(
      df_filter$continent,
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
  output$map <- leaflet::renderLeaflet({
    df_filter <- laureates_filtered()
    df_map <- df_filter |>
      dplyr::select(
        country
      )
    df_map$iso3 <- countrycode::countrycode(
      df_map$country,
      origin = "country.name",
      destination = "iso3c",
      custom_match = c(
        "France" = "FRA",
        "Guadeloupe, France" = "FRA",
        "Faroe Islands (Denmark)" = "FRO",
        "Scotland" = "GBR"
      )
    )
    country_counts <- df_map |>
      dplyr::count(iso3, name = "n")
    world <- rnaturalearth::ne_countries(
      scale = "medium",
      returnclass = "sf"
    )
    world_data <- world |>
      dplyr::left_join(
        country_counts,
        by = c("adm0_a3" = "iso3")
      )
    world_data$n[is.na(world_data$n)] <- 0
    pal <-  leaflet::colorBin(
      palette = "YlOrRd",
      domain = world_data$n,
      bins = c(0, 1, 2, 5, 10, 20, 50, 100, 200)
    )
    leaflet::leaflet(world_data) |>
      leaflet::addTiles() |>
      leaflet::addPolygons(
        fillColor = ~pal(n),
        fillOpacity = 0.9,
        color = "black",
        weight = 0.5,
        popup = ~paste0(
          "<b>", name, "</b><br>",
          "Count: ", n
        )
      ) |>
      leaflet::addLegend(
        pal = pal,
        values = ~n,
        title = "Laureates"
      )
  })
  output$gender_plot <- shiny::renderPlot({
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
    output$gender_plot <- shiny::renderPlot({
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
  output$age_plot <- shiny::renderPlot({
    
    df_filter <- laureates_filtered()
    
    age_by_year <- df_filter |>
      dplyr::group_by(awardYear) |>
      dplyr::summarise(
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

shiny::shinyApp(ui, server)

