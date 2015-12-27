try(c(require(ggplot2),
  require(dplyr),
  require(leaflet),
  require(jsonlite),
  require(RCurl),
  require(shiny),
  require(lubridate),
  require(ggmap),
  silent=TRUE))

json_file <- getURL('https://data.seattle.gov/resource/y7pv-r3kh.json')
json_data <- fromJSON(json_file)
json_data$location <- NULL
str(json_data)
crimes <- tbl_df(json_data) %>% select(date_reported, latitude, longitude, offense_type, summarized_offense_description, year)
crimes$year <- as.numeric(crimes$year)

loc <- 'Seattle, WA'
location <- geocode(loc)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%"),
  absolutePanel(top=10,right=10,
                selectInput('year','Choose Year:', choices=unique(crimes$year)),
                selectInput('offense','Choose Offense:', choices=unique(crimes$summarized_offense_description))
))


server <- (function(input, output, session) {
  filteredData <- reactive({
    crimes[crimes$summarized_offense_description == input$offense & crimes$year == input$year,]
  })
  output$mymap <- renderLeaflet({
    map <- leaflet() %>% addTiles()
    map <- map %>% setView(location$lon,location$lat, zoom = 13)      
  })
  observe({
    leafletProxy("mymap", data = filteredData()) %>%
      clearMarkers() %>%
      addMarkers(filteredData()$longitude,filteredData()$latitude,popup=
                   paste(paste(month(filteredData()$date_reported,label=T,abbr=F),day(filteredData()$date_reported),sep=' '),
                         year(filteredData()$date_reported),sep=', '))
  })
})
shinyApp(ui, server)


