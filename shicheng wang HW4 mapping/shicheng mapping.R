#MA615 hw4 mapping

#install the package
library(tidyverse)
library(magrittr)
library(readxl)
library(ggmap)
library(shiny)
library(leaflet)
library(maps)
library(htmlwidgets)
library(mapdata) 
library(Hmisc) 

#read and clean data
WIFI <- read.csv("WIFI.csv")
data <- select(WIFI,X,Y,Address,Neighborhood)
da <- na.omit(data)

#rename variables
da$x = da$X
da$y = da$Y
da$address = da$Address
da$neighborhood = da$Neighborhood
wifi <- select(da,x,y,address,neighborhood)
wifi$neighborhood %<>% as.character
wifi = wifi[-1*which(wifi$neighborhood == ""),]

#early step
bounds <- map('state', c('Massachusetts'), fill=TRUE, plot=F)

#icon
icons <- awesomeIcons(
  icon = 'disc',
  iconColor = 'white',
  library = 'ion',
  markerColor = 'black',
  squareMarker = F
)
sub_wifi = wifi %>% filter(neighborhood == "Roxbury")

#save HTML
#saveWidget(map, file="sitesmap2.html", selfcontained=TRUE)

######################################################################################################
ui <- fluidPage(
  
  # Application title
  titlePanel("Boston Wicked Free Wi-fi Locations"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("neighborhood", "Select an area", sort(unique(wifi$neighborhood)))
    ),
    mainPanel(
      leafletOutput(outputId = "map")
    )
  )
)


server <- function(input, output, session) {
  output$map = renderLeaflet({
    bounds <- map('state', c('Massachusetts'), fill=TRUE, plot=F)
    #icon
    icons <- awesomeIcons(
      icon = 'disc',
      iconColor = 'white',
      library = 'ion',
      markerColor = 'black',
      squareMarker = F
    )
  
    wifi = wifi %>% filter(neighborhood == input$neighborhood)
    #Create the Leaflet map
    map <- leaflet(data = wifi) %>%
      setView(-71.07779, 42.31973, zoom = 11) %>% 
      addProviderTiles("Esri.NatGeoWorldMap", group = "Vintage")%>% 
      addProviderTiles("Stamen.TonerHybrid", group = "Sketch")%>%
      addProviderTiles("Stamen.Watercolor", group = "Tropical")%>%
      addProviderTiles("OpenTopoMap", group = "Complex") %>%
    #addMarkers(~x, ~y, label = ~neighborhood, group = "District") %>% 
      addAwesomeMarkers(lat =~y,lng =~x, label = ~address, group = "Address", icon=icons) %>%#
      addPolygons(data=bounds, group="States", weight=2, fillOpacity = 0) %>%
      addScaleBar(position = "bottomleft") %>%
      addLayersControl(
        baseGroups = c("Vintage", "Sketch", "Tropical","Complex"),
        overlayGroups = c("States", "Address"),
        options = layersControlOptions(collapsed = TRUE)
      )
    invisible(print(map))
  })
  
}

shinyApp(ui = ui, server = server)


















