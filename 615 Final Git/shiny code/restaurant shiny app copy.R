## 615 final project shiny app

##################################

## install packages
library(shiny)
library(leaflet)
library(tidyverse)
library(leaflet.extras)
library(ggmap)

## data process
d <- read.csv('/Applications/BU/BU/615 Data Science in R/hw/615 final project/data/zomato.csv')
d <- na.omit(d)

### give country names
d$Country.Code[d$Country.Code==1] <- 'India'
d$Country.Code[d$Country.Code==216] <- 'Us'
d$Country.Code[d$Country.Code==215] <- 'Uk'
d$Country.Code[d$Country.Code==189] <- 'South Africa'
d$Country.Code[d$Country.Code==14] <- 'Australia'
d$Country.Code[d$Country.Code==30] <- 'Brazil'
d$Country.Code[d$Country.Code==37] <- 'Canada'
d$Country.Code[d$Country.Code==94] <- 'Indonesia'
d$Country.Code[d$Country.Code==162] <- 'Phillipines'
d$Country.Code[d$Country.Code==166] <- 'Qatar'
d$Country.Code[d$Country.Code==184] <- 'Singapore'
d$Country.Code[d$Country.Code==191] <- 'Sri Lanka'
d$Country.Code[d$Country.Code==208] <- 'Turkey'
d$Country.Code[d$Country.Code==214] <- 'UAE'
d$Country.Code[d$Country.Code==148] <- 'New Zealand'
### adjustments
d <- d[,c(2,3,4,8,9,17,20)]
d$Price.range <- factor(d$Price.range,levels=c(1,2,3,4))
d$Rating.text <- factor(d$Rating.text,levels=c("Excellent","Very Good","Good","Average","Poor","Not rated"))
d <- filter(d,d$Rating.text!='Not rated')
d$Longitude <- as.numeric(d$Longitude)
d$Latitude <- as.numeric(d$Latitude)

##################################

## layout
ui <- fluidPage(
  titlePanel("Restaurant Information Worldwide"),
  headerPanel(title = 'Define filter criteria'),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'Country',label = 'Select a country',choices = unique(d$Country.Code)),
      selectInput(inputId = 'city',label = 'Select a city',choices = unique(d$City)),
      selectInput(inputId = 'Rate',label='Select rate',choices = unique(d$Rating.text))
      ),
    mainPanel(leafletOutput(outputId = "mymap")
    )
  ) 
)

## output
server <- function(input, output, session) {
  
  observeEvent(input$Country,{
    a = d %>% filter(Country.Code %in% input$Country) %>% `$`(City) %>% unique() %>% sort()
    updateSelectInput(session,inputId = "city",choices = a)
  })
  
  observeEvent(c(input$city,input$Country),{
    aa = d %>% filter(Country.Code %in% input$Country & City %in% input$city) %>%
      `$`(Rating.text) %>% unique() %>% sort()
    updateSelectInput(session,inputId = "Rate",choices = aa)
  })

  
  output$mymap = renderLeaflet({
    
  pal <- colorFactor(palette = "viridis",domain = d$Rating.text)
  
  tem = d %>% filter(Country.Code == input$Country & 
                       City %in% input$city & 
                       Rating.text %in% input$Rate)
  
  leaflet(tem) %>% addTiles() %>%
    setView(mean(tem$Longitude), mean(tem$Latitude), zoom = 12) %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Map") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite")%>%
    addProviderTiles("Esri.WorldShadedRelief", group = "Relief")%>%
    addCircleMarkers(color = ~pal(Rating.text),stroke = FALSE, fillOpacity = 0.8,lng = ~tem$Longitude,lat = ~tem$Latitude,group = "Restaurants")%>%
    addScaleBar(position = "bottomleft") %>%
    addLayersControl(
      baseGroups = c("Map", "Satellite", "Relief"),
      overlayGroups = c("Restaurants"),
      options = layersControlOptions(collapsed = T)
    )

  })
  
  }

shinyApp(ui = ui,server = server)

########################













