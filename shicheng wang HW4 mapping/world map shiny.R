
library(ggmap)
library(maptools)
library(maps)

mapWorld <- map_data("world")

mp1 <- ggplot(mapWorld, aes(x=long, y=lat, group=group))+
  geom_polygon(fill="white", color="black") +
  coord_map(xlim=c(-180,180), ylim=c(-60, 90))

map_vector <- c(cyl="cylindrical",mer="mercator",sin="sinusoidal",gno="gnomonic")
m_vec<-c(rec="rectangular",cyle="cylequalarea")

########################
ui <- fluidPage(
  titlePanel("Different type of maps"),
  selectInput("map", label = "Map selection", choices = map_vector),
  plotOutput("plot")
)



#########################
server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    mp2 <- mp1 + coord_map(input$map, xlim=c(-180,180), ylim=c(-60, 90))
    mp2
  })
}

shinyApp(ui = ui, server = server)


