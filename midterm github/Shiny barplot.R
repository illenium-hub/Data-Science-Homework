data = read.csv("data.csv")

library(tidyverse)
library(magrittr)

ye = 2008
re = "Europe & Central Asia"

sub_data = data %>% filter(year == ye & region == re) %>% select(income, apply) %>% 
  group_by(income) %>% summarise(apply = mean(apply))

ggplot(sub_data,aes(x = income, y = apply, fill=income, label = apply)) + geom_col(width = 0.3) + theme(legend.position = "none")+ 
geom_text(size=3, nudge_x=0,nudge_y =-2,colour='#660000')

f = function(x,y){
  ye = x
  re = y
  sub_data = data %>% filter(year == ye & region == re) %>% select(income, apply) %>% 
    group_by(income) %>% summarise(apply = mean(apply))
  
ggplot(sub_data,aes(x = income, y = apply, fill=income, label = apply)) + geom_col(width = 0.3) + theme(legend.position = "none")+ 
    geom_text(size=3, nudge_x=0,nudge_y =-2,colour='#660000')
  
}

###########################################################################################
library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Patent applications proportion"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("ye", "Select year", sort(unique(data$year))),
      selectInput("re", "Select a region", sort(unique(data$region)))
    ),
    
    mainPanel(
      plotOutput("Plot")
    )
  )
)


server <- function(input, output, session) {
  
  output$Plot = renderPlot({
    ye = as.numeric(input$ye)
    re = as.character(input$re)
    f(ye,re)
  })
  
}

shinyApp(ui = ui, server = server)
