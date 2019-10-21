setwd("/Applications/BU/BU/615 Data Science in R/hw3 midterm project/midterm project/final/midterm github")
data = read.csv("data.csv")

library(tidyverse)
library(magrittr)

ye = 2008
re = "Europe & Central Asia"

sub_data = data %>% filter(year == ye & region == re) %>% select(income, apply) %>% 
  group_by(income) %>% summarise(apply = mean(apply))

##################################

f = function(x,y){
  ye = x
  re = y
  sub_data = data %>% filter(year == ye & region == re) %>% select(income, apply) %>% 
    group_by(income) %>% summarise(apply = mean(apply))
  
  
  a <- c(sub_data$apply)
  b <- c("High income","Lower middle income","Upper middle income")
  piepercent<- paste(round(100*a/sum(a), 2), "%")
  pie(a,labels=piepercent,col= c("skyblue","lightgreen","pink","lavender","lightyellow","lightgrey"),
      radius = 1,border="brown",lty=6,main='Apply proportion')
  legend("topright",b,cex=1,fill=c("skyblue","lightgreen","pink"))
  
}

####################################
a <- c(sub_data$apply)
b <- c("High income","Lower middle income","Upper middle income")
piepercent<- paste(round(100*a/sum(a), 2), "%")

pie(a,labels=piepercent,col= c("skyblue","lightgreen","pink","lavender","lightyellow","lightgrey"),
    radius = 1,border="brown",lty=6,main='GDP distribution worldwide')

legend("topright",b,cex=1,fill=c("skyblue","lightgreen","pink","lavender","lightyellow","lightgrey"))

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
