

library(shiny)
library(tidyverse)
library(rsconnect)
library(readr)
library(plotly)
# rsconnect::setAccountInfo(name='asharma7',
#                            token='D172999E23581ED48DF14EA8D966DAE1',
#                           secret='SECRET')


airbnb <- read_delim("seattle_01.csv")


ui <- fluidPage(

  tabsetPanel(
    tabPanel("Airbnb",
             sidebarLayout(
               sidebarPanel(
                 h1("Greater Seattle Airbnb listings"),
                 p("This dataset shows ", 
                   strong("airbnb listings from around Greater Seattle.")),
                 p("This data allows us to ",
                   em("explore the real estate market "),
                   "in our city."),
                 p("The dataset contains ", ncol(airbnb), "variables."),
                 p("Here is a small sample of data: ")
               ),
             
             
                mainPanel(
                  dataTableOutput("sample")
                )
             )
             ),
   
    tabPanel("Plot",

             p("We have", nrow(airbnb), "listings"),
             sidebarLayout(
               sidebarPanel(
                 sliderInput("n","How many airbnb listings ",
                             min = 1,
                             max = nrow(airbnb),
                             value = 200),
                 p("For the selected satisfaction level(s), there are ",textOutput(outputId = "result", inline=T),
                   "listings"),
               fluidRow(
                 column(6,
                    radioButtons("color","Choose color",
                              choices = c("skyblue","lawngreen","orangered",
                                                   "purple","gold"))
             ),
             column(6,
                    uiOutput("checkboxSatisfaction")

                           )
                  )
             ),
             mainPanel(
               plotOutput("plot")

             ) )


               ),
    tabPanel("Table",
                p("See average airbnb cost per city"),
             sidebarLayout(
               sidebarPanel(
                   selectInput("cityName","City", choices = unique(airbnb$address)),
                   p("For the selected city, the average price", textOutput(outputId = "aboveBelow", inline=T),
                      "the average room price of $125."),
                   fluidRow(
                     column(4,
                            radioButtons("minMax","Choose price type",
                                         choices = c("minimum list price","maximum list price"))
                     )
                     
                   )
                 ),
               
               mainPanel(
                 tableOutput("table")
               )
               

                )
               )
   
         )
)

server <- function(input, output) {
  
  output$result <- renderText({
      return(input$n)
  })

  output$aboveBelow <- renderText({
    MP <- airbnb %>% 
    group_by(address) %>% 
    summarise(meanPrice=mean(price)) %>% 
  
      filter(address==input$cityName) 
  
    if(MP$meanPrice>125){
    return(" IS GREATER THAN")}
    else if (MP$meanPrice<125){
      return("IS LESS THAN")}
    else{
      return("IS EQUAL TO")
    }
  })
  
  output$checkboxSatisfaction <- renderUI({
    checkboxGroupInput("satisfaction","Choose customer satisfaction level",
          choices = unique(airbnb$overall_satisfaction)
    )
  })
  
 output$sample <- renderDataTable({
   
  s <- airbnb %>%
   sample_n(6)
  print(s)
  s
   })
 
  sample <- reactive({
    
    s1 <- airbnb %>% 
      filter(overall_satisfaction %in% input$satisfaction)
    if(nrow(s1)> input$n) 
      sample_n(s1, input$n)
    else
      s1
  })
  output$plot <- renderPlot({
  p <- sample() %>%
    ggplot(aes (price, reviews))+
  geom_point(col=input$color)+
    labs(x = "Airbnb List Prices", y = "Number of Reviews", title= "Comparing Price and Reviews")
    
  if(nrow(sample())==0){
    p <- p+
      labs(subtitle= "Please select a satisfaction level")
  }
  p
  })
  output$table <- renderTable({
   
      if(input$minMax =="minimum list price"){
        s3 <- airbnb %>%
          filter(address==input$cityName) %>% 
        summarise(`minimum list price of the city:`= min(price, na.rm=TRUE))
      } else{
        s3 <- airbnb %>%
        filter(address==input$cityName) %>% 
        summarise(`maximum list price of the city:`= max(price, na.rm=TRUE))
      }
     
      
    s3
  })
}
  


shinyApp (ui = ui, server = server)