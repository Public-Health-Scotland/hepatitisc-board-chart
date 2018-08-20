#Code to create chart of hepatitis c by board.


############################.
##Packages ----
############################.
library(dplyr) #data manipulation
library(plotly) #charts
library(shiny)

############################.
## Global ----
############################.
#Preparing data - not needed unless new data coming through
# library(reshape2)
# library (readr)
# 
# data <- read_csv("./data/hepatitisc_board.csv") %>% 
#   mutate_if(is.character, factor) %>%  #converting characters into factors
#   setNames(tolower(names(.))) %>% 
#   melt(variable.name = "year") 
# data$year <- gsub("y", "", data$year)
# 
# saveRDS(data, "./data/hepatitisc_board.rds")

data <- readRDS("./data/hepatitisc_board.rds")

#Use for selection of areas
board_list <- sort(unique(data$nhsboard[data$nhsboard != "Scotland"]))

#ScotPHO logo. 
#Needs to be https address or if local in code 64 (the latter does not work with 4.7 plotly)
scotpho_logo <-  list(source ="https://raw.githubusercontent.com/jvillacampa/test/master/scotpho.png",
                      xref = "paper", yref = "paper",
                      x= -0.09, y= 1.2, sizex = 0.22, sizey = 0.18, opacity = 1)

############################.
## Visual interface ----
############################.
#Height and widths as percentages to allow responsiveness
#Using divs as issues with classing css 
ui <- fluidPage(style="width: 650px; height: 500px; ", 
                div(style= "width:100%",
                          h4("Chart 1. Persons in Scotland reported to be hepatitis C antibody positive"),
                  div(style = "width: 50%; float: left;",
                      selectInput("measure", label = "Select a measure type",
                                  choices = c("Number", "Rate"), selected = "Rate")
                         ),
                  div(style = "width: 50%; float: left;",
                  selectInput("area", label = "Select a health board", 
                            choices = board_list))
                ),
                div(style= "width:100%; float: left;",
                  plotlyOutput("chart", width = "100%", height = "350px"),
                  p(div(style = "width: 25%; float: left;",
                        HTML("Source: <a href='http://www.hps.scot.nhs.uk/bbvsti/wrdetail.aspx?id=73581&wrtype=6'>HPS</a>")),
                    div(style = "width: 25%; float: left;",
                        downloadLink('download_data', 'Download data')),
                    div(style = "width: 50%; float: left;",
                        "Note: Year of earliest positive specimen.")
                        )
                  )
                )

############################.
## Server ----
############################.
server <- function(input, output) {
  
  # Allowing user to download data
  output$download_data <- downloadHandler( 
    filename =  'hepatitisc_data.csv', content = function(file) { 
      write.csv(data, file, row.names=FALSE) })
  
  ############################.
  #Visualization
  output$chart <- renderPlotly({
    #For Island plots and rates plot an empty chart
    if (input$area == "Island Boards" & input$measure == "Rate") {
        text_na <- list(x = 5, y = 5, text = "Rates are not published for the island boards" ,
                        xref = "x", yref = "y",  showarrow = FALSE, size=15)
        plot_ly() %>%
          layout(annotations = text_na,
                 yaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
                 xaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
                 font = list(family = 'Arial, sans-serif')) %>% 
          config( displayModeBar = FALSE) # taking out plotly logo and collaborate button
      } else {

    #Data for Scotland line
    data_scot <- data %>% subset(nhsboard=="Scotland" & measure==input$measure)
    #Data for Health board line
    data_board <- data %>% subset(nhsboard==input$area & measure==input$measure)
    
    #y axis title
    yaxistitle <- ifelse(input$measure == "Rate", "Rate per 100,000", "Number of diagnosis")
    
    plot <- plot_ly(data=data_board, x=~year, y = ~value, 
                    type = "scatter", mode = 'lines',  line = list(color = '#08519c'),
                    name = unique(data_board$nhsboard), width = 650, height = 350) %>% 
      add_lines(data = data_scot, y = ~value, mode = 'lines', 
                name = "Scotland", line = list(color = '#000000')) %>%
    #Layout
      layout(annotations = list(), #It needs this because of a buggy behaviour
           yaxis = list(title = yaxistitle, rangemode="tozero", fixedrange=TRUE), 
           xaxis = list(title = "Year",  fixedrange=TRUE),  
           font = list(family = 'Arial, sans-serif'), #font
           margin = list(pad = 4, t = 50), #margin-paddings
           #margin=list( l = 70, r = 50, b = 150, t = 50, pad = 4 ), #margin-paddings
           hovermode = 'false',  # to get hover compare mode as default
           images = scotpho_logo) %>% 
      config(displayModeBar= T, displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button
      }
    }) 
  
  } # end of server part

############################.
## Calling app ----
############################.

shinyApp(ui = ui, server = server)