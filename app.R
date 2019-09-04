#Code to create chart of hepatitis c by board.

############################.
## Global ----
############################.
############################.
##Packages 

library(dplyr) #data manipulation
library(plotly) #charts
library(shiny)

#Preparing data - not needed unless new data coming through
# library(tidyr)
# library(readr)
# 
# cl_out_pop <- "/conf/linkage/output/lookups/Unicode/Populations/Estimates/"
# 
# hep_c <- read_csv("data/hepatitisc_board.csv") %>%
#   mutate_if(is.character, factor) %>%  #converting characters into factors
#   setNames(tolower(names(.))) %>% #variable names to lower case
#   gather(year, number, -nhsboard) %>% # to long format
#   mutate(year = as.numeric(gsub("y", "", year))) #taking out y from year
# 
# # Bringing population to calculate rates
# pop_lookup <- readRDS(paste0(cl_out_pop, "HB2019_pop_est_1981_2018.rds")) %>%
#   setNames(tolower(names(.))) %>%  #variables to lower case
#   subset(year>2008) %>%  #select only 2002+
#   # Aggregating to get hb totals
#   rename(code = hb2019) %>%  select(code, year, pop) %>% group_by(code, year) %>%
#   summarise(denominator = sum(pop)) %>% ungroup %>% group_by(year) %>%
#   # Adding Scotland totals
#   bind_rows(summarise_all(., list(~if(is.numeric(.)) sum(.) else "S00000001"))) %>%
#   ungroup()
# 
# #Codes and names for areas
# names_lookup <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/HBdictionary.rds") %>%
#   mutate(areaname = gsub("NHS ", "", areaname), 
#          areaname = gsub(" and ", " & ", areaname))
# 
# # merging with codes
# hep_c <- left_join(hep_c, names_lookup, by = c("nhsboard" = "areaname")) %>%
#   mutate(code = case_when(nhsboard == "Scotland" ~ "S00000001", TRUE ~ code))
# 
# hep_c <- left_join(hep_c, pop_lookup, c("code", "year")) %>%
#   mutate(rate = round(number/denominator*100000, 1)) %>% # calculate rate
#   select(-denominator, -code) %>%
#   gather(measure, value, c(-nhsboard, -year)) %>%
#   mutate(measure = recode(measure, "number" = "Number", "rate" = "Rate"))
# 
# saveRDS(hep_c, "data/hepatitisc_board.rds")

hep_c <- readRDS("data/hepatitisc_board.rds") #reading data for app

#Use for selection of areas
board_list <- sort(unique(hep_c$nhsboard[hep_c$nhsboard != "Scotland"]))

#ScotPHO logo. 
#Needs to be https address or if local in code 64 (the latter does not work with 4.7 plotly)
scotpho_logo <-  list(source ="https://raw.githubusercontent.com/ScotPHO/plotly-charts/master/scotpho.png",
                      xref = "paper", yref = "paper",
                      x= -0.09, y= 1.2, sizex = 0.22, sizey = 0.18, opacity = 1)

############################.
## Visual interface ----
############################.
#Height and widths as percentages to allow responsiveness
#Using divs as issues with classing css 
ui <- fluidPage(style="width: 650px; height: 500px; ", 
                div(style= "width:100%", #Filters on top of page
                          h4("Chart 1. Persons in Scotland reported to be hepatitis C antibody positive"),
                  div(style = "width: 50%; float: left;",
                      selectInput("measure", label = "Select a measure type",
                                  choices = c("Number", "Rate"), selected = "Rate")
                         ),
                  div(style = "width: 50%; float: left;",
                  selectInput("area", label = "Select a health board", 
                            choices = board_list))
                ),
                div(style= "width:100%; float: left;", #Main panel
                  plotlyOutput("chart", width = "100%", height = "350px"),
                  p(div(style = "width: 25%; float: left;", #Footer
                        HTML("Source: <a href='https://hpspubsrepo.blob.core.windows.net/hps-website/nss/2834/documents/1_hcv-testing-diagnosis-treatment-scotland-2018.pdf' target='_blank'>HPS</a>")),
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
      write.csv(hep_c, file, row.names=FALSE) })
  
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
    data_scot <- hep_c %>% subset(nhsboard=="Scotland" & measure==input$measure)
    #Data for Health board line
    data_board <- hep_c %>% subset(nhsboard==input$area & measure==input$measure)
    
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

##END
