#Code to create chart of hepatitis c by board.

############################.
## Global ----
############################.
############################.
##Packages 

library(dplyr) #data manipulation
library(highcharter) #charts
library(phsstyles) #for chart colors
library(shiny) #shiny app
library(tidyr)
library(readr) #for reading in csv
library(janitor) #for data cleaning


#Set filepaths 
cl_out_pop <- "/conf/linkage/output/lookups/Unicode/Populations/Estimates/" #population lookups to calculate rates
filepath <- "/PHI_conf/ScotPHO/Website/Charts/Health Conditions/Hepatitis C/shiny_data" #shiny data

#read in currently deployed data
current_data <- read_csv(paste0(filepath, "/hepatitisc_data_to_2021.csv"))

#read in new data
hep_c <- read_csv(paste0(filepath, "/hepatitisc_data_2022.csv")) |> 
  mutate_if(is.character, factor) |>  #converting characters into factors
  clean_names() #variable names to lower case

#bring in population to calculate rates
pop_lookup <- readRDS(paste0(cl_out_pop, "HB2019_pop_est_1981_2022.rds")) |> 
  clean_names() |>   #variables to lower case
  subset(year=="2022") |>   #select only new year to be appended
  # Aggregating to get hb totals
  rename(code = hb2019) |>   select(code, year, pop) |>  group_by(code, year) |> 
  summarise(denominator = sum(pop)) |>  ungroup() |>  group_by(year) |> 
  # Adding Scotland totals
  adorn_totals("row", name = "S00000001") |>
  mutate(year = case_when(code == "S00000001" ~ 2022, TRUE ~ year)) #Update this line with newest year to match other rows
  
#Codes and names for areas
names_lookup <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/HBdictionary.rds") |> 
  mutate(areaname = gsub("NHS ", "", areaname),
         areaname = gsub(" and ", " & ", areaname))

# merging with codes
hep_c <- left_join(hep_c, names_lookup, by = c("nhsboard" = "areaname")) |> 
  mutate(code = case_when(nhsboard == "Scotland" ~ "S00000001", TRUE ~ code))

hep_c <- left_join(hep_c, pop_lookup, c("code", "year")) |> 
  mutate(rate = round(number/denominator*100000, 1))  |>  # calculate rate
  select(-denominator, -code) |> 
  gather(measure, value, c(-nhsboard, -year))  |> 
  mutate(measure = recode(measure, "number" = "Number", "rate" = "Rate"))

#append new data onto current data
hep_c <- rbind(current_data, hep_c)

#save files
saveRDS(hep_c, paste0(filepath, "/shiny_data_hepatitisc_board.rds"))

write.csv(hep_c, paste0(filepath, "/hepatitisc_data_to_2022.csv"), row.names = FALSE)

hep_c <- readRDS(paste0(filepath, "/shiny_data_hepatitisc_board.rds")) #reading data for app

#Use for selection of areas
board_list <- sort(unique(hep_c$nhsboard[hep_c$nhsboard != "Scotland"]))

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
                  highchartOutput("line_chart"),
                  #plotlyOutput("chart", width = "100%", height = "350px"),
                  p(div(style = "width: 25%; float: left;", #Footer
                        HTML("Source: <a href='https://hpspubsrepo.blob.core.windows.net/hps-website/nss/2834/documents/1_hcv-testing-diagnosis-treatment-scotland-2018.pdf' target='_blank'>HPS</a> (2009-18)"),
                        HTML("Source: <a href='https://www.publichealthscotland.scot/publications/surveillance-of-hepatitis-c-in-scotland/surveillance-of-hepatitis-c-in-scotland-progress-on-elimination-of-hepatitis-c-as-a-major-public-health-concern-2023-update/' target='_blank'>PHS</a> (2021-)")),
                    div(style = "width: 25%; float: left;",
                        downloadLink('download_data', 'Download data')),
                    div(style = "width: 50%; float: left;",
                        "Notes: Year of earliest positive specimen. Publication of 2019 and 2020 data was prevented by the COVID-19 pandemic.")
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
  
  #Creating chart 
  output$line_chart <- renderHighchart({
    
    #Data for plot
    data_chart <- hep_c |> subset(measure == input$measure & nhsboard == input$area)
    
    #Separate Scotland data out for separate series
    data_scot <- hep_c |> subset(measure == input$measure & nhsboard == "Scotland")
    
    #y axis title
    yaxistitle <- ifelse(input$measure == "Number", "Number of diagnoses",
                         "Rate per 100,000")
    
    #Creating dynamic text for if Island Board rates selected
    validate(
      need((input$area != "Island Boards" | input$measure == "Number"), "Rates are not published for the island boards"))
    
    highchart() |>
      hc_add_series(data_chart, "line", hcaes(y = value, x = year), name = input$area) |>  
      hc_add_series(data_scot, "line", hcaes(y = value, x = year), name = "Scotland") |>  
      hc_xAxis(title = list(text = "Year")) |> 
      hc_yAxis(title = list(text = yaxistitle)) |> 
      hc_legend(align = "left", verticalAlign = "top")
    
  })
  
  } # end of server part



############################.
## Calling app ----
############################.

shinyApp(ui = ui, server = server)

##END
