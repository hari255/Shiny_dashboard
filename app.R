#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

options(repos = "https://cran.rstudio.com/")

library(shiny)
library(leaflet)
library(shinydashboard)
library(plotly)
library(shinythemes)
library(readxl)




project_root <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Define relative paths
pfizer_path <- file.path(project_root,  "pfizer.xlsx")
Moderna_path <- file.path(project_root,  "moderna.xlsx")
janssen_path <- file.path(project_root,  "janssen.xlsx")

# Read Excel files using relative paths
pfizer <- readxl::read_excel(pfizer_path)
Moderna <- readxl::read_excel(Moderna_path)
janssen <- readxl::read_excel(janssen_path)




library(devtools)
devtools::install_github('FIRST-Data-Lab/IDDA', force = TRUE)

State <- pfizer$Jurisdiction
Date <- pfizer$Week.of.Allocations
Pfizer.1st.Dose.Allocations <- pfizer$X1st.Dose.Allocations 
Pfizer.2nd.Dose.Allocations <- pfizer$X2nd.Dose.Allocations 
Moderna.1st.Dose.Allocations <- Moderna$X1st.Dose.Allocations
Moderna.2nd.Dose.Allocations <- Moderna$X2nd.Dose.Allocations 
Janssen.1st.Dose.Allocations <- janssen$X1st.Dose.Allocations 
All.Dose.Allocations <- 0.5 *(Pfizer.2nd.Dose.Allocations+Pfizer.1st.Dose.Allocations+Moderna.1st.Dose.Allocations + Moderna.2nd.Dose.Allocations)+Janssen.1st.Dose.Allocations
Cum.Allocation <-cumsum(All.Dose.Allocations)

max_length <- max(c(length(Janssen.1st.Dose.Allocations),length(Moderna.1st.Dose.Allocations),length(Moderna.2nd.Dose.Allocations),length(Pfizer.1st.Dose.Allocations),length(Pfizer.2nd.Dose.Allocations)))

lengths_vector <- c(length(Janssen.1st.Dose.Allocations), length(Moderna.1st.Dose.Allocations), length(Moderna.2nd.Dose.Allocations), length(Pfizer.1st.Dose.Allocations), length(Pfizer.2nd.Dose.Allocations))

if (length(lengths_vector) > 0) {
  max_length <- max(lengths_vector)
} else {
  # Handle the case where all vectors have length zero
  max_length <- 0
}

df_vaccine <- data.frame(
  Janssen.1st.Dose.Allocations = c(Janssen.1st.Dose.Allocations, rep(NA, max_length - length(Janssen.1st.Dose.Allocations))),
  Moderna.1st.Dose.Allocations = c(Moderna.1st.Dose.Allocations, rep(NA, max_length - length(Moderna.1st.Dose.Allocations))),
  Moderna.2nd.Dose.Allocations = c(Moderna.2nd.Dose.Allocations, rep(NA, max_length - length(Moderna.2nd.Dose.Allocations))),
  Pfizer.1st.Dose.Allocations = c(Pfizer.1st.Dose.Allocations, rep(NA, max_length - length(Pfizer.1st.Dose.Allocations))),
  Pfizer.2nd.Dose.Allocations = c(Pfizer.2nd.Dose.Allocations, rep(NA, max_length - length(Pfizer.2nd.Dose.Allocations))),
  State = c(State, rep(NA, max_length - length(State))),
  Date = c(Date, rep(NA, max_length - length(Date))),
  All.Dose.Allocations = c(All.Dose.Allocations, rep(NA, max_length - length(All.Dose.Allocations))),
  Cum.Allocation = c(Cum.Allocation, rep(NA, max_length - length(Cum.Allocation)))
)

population_data <- IDDA::pop.state

population_data  <- population_data%>% rename_at('State', ~'name')

df_vaccine <- df_vaccine%>%
  mutate(name = sapply(State, gsub, pattern = " ", 
                       replacement = ""))



df<- left_join(df_vaccine,population_data, by="name")


library(dplyr)
df <- mutate(df, Doseperpop=Cum.Allocation/df$population)

df <- df[!(df$State=="American Samoa" | df$State=="Palau" | df$State == "Guam" | df$State == "Mariana Islands" | df$State == "Marshall Islands" | df$State == "PuertoRico" | df$State == "Philadelphia" | df$State == "New York City" |df$State == "Puerto Rico" |df$State == "U.S. Virgin Islands" | df$State == "Chicago" |df$State == "Federal Entities" | df$State == "Micronesia"),] 



min_date <- min(df$Date)
max_date <- max(df$Date)

# Assuming you have the necessary data preparation steps here...

# UI
ui <- fluidPage(theme = shinytheme("readable"),
                dashboardPage(
                  dashboardHeader(
                    title = "Covid-19 Vaccinations",
                    titleWidth = 230,
                    tags$li(class="dropdown", tags$a(href="https://harinathportfolio.online/",
                                                     icon("portfolio"), "My Portfolio", target="_blank")),
                    tags$li(class="dropdown", tags$a(href="https://www.linkedin.com/in/harinathmandha/",
                                                     icon("linkedin"), "Linkedin" , target="_blank")),
                    tags$li(class="dropdown", tags$a(href="https://github.com/hari255",
                                                     icon("github"), "Github", target="_blank"))
                  ),
                  dashboardSidebar(
                    sliderInput("date_range", "Select Date Range:",     
                                min = min_date, max = max_date, 
                                value = c(min_date, max_date),
                                step = 1,  timeFormat = "%Y-%m-%d")
                  ),
                  
                  dashboardBody(
                    tabsetPanel(
                      tabPanel("Dataset",
                               icon = icon("database"),
                               tabsetPanel(
                                 tabPanel("Dataset 1",
                                          fluidRow(
                                            column(width = 12, 
                                                   tags$h3("Dataset Details"),
                                                   tags$p(
                                                     "About: This dataset has been sourced from the Centers for Disease Control and Prevention (CDC), a reputable government agency dedicated to public health. The CDC is an authoritative source known for maintaining official health-related data in the United States. The dataset provides comprehensive insights into vaccine allocations across the USA and is inclusive of data from prominent vaccine providers such as Pfizer, Moderna, and Janssen. Specifically, it details the distribution of each vaccine dose, distinguishing between 1st and 2nd doses."
                                                   ),
                                                   tags$p(
                                                     "In addition to the CDC dataset, we have incorporated information from the IDDA R packages, which focuses on state-wise population data. This supplementary dataset enriches our analysis by providing demographic context for each state. Together, these datasets form a robust foundation for exploring and understanding the dynamics of vaccine allocations and population demographics in the United States."
                                                   ),
                                                   tags$p("Data Types: Numeric, Categorical, Datetime, object"),
                                                   tags$p("Structure: 1596 records * 12 columns"),
                                                   tags$h4("Description about columns"),
                                                   tags$p("Janssen.1st.Dose.Allocations: The number of Janssen (Johnson & Johnson) COVID-19 vaccine first doses allocated for distribution in a specific location on a given date."),
                                                   tags$p("Moderna.1st.Dose.Allocations: The number of Moderna COVID-19 vaccine first doses allocated for distribution in a specific location on a given date."),
                                                   tags$p("Moderna.2nd.Dose.Allocations: The number of Moderna COVID-19 vaccine second doses allocated for distribution in a specific location on a given date."),
                                                   tags$p("Pfizer.1st.Dose.Allocations: The number of Pfizer COVID-19 vaccine first doses allocated for distribution in a specific location on a given date."),
                                                   tags$p("Pfizer.2nd.Dose.Allocations: The number of Pfizer COVID-19 vaccine second doses allocated for distribution in a specific location on a given date."),
                                                   tags$p("State: The name of the state for which vaccine allocation data is recorded."),
                                                   tags$p("Date: The date on which the vaccine allocation data was recorded, in POSIXct format."),
                                                   tags$p("All.Dose.Allocations: The total number of all COVID-19 vaccine doses (first and second doses combined) allocated for distribution in a specific location on a given date."),
                                                   tags$p("Cum.Allocation: The cumulative total of COVID-19 vaccine doses allocated for distribution up to a specific date."),
                                                   tags$p("population: The population of the state for which demographic data is recorded."),
                                                   tags$p("Doseperpop: This column represents the ratio or percentage of vaccine doses per population, indicating the distribution level relative to the total population.")
                                            )
                                          )
                                 ),
                                 tabPanel("Dataset 2",
                                          fluidRow(
                                            column(width = 12, 
                                                   tags$h3("Dataset Details"),
                                                   tags$p(
                                                     "This dataset is obtained from the R package SP, containing essential geographical information such as longitude and latitude data for each state. The primary objective of incorporating this dataset is to leverage its geographical coordinates. Specifically, the longitude and latitude information are pivotal for creating an interactive map using the Leaflet package in R. To achieve this, the dataset has been merged with the main dataframe after transforming it into a spatialPolygon DataFrame. The seamless integration of geographical details enhances the visualization capabilities of the overall dataset, providing a spatial context to the analysis and facilitating the creation of dynamic and informative maps within the Shiny dashboard."
                                                   ),
                                                   tags$p("Structure: 52 rows x 5 columns"),
                                                   tags$p("Data Types: Float, Integer, Numeric")
                                            )
                                          )
                                 )        
                               )
                      ),
                      
                      tabPanel("Interactive Viz",
                               icon = icon("chart-line"),
                               tabsetPanel(
                                 tabPanel("Plot 1",
                                          fluidRow(
                                            column(width = 12, 
                                                   tags$h3("Vaccination Allocations in Virginia"),
                                                   plotlyOutput("plot1", height = "600px")),
                                            column(width = 12, 
                                                   p(
                                                     "Plot Description: The interactive time-series plot depicts the allocation trends of various COVID-19 vaccine doses in the state of Virginia. The plot captures the distribution over time for different vaccine providers, including Janssen (Johnson & Johnson), Pfizer, and Moderna."
                                                   ),
                                                   tags$h4("Key Elements"),
                                                   tags$ul(
                                                     tags$li(
                                                       "Janssen Dose: The blue line with markers represents the allocation trend of Janssen COVID-19 vaccine first doses in Virginia. The plotted points connected by lines provide a clear visualization of how the allocation of Janssen doses has evolved over time."
                                                     ),
                                                     tags$li(
                                                       "Pfizer 2nd Dose: The orange line with markers illustrates the allocation trend of Pfizer COVID-19 vaccine second doses in Virginia. This line graph shows the changes in allocation amounts for the second dose of the Pfizer vaccine across different dates."
                                                     ),
                                                     tags$li(
                                                       "Pfizer 1st Dose: The green line with markers showcases the allocation trend of Pfizer COVID-19 vaccine first doses in Virginia. Similar to the other lines, this graph represents the distribution of first doses of the Pfizer vaccine over time."
                                                     ),
                                                     tags$li(
                                                       "Moderna 2nd Dose: The red line with markers displays the allocation trend of Moderna COVID-19 vaccine second doses in Virginia. It provides insights into how the allocation of the second dose from Moderna has varied over the specified timeline."
                                                     ),
                                                     tags$li(
                                                       "Moderna 1st Dose: The purple line with markers exhibits the allocation trend of Moderna COVID-19 vaccine first doses in Virginia. This line graph enables a visual understanding of the distribution patterns for the first dose of the Moderna vaccine."
                                                     )
                                                   ),
                                                   p(
                                                     "Interactivity: The plot is interactive, allowing users to hover over data points to view specific allocation values for each provider on different dates. Additionally, the legend facilitates the selection and deselection of specific providers for a more focused analysis."
                                                   )
                                            )
                                          )
                                 ),
                                 tabPanel("Plot 2", 
                                          fluidRow(
                                            column(width =12, 
                                                   tags$h4("Equitable Distribution of COVID-19 Vaccine Doses Across States"),
                                                   plotlyOutput("pie_plot", height = "600px")), 
                                            column(width = 12,
                                                   p(
                                                     "Plot Description: The pie chart provides a concise yet insightful representation of the distribution of COVID-19 vaccine doses relative to the population across different states. This visually engaging plot allows for an immediate understanding of the proportional impact of vaccination efforts in each state."
                                                   ),
                                                   tags$h4("Key Elements"),
                                                   tags$ul(
                                                     tags$li(
                                                       "Pie Slices: Each slice of the pie corresponds to a specific state, with the size of the slice directly proportional to the proportion of vaccine doses allocated concerning the state's population."
                                                     ),
                                                     tags$li(
                                                       "State Labels: The labels around the perimeter of the pie identify individual states, making it easy to associate each slice with its respective location."
                                                     ),
                                                     tags$li(
                                                       "Hole in the Center: The central hole in the pie, set at 40% (hole = 0.4), enhances the visual appeal of the chart and allows for the inclusion of a title within the vacant center space."
                                                     )
                                                   ),
                                                   tags$h4("Insights"),
                                                   tags$ul(
                                                     tags$li(
                                                       "Proportional Distribution: The varying sizes of the pie slices reveal the relative distribution of vaccine doses across states concerning their population sizes. Larger slices represent states with a higher proportion of vaccine doses relative to their population."
                                                     ),
                                                     tags$li(
                                                       "At-a-Glance Analysis: The pie chart offers a quick and intuitive overview, making it easy to identify states that have received a substantial share of vaccine doses in comparison to their population."
                                                     ),
                                                     tags$li(
                                                       "Legend: The legend, when displayed, provides additional information about the states corresponding to each slice, enhancing the interpretability of the pie chart."
                                                     )
                                                   )
                                            )
                                          )
                                 ),
                                 tabPanel("Plot 3", 
                                          fluidRow(
                                            column(width =12, 
                                                   tags$h4("COVID-19 Vaccine Allocations Across States"),
                                                   plotlyOutput("state_bar_plot", height = "600px")),
                                            column(width = 12,
                                                   p(
                                                     "Plot Description: The stacked bar plot visualizes the comprehensive distribution of COVID-19 vaccine doses across different states. The plot effectively represents the total allocations for each vaccine provider—Janssen (Johnson & Johnson), Moderna, and Pfizer—through distinct bars stacked on top of one another for each state."
                                                   ),
                                                   tags$h4("Insights"),
                                                   tags$ul(
                                                     tags$li(
                                                       "The stacked bars allow for a quick comparison of the total vaccine allocations between different states, revealing patterns and disparities in distribution. The legend provides the option to selectively display or hide specific vaccine providers, aiding in a focused analysis based on individual preferences."
                                                     )
                                                   )
                                            )               
                                          )
                                 ),
                                 tabPanel("Plot 4", 
                                          fluidRow(
                                            column(width =12, 
                                                   tags$h4("Relationship Between Population and COVID-19 Vaccine Allocations"),
                                                   plotlyOutput("avp_plot", height = "600px")),
                                            column(width = 12,
                                                   p(
                                                     "This scatter plot visually explores the relationship between the population of states and their respective COVID-19 vaccine allocations. This plot provides an interactive representation that allows for a nuanced understanding of how vaccine doses are distributed in relation to the size of state populations."
                                                   ),
                                                   tags$h4("Insights"),
                                                   tags$ul(
                                                     tags$li(
                                                       "Population vs. Vaccine Allocations: The scatter plot enables the examination of how vaccine allocations scale concerning the population of each state. States situated towards the upper-right portion of the plot may have both higher populations and larger vaccine allocations."
                                                     ),
                                                     tags$li(
                                                       "Identification of Outliers: Outliers, representing states with notable deviations in either population or vaccine allocations, can be easily identified and analyzed for unique patterns or circumstances."
                                                     ),
                                                     tags$li(
                                                       "Interactivity: The plot is interactive, allowing users to hover over data points to view specific details about each state."
                                                     )
                                                   )
                                            )
                                          )
                                 ),
                                 tabPanel("Plot 5", 
                                          fluidRow(
                                            column(width =12, 
                                                   tags$h4("Cumulative Impact of COVID-19 Vaccination Efforts"),
                                                   plotlyOutput("Cum_alloc_plot", height = "600px")),
                                            column(width = 12,
                                                   p(
                                                     "Plot Description: This line plot visually tracks the cumulative distribution of COVID-19 vaccine doses across a specified timeline. This dynamic plot provides insights into how the cumulative allocations have evolved over the recorded dates, offering a comprehensive view of the progression of vaccination efforts."
                                                   ),
                                                   tags$h4("Key Elements"),
                                                   tags$ul(
                                                     tags$li(
                                                       "Connected Line: The plot features a continuous line that connects data points, showcasing the cumulative vaccine allocations over time. The linear shape of the line signifies the progression of cumulative allocations from one date to the next."
                                                     )
                                                   ),
                                                   tags$h4("Insights"),
                                                   tags$ul(
                                                     tags$li(
                                                       "Temporal Evolution: The plot allows for the observation of how cumulative vaccine allocations change over time. Upward trends in the line reflect periods of increased allocations, while flat or downward trends may indicate stable or decreasing allocation rates."
                                                     ),
                                                     tags$li(
                                                       "Temporal Analysis: The plot is particularly useful for conducting a temporal analysis, revealing patterns and trends in the cumulative distribution of vaccine doses over the specified timeline."
                                                     )
                                                   )
                                            )
                                          )
                                 )
                               )
                      ),
                      tabPanel("Choropleth Map",
                               icon = icon("map"),
                               fluidRow(
                                 column(
                                   width =12,
                                   tags$h4(
                                     "The map is interactive, enabling users to explore and analyze specific states. Hovering over states provides detailed information, enhancing the interpretability of the choropleth map. This choropleth map serves as a valuable tool for policymakers, public health officials, and researchers, offering a spatial perspective on the equitable distribution of COVID-19 vaccine doses across the United States."
                                   ),
                                   leafletOutput("map", height="600px")
                                 ),
                                 column(width = 12,
                                        tags$h4("Choropleth Map Description:"),
                                        tags$p(
                                          "The choropleth map dynamically visualizes the distribution of COVID-19 vaccine doses across different states in the United States. Utilizing geographical data and statistical information, this interactive map offers insights into the dosage per population for each state, providing a spatial context to vaccination efforts."
                                        ),
                                        tags$h4("Key Elements:"),
                                        tags$ul(
                                          tags$li(
                                            "Geographical Data: The map utilizes geographical data from the US States GeoJSON file, outlining the boundaries of each state."
                                          ),
                                          tags$li(
                                            "Dosage per Population Color Gradient: The color gradient on the map represents the dosage per population for each state. States with a higher dosage per population are shaded with warmer colors, while states with lower ratios are depicted in cooler colors."
                                          ),
                                          tags$li(
                                            "Interactive Labels: Hovering over a state on the map reveals a tooltip with detailed information, including the state's name, population, dosage per population, and total vaccine allocations."
                                          ),
                                          tags$li(
                                            "Legend: The legend positioned in the bottom-right corner of the map provides a reference for interpreting the color scale. It indicates the dosage per population range associated with each color on the map."
                                          )
                                        ),
                                        tags$h4("Insights"),
                                        tags$ul(
                                          tags$li(
                                            "Dosage per Population Variation: The map highlights variations in dosage per population across states, allowing for a quick assessment of how efficiently each state has allocated vaccine doses relative to its population."
                                          ),
                                          tags$li(
                                            "Performance Evaluation: States with warmer colors may indicate more effective vaccine distribution, while cooler colors suggest areas where the dosage per population might need improvement."
                                          )
                                        ),
                                        tags$h4("Usage Note"),
                                        tags$p(
                                          "A high dosage per population value for a state implies that the state may have faced challenges or performed poorly in allocating vaccine doses to its communities."
                                        )
                                 )
                                 
                               )
                      )
                    )
                  )
                )
)




# Set the CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))


server <- function(input, output) {
  
  shinyjs::useShinyjs()
  
  # Reactive filtered data based on date range
  filtered_data <- reactive({
    df %>%
      filter(Date >= input$date_range[1] & Date <= input$date_range[2])
  })
  
  # Plot 1
  output$plot1 <- renderPlotly({
    plot_ly(data = filtered_data() %>% filter(State == 'Virginia')) %>%
      add_trace(x = ~Date, y = ~Janssen.1st.Dose.Allocations, type = 'scatter', mode = 'lines+markers', showlegend = TRUE, name = 'Janssen Dose') %>%
      add_trace(x = ~Date, y = ~Pfizer.2nd.Dose.Allocations, type = 'scatter', mode = 'lines+markers', showlegend = TRUE, name = 'Pfizer 2nd dose') %>%
      add_trace(x = ~Date, y = ~Pfizer.1st.Dose.Allocations, type = 'scatter', mode = 'lines+markers', showlegend = TRUE, name = 'Pfizer 1st dose') %>%
      add_trace(x = ~Date, y = ~Moderna.2nd.Dose.Allocations, type = 'scatter', mode = 'lines+markers', showlegend = TRUE, name = 'Moderna 2nd dose') %>%
      add_trace(x = ~Date, y = ~Moderna.1st.Dose.Allocations, type = 'scatter', mode = 'lines+markers', showlegend = TRUE, name = 'Moderna 1st dose')
  })
  
  # Plot 2
  output$pie_plot <- renderPlotly({
    plot_ly(data = filtered_data(), labels = ~State, values = ~Doseperpop, type = 'pie', hole = 0.4) %>%
      layout(showlegend = TRUE)
  })
  
  # Plot 3
  output$state_bar_plot <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~State, y = ~Janssen.1st.Dose.Allocations, type = 'bar', name = 'Janssen 1stDose') %>%
      add_trace(y = ~Moderna.1st.Dose.Allocations, name = 'Moderna 1st Dose') %>%
      add_trace(y = ~Moderna.2nd.Dose.Allocations, name = 'Moderna 2nd Dose') %>%
      add_trace(y = ~Pfizer.1st.Dose.Allocations, name = 'Pfizer 1st Dose') %>%
      add_trace(y = ~Pfizer.2nd.Dose.Allocations, name = 'Pfizer 2nd Dose') %>%
      layout(title = 'Total Vaccine Allocations by State', xaxis = list(title = 'State'), yaxis = list(title = 'Total Allocations'), barmode = 'stack')
  })
  
  output$avp_plot <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~population, y = ~All.Dose.Allocations, text = ~State, mode = 'markers', type = 'scatter', marker = list(size = 10)) %>%
      layout(title = 'Scatter Plot: Population vs. Vaccine Allocations', xaxis = list(title = 'Population'), yaxis = list(title = 'Vaccine Allocations'))
  })
  
  output$Cum_alloc_plot <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~Date, y = ~Cum.Allocation, type = 'scatter', mode = 'lines', line = list(shape = "linear")) %>%
      layout(title = 'Cumulative Vaccine Allocations Over Time', xaxis = list(title = 'Date'), yaxis = list(title = 'Cumulative Allocations'))
  })
  
  # Map
  output$map <- renderLeaflet({
    library(geojsonio)
    library(leaflet)
    library(dplyr)
    library(sp)
    library(sf)
    
    # Assuming 'df' is your main dataset
    
    states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
    states <- sp::merge(states, filtered_data(), by = "name", duplicateGeoms = TRUE)
    
    m <- leaflet(states) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    
    bins <- c(0, 25, 50, 75, 100, 125, 150)
    pal <- colorBin("YlOrRd", domain = states$Doseperpop, bins = bins)
    
    labels_cases <- sprintf("<strong>%s</strong><br/>Population: %g M<br>
                            Dosage per population: %g<br>
                            All dose allocation: %g",
                            states$name, round(states$population / (1e6), 2),
                            states$Doseperpop, states$All.Dose.Allocations) %>%
      lapply(htmltools::HTML)
    
    m <- m %>% addPolygons(
      fillColor = ~pal(states$Doseperpop),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels_cases,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))
    
    m <- m %>% addLegend(
      pal = pal,
      values = states$Doseperpop,
      opacity = 0.7,
      title = "Dosage per population",
      position = "bottomright")
    
    m
  })
}


shinyApp(ui, server)







