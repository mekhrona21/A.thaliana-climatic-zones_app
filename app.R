# Step 3 (after climate data extraction and heatmap) - shiny app for climatic variables access for 120 A. thaliana accessions

# updating libraries

library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)

# setting up the root folder

setwd("~/Downloads/Arabidopsis_climatic")

# Read the file with accessions

samples_bio <- read.csv("samples_bio_new.csv", header = TRUE)



# Separate the single column into multiple columns using semicolon as a delimiter
samples_bio <- separate(samples_bio, col = "sample.country.latitude.longitude.Annual.Mean.Temperature.Mean.Diurnal.Range.Isothermality.Temperature.Seasonality.Max.Temperature.of.Warmest.Month.Min.Temperature.of.Coldest.Month.Temperature.Annual.Range.Mean.Temperature.of.Wettest.Quarter.Mean.Temperature.of.Driest.Quarter.Mean.Temperature.of.Warmest.Quarter.Mean.Temperature.of.Coldest.Quarter.Annual.Precipitation.Precipitation.of.Wettest.Month.Precipitation.of.Driest.Month.Precipitation.Seasonality.Precipitation.of.Wettest.Quarter.Precipitation.of.Driest.Quarter.Precipitation.of.Warmest.Quarter.Precipitation.of.Coldest.Quarter",
                        into = c("ID", "Country", "Latitude", "Longitude", "Annual_Mean_Temperature", "Mean_Diurnal_Range", "Isothermality", "Temperature_Seasonality", "Max_Temperature_of_Warmest_Month", "Min_Temperature_of_Coldest_Month", "Temperature_Annual_Range", "Mean_Temperature_of_Wettest_Quarter", "Mean_Temperature_of_Driest_Quarter", "Mean_Temperature_of_Warmest_Quarter", "Mean_Temperature_of_Coldest_Quarter", "Annual_Precipitation", "Precipitation_of_Wettest_Month", "Precipitation_of_Driest_Month", "Precipitation_Seasonality", "Precipitation_of_Wettest_Quarter", "Precipitation_of_Driest_Quarter", "Precipitation_of_Warmest_Quarter", "Precipitation_of_Coldest_Quarter"),
                        sep = ";", remove = TRUE, convert = TRUE)


# UI for the design of the app
ui <- fluidPage(
  titlePanel("Arabidopsis thaliana Genotypes"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select Variable", choices = colnames(samples_bio)[5:ncol(samples_bio)]),
      selectInput("value", "Select Value", choices = "all"),
      actionButton("update", "Update Map"),
      actionButton("reset", "Reset Selection")
    ),
    mainPanel(
      leafletOutput("map", width = "100%", height = "500px")  # Adjusted height
    )
  )
)

# Code for server 
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    selected_variable <- input$variable
    selected_value <- input$value
    if (selected_value == "all" || is.null(selected_value)) {
      return(samples_bio)
    }
    subset(samples_bio, samples_bio[, selected_variable] == selected_value)
  })
  
  observe({
    variable_values <- c("all", unique(samples_bio[, input$variable]))
    updateSelectInput(session, "value", choices = variable_values)
  })
  
  observe({
    variable_choices <- colnames(samples_bio)[5:ncol(samples_bio)]
    updateSelectInput(session, "variable", choices = variable_choices)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      addCircleMarkers(
        data = filtered_data(),
        lat = ~Latitude,
        lng = ~Longitude,
        color = "red",
        radius = 5,
        popup = ~{
          current_data <- filtered_data()
          if (input$value == "all" || is.null(input$value)) {
            popup_content <- paste(
              "Sample ID: ", current_data$ID, "<br>",
              "Latitude: ", current_data$Latitude, "<br>",
              "Longitude: ", current_data$Longitude, "<br>",
              "Country: ", current_data$Country, "<br>"
            )
            for (var in colnames(current_data)[5:ncol(current_data)]) {
              popup_content <- paste(popup_content, var, ": ", as.character(current_data[, var]), "<br>", sep = "")
            }
            return(popup_content)
          } else {
            paste(
              "Sample ID: ", current_data$ID, "<br>",
              "Latitude: ", current_data$Latitude, "<br>",
              "Longitude: ", current_data$Longitude, "<br>",
              "Country: ", current_data$Country, "<br>",
              paste(input$variable, ": ", as.character(current_data[, input$variable]), "<br>"),
              "<br>"
            )
          }
        },
        popupOptions = popupOptions(maxWidth = 1000)  # Adjusted maxWidth
      )
  })
  
  
  observeEvent(input$reset, {
    updateSelectInput(session, "value", selected = "all")
    updateSelectInput(session, "variable", selected = input$variable)
  })
  
  observeEvent(input$update, {
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = filtered_data(),
        lat = ~Latitude,
        lng = ~Longitude,
        color = "red",
        radius = 5,
        popup = ~{
          current_data <- filtered_data()
          popup_content <- paste(
            "Sample ID: ", current_data$ID, "<br>",
            "Latitude: ", current_data$Latitude, "<br>",
            "Longitude: ", current_data$Longitude, "<br>",
            "Country: ", current_data$Country, "<br>"
          )
          if (input$value == "all" || is.null(input$value)) {
            for (i in seq_len(nrow(current_data))) {
              popup_content <- paste(popup_content, paste(
                colnames(current_data)[5:ncol(current_data)],
                ": ",
                as.character(current_data[i, 5:ncol(current_data)]),
                collapse = "<br>"
              ), "<br>", sep = "")
            }
          } else {
            for (variable in colnames(current_data)[5:ncol(current_data)]) {
              popup_content <- paste(popup_content, paste(
                variable,
                ": ",
                as.character(current_data[1, variable]),
                collapse = "<br>"
              ), "<br>", sep = "")
            }
          }
          return(popup_content)
        },
        popupOptions = popupOptions(maxWidth = 1000)  # Adjusted maxWidth
      )
  })
  
}

# run the app

shinyApp(ui, server)

