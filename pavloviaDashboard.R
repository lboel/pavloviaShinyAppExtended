library(shiny)
library(stringr)
library(tidyverse)
library(httr)
library(jsonlite)
library(ggplot2)
library(shinyjs)
library(shinydashboard)
library(shinycssloaders)


# library(rsconnect)
# deployApp()


ui <- dashboardPage(
  # Application title
  dashboardHeader(title = "pavloviaShinyApp"),
  dashboardSidebar(
    column(12,
           align = "center", offset = 0,
           tags$style(".skin-blue .sidebar a { color: #444; }"),
           # Input token and submit
           passwordInput("token", label = h3("Token"), placeholder = "Enter Token to get access to Data..."),
           actionButton("submitToken", label = "submit"),
          # Select project
          selectInput("project", label = h3("Project"), choices = "Enter valid token..."),
          actionButton("selectProject", label = "select"),
          # Button
          fluidRow(
            column(6,div(style = "height:100px;"))),
            downloadButton("downloadData", "Download merged data")# %>% withSpinner(color = "#0dc5c1")

    )
  ),
  
  
  # Main panel for displaying outputs ----
  dashboardBody(
    useShinyjs(),
    h3("Files"),
    fluidRow(
      DT::dataTableOutput("dataOverview") %>% withSpinner(color = "#0dc5c1")
    ),
    h3("Descriptives"),
    fluidRow(
      DT::dataTableOutput("descriptives") %>% withSpinner(color = "#0dc5c1")
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  disable("downloadData")
  projects <- reactiveValues(list = NULL)
  
  observeEvent(input$submitToken,{
  token <- reactive({input$token}) # Personal Access Token for the Project
  
  # projectlistInput <- reactive({
    gitlabPavloviaURL <- paste0("https://gitlab.pavlovia.org/api/v4/projects/?owned=true") # API - URL to download whole repository
    r <- GET(gitlabPavloviaURL, add_headers("PRIVATE-TOKEN" = token())) # Get list of available projects
    bin <- content(r, "raw") # Writing Binary
    projects$list <- read_file(bin) %>% jsonlite::fromJSON()
  # })
  
  updateSelectInput(session, "project", choices = projects$list$name)
    
  })
  
  observeEvent(input$selectProject,{
    disable("downloadData")
    # datasetInput <- reactive({
    # disable("downloadData")
    project_id <- projects$list$id[projects$list$name == input$project]  # Project ID
    gitlabPavloviaURL <- paste0("https://gitlab.pavlovia.org/api/v4/projects/", project_id, "/repository/archive.zip") # API - URL to download whole repository
    r <- GET(gitlabPavloviaURL, add_headers("PRIVATE-TOKEN" = input$token)) # Getting Archive
    bin <- content(r, "raw") # Writing Binary
    temp <- tempfile() # Init Tempfile
    writeBin(bin, temp) # Write Binary of Archive to Tempfile
    
    listofFiles <- unzip(
      zipfile = temp, overwrite = T,
      junkpaths = T, list = T
    ) # Unzip only list of all files in the archive.zip file
    
    csvFiles <- grep("*.csv", x = listofFiles$Name, value = T) # Grep only the csv Files (Pattern can be extended to get only data-csv file)
    
    unzip(
      zipfile = temp, overwrite = T,
      junkpaths = T, files = csvFiles, exdir = "temp"
    ) # Unzip the csv Files in the temp-file
    
    csvFilesPaths <- list.files("temp/", full.names = T) # Get the unzipped csv-Files in the temp-directory
    
    # To get only Valid CSV-Files and enable us to filter by DateTime of the File we can parse the files standard date-time string in the Pavlovia-Default FileNames
    dateTimeOfFiles <- tibble(filepaths = csvFilesPaths) %>%
      mutate(dateTime = str_extract(filepaths, "[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}h[0-9]{2}")) %>%
      filter(!is.na(dateTime)) %>%
      mutate(dateTime = parse_datetime(dateTime, "%Y-%m-%d_%Hh%M"))
    # %>%  filter(dateTime > parse_datetime("2019-02-01_15h00", "%Y-%m-%d_%Hh%M")) # This can be used to Filter by a specific time
    
    # Purrr Magic  - Thanks to https://clauswilke.com/blog/2016/06/13/reading-and-combining-many-tidy-data-files-in-r/
    
    # Now the read the desired data Files with purrr:
    data <- data_frame(filename = dateTimeOfFiles$filepaths, date = dateTimeOfFiles$dateTime) %>% # create a data frame
      # holding the file names
      mutate(
        file_contents = map(
          filename, # read files into
          ~ read.csv(file.path(.), colClasses = "character")
          # ~ read.table(file.path(.), 
          # allowEscapes = TRUE, 
          # sep = ",", header=T, 
          # fileEncoding = "UTF-8")
        ) # a new data column
      ) %>% mutate(bytes = file.info(csvFilesPaths)$size,
                   size = file.info(csvFilesPaths)$size %>% utils:::format.object_size(., "auto")
                   )
    
    # Unlink temp because we don't need it anymore
    unlink("temp", recursive = T)
    # disable("downloadData")
    data
  # })
  
  # Table of selected dataset ----
  output$dataOverview <- DT::renderDataTable({
    DT::datatable(data %>%
                    rowwise() %>%
                    mutate(participant = list(file_contents$participant[1]), 
                           fileDim = paste0("Rows: ", dim(file_contents)[1], ", Vars: ", dim(file_contents)[2])[1]) %>%
                    select(-file_contents) %>% mutate(participant = as.character(participant)),
                  options = list(scrollX = TRUE)
    )
  })
  
  dataMerged <- 
    # Read in all available data in a single tibble
    data %>% select(file_contents) %>% # remove filenames, not needed anynmore
      unnest(cols = c(file_contents)) %>% replace(.=="", NA) %>% mutate_all(type.convert)
  
  # delete all empty columns
  all.empty <- function(x){all(x %in% c("",NA))}
  dataMerged <- dataMerged[,which(!sapply(dataMerged, all.empty))]

  output$descriptives <- DT::renderDataTable({
    DT::datatable(dataMerged %>% group_by(participant) %>% psych::describe() %>% select(-1) %>% round(2),
                  options = list(scrollX = TRUE)
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$project,"_download_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataMerged, file, row.names = FALSE)
    }
  )
  
  enable("downloadData")
  
  })
    
  
  
}

# Run the application
shinyApp(ui = ui, server = server)

