library(shiny)
library(stringr)
library(tidyverse)
library(httr)
library(jsonlite)
library(ggplot2)
library(shinyjs)
library(shinydashboard)
library(shinycssloaders)
library(shinyalert)

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
      actionButton("getData", label = "Get Data"),
      # Button
      fluidRow(
        column(6, div(style = "height:100px;"))
      ),
      downloadButton("downloadData", "Download merged data") # %>% withSpinner(color = "#0dc5c1")
    )
  ),


  # Main panel for displaying outputs ----
  dashboardBody(
    useShinyjs(),
    uiOutput("dataAnalysisUI")
  )
)



getProjectList <- function(accessToken) {
  gitlabPavloviaURL <- paste0("https://gitlab.pavlovia.org/api/v4/projects/?owned=true") # API - URL to download whole repository
  r <- GET(gitlabPavloviaURL, add_headers("PRIVATE-TOKEN" = accessToken)) # Get list of available projects
  bin <- content(r, "raw") # Writing Binary
  projects <- read_file(bin) %>% jsonlite::fromJSON()
  projects
}

getTibbleOfDataDirectoryOfProject <- function(accessToken, projectID) {
  gitlabPavloviaURL <- paste0("https://gitlab.pavlovia.org/api/v4/projects/", projectID, "/repository/archive.zip") # API - URL to download whole repository
  r <- GET(gitlabPavloviaURL, add_headers("PRIVATE-TOKEN" = accessToken)) # Getting Archive
  bin <- content(r, "raw") # Writing Binary
  temp <- tempfile() # Init Tempfile
  writeBin(bin, temp) # Write Binary of Archive to Tempfile

  listofFiles <- unzip(
    zipfile = temp, overwrite = T,
    junkpaths = T, list = T
  ) # Unzip only list of all files in the archive.zip file

  csvFiles <- grep("data/*.csv", x = listofFiles$Name, value = T) # Grep only the csv Files (Pattern can be extended to get only data-csv file)

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
    ) %>% mutate(
      bytes = file.info(csvFilesPaths)$size,
      size = file.info(csvFilesPaths)$size %>% utils:::format.object_size(., "auto")
    )

  # Unlink temp because we don't need it anymore
  unlink("temp", recursive = T)
  data
}


mergeDataTibble <- function(data) {
  dataMerged <-
    # Read in all available data in a single tibble
    data %>%
    filter(fileDimRows > 0) %>%
    select(file_contents) %>%
    # remove filenames, not needed anynmore
    unnest(cols = c(file_contents)) %>%
    replace(. == "", NA)
}





# Define server logic
server <- function(input, output, session) {
  disable("downloadData")
  data <- reactiveVal()
  token <- reactiveVal()
  projects <- reactiveVal()



  output$dataAnalysisUI <- renderUI({
    if (is.null(dataMerged())) {
      return(h1("First level title"))
    } else {
      return(tabsetPanel(
        tabPanel("Files", DT::dataTableOutput("dataOverview")),
        tabPanel("Descriptives", DT::dataTableOutput("descriptives")),
        tabPanel("Histogram", fluidRow(
          selectInput("histAV", choices = names(dataMerged()), label = "Histogram-AV"),
          plotOutput(outputId = "plot"))),
        tabPanel("MergedData", DT::dataTableOutput("fullData") ))
      )
    }
  })
  
  

  observeEvent(input$submitToken, {
    req(input$token)
    token(input$token)
    projects(getProjectList(token()))
    projectNames <- projects()$name
    projectIDs <- projects()$id
    choices <- setNames(as.list(projectIDs), projectNames)
    updateSelectInput(session, "project", choices = choices)
  })

  output$plot <- renderPlot({
    req(input$histAV)
    dataToPlot <- as.data.frame(dataMerged())
    print(dataToPlot)
    print(as.numeric(dataToPlot[, input$histAV]))
    dataToPlot[, input$histAV] <- as.numeric(dataToPlot[, input$histAV])
    p <- ggplot(dataToPlot, aes(x = get(input$histAV))) +
      geom_histogram()
    p
  })

  observeEvent(input$getData, {
    data(NULL)
    req(input$project)
    req(token())
    print(input$project)
    projectID <- input$project # input$project
    accessToken <- token() # token()


    responseObject <- list(data = c(), message = "OK", isError = F)
    gitlabPavloviaURL <- paste0("https://gitlab.pavlovia.org/api/v4/projects/", projectID, "/repository/archive.zip") # API - URL to download whole repository
    r <- GET(gitlabPavloviaURL, add_headers("PRIVATE-TOKEN" = accessToken)) # Getting Archive

    if (r$status_code == "200") {
      print("Got Response")
      bin <- content(r, "raw") # Writing Binary
      temp <- tempfile() # Init Tempfile
      writeBin(bin, temp) # Write Binary of Archive to Tempfile

      listofFiles <- unzip(
        zipfile = temp, overwrite = T,
        junkpaths = T, list = T
      ) # Unzip only list of all files in the archive.zip file
      print(listofFiles)
      csvFiles <- grep(pattern = "*data/.*.csv", perl = F, x = listofFiles$Name, value = T) # Grep only the csv Files (Pattern can be extended to get only data-csv file)
      print(csvFiles)
      if (length(csvFiles) > 0) {
        print(csvFiles)
        print("Got CSVFiles")
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
        print(dateTimeOfFiles)
        # Now the read the desired data Files with purrr:
        datatemp <- data_frame(filename = dateTimeOfFiles$filepaths, date = dateTimeOfFiles$dateTime) %>%
          # create a data frame
          # holding the file names
          mutate(
            file_contents = map(
              filename, # read files into
              ~ tryCatch(read.csv(file.path(.), colClasses = "character"), error = function(e) {
                NULL
              })
            ) # a new data column
          ) %>%
          rowwise() %>%
          mutate(
            fileDimRows = ifelse(is.null(dim(file_contents)[1]), 0, dim(file_contents)[1]),
            fileDimColumns = ifelse(is.null(dim(file_contents)[2]), 0, dim(file_contents)[2])
          ) %>%
          ungroup()

        unlink("temp", recursive = T)

        responseObject$data <- datatemp

        data(datatemp)
        enable("downloadData")
      }
    }

    shinyalert("Oops!", "Something went wrong.", type = "error")
  })
  output$fullData <- DT::renderDataTable({
    DT::datatable( dataMerged() %>%
                    select(-file_contents),
                  options = list(scrollX = TRUE)
    )})
    
    
  # Table of selected dataset ----
  output$dataOverview <- DT::renderDataTable({
    if (!is.null(data())) {
      DT::datatable(data() %>%
        select(-file_contents),
      options = list(scrollX = TRUE)
      )
    }
  })


  dataMerged <- reactive({
    if (!is.null(data())) {
      if (nrow(data()) > 0) {
        mergeDataTibble(data())
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  })


  output$descriptives <- DT::renderDataTable({
    DT::datatable(dataMerged() %>% psych::describe() %>% select(-1) %>% round(2),
      options = list(scrollX = TRUE)
    )
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$project, "_download_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataMerged(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
