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
source("pavloviaHelperFunctions.R")

ui <- dashboardPage(
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
      downloadButton("downloadData", "Download merged data")
    )
  ),
  dashboardBody(
    useShinyjs(),
    useShinyalert(),
    uiOutput("dataAnalysisUI")
  )
)


server <- function(input, output, session) {
  disable("downloadData")
  data <- reactiveVal()
  token <- reactiveVal()
  projects <- reactiveVal()


  output$dataAnalysisUI <- renderUI({
    if (is.null(dataMerged())) {
      if (is.null(projects())) {
        return(h1("Submit valid Access Token (ensure you have projects in Pavlovia)"))
      }
      if (nrow(projects()) > 0) {
        return(tagList(
          h1("Your Projects"),
          DT::dataTableOutput("availableProjects")
        ))
      } else {
      }
    } else {
      return(tabsetPanel(
        tabPanel("Files", DT::dataTableOutput("dataOverview")),
        tabPanel("Descriptives", DT::dataTableOutput("descriptives")),
        tabPanel("Histogram", fluidRow(
          selectInput("histAV", choices = names(dataMerged()), label = "Histogram-AV"),
          plotOutput(outputId = "plot")
        )),
        tabPanel("MergedData", DT::dataTableOutput("fullData"))
      ))
    }
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


  observeEvent(input$submitToken, {
    projects(NULL)
    data(NULL)
    req(input$token)
    token(input$token)
    responseObject <- getProjectList(token())
    if (!responseObject$isError) {
      projects(responseObject$data)
      projectNames <- projects()$name
      projectIDs <- projects()$id
      choices <- setNames(as.list(projectIDs), projectNames)
      updateSelectInput(session, "project", choices = choices)
    } else {
      shinyalert(html = TRUE, "Oops!", responseObject$message, type = "error")
    }
  })

  observeEvent(input$getData, {
    data(NULL)
    disable("downloadData")
    req(input$project)
    req(token())
    print(input$project)
    projectID <- input$project
    accessToken <- token()
    responseObject <- getTibbleOfDataDirectoryOfProject(accessToken, projectID)

    if (!responseObject$isError) {
      data(responseObject$data)
      enable("downloadData")
    } else {
      shinyalert(html = TRUE, "Oops!", responseObject$message, type = "error")
    }
  })

 observeEvent(input$availableProjects_rows_selected,
              {
                print(input$availableProjects_rows_selected)
                projectNames <- projects()$name
                projectIDs <- projects()$id
                choices <- setNames(as.list(projectIDs), projectNames)
                updateSelectInput(session, "project", choices = choices, selected =choices[input$availableProjects_rows_selected] )
                
              })
  output$fullData <- DT::renderDataTable({
    DT::datatable(dataMerged(),
      options = list(scrollX = TRUE)
    )
  })

  output$availableProjects <- DT::renderDataTable({
    DT::datatable(projects(), selection = list(mode = 'single'),
      options = list(scrollX = TRUE)
    )
  })
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
