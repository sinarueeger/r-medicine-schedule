source("utils.R")
library(shiny)
library(shinydashboard)
library(shinythemes)

ui <-
  navbarPage(
    "Change timezone for R/Medicine",
    theme = shinytheme("flatly"),
    tabPanel(
      "Schedule",
      # Sidebar layout with input and output definitions ----
      sidebarLayout(
        sidebarPanel(
          # Input: Select a dataset ----
          selectInput(
            "dataset",
            "Choose a day:",
            choices = c("2020-08-27 (Friday)", "2020-08-28 (Saturday)")
          ),
          
          
          # Input: Specify timezone ----
          selectizeInput(
            'tz',
            label = NULL,
            choices = OlsonNames(),
            options = list(create = TRUE)
          ),
          
          # Include clarifying text ----
          helpText("Select a day (in EDT) and your preferred time zone"),
          
          # Button
          downloadButton("downloadData", "Download"),
          
          # Input: actionButton() to defer the rendering of output ----
          # until the user explicitly clicks the button (rather than
          # doing it immediately when inputs change). This is useful if
          # the computations required to render output are inordinately
          # time-consuming.
          actionButton("update", "Update View")
          
        ),
        # Main panel for displaying outputs ----
        mainPanel(# Output: Header + table of distribution ----
                  h4("Schedule"),
                  tableOutput("view"))
      )
      ),
      tabPanel("About R/Medicine",
               h2("About R/Medicine"),
               fluidPage(HTML("<p>For all details about the R/Medicine conference, see <a href='https://events.linuxfoundation.org/r-medicine/program/schedule/'>https://events.linuxfoundation.org/r-medicine/program/schedule/</a></p>")
               ))
    )
    

    # # Define UI for dataset viewer app ----
    # ui <- fluidPage(
    
    #     # App title ----
    #     titlePanel("Change timezone for R/Medicine"),
    #
    #     # Sidebar layout with input and output definitions ----
    #     sidebarLayout(
    #
    #         # Sidebar panel for inputs ----
    #         sidebarPanel(
    #
    #             # Input: Select a dataset ----
    #             selectInput("dataset", "Choose a day:",
    #                         choices = c("2020-08-27 (Friday)", "2020-08-28 (Saturday)")),
    #
    #
    #             # Input: Specify timezone ----
    #             selectizeInput(
    #               'tz', label = NULL, choices = OlsonNames(),
    #               options = list(create = TRUE)
    #             ),
    #
    #             # Include clarifying text ----
    #             helpText("Select a day (in EDT) and your preferred time zone"),
    #
    #             # Button
    #             downloadButton("downloadData", "Download"),
    #
    #             # Input: actionButton() to defer the rendering of output ----
    #             # until the user explicitly clicks the button (rather than
    #             # doing it immediately when inputs change). This is useful if
    #             # the computations required to render output are inordinately
    #             # time-consuming.
    #             actionButton("update", "Update View")
    #
    #         ),
    #
    #         # Main panel for displaying outputs ----
    #         mainPanel(
    #
    #             # Output: Header + table of distribution ----
    #             h4("Schedule"),
    #             tableOutput("view")
    #         )
    #
    #     )
    # )
    
    # Define server logic to summarize and view selected dataset ----
    server <- function(input, output) {
      # Return the requested dataset ----
      # Note that we use eventReactive() here, which depends on
      # input$update (the action button), so that the output is only
      # updated when the user clicks the button
      datasetInput <- eventReactive(input$update, {
        switch(
          input$dataset,
          "2020-08-27 (Friday)" = update_schedule(
            link = "r_medicine_schedule.xlsx",
            sheet = "2020-08-27",
            tz_goal = input$tz
          ),
          "2020-08-28 (Saturday)" = update_schedule(
            link = "r_medicine_schedule.xlsx",
            sheet = "2020-08-28",
            tz_goal = input$tz
          )
        )
        
        
        
      }, ignoreNULL = FALSE)
      
      
      # Show the first "n" observations ----
      # The use of isolate() is necessary because we don't want the table
      # to update whenever input$obs changes (only when the user clicks
      # the action button)
      output$view <- renderTable({
        datasetInput()
      },   striped = TRUE, width = "auto")
      
      # Downloadable csv of selected dataset ----
      output$downloadData <- downloadHandler(
        filename = function() {
          paste(input$dataset, ".csv", sep = "")
        },
        content = function(file) {
          write.csv(datasetInput(), file, row.names = FALSE)
        }
      )
      
      
    }
    
    # Create Shiny app ----
    shinyApp(ui, server)
    