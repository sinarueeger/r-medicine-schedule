source("utils.R")
library(shiny)
library(shinydashboard)
library(shinythemes)
#dashboardPage(skin = "green")

ui <-
  navbarPage(
    title = "Change timezone for R/Medicine conference",
    theme = shinytheme("sandstone"),
    tabPanel(
      "Schedule",
      # Sidebar layout with input and output definitions ----
      sidebarLayout(
        sidebarPanel(
          # Input: Select a dataset ----
          selectInput(
            "dataset",
            "Choose a day:",
            selected = "2020-08-27 (Friday)",
            choices = c("2020-08-26 (Thursday)", "2020-08-27 (Friday)", "2020-08-28 (Saturday)")
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
          box(
            title = 'Schedule', width = NULL, status = 'primary',
            DT::dataTableOutput('view')  
          )

      )
      ),
      tabPanel("About",
               h2("About"),
               fluidPage(HTML("<p>For all details about the R/Medicine conference, see <a href='https://events.linuxfoundation.org/r-medicine/program/schedule/'>https://events.linuxfoundation.org/r-medicine/program/schedule/</a>.</p>")),
              fluidPage(HTML("<p>Propose changes to this app: <a href='https://github.com/sinarueeger/r-medicine-schedule'>https://github.com/sinarueeger/r-medicine-schedule</a>.</p>")
                                   
                         ))
    )
    

    # Define server logic to summarize and view selected dataset ----
    server <- function(input, output) {
      # Return the requested dataset ----
      # Note that we use eventReactive() here, which depends on
      # input$update (the action button), so that the output is only
      # updated when the user clicks the button
      datasetInput <- eventReactive(input$update, {
        switch(
          input$dataset,
          "2020-08-26 (Thursday)" = update_schedule(
            link = "r_medicine_schedule.xlsx",
            sheet = "2020-08-26",
            tz_goal = input$tz
          ),
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
      #output$view <- renderTable({
      #  datasetInput()
      #},   striped = TRUE, width = "auto")
      output$view <- DT::renderDataTable(
        datasetInput(), options = list(
          autoWidth = TRUE,
          columnDefs = list(list(width = '100px', targets = c(0, 3))), 
                       #      list(width = '300px', targets = c(1)), 
                      #       list(width = '200px', targets = c(2))),
          scrollX = TRUE,
          pageLength = 30
        ), rownames= FALSE
)
   
     
      
      

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
    