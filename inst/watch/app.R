library(shiny)
library(shinydashboard)
library(shinyFiles)
library(rhandsontable)
library(shinyBS)

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Materials"),
  dashboardSidebar(width = 300, disable = TRUE),

    dashboardBody(
      fluidRow(
        tabBox(
          title = "First tabBox",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1", height = "250px",
          tabPanel("Tab1", "First tab content"),
          tabPanel("Tab2", "Tab content 2")
        ),
        tabBox(
          side = "right", height = "250px",
          selected = "Tab3",
          tabPanel("Tab1", "Tab content 1"),
          tabPanel("Tab2", "Tab content 2"),
          tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
        )
      ),
      fluidRow(
        tabBox(
          # Title can include an icon
          title = tagList(shiny::icon("gear"), "tabBox status"),
          tabPanel("Tab1",
                   "Currently selected tab from first box:",
                   verbatimTextOutput("tabset1Selected")
          ),
          tabPanel("Tab2", "Tab content 2")
        )
      )
  )
)

server <- function(input, output, session){
  adir  = "D:/test/"
  #
  # values <- reactiveValues()
  # setFiles = function(x) values[["files"]] = x
  #
  update_file_display <- function(added, deleted, modified){
    if(length(added) == 0) added = "No files detected!"
    #fl <- paste(added, collapse = ", ")
    fl <- list.files(adir)
    print(fl)
    setFiles(fl)
    #print(values[["files"]])
    TRUE
  }

  #x = testthat::watch(adir, update_file_display, pattern = "\\.txt")

  output$tabset1Selected <- renderText({
    #print(values[["files"]])
    #values[["files"]]
    #print("Hello")
    input$tabset1
  })

#  testthat::watch(adir, update_file_display, pattern = "\\.txt")


  # output$filelist <- renderText({
  #   #cat("Hello")
  #   input$tabset1
  # })

}

shinyApp(ui, server)

