

library(shiny)
library(shinydashboard)
library(shinyBS)

dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Controls",
        menuSubItem("Chart",
                    sliderInput("slider", "Number of observations:", 1, 100, 50),
                    actionButton("tabBut", "View Table")

                    )

               )
    )

  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),

      # box(
      #   title = "Controls",
      #   sliderInput("slider", "Number of observations:", 1, 100, 50)
      # ),
      bsModal("modalExample", "Data Table", "tabBut",
                            dataTableOutput("distTable"))
      )
    )
  )


# fluidPage(
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput("bins",
#                   "Number of bins:",
#                   min = 1,
#                   max = 50,
#                   value = 30),
#       actionButton("tabBut", "View Table")
#     ),
#
#     mainPanel(
#       plotOutput("distPlot"),
#       bsModal("modalExample", "Data Table", "tabBut", size = "large",
#               dataTableOutput("distTable"))
#     )
#   )
# )

