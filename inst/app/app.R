library(shiny)
library(shinydashboard)
library(rhandsontable)
library(fbglobal)
library(fbcrops)
library(fbmaterials)

ui <- dashboardPage(skin = "yellow",

                dashboardHeader(title = "Materials"
                ),

                dashboardSidebar(width = 300,
                 sidebarMenu(id = "menu",
                   menuItem("Resources",
                            menuSubItem("Plant materials", icon = shiny::icon("star"),
                                        tabName = "resource_material_list")
                            ,
                            fbmaterials::ui_material_list_params()
                 )
                 )

                ),
                dashboardBody(
                  tabItems(
                    fbmaterials::ui_material_list()
                  )
                )
  )

server <- function(input, output, session, values){
  values = shiny::reactiveValues()
  fbmaterials::server_material_list(input, output, session, values = values)
}

shinyApp(ui, server)

