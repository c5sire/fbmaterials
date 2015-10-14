library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinyBS)

ui <- dashboardPage(skin = "yellow",

                dashboardHeader(title = "Materials"
                ),

                dashboardSidebar(width = 300,
                 sidebarMenu(id = "menu",
                   menuItem("Resources",
                            menuSubItem("Crops", icon = shiny::icon("leaf"),
                                        tabName = "resource_crop")
                            ,
                            menuSubItem("Plant materials", icon = shiny::icon("star"),
                                        tabName = "resource_material_list")
                            ,
                            fbmaterials::ui_material_list_params()
                 )
                 )

                ),
                dashboardBody(
                  tabItems(
                    fbcrops::ui_crop(),
                    fbmaterials::ui_material_list()
                  )
                )
  )

server <- function(input, output, session){
  values = shiny::reactiveValues()
  fbcrops::server_crop(input, output, session, values = values)
  fbmaterials::server_material_list(input, output, session, values = values)
}

shinyApp(ui, server)

