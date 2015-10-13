#' ui_material_list
#'
#' returns a re-usable user interface element
#'
#' @author Reinhard Simon
#' @param type of ui Element; default is a tab in a shiny dashboard
#' @param title display title
#' @param name a reference name
#' @param output name of output element
#' @export
ui_material_list <- function(type = "tab", title = "Material list configuration",
                       name = "resource_material_list",
                       output = "hot_materials"){
  shinydashboard::tabItem(tabName = name,
          shiny::fluidRow(
            shinydashboard::box( height = 600, width = 1200,
                 title = title,
                 rhandsontable::rHandsontableOutput(output, height = 600, width = 1200)
            )
            ),
          shinyBS::bsModal("bsModalListParam", "Define a new list of plant materials",
                           #paste0("input.menu == '",name,"'"),
                           "butNewMaterials", size = "small",
                           #shiny::HTML("<i>Hi</i>")

                           shiny::uiOutput("mlist_year_new", inline = TRUE),
                           shiny::textInput("mlist_name_new", NULL, "A unique list name"),
                           shiny::textInput("mlist_notes_new", NULL, "Description"),
                           shiny::radioButtons("mlist_choose_list_source", NULL,
                                               choices = c("List", "Excel") ),
                           shiny::conditionalPanel(
                             "input.mlist_choose_list_source == 'Excel'",
                             shinyFiles::shinyFilesButton('mlist_files', 'File selection!', 'Please select a file', FALSE )
                           ),
                           shiny::textOutput("new_list_success"),
                           shiny::actionButton("doListButton", "Create new list!")
          )


      )
}
