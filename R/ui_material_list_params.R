#' UI material list paramters
#'
#' An interface to parameters
#'
#' @param name character
#' @author Reinhard Simon
#' @export
ui_material_list_params <- function(name = "resource_material_list"){
  shiny::conditionalPanel(
    paste0("input.menu == '",name,"'"),
    shiny::uiOutput("mlist_crop", inline = TRUE),
    shiny::uiOutput("mlist_year", inline = TRUE),
    shiny::uiOutput("mlist_name"),

    shiny::HTML("<center>"),
    shinyBS::bsAlert("saveMaterialListAlert"),
    shiny::uiOutput("mlist_butSave", inline = TRUE),
    shiny::actionButton("butNewMaterials", "New", inline = TRUE),
    shiny::uiOutput("mlist_butExport", inline = TRUE),
    shiny::HTML("</center>")
  )
}
