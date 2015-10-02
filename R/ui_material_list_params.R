#' ui_material_list_params
#'
#' An interface to parameters
#'
#' @param name character
#' @author Reinhard Simon
#' @export
ui_material_list_params <- function(name = "resource_material_list"){
  shiny::conditionalPanel(
    paste0("input.menu == '",name,"'"),
    shiny::uiOutput("mlist_crop"),
    shiny::uiOutput("mlist_year"),
    shiny::uiOutput("mlist_name"),

    shiny::HTML("<center>"),
    shinyBS::bsAlert("saveMaterialListAlert"),
    shiny::actionButton("saveMListButton", "Save!"),
    shiny::actionButton("butNewMaterials", "New!"),
    shiny::downloadButton("downloadMaterialListData", "Export!"),
    shiny::HTML("</center>")
  )
}
