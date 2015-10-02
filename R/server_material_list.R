#' server_material_list
#'
#' Constructs table
#'
#' @param input shinyserver input
#' @param output shinyserver output
#' @param session shinyserver session
#' @param dom target dom element name
#' @param values reactive values
#' @author Reinhard Simon
#' @export
server_material_list <- function(input, output, session, dom="hot_materials", values){
  setHot_materials = function(x) values[["hot_materials"]] = x
  setFile_materials = function(x) values[["file_materials"]] = x
  setMat_list_sel = function(x) values[["mat_list_sel"]]

  volumes <- shinyFiles::getVolumes()

  rv_fp_ml <- shiny::reactive({
  roots = c(wd = ".")
  fp <- shinyFiles::parseFilePaths( roots, input$mlist_files)$datapath
  fp <- stringr::str_replace(fp, "NA", "")
  fp
})

output$mlist_fc <- renderText({
  rv_fp_ml()
})


shinyFileChoose(input, 'mlist_files', session = session,
                roots = volumes , filetypes = c('', 'xlsx')
)



output$mlist_crop <- renderUI({
  if(is.null(values[["hot_crops"]])){
    values[["hot_crops"]] <- fbcrops::get_crop_table()
  }
  crops <- values[["hot_crops"]]$crop_name
  selectInput("mlist_crop", NULL, choices = crops, width = '50%')
})

output$mlist_year <- renderUI({
  chc <- fbcrops::list_years_for_crop(input$mlist_crop)
  selectInput("mlist_year", NULL, choices = chc, width = '50%')
})



output$mlist_name <- renderUI({
  chc <- list_material_lists(input$mlist_crop, input$mlist_year, TRUE)
  selectInput("mlist_name", NULL, choices = chc, selected = 1)
})

output$mlist_year_new <- renderUI({
  ayear = input$mlist_year
  selectInput("mlist_year_new", "Target year:", 2000:2050, width = '50%', selected = ayear)
})

output$selectMList <- renderUI({
  lbl <-paste0("Save: ",input$mlist_crop,"/",
               input$mlist_year,"_",input$mlist_name)

  actionButton("saveMListButton", lbl)

})

observeEvent(input$doListButton, {
  if(input$mlist_choose_list_source == "List"){
    fn = input$mlist_name #file.path(fbglobal::fname_material_lists(), input$mlist_lists)
  } else {
    fn = rv_fp_ml()
  }

  res <- import_list_from_prior(crop = input$mlist_crop, year = input$mlist_year, fname = fn,
                         year_new = input$mlist_year_new,
                         mlist_name = input$mlist_name_new,
                         notes = input$mlist_notes_new
                         )
  if(res) {
    msg = paste("List", input$mlist_name_new, "created!")
    output$new_list_success = renderText({
      msg
    })
    output$messageMenu <- shinydashboard::renderMenu({
      # Code to generate each of the messageItems here, in a list. This assumes
      # that messageData is a data frame with two columns, 'from' and 'message'.
      # msgs <- apply(messageData, 1, function(row) {
      #   messageItem(from = row[["from"]], message = row[["message"]])
      # })

      # This is equivalent to calling:
      #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
      #dropdownMenu(type = "messages", .list = msgs)
      msg <- shinydashboard::messageItem("HIDAP4RTB", msg)
      shinydashboard::dropdownMenu(msg, type = "messages")
    })
  }
})

observeEvent(input$saveMListButton, {
  table_materials = hot_to_r(input$hot_materials)
  if(!is.null(table_materials)){
    post_material_table(table_materials,
                        input$mlist_crop, input$mlist_year, input$mlist_name)
    # shinyBS::createAlert(session, "saveMaterialListAlert", "saveMLAlert", title = "Note",
    #             content = "List of plant materials saved.", append = FALSE)
  }
})


output$hot_materials = renderRHandsontable({
  withProgress(message = 'Loading table', {
  list_name <- input$mlist_name
  DF_materials <- get_material_table(input$mlist_crop,
                                     input$mlist_year,
                                     list_name)

  if(!is.null(DF_materials)){
    setHot_materials(DF_materials)
    rhandsontable(DF_materials,   stretchH = "all") %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  } else {
    NULL
  }
})
})

output$downloadMaterialListData <- downloadHandler(
  filename = function() {
    paste('germplasm_list-', input$mlist_crop,"_", input$mlist_year,"_",
          input$mlist_name, "_",
          Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    write.csv( values[["hot_materials"]], con, row.names = FALSE)
  }
)

}
