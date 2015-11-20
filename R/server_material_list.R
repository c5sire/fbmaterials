#'  server_material_list
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
  setHot_materials = function(x) values[[dom]] = x
  roots = shinyFiles::getVolumes("Page File (F:)")
  #print(roots)
  shinyFiles::shinyFileChoose(input, 'mlist_files', session=session,
          #roots=roots,
          roots = roots,
          filetypes=c('', '.xlsx'))

  # rv_fp_ml <- shiny::reactive({
  #   fp <- shinyFiles::parseFilePaths( roots, input$mlist_files)$datapath
  #   #fp <- stringr::str_replace(fp, "NA", "")
  #   fp
  # })

  rv_fp_ml <- function(){"D:"}

  get_ml_list_crop <- reactive({
    fbl <- values[["ml_list_crop"]]
    #print(fbl)
    if(is.null(fbl)) {
      fbl <- fbmaterials::list_years_for_crop(input$mlist_crop)
    }
    #print(fbl)
    fbl
  })

  get_ml_list_crop_year <- reactive({
    fbl <- values[["ml_list_crop_year"]]
    #print(fbl)
    if(is.null(fbl)) {
      fbl <- fbmaterials::list_material_lists(input$mlist_crop,
                                              input$mlist_year,
                                              TRUE
                                                )
    }
    #print(fbl)
    fbl
  })


output$mlist_crop <- shiny::renderUI({
  if(is.null(values[["hot_crops"]])){
    values[["hot_crops"]] <- fbcrops::get_crop_table()
  }
  crops <- values[["hot_crops"]]$crop_name
  shiny::selectInput("mlist_crop", NULL, choices = crops, width = '50%')
})

output$mlist_year <- shiny::renderUI({
  #chc <- list_years_for_crop(input$mlist_crop)
  chc <- get_ml_list_crop()
  if(!is.null(chc)){
    shiny::selectInput("mlist_year", NULL, choices = chc, width = '50%')
  }

})



output$mlist_name <- shiny::renderUI({
  #chc <- list_material_lists(input$mlist_crop, input$mlist_year, TRUE)
  chc <- get_ml_list_crop_year()
  if (chc[1] != ""){
    shiny::selectInput("mlist_name", NULL, choices = chc, selected = 1)
  }
})

output$mlist_butSave <- shiny::renderUI({
  chc <- list_material_lists(input$mlist_crop, input$mlist_year, TRUE)
  if (chc[1] != ""){

    shiny::actionButton("saveMListButton", "Save", inline = TRUE)

  }
})

output$mlist_butExport <- shiny::renderUI({
  chc <- list_material_lists(input$mlist_crop, input$mlist_year, TRUE)
  if (chc[1] != ""){
    shiny::downloadButton("downloadMaterialListData", "Export")
  }
})


output$mlist_year_new <- shiny::renderUI({
  ayear = input$mlist_year
  shiny::selectInput("mlist_year_new", "Target year:", 2000:2050, width = '50%', selected = ayear)
})


shiny::observeEvent(input$doListButton, ({
  #if(is.null(input$doListButton)) return(NULL)
  #print(input$mlist_name)
  if (input$mlist_choose_list_source == "List") {
    fn = input$mlist_name #
    #fn <- file.path(fbglobal::fname_material_lists(), input$mlist_lists)
  } else {
    fn = rv_fp_ml()
  }
  #print(fn)

  import_list_from_prior(crop = input$mlist_crop, year = input$mlist_year,
                                fname = fn,
                         year_new = input$mlist_year_new,
                         mlist_name = input$mlist_name_new,
                         notes = input$mlist_notes_new
                         )
  values[["ml_list_crop_year"]] <- NULL
  output$new_list_success = renderText(paste(input$mlist_name_new, "created!"))
})#, suspended = TRUE
)


shiny::observeEvent(input$saveMListButton, ({
  if (!is.null(input[[dom]])) {
    table_materials = rhandsontable::hot_to_r(input[[dom]])
    post_material_table(table_materials,
                        input$mlist_crop, input$mlist_year, input$mlist_name)
    # The following could be better
    values[["ml_list_crop"]] <- NULL
    values[["ml_list_crop_year"]] <- NULL
  }
})
)


output$hot_materials = rhandsontable::renderRHandsontable({
  shiny::withProgress(message = 'Loading table', {
  list_name <- input$mlist_name
  DF_materials <- get_material_table(input$mlist_crop,
                                     input$mlist_year,
                                     list_name)

  if(!is.null(DF_materials)){
    setHot_materials(DF_materials)
    rh <- rhandsontable::rhandsontable(DF_materials,   stretchH = "all")
    rhandsontable::hot_table(rh, highlightCol = TRUE, highlightRow = TRUE)
  } else {
    NULL
  }
})
})

output$downloadMaterialListData <- shiny::downloadHandler(
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
