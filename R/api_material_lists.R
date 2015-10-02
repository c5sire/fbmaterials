#' new_materials_table
#'
#' creates an empty table with dummy values
#'
#' @return data.frame
#' @author Reinhard Simon
#' @export
new_materials_table <- function(){
  #numeration <- 1:2
  is_control   <- c(TRUE, FALSE) # must be two upper case letters
  scale_audpc <- c(6, NA)
  institutional_number <- c("CIP720064", "CIP377744.1")
  material_name <- c("Yungay", "Kori-INIA")
  material_code <- c("KB 507", "P-8")
  family_number <- c("", "")
  pedigree <- c("", "")
  female_number <- c("", "")
  female_code  <- c("", "")
  male_number  <- c("", "")
  male_code  <- c("", "")
  seed_source  <- c("", "")
  simultanious_trials  <- c("", "")
  previous_trials  <- c("", "")

  out <- as.data.frame(cbind(institutional_number, material_name, material_code,
                      is_control, scale_audpc,
                      family_number, pedigree, female_number, female_code,
                      male_number, male_code, seed_source, simultanious_trials,
                      previous_trials),
                stringsAsFactors = FALSE)
  attr(out, "crop" ) <- "crop"
  attr(out, "year" ) <- 2015
  attr(out, "name" ) <- "name"
  attr(out, "notes" ) <- "description"
  out
}

#' get_materials_table
#'
#' always returns a table
#'
#' @param crop character
#' @param year year integer
#' @param mlist_name character
#' @return data.frame
#' @author Reinhard Simon
#' @export
get_materials_table <- function(crop, year, mlist_name){
  fns <- fbglobal::fname_material_lists()

  fns <- file.path(fns, crop,
                               paste0(year,"_", mlist_name))

  if(!file.exists(fns)) {
    base_dir <-  dirname(fns)
    if(!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)
    table_materials <- new_materials_table()
    save(table_materials, file = fns, compress = "bzip2")
  }
  load(fns)
  if(is.null(attr(table_materials, "crop")) || attr(table_materials, "crop") == "crop"){
    attr(table_materials, "crop" ) <- crop
    attr(table_materials, "year" ) <- year
    attr(table_materials, "name" ) <- mlist_name
  }
  table_materials
}

#' post_material_table
#'
#' store program table
#'
#' @param table_materials a data frame
#' @param crop character
#' @param year year integer
#' @param mlist_name character
#' @param notes character
#' @author Reinhard Simon
#' @export
post_material_table <- function(table_materials, crop, year, mlist_name, notes){
  fname <- file.path(fbglobal::fname_material_lists(), crop, paste0(year, "_",  mlist_name))
  attr(table_materials, "crop" ) <- crop
  attr(table_materials, "year" ) <- year
  attr(table_materials, "name" ) <- mlist_name
  attr(table_materials, "notes" ) <- notes
  save(table_materials, file = fname , compress = "bzip2")
}

#' list_material_lists
#'
#' lists material-lists
#'
#' @return list
#' @author Reinhard Simon
#' @export
list_material_lists <- function(){
  #list.files(fbglobal::fname_material_lists(), recursive = TRUE )
  fns <- fbglobal::fname_material_lists()
  paste0(fns, .Platform$file.sep, list.files(fns, recursive = TRUE))
}

#' import_list_from_prior
#'
#' Imports list from a prior one in the local database if no filename (fname)
#' is given. Otherwise reads the file (in .xlsx format).
#'
#' @param crop character
#' @param year integer
#' @param fname an Excel file name
#' @param mlist_name list name
#' @return boolean TRUE if successful
#' @author Reinhard Simon
#' @export
import_list_from_prior <- function(crop, year, fname=NULL, mlist_name=NULL){
  stopifnot(!is.null(fname))
  stopifnot(!is.null(mlist_name))

  dp <- file.path(fbglobal::fname_material_lists(), crop)
  # print(paste("fname",fname))
  # print(paste("dp", dp))

  if(!dir.exists(dp)) dir.create(dp)
  dp <- file.path(dp, paste0(year, "_",mlist_name))
  # print(paste("dp", dp))
  out = FALSE

  try({
  if(stringr::str_detect(fname, ".xlsx")){

    table_materials <- readxl::read_excel(fname, "materials")
    save(table_materials, file = dp)
    # print("excel")
    # print(dp)
    out = TRUE
  }

  fname <- file.path(fbglobal::fname_material_lists(), crop, fname)
#  print(paste("fname",fname))
  if(file.exists(fname) && !file.exists(dp)) {
    file.copy(fname, dp)
    out = TRUE
    # print("list")
    # print(dp)
  }
  })
  out
}

# get_selected_tree_node <- function(tree) {
#   unlist(get_selected(tree))
# }


get_material_n <- function(fp){
  table_materials = NULL
  load(fp)
  #nrow(table_materials)
  table_materials$institutional_number
}

# list_material_lists <- function(){
#   paste0(fname_materials, .Platform$file.sep, list.files(fname_materials, rec=T))
# }


#' get_material_total
#'
#' Counts total number of unique plant materials
#'
#' @author Reinhard Simon
#' @export
get_material_total <- function(){
  out = lapply(list_material_lists(), get_material_n)
  length(unique(unlist(out)))
}
