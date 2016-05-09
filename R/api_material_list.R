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

get_full_table_name <- function(crop, year, mlist_name){
  fns <- fbglobal::fname_material_lists(crop)
  if (is.null(fns)) return(NULL)
  if (length(fns) < 1) return(NULL)
  fns <- file.path(fns, paste0(year,"_", mlist_name))
  fns
}

#' get_material_table
#'
#' always returns a table
#'
#' @param crop character
#' @param year year integer
#' @param mlist_name character
#' @return data.frame
#' @author Reinhard Simon
#' @export
get_material_table <- function(crop, year, mlist_name){
  fns <- get_full_table_name(crop, year, mlist_name)
  #print(fns)
  if (is.null(fns)) return(NULL)
  if (length(fns) < 1) return(NULL)
  if (!file.exists(fns[1])) {
    return(NULL)
  }
  load(fns)
  # must add extension to files for readRDS to work
  #table_materials <- readRDS(fns)
  if (is.null(attr(table_materials, "crop")) || attr(table_materials, "crop") == "crop") {
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
post_material_table <- function(table_materials, crop, year, mlist_name, notes = NULL){
  fname <- file.path(fbglobal::fname_material_lists(crop),  paste0(year, "_",  mlist_name))
  fdir <- dirname(fname)
  if(!dir.exists(fdir)){
    dir.create(fdir, recursive = TRUE)
  }
  attr(table_materials, "crop" ) <- crop
  attr(table_materials, "year" ) <- year
  attr(table_materials, "name" ) <- mlist_name
  if(!is.null(notes)){
    attr(table_materials, "notes" ) <- notes
  }
  save(table_materials, file = fname , compress = "bzip2")
}

#' list_material_lists
#'
#' lists material-lists
#'
#' @param crop character
#' @param year integer
#' @param short short list name
#' @return list
#' @author Reinhard Simon
#' @export
list_material_lists <- function(crop=NULL, year=NULL, short=FALSE){
  #list.files(fbglobal::fname_material_lists(), recursive = TRUE )
  if(is.null(crop)){
    fns <- fbglobal::fname_material_lists()
  } else {
    fns <- file.path(fbglobal::fname_material_lists(crop))
  }


  fns <- paste0(fns, .Platform$file.sep, list.files(fns, recursive = TRUE))

  yr = paste0(year, "_")
  if(!is.null(year)){
    fy = stringr::str_detect(fns, yr)
    fns = fns[fy]
  }
  if(short){
    fns = basename(fns)
    fns = stringr::str_replace(fns, yr, "_")
    if(is.null(fns)) return("")
    #print(fns)
    #print(crop)
    if(is.null(crop)) return("")
    if(is.na(fns[1])) return("")
    if(fns[1] == crop) return("")
    #if(fns[1] == "materiallists") return(NULL)
  }
   chk = basename(fns)
   if (stringr::str_detect(chk[1],"material")) return(NULL)

   fns
}

#' list_years_for_crop
#'
#' list years
#'
#' @param crop character
#' @return vector of integer
#' @author Reinhard Simon
#' @export
list_years_for_crop <- function(crop){
  out = 2015 # replace with current year
  x = list_material_lists(crop = crop, short = TRUE)
  if(is.null(x)) return(NULL)
  if(x[1] == "") return(out)
  if (is.null(crop)) return(out)
  if (length(crop) <  1) return(out)
  if (x[1] == crop) return(out)
  x = stringr::str_sub(x, 1, 4)
  if(!is.null(x)){
    x = sort(unique(as.integer(x)))
  } else {
    x = 2015
  }
  x
}

#' import_list_from_prior
#'
#' Imports list from a prior one in the local database if no filename (fname)
#' is given. Otherwise reads the file (in .xlsx format).
#'
#' @param crop character; crop
#' @param year integer; source year
#' @param fname an Excel file name
#' @param year_new integer; new year
#' @param mlist_name list; new name
#' @param notes character
#' @return boolean TRUE if successful
#' @author Reinhard Simon
#' @export
import_list_from_prior <- function( crop, year, fname=NULL, # source
                                    year_new, mlist_name=NULL, notes = NULL # target; crop same
                                    ){
  stopifnot(!is.null(fname))
  stopifnot(is.null(mlist_name))
  out = FALSE

  table_materials = NULL
  try({
  if(stringr::str_detect(fname, "xlsx")){
    table_materials <- readxl::read_excel(fname, "materials")
    print(table_materials)

    #fname = basename(fname)
    out = TRUE
  } else {
    fname = get_full_table_name(crop, year, fname)
  }
  if(file.exists(fname)) {
    load(fname)
    out = TRUE
  }
  post_material_table(table_materials, crop, year, mlist_name, notes)
  })
  out
}


get_material_n <- function(fp){
  table_materials = NULL
  load(fp)
  #nrow(table_materials)
  table_materials$institutional_number
}

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
