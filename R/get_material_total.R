#' get fieldboook list
#'
#' get fieldbook list
#'
#' @param crop a crop
#' @param name_only only name
#' @param full_path full path
#' @export
get_fieldbook_list <- function(crop="all", name_only = FALSE, full_path = FALSE){
  bp = fbglobal::get_base_dir()
  if(is.null(crop)) return(NULL)
  if(crop != "all") {
    fs = .Platform$file.sep
    crop = paste0(fs, crop, fs)
    pattern = paste0(crop, "fieldbook")
  } else {
    pattern = "fieldbook"
  }
  lf = list.files(bp, recursive = TRUE, full.names = TRUE)
  lf = lf[stringr::str_detect(lf, pattern)]
  if (!full_path) lf = basename(lf)
  if(name_only){
    lf = stringr::str_replace(lf, ".rda", "")
  }
  lf
}

#' exists fieldbook
#'
#' exists fieldbook
#'
#' @param crop a crop
#' @export
exists_fieldbook <- function(crop) {
  lf <- fbmaterials::get_fieldbook_list(crop = crop)
  length(lf) > 0
}


#' get fieldbook total
#'
#' get fieldbook total.
#'
#' @param crop a crop name or 'all' (default)
#' @author Reinhard Simon
#' @export
get_fieldbook_total <- function(crop = "all"){
  lf <- get_fieldbook_list(crop)
  length(lf)
}

get_materials <- function(file_path){
  fb <- readRDS(file_path)
  meta <- attr(fb, "meta")
  meta$materials
}

#' get_material_total
#'
#' get materials total.
#'
#' @param crop a crop name or 'all' (default)
#' @author Reinhard Simon
#' @export
get_material_total <- function(crop = "all"){
  lf <- get_fieldbook_list(crop, full_path = TRUE)
  n <- length(lf)
  ml <- character()
  if(n > 0) {
    for(i in 1:n){
      ml <- c(ml, get_materials(lf[i]))
    }
  }
  length(unique(ml))
}


#' get fieldbooks
#'
#' @param loc location short name
#' @export
get_fieldbook_list_by_loc <- function(loc){
  # TODO refactor
  lf <- get_fieldbook_list(name_only = TRUE, full_path = FALSE)
  lf = lf[stringr::str_detect(toupper(lf), toupper(loc))]
  # lf <- basename(lf)
  # unlist(lapply(lf, stringr::str_replace, ".rda", ""))
  lf
}

#' get fieldbook data
#'
#' Getting data.
#'
#' @param aname of a fieldbook (unique)
#' @export
get_fieldbook_data <- function(aname) {
  lf <- get_fieldbook_list(full_path = TRUE, name_only = FALSE)
  lf <- lf[stringr::str_detect(toupper(lf), toupper(aname))]
  out = NULL
  if(length(lf) == 0) return(NULL)
  if (file.exists(lf)) {
    out = readRDS(lf)
  }
  out
}


#' get unique genotypes by location
#'
#' (up to hundred)
#'
#' @param loc location short name
#' @export
get_genotype_list_by_loc <- function(loc){
  lf <- get_fieldbook_list(full_path = TRUE, name_only = FALSE)
  lf <- lf[stringr::str_detect(toupper(lf), toupper(loc))]
  n = length(lf)
  lst = "none"
  if(n > 0){
    lst = character()
    for(i in 1:n){
      fb <- readRDS(lf[i])
      meta <- attr(fb, "meta")
      lst = c(lst, meta$materials)
    }
    lst = sort(unique(lst))
  }
  lst
}





get_last_factor_index <- function(trial_name) {
  if(trial_name == "" | is.na(trial_name)) return(-1)
  bp <- fbglobal::get_base_dir()

  #TODO now lists also material lists after fieldbooks; so fieldbook is first
  # TODO explizit filtering
  ls <- list.files(bp, pattern = trial_name, recursive = TRUE, full.names = TRUE)[1]
  fb <- readRDS(ls)
  nm <- names(fb)
  fl <- which(nm == "CODE" | nm == "INSTN" | nm == "GENOTYPE" | nm == "PED1")
  list(nm = nm, fl = fl)
}

#' get trial factors
#'
#' get trial factors.
#'
#' @param trial_name a trial name
#' @export
get_trial_factors <- function(trial_name){
  fl <- get_last_factor_index(trial_name)
  if(!is.list(fl)) return("")
  fl$nm[1:fl$fl[length(fl$fl)]]
}

#' get trial variables
#'
#' get trial variables.
#'
#' @param trial_name a trial name
#' @export
get_trial_variables <- function(trial_name){
  fl <- get_last_factor_index(trial_name)
  if(!is.list(fl)) return("")
  n = fl$fl[length(fl$fl)] + 1
  #fl$fl = n + 1
  fl$nm[n:length(fl$nm)]
}


