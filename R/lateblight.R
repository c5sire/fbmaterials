

audpc <- function(fieldbook, type = "absolute", use_stairs = FALSE,
  variable = "LB = Late Blight", variable_only = TRUE) {
  # get colummns with prefix LB
  the_vars <- stringr::str_split(variable, " = ")[[1]]
  prefix = the_vars[1]
  var_name = the_vars[2]
  with_prefix = stringr::str_detect(names(fieldbook), prefix)
  fb = fieldbook[, with_prefix]
  fb <- as.data.frame(fb)
  # make sure those columns are integer
  for (i in 1:ncol(fb)) {
    fb[, i] = as.integer(fb[, i])
  }
  # row.names(fb) = fieldbook$INSTN %>% as.character fb <-
  # cbind(fieldbook$INSTN, fb) get dates from metadata
  cm = attr(fieldbook, "CropManagement")
  if (is.null(cm))
    stop("Not a fieldbook with meta data for crop management.")
  dates = cm[stringr::str_detect(cm$`Intervention type`, var_name),
    "Date"]
  # check if same number as observations
  if (nrow(dates) != ncol(fb))
    stop("Number of dates different from number of observations.")
  fb$INSTN <- fieldbook$INSTN %>% as.character

  # dates = sapply(dates, lubridate::ymd) %>% unlist
  days = integer(nrow(dates))
  # convert dates to differential days
  for (i in 2:nrow(dates)) {
    date_now <- lubridate::ymd(dates[i, 1])
    date_before <- lubridate::ymd(dates[i - 1, 1])
    days[i] = as.integer(date_now - date_before) + days[i -
      1]
  }
  # calculate according to type
  if (!use_stairs) {
    xAUDPC <- switch(type, absolute = agricolae::audpc(fb[,
      -ncol(fb)], days, "absolute"), relative = agricolae::audpc(fb[,
      -ncol(fb)], days, "relative"), standardized = agricolae::audpc(fb[,
      -ncol(fb)], days, "absolute"))
  } else {
    xAUDPC <- switch(type, absolute = agricolae::audps(fb[,
      -ncol(fb)], days, "absolute"), relative = agricolae::audps(fb[,
      -ncol(fb)], days, "relative"), standardized = agricolae::audps(fb[,
      -ncol(fb)], days, "absolute"))

  }
  if (type == "standardized") {
    ms <- attr(fieldbook, "MaterialList")
    scol <- stringr::str_detect(names(ms), "AUDPC")
    srow <- which(!is.na(ms[scol]))
    sval <- ms[srow, scol] %>% as.integer
    sgen <- ms[srow, "Institutional number"] %>% as.character
    audpcc <- xAUDPC[which(fb$INSTN == sgen)] %>% mean(na.omit = TRUE)
    xAUDPC <- sval * xAUDPC/audpcc
    xAUDPC <- round(xAUDPC, 1)
  }
  if (type == "relative") {
    xAUDPC = round(xAUDPC, 4)
  }

  if (variable_only)
    return(xAUDPC)

  lbl <- switch(type, absolute = "AUDPC", relative = "rAUDPC",
    standardized = "SAUDPC")
  if (use_stairs) {
    lbl = stringr::str_replace(lbl, "C", "S")
  }

  if (lbl %in% names(fieldbook)) {
    fieldbook[, lbl] <- xAUDPC
  } else {
    # rescue meta data before changing table structure; otherwise
    # they are lost
    meta <- attr(fieldbook, "meta")
    mini <- attr(fieldbook, "Minimal")
    inst <- attr(fieldbook, "Installation")
    mate <- attr(fieldbook, "MaterialList")
    crop <- attr(fieldbook, "CropManagement")
    vari <- attr(fieldbook, "VariableList")
    soil <- attr(fieldbook, "SoilAnalysis")
    weat <- attr(fieldbook, "WeatherData")

    fieldbook = cbind(fieldbook, xAUDPC)
    names(fieldbook)[ncol(fieldbook)] = lbl

    attr(fieldbook, "meta") <- meta
    attr(fieldbook, "Minimal") <- mini
    attr(fieldbook, "Installation") <- inst
    attr(fieldbook, "MaterialList") <- mate
    attr(fieldbook, "CropManagement") <- crop
    attr(fieldbook, "VariableList") <- vari
    attr(fieldbook, "SoilAnalysis") <- soil
    attr(fieldbook, "WeatherData") <- weat
  }

  fieldbook
}

# DF <- fbmaterials::get_fieldbook_data(
# 'PTLB199909_OXAPMP_B3C2OXA00-09') fb <- audpc(DF,
# variable_only = FALSE) fb <- audpc(fb, type = 'relative',
# use_stairs = T, variable_only = FALSE) fb <- audpc(fb, type
# = 'relative', variable_only = FALSE) fb <- audpc(fb, type =
# 'standardized', use_stairs = T, variable_only = FALSE)
