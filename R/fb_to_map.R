#' converts fieldbook to field map
#'
#' Simple implementation; currently only works for 1 factorials with equal plots per
#' replication (as in an RCBD trial).
#'
#' @param DF a fieldbook data frame
#' @param gt a genotype
#' @param rep column short label used for replication
#' @param blk column short label used for block (ignored currently)
#' @param plt column short label for plot ID
#' @param variable the main variable value to plot
#' @export
fb_to_map <- function(DF, gt = "INSTN", rep="REP", blk = NULL, plt = "PLOT", variable = "HI"){
  #DF is a fieldbook
  #print(head(DF))
  DF = as.data.frame(DF)
  # print("FB map")
  # print(str(DF))
  # print(rep)
  # print("FB map end")
  DF[, rep] = as.integer(DF[, rep])
  PLTL <- DF[, plt]
  DF[, plt] = as.integer(DF[, plt])
  # TODO block treatment
  #print("====")
  #print(head(DF))
  #print(variable)

  nc = max(table(DF[, rep]))
  nr = length(unique(DF[, rep]))
  fb_map = matrix(NA, ncol = nc, nrow = nr)

  #cnl <- c(rep, plt, blk, variable)
  #other_vars <- c(variable, names(DF)[!names(DF) %in% cnl])
  # if(all(is.na(variable))){
  #   variable = rep(0, length(variable))
  # }
  if(all(is.na(DF[, variable]))){
    DF[, variable] = rep(0, nrow(DF))
  }
  for(i in 1:nr){
    fb_map[i, ] = DF[DF[rep] == i, variable]
  }
  cn = matrix("", ncol = nc, nrow=nr)
  for(j in 1:nrow(DF)) {
    #ll = DF[j, other_vars]
    #ll = paste0(names(ll),": ", ll)
    #ll = paste0(variable,": ", DF[j, variable], " @ plot: ", as.character(PLTL[j]))
    ll = paste0(variable,": ", DF[j, variable], " @ plot: ", as.character(PLTL[j]),
                ", genotype: ", DF[j, gt ])
    #ll = paste(ll, collapse = "; ")

    ri = DF[j, rep]
    if(nr > 1){
      ci = DF[j, plt] - (ri * nc) + nc
    } else {
      ci = j
    }

    #print(ll)
    cn[ri, ci ] = ll
  }
  list(map = fb_map, notes = cn)
}


# DF = fbmaterials::get_fieldbook_data("SPYLAT2013_MZ-Gurue")
# DF <- fbmaterials::get_fieldbook_data(  "PTLB199909_OXAPMP_B3C2OXA00-09")
# fm <- fb_to_map(DF, variable = "LB1", plt = "PLOT", blk = NULL)
# d3heatmap::d3heatmap(fm$map, cellnote = fm$notes,
#                      Rowv = FALSE, Colv = FALSE, dendrogram = "none")
