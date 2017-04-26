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
fb_to_map <- function(DF, gt = "INSTN", rep = "REP", blk = NULL, plt = "PLOT", variable = "HI"){
  #DF is a fieldbook
<<<<<<< HEAD
  print("fieldbook map")
  # print(head(DF))
=======
  #print(head(DF))
  DF = as.data.frame(DF)
  # print("FB map")
  # print(str(DF))
  # print(rep)
  # print("FB map end")
>>>>>>> f959aa76a25fa7fb157ef9ad7087541a19edf596
  DF[, rep] = as.integer(DF[, rep])
  PLTL <- DF[, plt]
  DF[, plt] = as.integer(DF[, plt])
  # TODO block treatment
<<<<<<< HEAD
  # print("====")
  # print(head(DF))
  #print(variable)
=======
>>>>>>> f959aa76a25fa7fb157ef9ad7087541a19edf596

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
<<<<<<< HEAD
  print(rep)
  print(nr)
  print(DF[, rep])
  print(fb_map)
  print(DF[DF[, rep] == nr, variable])
  for(i in 1:nr){
    print(i)
    print(length(DF[DF[, rep] == i, variable]))
    fb_map[i, ] <- DF[DF[, rep] == i, variable]
=======

  # TODO better handling of plot IDs
  # if(DF[1, plt] > 100) DF[, plt] = DF[, plt] - 100
  # if(DF[1, plt] > 1000) DF[, plt] = DF[, plt] - 1000
  # print("====")
  # print(head(DF))
  # print(str(DF))
  # print(variable)


  for(i in 1:nr){
    fb_map[i, ] = DF[DF[ ,rep] == i, variable]
>>>>>>> f959aa76a25fa7fb157ef9ad7087541a19edf596
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
      #ci = DF[j, plt] - (ri * nc) + nc
      ci = j - (ri * nc) + nc
    } else {
      ci = j
    }
    # print("fbmaterials")
    # print(ll)
    # print(cn[j])
    # print(ri)
    # print(ci)
    # print(ll)
    cn[ri, ci ] = ll
  }
  list(map = fb_map, notes = cn)
}


# DF = fbmaterials::get_fieldbook_data("SPYLAT2013_MZ-Gurue")
# DF <- fbmaterials::get_fieldbook_data(  "PTLB199909_OXAPMP_B3C2OXA00-09")
# fm <- fb_to_map(DF, variable = "LB1", plt = "PLOT", blk = NULL)
# d3heatmap::d3heatmap(fm$map, cellnote = fm$notes,
#                      Rowv = FALSE, Colv = FALSE, dendrogram = "none")
