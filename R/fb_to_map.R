#' converts fieldbook to field map
#'
#' Simple implementation; currently only works for 1 factorials with equal plots per
#' replication (as in an RCBD trial).
#'
#' @param DF a fieldbook data frame
#' @param rep column short label used for replication
#' @param blk column short label used for block (ignored currently)
#' @param plot column short label for plot ID
#' @param variable the main variable value to plot
#' @export
fb_to_map <- function(DF, rep="REP", blk = "BLK", plot = "PLOT", variable = "HI"){
  #DF is a fieldbook
  nc = max(table(DF[, rep]))
  nr = length(unique(DF[, rep]))
  fb_map = matrix(NA, ncol = nc, nrow = nr)

  cnl <- c(variable, rep, plot, blk, variable)
  #other_vars <- c(variable, names(DF)[!names(DF) %in% cnl])

  for(i in 1:nr){
    fb_map[i, ] = DF[DF[rep] == i, variable]
  }
  cn = matrix("", ncol = nc, nrow=nr)
  for(j in 1:nrow(DF)) {
    #ll = DF[j, other_vars]
    #ll = paste0(names(ll),": ", ll)
    ll = paste0(variable,": ", DF[j, variable])
    #ll = paste(ll, collapse = "; ")

    ri = DF[j, rep]
    ci = DF[j, plot] - (ri * nc) + nc
    #print(paste(ri, ci))
    cn[ri, ci ] = ll
  }
  list(map = fb_map, notes = cn)
}


DF = fbmaterials::get_fieldbook_data("SPYLAT2013_MZ-Gurue")
fm <- fb_to_map(DF, variable = "RS")
d3heatmap::d3heatmap(fm$map, cellnote = fm$notes,
                     Rowv = FALSE, Colv = FALSE, dendrogram = "none")
