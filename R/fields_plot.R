### convert field locations to matrix indices (24-2 patterns):
indices = rbind(
  cbind(1, 4:7),
  cbind(2, 3:8),
  cbind(3, 2:9),
  cbind(4, 1:9),
  cbind(5, 1:9),
  cbind(6, 2:9),
  cbind(7, 3:8),
  cbind(8, 4:7))
### same for 30-2 patterns:
indices30 = rbind(
  cbind(1, 4:7),
  cbind(2, 3:8),
  cbind(3, 2:9),
  cbind(4, 1:10),
  cbind(5, 1:10),
  cbind(6, 1:10),
  cbind(7, 1:10),
  cbind(8, 2:9),
  cbind(9, 3:8),
  cbind(10, 4:7))

#===========================================================================#

### Value plotting function:
#' Value plotting function for 24-2 or 30-2 visual field measurement:
#'
#' \code{plotTDvalues} plots the following 24-2 or 30-2 visual field measurement: sensitivity, TD, and PD:
#' @param tds a vector contains sensitivity/TD/PD measurement. For 24-2 VF \code{tds} should have 52 or 54 elements. For 30-2 VF, \code{tds} should have 74 or 76 elements.
#' @param cex.tds a numeric variable for label size (default: 1).
#' @param textcolor a function defines the label color.
#' @param show.lines a logical variable indicates whether to show the horizontal and vertical lines.
#' @param ... other variables to be added.
#' @return value plot for sensitivity, TD and PD input.
#' @examples
#' data(vfseries)
#' tds = t(vfseries[1, grepl('^s[0-9]+', colnames(vfseries))])
#' plotTDvalues(tds)
#' title(main = "Sensitivity", line = 3)
#' tds = t(vfseries[1, grepl('^td[0-9]+', colnames(vfseries))])
#' plotTDvalues(tds)
#' title(main = "Total Dviation", line = 3)
#' tds = t(vfseries[1, grepl('^pd[0-9]+', colnames(vfseries))])
#' plotTDvalues(tds)
#' title(main = "Pattern Dviation", line = 3)
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics abline axis image layout par plot points rect text
#' @export

plotTDvalues <- function(tds, cex.tds=1, textcolor=function(x) "black", show.lines=T,   ...)
  # plot TDs as numeric values;
  # works for both 24-2 and 30-2
{
  tp = switch(paste(length(tds)),
              "52" = c(tds[1:25], NA, tds[26:33], NA, tds[34:52]),
              "74" = c(tds[1:35], NA, tds[36:44], NA, tds[45:74]),
              tds)

  tp[ifelse(length(tp)==54, 26, 36)] = NA
  # tp[ifelse(length(tp)==54, 35, 46)] = NA
  tp[ifelse(length(tp)==76, 35, 46)] = NA   # edited by Dian, 2019-5-21
  inds = if(length(tp)== 54) indices else indices30

  op = par(mar=c(2, 1, 2, 2)+0.1)
  plot(
    inds[,2],
    -inds[,1],
    xlim=c(1, ifelse(length(tp)==54, 9.2, 10.2)),
    pch='.',
    type='n',
    xaxt='n', yaxt='n',
    xlab="", ylab="",
    bty='n',
    ...)
  if(show.lines)
  {
    abline(h=ifelse(length(tp)==54, -4.5, -5.5))
    abline(v=5.5)
  }
  for(i in 1:length(tp))
  {
    x = inds[i,2]
    y = -inds[i ,1]
    if(!is.na(tp[i]))
      text(x, y, tp[i], cex=cex.tds, col=textcolor(tp[i]))
  }
  par(op)
}


#===========================================================================#
### Value plotting function:
#' Value plotting function for 24-2 or 30-2 visual field measurement:
#'
#' \code{plotTdProbabilities} plots the following 24-2 or 30-2 visual field measurement: TD probs, and PD probs:
#' @param tdprob a vector contains TD probs/PD probs measurement. For 24-2 VF \code{tdprob} should have 52 or 54 elements. For 30-2 VF, \code{tdprob} should have 74 or 76 elements.
#' @param cex a numeric variable for label size (default: 2).
#' @param rectangle.color a string variable defines label color (default: 'black').
#' @param rectangle.width a numeric variable defines label width (default: '0.16').
#' @param margins a vector define the plot margins (default: c(2, 1, 2, 2)+0.1).
#' @param ... other variables to be added.
#' @return value plot for TD prob and PD prob input.
#' @examples
#' data(vfseries)
#' tdprob = t(vfseries[1, grepl('^tdp[0-9]+', colnames(vfseries))])
#' plotTdProbabilities(tdprob)
#' title(main = "Total Deviation Probability", line = 3)
#' tdprob = t(vfseries[1, grepl('^pdp[0-9]+', colnames(vfseries))])
#' plotTdProbabilities(tdprob)
#' title(main = "Pattern Deviation Probability", line = 3)
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics abline axis image layout par plot points rect text
#' @export
#'
plotTdProbabilities <- function(
  tdprob,
  cex=2,
  rectangle.color = "black",
  rectangle.width = 0.16,
  margins = c(2, 1, 2, 2)+0.1,
  ...)
  # works for both 24-2 and 30-2
{
  tp = switch(paste(length(tdprob)),
              "52" = c(tdprob[1:25], NA, tdprob[26:33], NA, tdprob[34:52]),
              "74" = c(tdprob[1:35], NA, tdprob[36:44], NA, tdprob[45:74]),
              tdprob)

  tp[ifelse(length(tp)==54, 26, 36)] = NA
  tp[ifelse(length(tp)==54, 35, 46)] = NA
  inds = if(length(tp)== 54) indices else indices30

  op = par(mar=margins)
  plot(
    inds[,2],
    -inds[,1],
    pch='.',
    type='n',
    xaxt='n', yaxt='n',
    xlab="", ylab="",
    bty='n',
    ...)
  abline(h=ifelse(length(tp)==54, -4.5, -5.5))
  abline(v=5.5)
  plot005 <- function(x, y)
  {
    d = 0.08
    points(c(x-d, x-d, x+d, x+d), c(y-d, y+d, y-d, y+d), pch='.', cex=cex)
  }
  plot002 <- function(x, y)
  {
    d=0.04
    points(
      x + d*c(-2, 0, 2, -3, -1, -2, 0, -3, 1, -2, 2, -3, -1, 1, 0, 2, 1),
      y + d*c(-4, -4, -4, -3, -3, -2, -2, -1, -1, 0, 0, 1, 1, 1, 2, 2, 3),
      pch='.',
      cex=cex)
  }
  plot001 <- function(x, y)
  {
    d=0.04
    points(
      x + d*c(-3, -2, 0, 1, 2, -3, -2, -1, 1, 3, -1:3, -1, 0, 2, 3, -3, -2, 0, 1, -2:3, -3:-1, 1, -2, 1),
      y + d*c(-4, -4, -4, -4, -4, -3, -3, -3, -3, -3, rep(-2, 5), rep(-1, 4), rep(0, 4), rep(1,6), rep(2, 4), 3, 3),
      pch='.',
      cex=1.5*cex)
  }
  plot0005 <- function(x, y)
  {
    d = rectangle.width
    rect(x-d, y-d, x+d, y+d, col=rectangle.color, border=NA)
  }
  for(i in 1:length(tp))
  {
    x = inds[i,2]
    y = -inds[i ,1]
    if(!is.na(tp[i]))
      switch(paste(tp[i]),
             "0.05" = plot005(x, y),
             "0.02" = plot002(x, y),
             "0.01" = plot001(x, y),
             "0.005" = plot0005(x, y),
             points(x, y, pch='.', cex=cex))
  }
  par(op)
}

#===========================================================================#

### Single plotting function:
#' Single plotting function for one 24-2 or 30-2 visual field measurement:
#'
#' \code{plotfield.normalized} plots the following 24-2 or 30-2 visual field measurement: sensitivity, TD, TD prob, PD, and PD prob:
#' @param eigenfields a vector contains Sensitivity/TD/PD measurement. For 24-2 VF \code{eigenfields} should have 52 or 54 elements. For 30-2 VF, \code{eigenfields} should have 74 or 76 elements.
#' @param component Number of components to be plotted (default: 1).
#' @param zmin minimum value of the color scale (default: auto defined).
#' @param zmax maximum value of the color scale (default: auto defined).
#' @param color.pal an object that defines color scale theme (default: colorRampPalette(c("red", "white", "blue"), space = "Lab")(256)).
#' @param show.colorbar a logic value to show colorbar (default: TRUE).
#' @param topleftannotation a string annotation shown on the top left side of the plot (default: NULL).
#' @param bottomleftannotation a string annotation shown on the bottom left side of the plot (default: NULL).
#' @param labelcex a numeric variable for label size (default: 2).
#' @param ... other variables to be added.
#' @return heatmap for sensitivity, TD and PD input
#' @examples
#' data(vfseries)
#' eigenfields = t(vfseries[1, grepl('^s[0-9]+', colnames(vfseries))])
#' plotfield.normalized(eigenfields)
#' title(main = "Sensitivity", line = 3)
#' eigenfields = t(vfseries[1, grepl('^td[0-9]+', colnames(vfseries))])
#' plotfield.normalized(eigenfields)
#' title(main = "Total Deviation", line = 3)
#' eigenfields = t(vfseries[1, grepl('^pd[0-9]+', colnames(vfseries))])
#' plotfield.normalized(eigenfields)
#' title(main = "Pattern Deviation", line = 3)
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics abline axis image layout par plot points rect text
#' @export

plotfield.normalized <- function(
  eigenfields,
  component = 1,
  zmin=-max(abs(c(min(eigenfields), max(eigenfields)))),
  zmax=max(abs(c(min(eigenfields), max(eigenfields)))),
  color.pal=colorRampPalette(c("red", "white", "blue"), space = "Lab")(256),
  show.colorbar = TRUE,
  topleftannotation = NULL,
  bottomleftannotation = NULL,
  labelcex = 2,
  ...)
{
  v = eigenfields[, component]
  include.blindspot = length(v) == 54	# if locations of blind spot are included
  m = matrix(rep(NA, 8*9), ncol=8)
  i.v = 1:length(v)
  i.mat = if(include.blindspot) i.v else c(i.v[c(-26, -35)], 53, 54)
  # rotate the matrix 90 degree counter-clockwise (for function image()):
  for(i in i.v)
    m[indices[i.mat[i], 2], 9-indices[i.mat[i], 1]] = v[i]

  op = par(mar=c(2, 1, 2, 2)+0.1)
  image(
    y = 1:8,
    z = m,
    zlim=c(zmin, zmax),
    ylim=c(ifelse(show.colorbar, -0.5, 0), 8.5),
    col=color.pal,
    axes=FALSE,
    xlab="", ylab="",
    ...)
  # mark blind spot:
  if(!include.blindspot)
    rect(7.3/9, 3.5, 8.4/9, 5.48, col='gray')
  # annotate:
  if(!is.null(topleftannotation))
    text(-0.05, 7.9, labels=topleftannotation, cex=labelcex, pos=4)
  if(!is.null(bottomleftannotation))
    text(-0.05, 0.9, labels=bottomleftannotation, cex=labelcex, pos=4)
  # color bar:
  if(show.colorbar)
  {
    for(k in 1:256)
    {
      xleft = k/256-0.5/256
      rect(xleft, -0.25, xleft+1/256, 0.25, col=color.pal[k], border=NA)
    }
    axis(1, at=c(0, 0.5, 1), labels=round(c(zmin, (zmin+zmax)/2, zmax), 3))
  }
  par(op)
}

#===========================================================================#

### General plotting function:
#' General plotting function for multiple 24-2 or 30-2 visual field measurements together:
#'
#' \code{plotComponentMatrix} plots the following 24-2 or 30-2 visual field measurement: sensitivity, TD, TD prob, PD, and PD prob:
#' @param componentmatrix a matrix or data frame, column represents different eyes and rows are the VF measurements of the same type (sensitivity, TD, TD prob, PD, or PD prob).
#' @param ncomp a numeric variable defines the number of components to be plotted (default: all).
#' @param plot.ncols a numeric variable defines the number of columns to be plotted (default: 5).
#' @param plot.nrows a numeric variable defines the number of rows to be plotted (default: NULL (automatically calculated)).
#' @param plot.annot.topleft.function a function(i) that is given to any subplot i to create its top left annotation.
#' @param plot.annot.bottomleft.function a function(i) that is given to any subplot i to create its bottom left annotation (default: returns NULL).
#' @param globaltitle a string for global title (default: k = ncomp; set to NULL to suppress global title).
#' @param globalannotright a string annotation to the right of the global title (default: NULL).
#' @param zmin minimum value of the color scale (default: auto defined).
#' @param zmax maximum value of the color scale (default: auto defined).
#' @param color.pal an object that defines color scale theme (default: colorRampPalette(c("red", "white", "blue"), space = "Lab")(256)).
#' @param td.probabilities a logic variable indicates whether to plot TD probability symbols instead of TD colors (default: FALSE).
#' @param show.colorbar a logic variable indicates whether to show a global colorbar (default: !td.probabilities).
#' @param titleheight a numeric variable defines the height of the title relative to height of row one.
#' @param ... other variables to be added.
#' @return heatmap for sensitivity, TD and PD input. Value plot for TD prob and PD prob input.
#' @examples
#' data(vfseries)
#' componentmatrix = t(vfseries[1:10, grepl('^s[0-9]+', colnames(vfseries))])
#' globaltitle = paste("Sensitivities, k = ", ncol(componentmatrix), sep = '')
#' plotComponentMatrix(componentmatrix, globaltitle = globaltitle)
#' componentmatrix = t(vfseries[1:10, grepl('^td[0-9]+', colnames(vfseries))])
#' globaltitle = paste("TDs, k = ", ncol(componentmatrix), sep = '')
#' plotComponentMatrix(componentmatrix, globaltitle = globaltitle)
#' componentmatrix = t(vfseries[1:10, grepl('^pd[0-9]+', colnames(vfseries))])
#' globaltitle = paste("PDs, k = ", ncol(componentmatrix), sep = '')
#' plotComponentMatrix(componentmatrix, globaltitle = globaltitle)
#' componentmatrix = t(vfseries[1:10, grepl('^tdp[0-9]+', colnames(vfseries))])
#' globaltitle = paste("TD Probs, k = ", ncol(componentmatrix), sep = '')
#' plotComponentMatrix(componentmatrix, globaltitle = globaltitle, td.probabilities = TRUE)
#' componentmatrix = t(vfseries[1:10, grepl('^pdp[0-9]+', colnames(vfseries))])
#' globaltitle = paste("PD Probs, k = ", ncol(componentmatrix), sep = '')
#' plotComponentMatrix(componentmatrix, globaltitle = globaltitle, td.probabilities = TRUE)
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics abline axis image layout par plot points rect text
#' @export

plotComponentMatrix <- function(
  componentmatrix,
  ncomp = ncol(componentmatrix),
  plot.ncols = 5,
  plot.nrows = NULL,
  plot.annot.topleft.function = toString,
  plot.annot.bottomleft.function = function(i) NULL,
  globaltitle = sprintf("k = %i", ncol(componentmatrix)),
  globalannotright = NULL,
  zmin=-ceiling(max(abs(c(min(componentmatrix), max(componentmatrix))))),
  zmax=-zmin,
  color.pal=colorRampPalette(c("red", "white", "blue"), space = "Lab")(256),
  td.probabilities = FALSE,
  show.colorbar = !td.probabilities,
  titleheight = 0.2,
  ...)
# plot all components (e.g. eigenfields, archetypes, etc.) together
# componentmatrix: matrix the columns of which contain the components
# ncomp: plot ncomp of these components (default: all)
# plot.ncols: plot components in matrix with plot.ncols columns
# plot.nrows: plot components in matrix with plot.ncols rows
#  default: NULL (automatically calculated)
# plot.annot.topleft.function: function(i) that is given to any subplot i to create its
#   top left annotation
# plot.annot.bottomleft.function: function(i) that is given to any subplot i to create its
#   bottom left annotation (default: returns NULL)
# globaltitle: default: k = ncomp; set to NULL to suppress global title
# globalannotright: annotation to the right of the global title (default: NULL)
# zmin, zmax, color.pal: see plotfield.normalized
# td.probabilities: plot TD probability symbols instead of TD colors
# show.colorbar: show a global colorbar (default: !td.probabilities)
# titleheight: height of the title relative to height of one row
{
  #op <- par(mfrow=c(ceiling(ncomp/plot.ncols), plot.ncols))
  plotindex = ifelse(is.null(globaltitle), 1, 2)
  nrows = ifelse(is.null(plot.nrows), ceiling(ncomp/plot.ncols), plot.nrows)
  layoutmatrix = rbind(
    rep(1, ifelse(is.null(globaltitle), 0, plot.ncols)),
    matrix(
      c(plotindex:(ncomp+plotindex-1), numeric(nrows * plot.ncols - ncomp)),
      ncol=plot.ncols,
      byrow=TRUE),
    rep(ncomp + plotindex, ifelse(show.colorbar, plot.ncols, 0)))
  layout(
    layoutmatrix,
    heights=c(
      rep(titleheight, ifelse(is.null(globaltitle), 0, 1)),
      rep(1, nrows),
      rep(titleheight, ifelse(show.colorbar, 1, 0))))

  if(!is.null(globaltitle))
  {
    op <- par(mar=numeric(4))
    plot(c(0,1), c(0,1), type="n", axes=FALSE, ylab="", xlab="", xaxs="i")
    text(0.5, 1, globaltitle, font=2, cex=3, pos=1)
    if(!is.null(globalannotright))
      text(0.95, 0.55, globalannotright, cex=1.8, pos=2)
    par(op)
  }
  for(i in 1:ncomp)
  {
    if(td.probabilities)
      plotTdProbabilities(componentmatrix[, i], ...)
    else
      plotfield.normalized(
        componentmatrix,
        i,
        topleftannotation=plot.annot.topleft.function(i),
        bottomleftannotation=plot.annot.bottomleft.function(i),
        show.colorbar = FALSE,
        zmin, zmax,
        color.pal,
        ...)
  }
  if(show.colorbar)
  {
    op <- par(mar=c(2, 0, 0, 0))
    plot(c(0,1), c(0,1), type="n", axes=FALSE, ylab="", xlab="")
    for(k in 1:256)
    {
      xleft = k/256-1/256
      rect(xleft, 0, xleft+1/256, 1, col=color.pal[k], border=NA)
    }
    axis(1, at=c(0, 0.5, 1), labels=round(c(zmin, (zmin+zmax)/2, zmax), 3), cex.axis=1.5)
    par(op)
  }
}

