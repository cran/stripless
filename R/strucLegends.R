#' @title Default legend function for \code{strucplot} displays.
#'
#' @description
#'   A structure legend is always printed on the console. It can also be
#'   optionally added to the trellis plot. This function constructs a default
#'   legend for this option.
#'
#' @return A text grob that can be included as part of the \code{strucplot} trellis plot.
#'
#' @param struc The "structure" attribute of a \code{strucplot} object
#'
#' @param legendLoc One of c("top","right","bottom","left","newpage") The first 4 specify
#'   on which side of the trellis display to place the legend. The last indicates that the
#'   legend will be plotted at the center of a separate page.
#'
#' @param abbrevLength Default = \code{c(0,0)}. Either a length 2 vector or a named list
#'   to control lengths of factor names and levels in the legend. If a length 2 vector, it
#'   gives the \code{minlength} argument for the abbreviate function for abbreviating all
#'   the factor names and their levels, in that order. A value of 0 for either means
#'   "don't abbreviate." For back compatibility, a single numeric y will also be accepted
#'   and changed to \code{c(y,0)}. If a named list, the names must be those of of the
#'   conditioning factors to abbreviate, and values length 2 vectors as above to
#'   control abbreviation lengths for the corresponding factor names and levels.
#'
#' @param  legendLab A character string title for the legend. A value of
#'  \code{NA} omits the title. Default = \code{NA}
#'
#' @param heading A character vector of length 2 giving the headings for the horizontal
#'   and vertical conditioning variables portions of the legend. Default =
#'   \code{c("Horizontal", "Vertical")}.
#'
#' @param miss A character string to use when there is no conditioning either horizontally
#'   or vertically. Default = \code{"No Conditioning"}
#'
#' @param cex.font  Multiplier for text font size in a trellis legend. Default = 1.
#'
#' @param cex.lab  Multiplier for legend title font size in a trellis legend.
#'  Default = 1.25.
#'
#' @param col Text color for text in a trellis legend. Default = \code{"black"}.
#'
#' @param ... Additional arguments to be used in a \code{gp} list for controlling text
#'   appearance in a trellis legend. See \code{\link[grid]{gpar}} for possibilities.
#'
#' @seealso \code{\link{print.structured}}
#'
#' @examples
#' # Controlling the console and plot legends
#'  library(datasets) ## for the barley data
#' #
#'  out <- strucplot(variety~yield|year*site,data=barley, horizontal=TRUE,
#'    panel=panel.dotplot, col = "darkblue",
#'    scales = list(alternat = 1, y = list(cex=.5)),
#'    spacings = list(x=0, y=.5))
#' #
#' # Default with legend on top; note that no title is the plot legend default
#'   print(out, legendLoc = "t", abbrev= list(site = c(0,4)))
#' #
#' # Include title on plot and reduce default font sizes in red text
#'   print(out, legendLoc = "T", abbrev = list(site = c(0,6)),
#'    legendLab = "Structure", cex.lab = 1, cex.font = .75, col = "darkred" )
#'
defaultStrucLegend <-
  function(struc
   ,legendLoc = c("top","right","bottom","left","newpage")
   ,legendLab = NA ## text for labeling the plot structure
   ,heading = c("Horizontal", "Vertical") ## Heading for dimensions
   ,miss = "No Conditioning" ## text if there is no conditioning in a dimension
   ,abbrevLength = c(0,0) ## minLength for abbreviations of factor names and levels
   ,cex.font = 1 ## multiplier for text font size
   ,cex.lab = 1.25 ## multiplier for label font size
   ,col = "black" ## text color for text
   ,... ## Additional arguments for controlling font appearance to be used as
  ## part of gp argument
)
{
  dots <- list(...)
  if(is.null(dots[["col"]]))dots[["col"]] <- col
  gp <- do.call(gpar,dots)
  ## create text display
  txt <- displayStruc(struc= struc,heading = heading, miss = miss,
                      abbrevLength = abbrevLength)

  ## construct grobs
  if(is.na(legendLab))titl <- textGrob("",gp = gpar(cex=0))
  else {
    if(!(is.character(legendLab) && length(legendLab) ==1))
      stop("Bad legendLab for plot legend")
    else titl <- textGrob(legendLab,gp = gpar(cex = cex.lab))
  }
  hz <- textGrob(txt[[1]], gp = gpar(cex=cex.font), x = 0, y=1,
                 just = c("left","top"))
  vrt <- textGrob(txt[[2]],gp = gpar(cex=cex.font), x = 0, y=1,
                  just = c("left", "top"))
  parts <- list(titl,hz,vrt)
  grbwid <- lapply(parts,grobWidth)
  grbht <- lapply(parts,grobHeight)
  spcht <- unit(1,"line")
  spcwid <- unit(1,"char")
  if(legendLoc %in% c("right","left","newpage")){
    owid <- do.call(max,grbwid)
    midwid <- do.call(max,grbwid[2:3])
    sidewid <- (owid - midwid )*.5
    pad <- (if(legendLoc == "right")c(.5,0) else c(0,.5))*spcwid
    glay <- grid.layout(nrow = 5, ncol = 5,
              widths = unit.c(pad[1],sidewid, midwid,sidewid,pad[2]),
              heights = unit.c(grbht[[1]],spcht,grbht[[2]],spcht,grbht[[3]])
            )
    frm <- frameGrob(layout = glay, gp = gp )
    frm <- placeGrob(frm, parts[[1]],row = 1,col = 2:4)
    frm <- placeGrob(frm,parts[[2]], row = 3, col = 3)
    frm <- placeGrob(frm,parts[[3]], row = 5, col = 3)
  } else{
    leftwid <- grbwid[[2]] + spcwid
    w <- max(leftwid + grbwid[[3]],grbwid[[1]])
    pad <- (if(legendLoc == "top")c(0,.5) else c(.5,0)) * spcht
    glay <- grid.layout(nrow = 5, ncol = 3,
                    widths = unit.c(grbwid[[2]],spcwid, w - leftwid),
                    heights = unit.c(pad[1],grbht[[1]],spcht,
                            do.call(max,grbht[2:3]),pad[2])
              )
    frm <- frameGrob(layout = glay, gp = gp)
    frm <- placeGrob(frm, parts[[1]],row = 2)
    frm <- placeGrob(frm, parts[[2]],row = 4, col = 1)
    frm <- placeGrob(frm, parts[[3]],row = 4, col = 3)
  }
  frm
}


