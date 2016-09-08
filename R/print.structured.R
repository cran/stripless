#' @title Methods for structured objects.
#' @description Print/plot and summary methods for class \code{"structured"} objects.
#'
#' @details The \code{print} and \code{plot} methods produce a plot and informative legend
#'   for \code{"structured"} objects. The \code{plot} method is an alias for the print
#'   method. The \code{summary} method gives a simple summary of the object with a
#'   \code{print}
#'
#' @aliases plot.structured
#'
#' @param x,object The object to be displayed or summarized.
#'
#' @param legendLoc An optional location of a legend describing the plot layout to be used
#'   as a legend on the trellis plot. Note that a text legend is \emph{always} printed on
#'   the console. If used, this argument specified the position of a legend in the trellis
#'   display. It must be one of \code{"left"}, \code{"right"}, \code{"top"},
#'   \code{"bottom"}, or \code{"newpage"} (case doesn't matter and
#'   matching is done using \code{\link{match.arg}},
#'   so \code{legendLoc} can be abbreviated). Any of the first four of these will become
#'   the name of a component of the \code{legend} argument of the
#'   \code{\link[lattice]{xyplot}} call, and so must not conflict with any in a
#'   \code{legend} argument that may already be part of the \code{\link{strucplot}} call.
#'
#'   If \code{legendLoc = "newpage"}, the legend will be plotted centered on a new trellis
#'   page.
#'
#' @param legend A \emph{function} that constructs a grob to use as a plot legend. It must
#'   accept at least 3 arguments named \code{"struc"}, \code{"legendLoc"}, and
#'   \code{"abbrevLength"}, and also have a \dots argument. The first three will be passed
#'   the \code{structure} attribute of the object to be plotted and the \code{legendLoc}
#'   and \code{abbrevLength} arguments of the \code{print} call. The \dots argument will
#'   be passed the \dots argument of the \code{print} call, so additional function
#'   arguments should be included there (as name = value pairs, as usual).
#'
#'   The default is the \code{\link{defaultStrucLegend}} function. Its Help file should be
#'   consulted for its full argument list.
#'
#' @param lbl Label for console legend. Default = "PLOT STRUCTURE"
#'
#' @param ... Further arguments to pass down to either the \code{print} methods, the
#'   \code{legend} function, or \code{\link[lattice]{print.trellis}}. Care should be taken
#'   to ensure that the names of arguments do not conflict. Note that \code{heading} and
#'   \code{miss} arguments of \code{defaultStrucLegend} are also used by the console
#'   display.
#'
#' @inheritParams defaultStrucLegend
#'
#' @note Do not use the \code{packet.panel} argument of \code{print.trellis}, as this will
#'   totally mess up the display.
#'
#' @seealso \code{\link[lattice]{print.trellis}, \link{defaultStrucLegend}}
#'
#' @examples
#' require(datasets)
#' # quakes data
#' #
#' # Create and save plot
#' out <- strucplot(lat ~ long|cut(mag,5)*cut(depth,4), data = quakes,
#'   col="blue", main = "Earthquake locations, by magnitude and depth")
#'
#' # Summary:
#' summary(out)
#'
#' # Default output -- structure legend on console only
#'    print(out)
#'
#' # Add legends to the plot on either right or bottom (note partial matching)
#'    print(out, legendLoc = "right")
#'    print(out, legendLoc = "b")
#' #
#' # Plot the legend by itself on a separate page
#'    print(out, legendLoc = "newp")
#' #
#' # Extra grid "gp" arguments to alter text appearance
#'    print(out, legendLoc = "b",col="blue",fontface = "italic", abbrev = 5)
#' #
#' # ******* Using the "abbrev" argument with the 'barley' data set ****
#' #
#'   out <- strucplot(variety~yield|year*site,data=barley, horizontal=TRUE,
#'   panel=panel.dotplot, col = "darkblue", scales = list(alternat = 1,
#'   y = list(cex=.5)), spacings = list(x=0, y=.5))
#'
#' # Default
#'   print(out)
#' #
#' # Abbreviate factor names and levels
#'   print(out, abbrev = c(3,4))
#' #
#' # abbreviate just the levels of 'site' and change the console legend title
#'   print(out, abbrev = list(site = c(0,4)), lbl = "Structure Key")
#' #
#' # Note that the 'abbreviate' argument is shared by console and plot
#' # legends; as are the optional 'heading' and 'miss' arguments
#' # by the 'defaultStrucLegend' function.
#'   print(out,abbrev = list(site = c(0,4)),legendLoc="t",
#'    heading = c("Left-Right", "Up-Down"))

print.structured <- function(x
  ,legendLoc = c("left","right","top","bottom","newpage")
  ## optional legend location for a plotted legend
  ,legend = defaultStrucLegend
  ,lbl = "PLOT STRUCTURE"
  ,abbrevLength = c(0,0) ## for abbreviating conditioning names and levels
   ,...  ## further named arguments to legend() or print.trellis()
  )
{
  struc <- attr(x,"structure")
## create text string for console display
  txt <-displayStruc(struc= struc ,abbrevLength = abbrevLength, ...)

  ## Check lbl
  stopifnot(is.character(lbl),length(lbl) ==1)
  ## and display
  cat(sprintf("\n%s\n\n%s\n\n%s\n\n",lbl, txt[1],txt[2]))
  ## create legend if needed
  if(!missing(legendLoc)){
    legendLoc <- tolower(legendLoc)
    pos <- tryCatch(match.arg(legendLoc),
                    error = function(e)stop("Incorrect legendLoc argument"))
    leg <- legend(struc,
                  legendLoc = pos,
                  abbrevLength = abbrevLength,
                  ...)
    if(!pos == "newpage"){
      oleg <- x$legend
      leg <- structure(list(list(fun = leg)), names = pos)
      if(pos %in% names(oleg))
        stop("Structure legend position conflicts with plot legend position")
        else x$legend <- c(oleg,leg)
    }
    NextMethod()
    if(pos == "newpage"){
        ans <- readline("\nHit <return> to show structure legend:\n")
        grid.newpage()
        grid.draw(leg)
      }
  } else NextMethod()
}

#' @rdname print.structured
#' @description plot and print methods are the same for \code{"structured"} objects
  plot.structured <- print.structured

#' @rdname print.structured
#' @description Summary method for \code{"structured"} objects.
summary.structured <- function(object,...)
{
  out <- list(
    call = object[["call"]],
    formula = attr(object,"formula"),
    structure = attr(object,"structure"))
  class(out) <- c("summary.structured", "list")
  out
}

#' @rdname print.structured
#' @description  Print method for \code{"summary.structured"} objects.
print.summary.structured <- function(x,lbl = "PLOT STRUCTURE",...)
{
  cat("\n  xyplot call:\n")
  print(x$call)
  cat("\n Actual formula:\n")
  print(x$formula)
  txt <- displayStruc(x$structure,...)
  cat(sprintf("\n%s\n\n%s\n\n%s\n\n",lbl,txt[1],txt[2]))
}
