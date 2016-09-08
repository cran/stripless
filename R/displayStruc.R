#' @title Construct text For \code{strucplot} legends.
#' @description Construct character strings for print and legend functions to display on
#' the console or lattice plot.
#'
#' @details This is an exported auxiliary function that constructs the character strings
#' for displaying plot structure on the console and possibly also in a legend on the
#' lattice display. While not intended to be directly called by the user, it is exported
#' for use by those who wish to supply custom legends for the lattice display.
#'
#' @return
#' A character vector of length 2 giving character strings,
#'  including \code{\\n} line breaks.
#'
#' @param ... Additional arguments, presently ignored
#' @inheritParams defaultStrucLegend
#'
#' @examples
#' require(datasets)
#' # quakes data
#' #
#' # Create and save plot
#'  out <- strucplot(lat ~ long|cut(mag,5)*cut(depth,4), data = quakes,
#'   col="blue", main = "Earthquake locations, by magnitude and depth")
#'
#'  displayStruc(attr(out,"structure"))

displayStruc <- function(struc
                         ,heading = c("Horizontal", "Vertical") ## Heading for dimensions
                         ,miss = "No Conditioning" ## text if there is no conditioning in
                         # a dimension
                         ,abbrevLength = c(0,0) ## minlength for factor,levels abbreviations
                         ,... ## additional arguments, ignored
)

  ## Helper function for constructing structure legends
{
  ## Create vector of text labels for horiz and vertical directions
  ## First check the arguments
  if(!(is.character(miss) && length(miss)==1)) stop(
    "miss must be a single character string")
  if(!(is.character(heading) && length(heading)==2)) stop(
    "heading must be a length 2 character vector")
  bad <-  function(x)!(is.numeric(x) && length(x) == 2)
  if(is.numeric(abbrevLength) && length(abbrevLength)==1L)
    abbrevLength <- c(abbrevLength,0)
  else if(is.list(abbrevLength)){
    ## get/check factor names
    nm <- unlist(lapply(struc,names))
    nm.ab <- names(abbrevLength)
    if(any(duplicated(nm.ab))) stop("Duplicate names in abbrevLength list\n")
    if(is.null(nm.ab) || any(!(nm.ab %in% nm)))
      stop("Names in abbrevLength list do not match factor names")
    if(any(vapply(abbrevLength,bad,NA)))
      stop("Incorrect abbrevLength values")
  } else if(bad(abbrevLength)) stop("Incorrect abbrevLength argument")
  if(!is.list(abbrevLength)){
    for(i in 1:2){
      if(length(struc[[i]])){
        if(abbrevLength[1] >=1)
          names(struc[[i]]) <- abbreviate(names(struc[[i]]),minlength = abbrevLength[1])
        if(abbrevLength[2] >=1)
          struc[[i]] <- lapply(struc[[i]],function(u)abbreviate(u, minlength = abbrevLength[2]))
      }
    }
  } else {
    nm <- names(abbrevLength)
    for(i in 1:2){
      if(length(struc[[i]])){
        this <- struc[[i]]
        allnames <- names(this)
        wh <- nm %in% allnames
        if(any(wh)){ ## are there any that may need abbrevating?
          for(u in nm[wh]){
            ab <- abbrevLength[[u]]
            indx <- which(allnames == u)
            if(ab[1])
              names(this)[indx] <- abbreviate(allnames[indx], minlength = ab[1])
            if(ab[2])this[[indx]]<- abbreviate(this[[indx]], minlength = ab[2])
          }
        }
      }
      struc[[i]] <- this
    }
  }
  txt <- lapply(struc,function(u){
    if(!length(u)) miss
    else paste(names(u),sapply(u, paste, collapse = "  "), sep = ":  ")
  })
  sprintf("_%s_\n%s",heading,sapply(txt,paste,collapse = "\n"))
}
