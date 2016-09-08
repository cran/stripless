#' @name stripless-internal
#' @title Unexported Utility and xyLayout Functions
#' @description Unexported functions not intended to be directly called by users.
#'
#' For \code{xyLayout}: Unexported generic and methods used by \code{strucplot} to
#' create, check and/or fix a \code{xyLayout} list.
#'
#' @details \strong{Brief utility function descriptions:}
#' \describe{
#'  \item{check2lvl}{Checks for a 2 level design with center point}
#'  \item{make_2level_with_center}{Makes a 2 level design with center point to
#'   compare to a fraction of a \eqn{3^n} design}
#'  \item{chkSpacings}{Checks and constructs proper spacings list if possible}
#'  \item{makeSpacings}{Constructs a vector of spacings values for the "between"
#'   argument of \code{\link[lattice]{xyplot}}}
#'   }
#'
#' \strong{xyLayout:}
#' The various methods provide convenience and flexibility for specifying
#' the \code{xyLayout} list argument that controls the format of \code{strucplot}
#' displays. Essentially any sensible way of specifying \code{xyLayout} should
#' work. See the Help page for \code{\link{strucplot}} for details.
#'
#' @param spacings A non-decreasing vector of positive values
#' @param levelLength An integer vector giving the number of levels per factor
#'   at each level of the plotting hierarchy
#' @param d Condition list from strucParseFormula to be checked to see if it
#'   represents a 2 level design with center point.
#' @param sep sep argument for \code{paste()}.
#' @param lvls A list of length 3 character vectors that are the level names of
#'   factors in a design to be checked.
#'
#' @param xylay An appropriate list, matrix, or vector for determining the
#' plot structure. Can also be missing, length 0, etc.
#'
#' @param n The number of conditioning factors. The integers in the combined list
#' that is created will be a permutation of 1, 2, \ldots ,n .
#'
#' @param \ldots x and/or y vector for \code{xylay} list. The remaining component will be
#' constructed as needed.
#'
#' @return For \code{xyLayout}: a list of class "xyLayout" suitable for the \code{xyLayout} argument of \code{strucplot}.
#'
#' @seealso \code{\link{strucplot}}
#'
#
#' @rdname stripless-internal
check2lvl <- function(d
  ,sep="." #sep character for paste()
  )
{
  lvl <- lapply(d,levels)
  all(sapply(lvl,length)==3) &&
  all(do.call(paste,c(d,list(sep=sep))) %in%
    make_2level_with_center(lvl,sep=sep))
}
#
#' @rdname stripless-internal
make_2level_with_center <- function(
  lvls ## a list of length 3 character vectors that give factor levels)
  ,sep = "." ## sep character for paste
)
{
   corners <-do.call(paste,c(do.call(expand.grid,
   lapply(lvls,function(x)x[c(1,3)])),list(sep=sep)))
   center <- do.call(paste,c(lapply(lvls,function(x)x[2]),list(sep=sep)))
   half <- seq_len(2^(length(lvls)-1))
  c(corners[half],center,corners[-half])
}
#
#' @rdname stripless-internal
makeSpacings <-   function(
  spacings
  ## vector giving spacing values. Will be extended as necessary by replicating last value
  ## Must all be >= 0
  ,levelLength
  ## vector giving sizes (lengths) of each level
)
{
  ## check vector values
  if(any(is.na(levelLength)))
    stop("NA values not allowed")
  stopifnot(all(levelLength == floor(levelLength) & levelLength >=1))
  #####
  k <- length(levelLength)
  if(length(spacings)< k)
    spacings <- c(spacings,rep(spacings[length(spacings)],k))
  x <- NULL
  for(i in seq_len(k)) x <-
    c(x,rep(c(spacings[i],x),levelLength[i]-1))
  x
}

#' @rdname stripless-internal
  chkSpacings <- function(spacings){
    if(!is.list(spacings)) spacings <- tryCatch (list(spacings),
        error = function(e)stop(e,"\nCannot coerce spacingsacings argument to a list"))
    stopifnot(length(spacings) <= 2,
      all(sapply(spacings,function(x)
        is.numeric(x) &&
        !any(is.na(x)) &&
        all(x >= 0) &&
        all(diff(x)>=0))
        ) )
    nm <- names(spacings) <- tolower(names(spacings))
    xy <- c("x","y")
    stopifnot(length(nm) <= 2, all(nm %in% c(xy,"")), all(!is.na(nm)))
    if(any(duplicated(nm)) && any(nm != ""))
      stop("Duplicated spacingsacings names")
    if(length(spacings) ==2){
      if(all(xy %in% nm)) spacings
      else {
        if(all(nm == "")) nm <- xy
        else nm[nm == ""] <- setdiff(xy,nm)
        structure(spacings,names = nm)
      }
    }
    else list(x = spacings[[1]], y = spacings[[1]])
  }

################### xyLayout Functions #############################

#' @rdname stripless-internal
  xyLayout <- function(xylay, ...)UseMethod("xyLayout")

#' @rdname stripless-internal
  xyLayout.list <- function(xylay = list(), n =
                              stop("Number of conditioning factors is missing"))
    ## xylay is the layout list.
    ## n is the number of conditioning variables
  {
    ### check length, names arguments for agreement, duplicates,  missing x or y
    ### etc. and "fix" if possible
    if(n < 1 || floor(n) != n)
      stop("n must be a nonnegative integer = the number of conditioning factors")
    xy <- c("x","y")
    ix <- unlist(xylay)
    nm <- tolower(names(xylay))
    sq.n <- seq_len(n)
    make_even_layout <- function(n)
    {
      if(n== 1)list(x=1, y = integer(0)) else {
        k <- ceiling(n/2)
        list(x=seq_len(k),y=seq.int(k+1,n)) ## return
      }
    }
    if(!length(ix)) xylay <- make_even_layout(n)
    else if(!((length(xylay) == 2) && all(xy %in% nm) &&
              !anyDuplicated(ix) && all(sort(ix) == sq.n))) {
      if(length(xylay) > 2)stop("Layout list cannot have length > 2")
      if(anyNA(ix))stop("NA's not permitted")
      else if(!is.numeric(ix))stop("Layout lists must be numeric")
      else if(any(!ix %in% sq.n) ||anyDuplicated(ix) )
        stop(sprintf("Layout lists must be non-duplicated integers between 1 and %d",
                     n))
      else {
        if(identical(character(0),nm))names(xylay)<- nm <- xy[seq_along(xylay)]
        else {
          nm[is.na(nm)] <- ""
          if(any(!nm %in% c(xy,""))) stop("Bad names")
          else if(anyDuplicated(nm[nm != ""]))stop("Duplicated names not permitted")
        }
        if(length(xylay) == 2){
          if(any(nm == ""))names(xylay)[nm == ""]<- setdiff(xy,nm)
          hasvals <- sapply(xylay,length)
          if(all(hasvals) && any(!sq.n %in% ix))
            stop("Layout lists do not match number of conditioning factors")
          else if(any(!hasvals))xylay[[which(!hasvals)]]<- setdiff(sq.n, ix)
        } else {
          if(!length(xylay[[1]]))xylay <- make_even_layout(n)
          else {
            xylay[[2]] <- setdiff(sq.n, ix)
            if(nm == "")names(xylay) <- xy
            else names(xylay)[2] <- setdiff(xy,nm)
          }
        }
      }
    }
    structure(xylay[xy], class = c("xyLayout",class(xylay)))
  }

#' @rdname stripless-internal
  xyLayout.matrix <- function(xylay,n)
  {
    nc <- ncol(xylay)
    if(!(nc %in% 1:2)) stop("Matrix must have 1 or 2 columns")
    if(!is.numeric(xylay))stop("Matrix must be numeric")
    if(anyNA(xylay))stop("NA's not allowed")
    xy <- c("x","y","",NA)
    nm <- tolower(dimnames(xylay)[[2]])
    if(identical(nm,character(0)) || all(nm %in% xy[3:4]))
      dimnames(xylay) <- list(NULL,xy[seq_len(nc)])
    else if(any(!nm %in% xy)) stop(
      "Column names must be absent,NA,'x', or 'y'")
    else dimnames(xylay)[[2]][!nm %in% xy[1:2]] <-setdiff(xy[1:2], nm)
    xyLayout(as.list(data.frame(xylay)),n)
  }
#' @rdname stripless-internal
  xyLayout.data.frame <- function(xylay,n)xyLayout(as.list(xylay,n))

#' @rdname stripless-internal
  xyLayout.default <- function(...,n)xyLayout(list(...),n)
