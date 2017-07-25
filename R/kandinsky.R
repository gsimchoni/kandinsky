#' Turn any dataset into a Kandinsky painting
#'
#' A function to turn any numeric vector in the range 0 to 1, or a given dataset,
#' into a Kandinsky-like painting, with a few common Kandinsky shapes, e.g.
#' rectangles and circles, drawn with wrappers around the \code{grid} package.
#' If the input is a dataset, the function will first convert it into a numeric
#' vector in the range 0 to 1.
#'
#' @param df a dataset (tested: \code{ts} object, \code{data.frame}, \code{tibble})
#' @param rv a numeric vector in the range 0 to 1, if \code{df} is not NULL,
#' default is \code{runif(1000)}
#'
#' @details 
#' In order to convert a dataset (\code{ts} object, \code{data.frame}, \code{tibble})
#' into a numeric vector in the range 0 to 1, the function first converts the dataset
#' character or factor columns into numeric columns, then normalizes all (now
#' numeric) columns to be in the 0 to 1 range, then vectorizes the dataset into
#' a single vector by row.
#' 
#' @seealso 
#' \link{http://giorasimchoni.com/}
#' 
#' @note 
#' This function is currently tested on a Windows 7 machine only
#' 
#' @examples
#' kandinsky(airquality)
#'
#' @export
kandinsky <- function(df = NULL, rv = runif(1000)) {
  
  library(grid)
  library(purrr)
  
  if (!is.null(df)) {
    rv <- normalizeAndVectorize(df)
  }
  
  grid.newpage()
  
  i <<- 0
  
  grid.rect(gp=gpar(fill=rgb(nex(rv),
                             nex(rv),
                             nex(rv),
                             nex(rv))))
  
  nRectangles <- floor(nex(rv) * 10) + 3
  nCircles <- floor(nex(rv) * 10) + 3
  nTriangles <- floor(nex(rv) * 10) + 3
  nArchs <- floor(nex(rv) * 10) + 3
  nTiltedRectangles <- floor(nex(rv) * 10) + 3
  nWaves <- floor(nex(rv) * 10) + 3
  nCrissCross <- floor(nex(rv) * 3) + 1
  
  walk(1:nRectangles, drawRectangle, rv)
  walk(1:nCircles, drawCircle, rv)
  walk(1:nTriangles, drawTriangle, rv)
  walk(1:nArchs, drawArch, rv)
  walk(1:nTiltedRectangles, drawTiltedRectangle, rv)
  walk(1:nWaves, drawWave, rv)
  walk(1:nCrissCross, drawCrissCross, rv)
}