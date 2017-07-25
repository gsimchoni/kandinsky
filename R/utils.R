gCurve <- function (expr, from, to, n = 101,
                    gp = gpar(),
                    default.units = "npc", vp = NULL,
                    name = NULL, draw = TRUE, xname = "x", ...) 
{
  sexpr <- substitute(expr)
  if (is.name(sexpr)) {
    expr <- call(as.character(sexpr), as.name(xname))
  }
  else {
    if (!((is.call(sexpr) || is.expression(sexpr)) && xname %in% 
          all.vars(sexpr))) 
      stop(gettextf("'expr' must be a function, or a call or an expression containing '%s'", 
                    xname), domain = NA)
    expr <- sexpr
  }
  x <- seq.int(from, to, length.out = n)
  ll <- list(x = x)
  y <- eval(expr, envir = ll, enclos = parent.frame())
  if (length(y) != length(x)) 
    stop("'expr' did not evaluate to an object of length 'n'")
  x <- (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))
  y <- (y - min(y, na.rm = T))/(max(y, na.rm = T) - min(y, na.rm = T))
  grid.lines(x = x, y = y, default.units = default.units, gp = gp, vp = vp, ...)
  invisible(list(x = x, y = y))
}

gCrissCross <- function(n = 5, rv, gp = gpar(), vp = NULL, ...) {
  x0 <- nex(rv) * 0.1
  ccGap <- nex(rv) * 0.4 + 0.3
  y0 <- nex(rv) * 0.5
  for (i in 1:n) {
    x0 <- x0 + nex(rv) * 0.1 + 0.1
    y0 <- y0 + nex(rv) * 0.1 - 0.05
    y1 <- y0 + ccGap + nex(rv) * 0.1 - 0.05
    grid.segments(x0, y0, x0 + ccGap, y1, vp = vp, gp = gp, ...)
    grid.segments(x0, y1, x0 + ccGap, y0, vp = vp, gp = gp, ...)
  }
}

nex <- function(rv) {
  if (i == length(rv)) {
    i <<- 0
  }
  i <<- i + 1
  rv[i]
}

drawRectangle <- function(i, rv) {
  grid.rect(x = nex(rv), y = nex(rv), width = nex(rv), height = nex(rv),
            gp = gpar(col = NA,
                      fill=rgb(nex(rv),
                               nex(rv),
                               nex(rv),
                               nex(rv))))
}

drawCircle <- function(i, rv) {
  grid.circle(x = nex(rv), y = nex(rv), r = nex(rv),
              gp = gpar(
                lwd = floor(nex(rv) * 100),
                col = rgb(nex(rv),
                          nex(rv),
                          nex(rv),
                          nex(rv)),
                fill=rgb(nex(rv),
                         nex(rv),
                         nex(rv),
                         nex(rv))))
}

drawTriangle <- function(i, rv) {
  grid.polygon(x = c(nex(rv), nex(rv), nex(rv)), y = c(nex(rv), nex(rv), nex(rv)),
               gp = gpar(col = NA,
                         fill=rgb(nex(rv),
                                  nex(rv),
                                  nex(rv),
                                  nex(rv))))
}

drawArch <- function(i, rv) {
  grid.curve(nex(rv), nex(rv), nex(rv), nex(rv),
             curvature = nex(rv) * 2 + (-1), square = FALSE, ncp = floor(nex(rv) * 100),
             gp = gpar(lwd = nex(rv) * 10,
                       col = rgb(nex(rv),
                                 nex(rv),
                                 nex(rv),
                                 1)))
}

drawTiltedRectangle <- function(i, rv) {
  vp1 <- viewport(x = nex(rv), y = nex(rv), width = nex(rv), height = nex(rv), angle = nex(rv) * 360)
  grid.rect(x = nex(rv), y = nex(rv), width = nex(rv), height = nex(rv),
            vp = vp1,
            gp = gpar(col = NA,
                      fill=rgb(nex(rv),
                               nex(rv),
                               nex(rv),
                               nex(rv))))
}

drawWave <- function(i, rv) {
  vp2 <- viewport(x = nex(rv), y = nex(rv), width = nex(rv), height = nex(rv), angle = nex(rv) * 360)
  gCurve(sin(x)/(x), nex(rv) * 4 + 1, nex(rv) * 40 + 10, vp = vp2,
         gp = gpar(lwd = nex(rv) * 10,
                   col = rgb(nex(rv),
                             nex(rv),
                             nex(rv),
                             1)))
}

drawCrissCross <- function(i, rv) {
  vp3 <- viewport(x = nex(rv), y = nex(rv), width = nex(rv), height = nex(rv), clip = "off")
  gCrissCross(vp = vp3, rv = rv,
               gp = gpar(lwd = nex(rv) * 10,
                         col = rgb(nex(rv),
                                   nex(rv),
                                   nex(rv),
                                   1)))
}

zeroOneNormalize <- function(x) {
  if (!is.numeric(x)) {
    if (is.factor(x)) {
      x <- as.numeric(unclass(x))
    } else {
      x <- as.numeric(unclass(as.factor(x)))
    }
  }
  if (length(unique(x)) == 1) {
    return(rep(0.5, length(x)))
  } else {
    x <- (x - min(x, na.rm = TRUE)) /
      (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    x[is.na(x)] <- 0.5
    return(x)
  }
}

normalizeAndVectorize <- function(df) {
  do.call(c, purrr::map_df(tibble::as_tibble(df), zeroOneNormalize) %>% t() %>% tibble::as_tibble())
}