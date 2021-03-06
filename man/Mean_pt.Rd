% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotn.R
\name{Mean_pt}
\alias{Mean_pt}
\title{Drawing mean points used with overdraw()}
\usage{
Mean_pt(
  x = NULL,
  formula = NULL,
  data = NULL,
  at = NULL,
  SD = F,
  SE = F,
  group = NULL,
  mean.column = 1,
  dev.column = 2,
  pch = 21,
  cex = 1.5,
  col.mean = NULL,
  col.bg = NULL,
  lwd.mean = 1,
  col.bar = NULL,
  lwd.bar = 1,
  lty = 1,
  length = 0.5,
  horizontal = F,
  plot = T
)
}
\arguments{
\item{x}{Data, e.g. numeric vector, formula, e.g. y ~ x, or other object containing analysis result}

\item{formula}{formula}

\item{data}{If formula is inputted in "x" or "formula" parameter, a data.frame (or list) from which the variables in formula should be taken.}

\item{at}{Drawing position}

\item{SD}{If set "T", standard deviation is drawn. Default is "F".}

\item{SE}{If set "T", standard error is drawn. Default is "F".}

\item{group}{Grouping factor}

\item{mean.column}{Column which means are stored.}

\item{dev.column}{Column which sd or se are stored.}

\item{pch}{pch, default is 21.}

\item{cex}{cex, default is 1.5.}

\item{col.mean}{mean points color, default is "#000000".}

\item{col.bg}{mean points background color, default is "#FFFFFF".}

\item{lwd.mean}{mean lwd, default is 1.}

\item{col.bar}{error bar color, default is "#000000".}

\item{lwd.bar}{error bar lwd, default is 1.}

\item{lty}{line type of error bar, default is 1.}

\item{length}{Length of vertical bar of tip in error bar, default is 0.5}

\item{horizontal}{horizontal, default is "F".}

\item{plot}{If set "F", calculate is only done.}
}
\description{
Drawing mean points used with overdraw()
}
\examples{
d <- data.frame(x = c(1:10, 11:20, 21:30, 31:40),
                group = rep(c("A","B","C", "D"), each = 10))
pos <- barplotn(x ~ group, data = d)
overdraw(Mean_pt(x ~ group, data = d, at = pos, SD = TRUE))

pos <- barplotn(x ~ group, data = d)
overdraw(Mean_pt("x", data = d, group = "group", at = pos,
                 SE = TRUE))

pos <- barplotn(x ~ group, data = d)
overdraw(Mean_pt(d, group = "group", at = pos, SE = TRUE))
#d[,1] is data

pos <- barplotn(x ~ group, data = d)
md <- Mean_pt(x ~ group, data = d, SE = TRUE, plot = FALSE)
overdraw(Mean_pt(md, at = pos, SE = TRUE))

}
\seealso{
[plotn::overdraw]
}
