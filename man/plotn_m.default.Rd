% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotn.R
\name{plotn_m.default}
\alias{plotn_m.default}
\title{Drawing a figure like plot()}
\usage{
plotn_m.default(
  x = NULL,
  y = NULL,
  data = NULL,
  ...,
  xlim = NULL,
  ylim = NULL,
  las = 1,
  cex.axis = 1.1,
  cex.lab = 1.3,
  font.lab = 2,
  pch = 16,
  col.dot = NULL,
  col.fill = NULL,
  col.line = NULL,
  col.bor = "transparent",
  col.bg = "#FFFFFF",
  legend = F,
  pos.leg = "outright",
  pch.leg = NULL,
  bty.leg = "n",
  bg.leg = "transparent",
  lty = 1,
  lwd.dot = 1,
  lwd.line = 1,
  pt.cex.leg = 1.5,
  tx.cex.leg = 1.1,
  pt.col.leg = NULL,
  pt.bg.leg = NULL,
  lty.leg = NULL,
  pt.lwd.leg = NULL,
  ln.lwd.leg = NULL,
  tx.col.leg = NULL,
  leg.lab = NULL,
  leg.sp = 2.5,
  inset = 1,
  leg.title = NULL,
  tit.col.leg = NULL,
  mode = "s",
  group = NULL,
  fill = F,
  line = F,
  density = NA,
  angle = 45,
  warning = F,
  mar = c(3.8, 3.8, 1, 1),
  mgp = c(2.5, 0.5, 0),
  tcl = -0.2,
  inversion = F,
  inv.col = "#FFFFFF"
)
}
\arguments{
\item{x}{Data, e.g. numeric vector, formula, e.g. y ~ x, or other object containing analysis result}

\item{y}{If numeric vector is inputted in "formula" parameter, numeric vector is also inputted in y}

\item{data}{If formula is inputted in "x" or "formula" parameter, a data.frame (or list) from which the variables in formula should be taken.}

\item{...}{Argument to be passed to methods. Please see plot().}

\item{xlim}{x limit}

\item{ylim}{y limit}

\item{las}{las, defauls is 1}

\item{cex.axis}{axis cex, default is 1.1}

\item{cex.lab}{label cex, default is 1.3}

\item{font.lab}{label font size, default is 2}

\item{pch}{pch, default is 16}

\item{col.dot}{points color}

\item{col.fill}{fill color}

\item{col.line}{line color}

\item{col.bor}{border color}

\item{col.bg}{background color}

\item{legend}{If legend is needed, set "T". Default is "F".}

\item{pos.leg}{Legend position. In addition to position of legend(), "outtopright, "outright", "outbottomright" and "outbottom" are able to select. Default is "outright".}

\item{pch.leg}{Legend pch}

\item{bty.leg}{Legend box type. Default is ""n.}

\item{bg.leg}{Legend background}

\item{lty}{lty}

\item{lwd.dot}{Points lwd, default is 1.}

\item{lwd.line}{Line lwd, default is 1.}

\item{pt.cex.leg}{Points cex in legend, default is 1.5.}

\item{tx.cex.leg}{Text cex in legend, default is 1.1.}

\item{pt.col.leg}{Points color in legend.}

\item{pt.bg.leg}{Points background color in legend.}

\item{lty.leg}{lty in legend.}

\item{pt.lwd.leg}{Points lwd in legend.}

\item{ln.lwd.leg}{Line lwd in legend.}

\item{tx.col.leg}{Text color in legend.}

\item{leg.lab}{Legend label}

\item{leg.sp}{Legend space, default is 2.5.}

\item{inset}{Legend inset, default is 1.}

\item{leg.title}{Legend title}

\item{tit.col.leg}{Legend title color}

\item{mode}{Plotting mode. Setting "s" is single group plot, while setting "m" is multiple groups plot. Default is "s".}

\item{group}{Grouping factor in setting mode = "m".}

\item{fill}{If fill color is needed, set "T". Default is "F".}

\item{line}{If line is needed, set "T". Default is "F".}

\item{density}{Fill density}

\item{angle}{Fill stripe angle, default is 45 degree.}

\item{warning}{If it is set with T and plot is not able to outputted with default settings, warning message is outputted}

\item{mar}{mar, default is c(3.8,3.8,1,1).}

\item{mgp}{mgp, default is c(2.5,0.5,0).}

\item{tcl}{tcl, default is -0.2.}

\item{inversion}{Inversion mode. If set "T", plot is drawn with inversion color. Default is "F".}

\item{inv.col}{Inversion color, if set inversion = "T". Default is "#FFFFFF".}
}
\description{
Drawing a figure like plot()
}
