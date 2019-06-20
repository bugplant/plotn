#' Genelating dafault color in plotn
#'
#' @param palette Color palette, "default", "ggplot2" and palettes in ggsci
#' @param palette_types "Continuous" or "discrete" in ggplot2 palette or palette types in ggsci
#' @param number Number of colors to generate
#' @param alpha Transparency
#'
#' @importFrom ggsci pal_npg pal_aaas pal_nejm pal_lancet pal_jama pal_jco pal_ucscgb pal_d3 pal_locuszoom pal_igv pal_uchicago pal_startrek pal_tron pal_futurama pal_rickandmorty pal_simpsons pal_gsea pal_material
#'
#' @export
#'
col_genelator <- function (palette = "d3",
                           palette_types = NULL,
                           number = 10,
                           alpha = 1){

  ggColorHue <- function(n, l=65) {
    hues <- seq(15, 375, length=n+1)
    hcl(h=hues, l=l, c=100)[1:n]
  }

  if(palette == "default"){
    default <- "default"
  } else {
    if(palette == "ggplot2"){

      if(length(palette_types) == 0){
        palette_types <- "discrete"
      }

      if(palette_types == "continuous"){

        cols <- colorRampPalette(c("#132B43", "#56B1F7"))(n = number)
        al <- sub("#FF0000", "", rgb(1, 0, 0, alpha = alpha))
        default <- paste(cols, al, sep = "")

      } else {

        cols <- ggColorHue(n = number)
        al <- sub("#FF0000", "", rgb(1, 0, 0, alpha = alpha))
        default <- paste(cols, al, sep = "")

      }

    } else {
      pal_type <- switch(palette,
                     "npg" = "nrc",
                     "aaas" = "default",
                     "nejm" = "default",
                     "lancet" = "lanonc",
                     "jama" = "default",
                     "jco" = "default",
                     "ucscgb" = "default",
                     "d3" = palette_types,
                     "locuszoom" = "default",
                     "igv" = palette_types,
                     "uchicago" = palette_types,
                     "startrek" = "uniform",
                     "tron" = "legacy",
                     "futurama" = "planetexpress",
                     "rickandmorty" = "schwifty",
                     "simpsons" = "springfield",
                     "gsea" = "default",
                     "material" = palette_types,
                     stop("invalid palette name"))

      if(length(palette_types) == 0){
        palette_types <- switch(palette,
                           "d3" = "category10",
                           "igv" = "default",
                           "uchicago" = "default",
                           "material" = "red")
      }

      command <- paste0("pal_", palette,"(pal_type", ", alpha = ", alpha, ")(", number,")")
      eval(parse(text = command))
    }
  }
}

#' Drawing a figure like plot()
#'
#' @param formula Data, e.g. numeric vector, formula, e.g. y ~ x, or other object containing analysis result
#' @param y If numeric vector is inputted in "formula" parameter, numeric vector is also inputted in y
#' @param data If formula is inputted in "formula" parameter, a data.frame (or list) from which the variables in formula should be taken.
#' @param ... Argument to be passed to methods. Please see plot().
#' @param xlim x limit
#' @param ylim y limit
#' @param las las, defauls is 1
#' @param cex.axis axis cex, default is 1.1
#' @param cex.lab label cex, default is 1.3
#' @param font.lab label font size, default is 2
#' @param pch pch, default is 16
#' @param col.dot points color
#' @param col.fill fill color
#' @param col.line line color
#' @param col.bor border color
#' @param col.bg background color
#' @param legend If legend is needed, set "T". Default is "F".
#' @param pos.leg Legend position. In addition to position of legend(), "outtopright, "outright", "outbottomright" and "outbottom" are able to select. Default is "outright".
#' @param pch.leg Legend pch
#' @param bty.leg Legend box type. Default is ""n.
#' @param bg.leg Legend background
#' @param lty lty
#' @param lwd.dot Points lwd, default is 1.
#' @param lwd.line Line lwd, default is 1.
#' @param pt.cex.leg Points cex in legend, default is 1.5.
#' @param tx.cex.leg Text cex in legend, default is 1.1.
#' @param pt.col.leg Points color in legend.
#' @param pt.bg.leg Points background color in legend.
#' @param lty.leg lty in legend.
#' @param pt.lwd.leg Points lwd in legend.
#' @param ln.lwd.leg Line lwd in legend.
#' @param tx.col.leg Text color in legend.
#' @param leg.lab Legend label
#' @param leg.sp Legend space, default is 2.5.
#' @param inset Legend inset, default is 1.
#' @param leg.title Legend title
#' @param tit.col.leg Legend title color
#' @param mode Plotting mode. Setting "s" is single group plot, while setting "m" is multiple groups plot. Default is "s".
#' @param group Grouping factor in setting mode = "m".
#' @param fill If fill color is needed, set "T". Default is "F".
#' @param line If line is needed, set "T". Default is "F".
#' @param density Fill density
#' @param angle Fill stripe angle, default is 45 degree.
#' @param mar mar, default is c(3.8,3.8,1,1).
#' @param mgp mgp, default is c(2.5,0.5,0).
#' @param tcl tcl, default is -0.2.
#' @param inversion Inversion mode. If set "T", plot is drawn with inversion color. Default is "F".
#' @param inv.col Inversion color, if set inversion = "T". Default is "#FFFFFF".
#'
#' @importFrom grDevices boxplot.stats colorRampPalette hcl rgb
#' @importFrom graphics arrows axis barplot box boxplot hist lines matplot par plot points polygon abline
#' @importFrom stats density na.omit sd terms var
#'
#' @export
#'
plotn <- function(formula, y = NULL, data = NULL, ...,
                  xlim = NULL,
                  ylim = NULL,
                  las = 1,
                  cex.axis = 1.1,
                  cex.lab = 1.3,
                  font.lab = 2,
                  pch = 16,
                  col.dot = c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF",
                              "#D62728FF", "#9467BDFF", "#8C564BFF",
                              "#E377C2FF", "#7F7F7FFF", "#BCBD22FF", "#17BECFFF"),
                  col.fill = c("#1F77B47F", "#FF7F0E7F", "#2CA02C7F",
                               "#D627287F", "#9467BD7F", "#8C564B7F",
                               "#E377C27F", "#7F7F7F7F", "#BCBD227F", "#17BECF7F"),
                  col.line = c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF",
                               "#D62728FF", "#9467BDFF", "#8C564BFF",
                               "#E377C2FF", "#7F7F7FFF", "#BCBD22FF", "#17BECFFF"),
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
                  mar = c(3.8,3.8,1,1),
                  mgp = c(2.5,0.5,0),
                  tcl = -0.2,
                  inversion = F,
                  inv.col = "#FFFFFF"){

  is.formula <- function(x){
    class(x)=="formula"
  }


  if (inversion == T){
    bg <- "transparent"
    col <- inv.col
  } else {
    bg <- "#FFFFFF"
    col <- "#000000"
  }


  if(legend == T){
    switch (pos.leg,
            "outtopright" = eval(mar[4] <- mar[4]+leg.sp),
            "outright" = eval(mar[4] <- mar[4]+leg.sp),
            "outbottomright" = eval(mar[4] <- mar[4]+leg.sp),
            "outbottom" = eval(mar[1] <- mar[1]+leg.sp)
    )
  }

  mode <-  switch(mode,
                  "s" = mode,
                  "m" = mode,
                  "s")

  par.old <- par(mar = mar, mgp = mgp, tcl = tcl, bg = bg, fg = col)
  on.exit(par(par.old))

  if ( mode == "s"){

    if(!is.formula(formula)) {

      x <- formula

      if(!length(y) == 0){
        plot(x = x, y = y, ..., xlim = xlim, ylim = ylim,
             las = las, cex.axis = cex.axis, col = col.dot[1],
             cex.lab = cex.lab, font.lab = font.lab,
             col.axis = col, col.lab = col, pch = pch, lty = lty, lwd = lwd.dot)

      } else {

        error <- NULL
        error <- try(plot(x, ..., xlim = xlim, ylim = ylim,las = las, cex.axis = cex.axis,
                          col = col.dot[1], cex.lab = cex.lab, font.lab = font.lab,
                          col.axis = col, col.lab = col, pch = pch, lty = lty,
                          lwd = lwd.dot),
                     silent = T)

        if (class(error) == "try-error") {
          warning("Data wasn't plotted with default settings, so trying to plot with different settings.")
          plot(x, ..., las = las, cex.axis = cex.axis,
               col = col.dot,cex.lab = cex.lab, font.lab = font.lab,
               col.axis = col, col.lab = col, pch = pch, lty = lty,
               lwd = lwd.dot)
        }

      }

    } else {

      if(is.null(data)){

        y <- eval(attr(terms(formula), "variables")[[2]])
        x <- eval(attr(terms(formula), "variables")[[3]])

      } else {

        y <- data[,as.character(attr(terms(formula), "variables")[[2]])]
        x <- data[,as.character(attr(terms(formula), "variables")[[3]])]

      }

      plot(formula, data = data, ...,
           xlim = xlim, ylim = ylim, col = col.dot[1],
           las = las, cex.axis = cex.axis,
           cex.lab = cex.lab, font.lab = font.lab,
           col.axis = col, col.lab = col, pch = pch, lty = lty, lwd = lwd.dot)
    }

    if (fill == T) {
      polygon(x, y, col = col.fill[1], border = col.bor,
              density = density, angle = angle, lwd = lwd.line)
    }

    if (line == T) {
      lines(x, y, col = col.line, lty = lty, lwd = lwd.line)
    }

  } else {

    j <- (ncol(formula) > 1)||(ncol(y) > 1)
    j[is.na(j)] <- F

    if(j){

      x <- as.matrix(formula)

      if(ncol(x) > 1){
        n <- ncol(x)
        names <- colnames(x)
      } else {
        n <- ncol(y)
        names <- colnames(y)
      }

      col.dot <- rep(col.dot, length = n)
      col.fill <- rep(col.fill, length = n)
      col.line <- rep(col.line, length = n)
      lty <- rep(lty, length = n)
      pch <- rep(pch, length = n)
      density <- rep(density, length = n)
      angle <- rep(angle, length = n)


      if(!length(y) == 0){
        matplot(x = x, y = y, ..., pch = pch,
                las = las, cex.axis = cex.axis, xlim = xlim, ylim = ylim,
                cex.lab = cex.lab, font.lab = font.lab, col = col.dot,
                col.axis = col, col.lab = col,
                lty = lty, lwd = lwd.dot)
      } else {
        matplot(x = x, ..., pch = pch,
                las = las, cex.axis = cex.axis, xlim = xlim, ylim = ylim,
                cex.lab = cex.lab, font.lab = font.lab, col = col.dot,
                col.axis = col, col.lab = col,
                lty = lty, lwd = lwd.dot)
      }

      for(i in 1:n){
        if(ncol(x) > 1){
          if(fill == T){
            polygon(c(1:length(x[,1])), x[,i], col = col.fill[i], border = col.bor[i],
                    density = density[i], angle = angle[i], lwd = lwd.line)
          }
          if(line == T){
            lines(c(1:length(x[,1])), x[,i], col = col.line[i], lty = lty[i], lwd = lwd.line)
          }
        } else {
          if(fill == T){
            polygon(x, y[,i], col = col.fill[i], border = col.bor[i],
                    density = density[i], angle = angle[i], lwd = lwd.line)
          }
          if(line == T){
            lines(x, y[,i], col = col.line[i], lty = lty[i], lwd = lwd.line)
          }

        }
      }

    } else {

      if(is.character(group)){
        g <- data[,group]
      } else {
        g <- group
      }

      names <- levels(as.factor(g))
      col.dot <- rep(col.dot, length = length(names))
      col.fill <- rep(col.fill, length = length(names))
      col.line <- rep(col.line, length = length(names))
      lty <- rep(lty, length = length(names))
      pch <- rep(pch, length = length(names))
      density <- rep(density, length = length(names))
      angle <- rep(angle, length = length(names))

      if(!is.formula(formula)) {

        x <- formula

        if(length(xlim)==0){
          xlim <- range(x, na.rm = T)
        }
        if(length(ylim)==0){
          ylim <- range(y, na.rm = T)
        }

        plot(x = x, y = y, ...,
             las = las, cex.axis = cex.axis, xlim = xlim, ylim = ylim,
             cex.lab = cex.lab, font.lab = font.lab, col = col.dot[as.factor(g)],
             bg = col.bg, pch = pch[as.factor(g)], col.axis = col, col.lab = col,
             lwd = lwd.dot)

      } else {

        if(is.null(data)){

          y <- eval(attr(terms(formula), "variables")[[2]])
          x <- eval(attr(terms(formula), "variables")[[3]])

        } else {

          y <- data[,as.character(attr(terms(formula), "variables")[[2]])]
          x <- data[,as.character(attr(terms(formula), "variables")[[3]])]

        }

        if(length(xlim)==0){
          xlim <- range(x, na.rm = T)
        }
        if(length(ylim)==0){
          ylim <- range(y, na.rm = T)
        }

        plot(formula = formula, data = data, ...,
             las = las, cex.axis = cex.axis, xlim = xlim, ylim = ylim,
             cex.lab = cex.lab, font.lab = font.lab, col = col.dot[as.factor(g)],
             bg = col.bg, pch = pch[as.factor(g)], col.axis = col, col.lab = col,
             lwd = lwd.dot)

      }

      for (i in 1:length(names)){
        xx <- x[g == names[i]]
        yy <- y[g == names[i]]

        if (fill == T) {
          polygon(x = xx, y = yy, col = col.fill[i], border = col.bor[i],
                  density = density[i], angle = angle[i], lwd = lwd.line)
        }
        if (line == T) {
          lines(x = xx, y = yy, col = col.line[i], lty = lty[i], lwd = lwd.line)
        }

      }

    }

  }

  if(legend == T){

    par(xpd = T)
    par.old$xpd <- F

    if (length(leg.lab) == 0){
      if(mode == "s"){
        leg.lab <- 1
      } else {
        if (j){
          if(length(names) == 0){
            leg.lab <- 1:n
          } else {
            leg.lab <- names
          }
        } else {
          leg.lab <- names
        }
      }
    }

    if (length(pch.leg) == 0){
      pch.leg <- pch
    }

    if (length(pt.col.leg) == 0){
      pt.col.leg <- col.dot
    }

    if (length(pt.bg.leg) == 0){
      pt.bg.leg <- bg
    }

    if (length(tx.col.leg) == 0){
      tx.col.leg <- col
    }

    if (length(lty.leg) == 0){
      if(line == T){
        lty.leg <- lty
      } else {
        lty.leg <- 0
      }
    }

    if (length(pt.lwd.leg) == 0){
      pt.lwd.leg <- lwd.dot
    }

    if (length(ln.lwd.leg) == 0){
      ln.lwd.leg <- lwd.line
    }

    if (length(tit.col.leg) == 0){
      tit.col.leg <- col
    }

    x.intersp <- 1
    if (lty.leg[1] == 0){
      x.intersp <- 0
    }

    if(pos.leg =="outbottom"){
      horiz <-  T
      inset <- inset*1.1
    } else {
      horiz <- F
    }

    ins <- 0

    switch (pos.leg,
            "outtopright" = eval(parse(text = "pos.leg <- 'topleft'; ins <- c(inset,0)")),
            "outright" = eval(parse(text = "pos.leg <- 'left'; ins <- c(inset,0)")),
            "outbottomright" = eval(parse(text = "pos.leg <- 'bottomleft'; ins <- c(inset,0)")),
            "outbottom" = eval(parse(text = "pos.leg <- 'bottom'; ins <- c(0,inset)"))
    )

    legend(pos.leg[1] , pos.leg[2], inset = ins,
           legend = leg.lab, col = pt.col.leg, lty = lty.leg,
           pt.bg = pt.bg.leg, pch = pch.leg, pt.lwd = pt.lwd.leg,
           lwd = ln.lwd.leg, x.intersp = x.intersp,
           bty = bty.leg, bg = bg.leg, text.col = tx.col.leg,
           pt.cex = pt.cex.leg, cex = tx.cex.leg, horiz = horiz,
           title = leg.title, title.col = tit.col.leg)
  }

}

#' Drawing a figure like boxplot()
#'
#' @param formula Data, e.g. numeric vector, formula, e.g. y ~ x, or other object containing analysis result
#' @param data If formula is inputted in "formula" parameter, a data.frame (or list) from which the variables in formula should be taken.
#' @param ... Argument to be passed to methods. Please see boxplot().
#' @param las las, defauls is 1
#' @param cex.axis axis cex, default is 1.1
#' @param cex.lab label cex, default is 1.3
#' @param font.lab label font size, default is 2
#' @param lty lty
#' @param outline If set "T", outliners are drawn. Default is "F".
#' @param all If set "T", all points are drawn. Default is "T".
#' @param staplelwd staplelwd, default is "NA".
#' @param boxwex boxwex, default is 0.5.
#' @param xlab x label
#' @param ylab y label
#' @param names names
#' @param xlim x limit
#' @param ylim y limit
#' @param notch notch, default is "F".
#' @param horizontal horizontal, default is "F".
#' @param xaxt xaxt, default is "s".
#' @param yaxt yaxt, default is "s".
#' @param col.fill fill color
#' @param col.bor border color
#' @param col.dot points color
#' @param col.stat Mean and error bar color
#' @param col.bg background color
#' @param pch.dot points pch, default is 16
#' @param pch.stat mean points pch, default is 21
#' @param cex.dot points cex, default is 0.7
#' @param cex.stat mean points cex, default is 1
#' @param lwd.bor box border lwd, default is 1
#' @param lwd.stat mean and error bar lwd, default is 1
#' @param lwd.dot points lwd, default is 1
#' @param noise scatter level of points, default is 1
#' @param side move direction of boxplot, "left", "center" and "right" are able to select. Default is "center".
#' @param side.sp Magnitude of move of boxplot, default is 0.3.
#' @param reflect If set "T", points are not drawn in next to boxplot and reflected. Default is "T".
#' @param Mean If set "T", mean points are drawn. Default is "F".
#' @param SD If set "T", standard deviation is drawn. Default is "F".
#' @param SE If set "T", standard error is drawn. Default is "F".
#' @param legend If legend is needed, set "T". Default is "F".
#' @param pos.leg Legend position. In addition to position of legend(), "outtopright, "outright", "outbottomright" and "outbottom" are able to select. Default is "outright".
#' @param pch.leg Legend pch, default is 22.
#' @param bty.leg Legend box type. Default is ""n.
#' @param bg.leg Legend background
#' @param pt.cex.leg Points cex in legend, default is 2.
#' @param tx.cex.leg Text cex in legend, default is 1.1.
#' @param pt.col.leg Points color in legend.
#' @param pt.bg.leg Points background color in legend.
#' @param tx.col.leg Text color in legend.
#' @param leg.lab Legend label
#' @param leg.sp Legend space, default is 2.5.
#' @param inset Legend inset, default is 1.
#' @param leg.title Legend title
#' @param tit.col.leg Legend title color
#' @param mar mar, default is c(2,3.8,1,1).
#' @param mgp mgp, default is c(2.5,0.5,0).
#' @param tcl tcl, default is -0.2.
#' @param add If set "T", boxplot is able to overdrawn on previous boxplot. Default is "F".
#' @param inversion Inversion mode. If set "T", plot is drawn with inversion color. Default is "F".
#' @param inv.col Inversion color, if set inversion = "T". Default is "#FFFFFF".
#'
#' @importFrom grDevices boxplot.stats colorRampPalette hcl rgb
#' @importFrom graphics arrows axis barplot box boxplot hist lines matplot par plot points polygon abline
#' @importFrom stats density na.omit sd terms var
#'
#' @export
#'
boxplotn <- function(formula, data = NULL, ...,
                     las = 1,
                     cex.axis = 1.1,
                     cex.lab = 1.3,
                     font.lab = 2,
                     lty = 1,
                     outline = F,
                     all = T,
                     staplelwd = NA,
                     boxwex = 0.5,
                     xlab = NULL,
                     ylab = NULL,
                     names = NULL,
                     xlim = NULL,
                     ylim = NULL,
                     notch = F,
                     horizontal = F,
                     xaxt = "s",
                     yaxt = "s",
                     col.fill = c("#1F77B47F", "#FF7F0E7F", "#2CA02C7F",
                                  "#D627287F", "#9467BD7F", "#8C564B7F",
                                  "#E377C27F", "#7F7F7F7F", "#BCBD227F", "#17BECF7F"),
                     col.bor = c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF",
                                 "#D62728FF", "#9467BDFF", "#8C564BFF",
                                 "#E377C2FF", "#7F7F7FFF", "#BCBD22FF", "#17BECFFF"),
                     col.dot = c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF",
                                 "#D62728FF", "#9467BDFF", "#8C564BFF",
                                 "#E377C2FF", "#7F7F7FFF", "#BCBD22FF", "#17BECFFF"),
                     col.stat = "default",
                     col.bg = "#FFFFFF",
                     pch.dot = 16,
                     pch.stat = 21,
                     cex.dot = 0.7,
                     cex.stat = 1,
                     lwd.bor = 1,
                     lwd.stat = 1,
                     lwd.dot = 1,
                     noise = 1,
                     side = "center",
                     side.sp = 0.3,
                     reflect = T,
                     Mean = F,
                     SE = F,
                     SD =F,
                     legend = F,
                     pos.leg = "outright",
                     pch.leg = 22,
                     bty.leg = "n",
                     bg.leg = "transparent",
                     pt.cex.leg = 2,
                     tx.cex.leg = 1.1,
                     pt.col.leg = NULL,
                     pt.bg.leg = NULL,
                     tx.col.leg = NULL,
                     leg.lab = NULL,
                     leg.sp = 2.5,
                     inset = 1,
                     leg.title = NULL,
                     tit.col.leg = NULL,
                     mar = c(2,3.8,1,1),
                     mgp = c(2.5,0.5,0),
                     tcl = -0.2,
                     add = F,
                     inversion = F,
                     inv.col = "#FFFFFF"){

  is.formula <- function(x){
    class(x)=="formula"
  }

  se  <-  function(x){
    y  <-  x[!is.na(x)]
    sqrt(var(as.vector(y))/length(y))
  }

  if (inversion == T){
    bg <- "transparent"
    col <- inv.col
  } else {
    bg <- "#FFFFFF"
    col <- "#000000"
  }

  if(!is.na(col.fill[1])){
    if(col.fill[1] == "default"){
      col.fill <- bg
    }
  }
  if(!is.na(col.bor[1])){
    if(col.bor[1] == "default"){
      col.bor <- col
    }
  }
  if(!is.na(col.dot[1])){
    if(col.dot[1] == "default"){
      col.dot <- col
    }
  }
  if(!is.na(col.stat[1])){
    if(col.stat[1] == "default"){
      col.stat <- col
    }
  }

  if(horizontal == T){
    pos <- 2
    ls <- c(yaxt,xaxt)
    xaxt <- ls[1]
    yaxt <- ls[2]
    ls <- c(mar[2],mar[1])
    mar[1] <- ls[1]
    mar[2] <- ls[2]
  } else {
    pos <- 1
  }

  if(legend == T){
    switch (pos.leg,
            "outtopright" = eval(mar[4] <- mar[4]+leg.sp),
            "outright" = eval(mar[4] <- mar[4]+leg.sp),
            "outbottomright" = eval(mar[4] <- mar[4]+leg.sp),
            "outbottom" = eval(mar[1] <- mar[1]+leg.sp)
    )
  }

  side <- switch(side,
                 "center" = side,
                 "right" = side,
                 "left" = side,
                 "center")

  g <- switch(side,
              "center" = 0,
              "right" = side.sp,
              "left" = -side.sp)

  noise <- noise*10

  par.old <- par(mar = mar, mgp = mgp, tcl = tcl, bg = bg, fg = col)
  on.exit(par(par.old))

  if (!is.formula(formula)){
    nn <- "x"

    if(length(names)==0){
      names <- nn
    }

    if(horizontal == T){
      if(length(ylab)==0){
        ylab <- "group"
      }
      if(length(xlab)==0){
        xlab <- "data"
      }
    } else {
      if(length(xlab)==0){
        xlab <- "group"
      }
      if(length(ylab)==0){
        ylab <- "data"
      }
    }

    if(horizontal == T){
      ylim_t <- ylim
      if(length(xlim)==0){
        ylim <- range(y, na.rm = T)
      } else {
        ylim <- xlim
      }
      xlim <- ylim_t
    } else {
      if(length(ylim)==0){
        ylim <- range(y, na.rm = T)
      }
    }

  } else {
    if(is.null(data)){

      y <- eval(attr(terms(formula), "variables")[[2]])
      group <- eval(attr(terms(formula), "variables")[[3]])
      nn <- levels(as.factor(group))
      if(length(attr(terms(formula), "variables"))-1 > 2){
        for(i in 4:length(attr(terms(formula), "variables"))){
          nn <- paste(nn, rep(levels(as.factor(
            eval(attr(terms(formula), "variables")[[i]])
          )
          ), each = length(nn)), sep = ".")
          group <- paste(group, eval(attr(terms(formula), "variables")[[i]]), sep = ".")
        }
      }
      group <- factor(group, levels = nn)

      if(horizontal == T){
        if(length(xlab)==0){
          n <- as.character(attr(terms(formula), "variables")[[2]])
          xlab <- paste(n[2], n[1], n[3], sep = "")
        }
      } else {
        if(length(ylab)==0){
          n <- as.character(attr(terms(formula), "variables")[[2]])
          ylab <- paste(n[2], n[1], n[3], sep = "")
        }
      }

    } else {

      y <- data[,as.character(attr(terms(formula), "variables")[[2]])]
      group <- data[,as.character(attr(terms(formula), "variables")[[3]])]
      nn <- levels(as.factor(group))
      if(length(attr(terms(formula), "variables"))-1 > 2){
        for(i in 4:length(attr(terms(formula), "variables"))){
          nn <- paste(nn, rep(levels(as.factor(
            data[,as.character(attr(terms(formula), "variables")[[i]])]
          )
          ), each = length(nn)), sep = ".")
          group <- paste(group, data[,as.character(attr(terms(formula), "variables")[[i]])], sep = ".")
        }
      }
      group <- factor(group, levels = nn)

      if(horizontal == T){
        if(length(xlab)==0){
          xlab <- as.character(attr(terms(formula), "variables")[[2]])
        }
      } else {
        if(length(ylab)==0){
          ylab <- as.character(attr(terms(formula), "variables")[[2]])
        }
      }
    }

    if(length(names)==0){
      names <- nn
    }

    if(horizontal == T){
      if(length(ylab)==0){
        ylab <- "group"
      }
    } else {
      if(length(xlab)==0){
        xlab <- "group"
      }
    }

    if(horizontal == T){
      ylim_t <- ylim
      if(length(xlim)==0){
        ylim <- range(y, na.rm = T)
      } else {
        ylim <- xlim
      }
      xlim <- ylim_t
    } else {
      if(length(ylim)==0){
        ylim <- range(y, na.rm = T)
      }
    }
  }

  col.fill <- rep(col.fill, length = length(names))
  col.bor <- rep(col.bor, length = length(names))
  col.stat <- rep(col.stat, length = length(names))
  col.dot <- rep(col.dot, length = length(names))
  col.bg <- rep(col.bg, length = length(names))

  boxplot(formula, data = data, ..., xlim = xlim, ylim = ylim,
          outline = F, las = las, horizontal = horizontal,
          bty = "n", axes = F, add = add,
          col = NA, border = NA)

  if((!xaxt == "n")&&(horizontal == F) || (!yaxt == "n")&&(horizontal == T)){
    axis(side = pos, at = 1:length(names), labels = names, cex.axis = cex.axis, cex.lab = cex.lab,
         col.axis = col, col.lab = col, font.lab = font.lab, las = las)
  }

  if(horizontal == T){
    yaxt <- "n"
  } else {
    xaxt <- "n"
  }

  boxplot(formula, data = data, ..., xlim = xlim, ylim = ylim,
          cex.axis = cex.axis, cex.lab = cex.lab,
          col.axis = col, col.lab = col,
          font.lab = font.lab,
          lty = lty, outline = F, xaxt = xaxt, yaxt = yaxt,
          staplelwd = staplelwd, las = las,
          boxwex = boxwex, col = col.fill, lwd = lwd.bor,
          border = col.bor, notch = notch,
          xaxt = xaxt, yaxt = yaxt, horizontal = horizontal,
          xlab = xlab, ylab = ylab, names = names,
          add = T, at = (1+g):(length(names)+g))

  if(all == T){

    for (i in 1:length(nn)){

      if (!is.formula(formula)){
        xx <- formula
      } else {
        xx <- y[group == nn[i]]
      }

      pos <- jitter(rep(0, length(xx)), factor = noise) + i + g

      if (reflect == T){
        pos <- switch(side,
                      "left" = eval(parse(text = "pos[pos > i] <- 2*i - pos[pos > i]; pos")),
                      "right" = eval(parse(text = "pos[pos < i] <- 2*i - pos[pos < i]; pos")),
                      pos)
      }

      al <- xx

      if(horizontal == T){
        p1 <- al
        p2 <- pos
      } else {
        p1 <- pos
        p2 <- al
      }

      points(p1, p2, pch = pch.dot,col = col.dot[i],
             bg = col.bg, cex = cex.dot, lwd = lwd.dot)
    }

  }

  if (outline == T){

    for (i in 1:length(nn)){

      if (!is.formula(formula)){
        xx <- formula
      } else {
        xx <- y[group == nn[i]]
      }

      out <- boxplot.stats(xx)$out
      pos <- rep(i+g, length(out))

      if(horizontal == T){
        p1 <- out
        p2 <- pos
      } else {
        p1 <- pos
        p2 <- out
      }

      points(p1, p2, pch = pch.stat, col = col.stat,
             bg = col.bg, cex = cex.stat, lwd = lwd.stat)
    }

  }

  if (!(!(Mean == T)&&!(SE == T)&&!(SD == T))){

    if (!is.formula(formula)){
      m <- mean(formula, na.rm = T)
    } else {
      m <- tapply(y, list(group), mean, na.rm = T)
    }

    pos <- (1+g):(length(nn)+g)

    if (!(!(SE == T)&&!(SD == T))) {
      if (SE == T){

        if (!is.formula(formula)){
          d <- se(formula)
        } else {
          d <- tapply(y, list(group), se)
        }

      } else {

        if (!is.formula(formula)){
          d <- sd(formula, na.rm = T)
        } else {
          d <- tapply(y, list(group), sd, na.rm = T)
        }

      }

      if(horizontal == T){
        p1 <- m+d
        p2 <- pos
        p3 <- m-d
        p4 <- pos
      } else {
        p1 <- pos
        p2 <- m+d
        p3 <- pos
        p4 <- m-d
      }

      arrows(p1, p2, p3, p4, col = col.stat,
             angle = 90, length = 0, lwd = lwd.stat)

    }

    if(horizontal == T){
      p1 <- m
      p2 <- pos
    } else {
      p1 <- pos
      p2 <- m
    }

    points(p1, p2, col = col.stat, pch = pch.stat,
           lwd = lwd.stat, cex = cex.stat, bg = col.bg)

  }

  if(legend == T){

    if (!add == T){
      par(xpd=T)
    }
    par.old$xpd <- F

    if (length(leg.lab) == 0){
      leg.lab <- names
    }

    if (length(pt.col.leg) == 0){
      pt.col.leg <- col.bor
    }

    if (length(pt.bg.leg) == 0){
      pt.bg.leg <- col.fill
    }

    if (length(tx.col.leg) == 0){
      tx.col.leg <- col
    }

    if (length(tit.col.leg) == 0){
      tit.col.leg <- col
    }

    if(pos.leg =="outbottom"){
      horiz <-  T
      inset <- inset*1.1
    } else {
      horiz <- F
    }

    ins <- 0

    switch (pos.leg,
            "outtopright" = eval(parse(text = "pos.leg <- 'topleft'; ins <- c(inset,0)")),
            "outright" = eval(parse(text = "pos.leg <- 'left'; ins <- c(inset,0)")),
            "outbottomright" = eval(parse(text = "pos.leg <- 'bottomleft'; ins <- c(inset,0)")),
            "outbottom" = eval(parse(text = "pos.leg <- 'bottom'; ins <- c(0,inset)"))
    )

    legend(pos.leg[1] , pos.leg[2], inset = ins,
           legend = leg.lab, col = pt.col.leg,
           pt.bg = pt.bg.leg, pch = pch.leg,
           bty = bty.leg, bg = bg.leg, text.col = tx.col.leg,
           pt.cex = pt.cex.leg, cex = tx.cex.leg, horiz = horiz,
           title = leg.title, title.col = tit.col.leg)

  }

}

#' Drawing a figure like barplot()
#'
#' @param formula Data, e.g. numeric vector, formula, e.g. y ~ x, or other object containing analysis result
#' @param data If formula is inputted in "formula" parameter, a data.frame (or list) from which the variables in formula should be taken.
#' @param ... Argument to be passed to methods. Please see barplot().
#' @param las las, defauls is 1
#' @param cex.axis axis cex, default is 1.1
#' @param cex.lab label cex, default is 1.3
#' @param font.lab label font size, default is 2
#' @param lwd.bor box border lwd, default is 2
#' @param lwd.axis axis lwd, default is 1
#' @param lwd.0 Zero line lwd, default is 1
#' @param lwd.stat Error bar lwd, default is 1
#' @param lty.0 Line type of zero line, default is 3
#' @param col.fill fill color
#' @param col.bor border color
#' @param col.stat Mean and error bar color
#' @param col.0 Zero line color
#' @param length Length of vertical bar of tip in error bar
#' @param space Barplot space. Default is 0.5
#' @param names names
#' @param xlim x limit
#' @param ylim y limit
#' @param xlab x label
#' @param ylab y label
#' @param SD If set "T", standard deviation is drawn. Default is "F".
#' @param SE If set "T", standard error is drawn. Default is "F".
#' @param horizontal horizontal, default is "F".
#' @param beside beside
#' @param legend If legend is needed, set "T". Default is "F".
#' @param pos.leg Legend position. In addition to position of legend(), "outtopright, "outright", "outbottomright" and "outbottom" are able to select. Default is "outright".
#' @param pch.leg Legend pch, default is 22.
#' @param bty.leg Legend box type. Default is ""n.
#' @param bg.leg Legend background
#' @param pt.cex.leg Points cex in legend, default is 2.
#' @param tx.cex.leg Text cex in legend, default is 1.1.
#' @param pt.col.leg Points color in legend.
#' @param pt.bg.leg Points background color in legend.
#' @param tx.col.leg Text color in legend.
#' @param leg.lab Legend label
#' @param leg.sp Legend space, default is 2.5.
#' @param inset Legend inset, default is 1.
#' @param leg.title Legend title
#' @param tit.col.leg Legend title color
#' @param mar mar, default is c(2,3.8,1,1).
#' @param mgp mgp, default is c(2.5,0.5,0).
#' @param tcl tcl, default is -0.2.
#' @param inversion Inversion mode. If set "T", plot is drawn with inversion color. Default is "F".
#' @param inv.col Inversion color, if set inversion = "T". Default is "#FFFFFF".
#'
#' @importFrom grDevices boxplot.stats colorRampPalette hcl rgb
#' @importFrom graphics arrows axis barplot box boxplot hist lines matplot par plot points polygon abline
#' @importFrom stats density na.omit sd terms var
#'
#' @export
#'
barplotn <- function(formula, data = NULL, ...,
                     las = 1,
                     cex.axis = 1.1,
                     cex.lab = 1.3,
                     font.lab = 2,
                     lwd.bor = 2,
                     lwd.axis = 1,
                     lwd.0 = 1,
                     lwd.stat = 1,
                     lty.0 = 3,
                     col.fill = c("#1F77B47F", "#FF7F0E7F", "#2CA02C7F",
                                  "#D627287F", "#9467BD7F", "#8C564B7F",
                                  "#E377C27F", "#7F7F7F7F", "#BCBD227F", "#17BECF7F"),
                     col.bor = c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF",
                                 "#D62728FF", "#9467BDFF", "#8C564BFF",
                                 "#E377C2FF", "#7F7F7FFF", "#BCBD22FF", "#17BECFFF"),
                     col.stat = "default",
                     col.0 = "#0000007F",
                     length = "auto",
                     space = 0.5,
                     names = NULL,
                     xlim = NULL,
                     ylim = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     SE = F,
                     SD =F,
                     horizontal = F,
                     beside = F,
                     legend = F,
                     pos.leg = "outright",
                     pch.leg = 22,
                     bty.leg = "n",
                     bg.leg = "transparent",
                     pt.cex.leg = 2,
                     tx.cex.leg = 1.1,
                     pt.col.leg = NULL,
                     pt.bg.leg = NULL,
                     tx.col.leg = NULL,
                     leg.lab = NULL,
                     leg.sp = 2.5,
                     inset = 1,
                     leg.title = NULL,
                     tit.col.leg = NULL,
                     mar = c(2,3.8,1,1),
                     mgp = c(2.5,0.5,0),
                     tcl = -0.2,
                     inversion = F,
                     inv.col = "#FFFFFF"){

  is.formula <- function(x){
    class(x)=="formula"
  }

  se  <-  function(x){
    y  <-  x[!is.na(x)]
    sqrt(var(as.vector(y))/length(y))
  }


  if (inversion == T){
    bg <- "transparent"
    col <- inv.col
  } else {
    bg <- "#FFFFFF"
    col <- "#000000"
  }


  if(!is.na(col.fill[1])){
    if(col.fill[1] == "default"){
      col.fill <- bg
    }
  }

  if(!is.na(col.bor[1])){
    if(col.bor[1] == "default"){
      col.bor <- col
    }
  }

  if(!is.na(col.stat[1])){
    if(col.stat[1] == "default"){
      col.stat <- col
    }
  }

  if(horizontal == T){
    ls <- c(mar[2],mar[1])
    mar[1] <- ls[1]
    mar[2] <- ls[2]
  }


  if(legend == T){
    switch (pos.leg,
            "outtopright" = eval(mar[4] <- mar[4]+leg.sp),
            "outright" = eval(mar[4] <- mar[4]+leg.sp),
            "outbottomright" = eval(mar[4] <- mar[4]+leg.sp),
            "outbottom" = eval(mar[1] <- mar[1]+leg.sp)
    )
  }


  par.old <- par(mar = mar, mgp = mgp, tcl = tcl, bg = bg, fg = col, lwd = lwd.bor)
  on.exit(par(par.old))

  if (!is.formula(formula)){
    nn <- "x"

    if(length(names)==0){
      names <- nn
    }

    if(horizontal == T){
      if(length(ylab)==0){
        ylab <- "group"
      }
      if(length(xlab)==0){
        xlab <- "data"
      }
    } else {
      if(length(xlab)==0){
        xlab <- "group"
      }
      if(length(ylab)==0){
        ylab <- "data"
      }
    }

    cross0 <- min(formula, na.rm = T) * max(formula, na.rm = T) > 0

    if(horizontal == T){
      if(length(xlim)==0){
        if(cross0 > 0) {
          if(max(formula, na.rm = T) > 0) {
            xlim <- c(0 - max(formula, na.rm = T) * 0.05, max(formula, na.rm = T) * 1.05)
          } else {
            xlim <- c(min(formula, na.rm = T) * 1.05, 0 + min(formula, na.rm = T) * 0.05)
          }
        } else {
          xlim <- c(min(formula, na.rm = T) * 1.05, max(formula, na.rm = T) * 1.05)
        }
      }
    } else {
      if(length(ylim)==0){
        if(cross0 > 0) {
          if(max(formula, na.rm = T) > 0) {
            ylim <- c(-max(formula, na.rm = T) * 0.05, max(formula, na.rm = T) * 1.05)
          } else {
            ylim <- c(min(formula, na.rm = T) * 1.05, -min(formula, na.rm = T) * 0.05)
          }
        } else {
          ylim <- c(min(formula, na.rm = T) - (max(formula, na.rm = T) - min(formula, na.rm = T)) * 0.05,
                    max(formula, na.rm = T) + (max(formula, na.rm = T) - min(formula, na.rm = T)) * 0.05)
        }
      }
    }

  } else {
    if(is.null(data)){

      y <- eval(attr(terms(formula), "variables")[[2]])
      group <- eval(attr(terms(formula), "variables")[[3]])
      nn <- levels(as.factor(group))
      if(length(attr(terms(formula), "variables"))-1 > 2){
        for(i in 4:length(attr(terms(formula), "variables"))){
          nn <- paste(nn, rep(levels(as.factor(
            eval(attr(terms(formula), "variables")[[i]])
          )
          ), each = length(nn)), sep = ".")
          group <- paste(group, eval(attr(terms(formula), "variables")[[i]]), sep = ".")
        }
      }
      group <- factor(group, levels = nn)

      if(horizontal == T){
        if(length(xlab)==0){
          n <- as.character(attr(terms(formula), "variables")[[2]])
          xlab <- paste(n[2], n[1], n[3], sep = "")
        }
      } else {
        if(length(ylab)==0){
          n <- as.character(attr(terms(formula), "variables")[[2]])
          ylab <- paste(n[2], n[1], n[3], sep = "")
        }
      }

    } else {

      y <- data[,as.character(attr(terms(formula), "variables")[[2]])]
      group <- data[,as.character(attr(terms(formula), "variables")[[3]])]
      nn <- levels(as.factor(group))
      if(length(attr(terms(formula), "variables"))-1 > 2){
        for(i in 4:length(attr(terms(formula), "variables"))){
          nn <- paste(nn, rep(levels(as.factor(
            data[,as.character(attr(terms(formula), "variables")[[i]])]
          )
          ), each = length(nn)), sep = ".")
          group <- paste(group, data[,as.character(attr(terms(formula), "variables")[[i]])], sep = ".")
        }
      }
      group <- factor(group, levels = nn)

      if(horizontal == T){
        if(length(xlab)==0){
          xlab <- as.character(attr(terms(formula), "variables")[[2]])
        }
      } else {
        if(length(ylab)==0){
          ylab <- as.character(attr(terms(formula), "variables")[[2]])
        }
      }
    }

    if(length(names)==0){
      names <- nn
    }

    if(horizontal == T){
      if(length(ylab)==0){
        ylab <- "group"
      }
    } else {
      if(length(xlab)==0){
        xlab <- "group"
      }
    }

    cross0 <- min(tapply(y, list(group), mean, na.rm = T)) *
      max(tapply(y, list(group), mean, na.rm = T)) > 0

    if(horizontal == T){
      if(length(xlim)==0){
        if(cross0 > 0) {
          if(max(tapply(y, list(group), mean, na.rm = T)) > 0) {
            xlim <- c(0 - max(tapply(y, list(group), sd, na.rm = T)) * 0.5,
                      max(tapply(y, list(group), mean, na.rm = T)) + max(tapply(y, list(group), sd, na.rm = T)) * 1.5)
          } else {
            xlim <- c(min(tapply(y, list(group), mean, na.rm = T)) - max(tapply(y, list(group), sd, na.rm = T)) * 1.5,
                      0 + max(tapply(y, list(group), sd, na.rm = T)) * 0.5)
          }
        } else {
          xlim <- c(min(tapply(y, list(group), mean, na.rm = T)) - max(tapply(y, list(group), sd, na.rm = T)) * 1.5,
                    max(tapply(y, list(group), mean, na.rm = T)) + max(tapply(y, list(group), sd, na.rm = T)) * 1.5)
        }
      }
    } else {
      if(length(ylim)==0){
        if(cross0 > 0) {
          if(max(tapply(y, list(group), mean, na.rm = T)) > 0) {
            ylim <- c(-max(tapply(y, list(group), sd, na.rm = T)) * 0.5,
                      max(tapply(y, list(group), mean, na.rm = T)) + max(tapply(y, list(group), sd, na.rm = T)) * 1.5)
          } else {
            ylim <- c(min(tapply(y, list(group), mean, na.rm = T)) - max(tapply(y, list(group), sd, na.rm = T)) * 1.5,
                      max(tapply(y, list(group), sd, na.rm = T)) * 0.5)
          }
        } else {
          ylim <- c(min(tapply(y, list(group), mean, na.rm = T)) - max(tapply(y, list(group), sd, na.rm = T)) * 1.5,
                    max(tapply(y, list(group), mean, na.rm = T)) + max(tapply(y, list(group), sd, na.rm = T)) * 1.5)
        }
      }
    }
  }


  if (!is.formula(formula)){
    m <- formula
  } else {
    m <- tapply(y, list(group), mean, na.rm = T)
  }

  if(beside == T){
    col.fill <- col.fill[1:nrow(formula)]
    col.bor <- col.bor[1:nrow(formula)]

    if(!length(space) == 2){
      space <- c(0,1)
    }

  }


  pos <- barplot(m, ..., col = col.fill, las = las, names.arg = names, space = space,
                 cex.axis = cex.axis, cex.lab = cex.lab, cex.names = cex.axis,
                 font.lab = font.lab, border = col.bor, horiz = horizontal,
                 col.axis = col, col.lab = col, xlab = xlab, ylab = ylab,
                 xlim = xlim, ylim = ylim, beside = beside)

  if (horizontal == T){
    abline(v = 0, col = col.0, lty = lty.0)
  } else {
    abline(h = 0, col = col.0, lty = lty.0)
  }

  box(lty=1, lwd = lwd.axis)

  if ( is.formula(formula) && !(!(SE == T)&&!(SD == T))){

    if (SE == T){
      d <- tapply(y, list(group), se)
    } else {
      d <- tapply(y, list(group), sd, na.rm = T)
    }

    ep <- m+d
    em <- m-d

    if(horizontal == T){
      p1 <- m
      p2 <- pos
      p3 <- ep
      p4 <- pos
      p5 <- m
      p6 <- pos
      p7 <- em
      p8 <- pos
    } else {
      p1 <- pos
      p2 <- m
      p3 <- pos
      p4 <- ep
      p5 <- pos
      p6 <- m
      p7 <- pos
      p8 <- em
    }

    if(length == "auto"){

      if(horizontal == T){
        aj <- par()$mfrow[1]
      } else {
        aj <- par()$mfrow[2]
      }

      length <- 1/(2 * length(pos) * aj) * (pos[2] - pos[1])
    }

    arrows(p1, p2, p3, p4, col = col.stat,
           angle = 90, length = length, lwd = lwd.stat)
    arrows(p5, p6, p7, p8, col = col.stat,
           angle = 90, length = length, lwd = lwd.stat)

  }

  if(legend == T){

    par(xpd=T)
    par.old$xpd <- F

    if (length(leg.lab) == 0){
      leg.lab <- names
    }

    if (length(pt.col.leg) == 0){
      pt.col.leg <- col.bor
    }

    if (length(pt.bg.leg) == 0){
      pt.bg.leg <- col.fill
    }

    if (length(tx.col.leg) == 0){
      tx.col.leg <- col
    }

    if (length(tit.col.leg) == 0){
      tit.col.leg <- col
    }

    if(pos.leg =="outbottom"){
      horiz <-  T
      inset <- inset*1.1
    } else {
      horiz <- F
    }

    ins <- 0

    switch (pos.leg,
            "outtopright" = eval(parse(text = "pos.leg <- 'topleft'; ins <- c(inset,0)")),
            "outright" = eval(parse(text = "pos.leg <- 'left'; ins <- c(inset,0)")),
            "outbottomright" = eval(parse(text = "pos.leg <- 'bottomleft'; ins <- c(inset,0)")),
            "outbottom" = eval(parse(text = "pos.leg <- 'bottom'; ins <- c(0,inset)"))
    )

    legend(pos.leg[1] , pos.leg[2], inset = ins,
           legend = leg.lab, col = pt.col.leg,
           pt.bg = pt.bg.leg, pch = pch.leg,
           bty = bty.leg, bg = bg.leg, text.col = tx.col.leg,
           pt.cex = pt.cex.leg, cex = tx.cex.leg, horiz = horiz,
           title = leg.title, title.col = tit.col.leg)

  }

  invisible(pos)

}

#' Drawing a figure like hist()
#'
#' @param formula Data, e.g. numeric vector, formula, e.g. y ~ x, or other object containing analysis result
#' @param data If formula is inputted in "formula" parameter, a data.frame (or list) from which the variables in formula should be taken.
#' @param ... Argument to be passed to methods. Please see hist().
#' @param xlab x label
#' @param ylab y label
#' @param las las, defauls is 1
#' @param main Main title
#' @param cex.axis axis cex, default is 1.1
#' @param cex.lab label cex, default is 1.3
#' @param font.lab label font size, default is 2
#' @param col.fill histogram fill color
#' @param col.bor histogram border color
#' @param hist.dens Density of histgram fill
#' @param hist.ang Angle of histgram fill stripe, default is 45 degree.
#' @param kernel If set "T", density curve is also drawn. Default is "F".
#' @param freq If set "T", data is transformed into frequency data. Default is "F".
#' @param col.line density curve line color
#' @param col.ker density curve fill color
#' @param ker.dens Density of density curve fill
#' @param ker.ang Angle of density curve fill stripe, default is 45 degree.
#' @param lwd.hist histgram lwd, default is 1
#' @param lwd.line density curve lwd, default is 2
#' @param breaks breaks
#' @param horizontal horizontal, default is "F".
#' @param legend If legend is needed, set "T". Default is "F".
#' @param pos.leg Legend position. In addition to position of legend(), "outtopright, "outright", "outbottomright" and "outbottom" are able to select. Default is "outright".
#' @param pch.leg Legend pch, default is 22.
#' @param bty.leg Legend box type. Default is ""n.
#' @param bg.leg Legend background
#' @param pt.cex.leg Points cex in legend, default is 2.
#' @param tx.cex.leg Text cex in legend, default is 1.1.
#' @param pt.col.leg Points color in legend.
#' @param pt.bg.leg Points background color in legend.
#' @param tx.col.leg Text color in legend.
#' @param leg.lab Legend label
#' @param leg.sp Legend space, default is 2.5.
#' @param inset Legend inset, default is 1.
#' @param leg.title Legend title
#' @param tit.col.leg Legend title color
#' @param mar mar, default is c(3.8,3.8,1,1).
#' @param mgp mgp, default is c(2.5,0.5,0).
#' @param tcl tcl, default is -0.2.
#' @param inversion Inversion mode. If set "T", plot is drawn with inversion color. Default is "F".
#' @param inv.col Inversion color, if set inversion = "T". Default is "#FFFFFF".
#'
#' @importFrom grDevices boxplot.stats colorRampPalette hcl rgb
#' @importFrom graphics arrows axis barplot box boxplot hist lines matplot par plot points polygon abline
#' @importFrom stats density na.omit sd terms var
#'
#' @export
#'
histn <- function(formula, data = NULL, ...,
                  xlab = NULL,
                  ylab = NULL,
                  las = 1,
                  main = "",
                  cex.axis = 1.1,
                  cex.lab = 1.3,
                  font.lab = 2,
                  col.fill = c("#1F77B47F", "#FF7F0E7F", "#2CA02C7F",
                               "#D627287F", "#9467BD7F", "#8C564B7F",
                               "#E377C27F", "#7F7F7F7F", "#BCBD227F", "#17BECF7F"),
                  col.bor = c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF",
                              "#D62728FF", "#9467BDFF", "#8C564BFF",
                              "#E377C2FF", "#7F7F7FFF", "#BCBD22FF", "#17BECFFF"),
                  hist.dens = NA,
                  hist.ang = 45,
                  kernel = F,
                  freq = T,
                  col.line = c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF",
                               "#D62728FF", "#9467BDFF", "#8C564BFF",
                               "#E377C2FF", "#7F7F7FFF", "#BCBD22FF", "#17BECFFF"),
                  col.ker = "transparent",
                  ker.dens = NA,
                  ker.ang = 45,
                  lwd.hist = 1,
                  lwd.line = 2,
                  breaks = NULL,
                  horizontal = F,
                  legend = F,
                  pos.leg = "outright",
                  pch.leg = 22,
                  bty.leg = "n",
                  bg.leg = "transparent",
                  pt.cex.leg = 2,
                  tx.cex.leg = 1.1,
                  pt.col.leg = NULL,
                  pt.bg.leg = NULL,
                  tx.col.leg = NULL,
                  leg.lab = NULL,
                  leg.sp = 2.5,
                  inset = 1,
                  leg.title = NULL,
                  tit.col.leg = NULL,
                  mar = c(3.8,3.8,1,1),
                  mgp = c(2.5,0.5,0),
                  tcl = -0.2,
                  inversion = F,
                  inv.col = "#FFFFFF"){

  is.formula <- function(x){
    class(x)=="formula"
  }

  if (inversion == T){
    bg <- "transparent"
    col <- inv.col
  } else {
    bg <- "#FFFFFF"
    col <- "#000000"
  }

  if(!is.na(col.fill[1])){
    if(col.fill[1] == "default"){
      col.fill <- bg
    }
  }

  if(!is.na(col.bor[1])){
    if(col.bor[1] == "default"){
      col.bor <- col
    }
  }

  if(!is.na(col.line[1])){
    if(col.line[1] == "default"){
      col.line <- col
    }
  }

  if (kernel == T){
    freq <- F
  }

  if (length(ylab) == 0){
    if (!freq == T){
      ylab <- "Density"
    } else {
      ylab <- "Frequency"
    }
  }

  if(legend == T){
    switch (pos.leg,
            "outtopright" = eval(mar[4] <- mar[4]+leg.sp),
            "outright" = eval(mar[4] <- mar[4]+leg.sp),
            "outbottomright" = eval(mar[4] <- mar[4]+leg.sp),
            "outbottom" = eval(mar[1] <- mar[1]+leg.sp)
    )
  }

  par.old <- par(mar = mar, mgp = mgp, tcl = tcl, bg = bg, fg = col)
  on.exit(par(par.old))

  if (!is.formula(formula)){

    if(length(xlab)==0){
      xlab <- "index"
    }

    n <- 1
    names <- n

  } else {

    if(is.null(data)){

      y <- eval(attr(terms(formula), "variables")[[2]])
      group <- eval(attr(terms(formula), "variables")[[3]])
      nn <- levels(as.factor(group))
      if(length(attr(terms(formula), "variables"))-1 > 2){
        for(i in 4:length(attr(terms(formula), "variables"))){
          nn <- paste(nn, rep(levels(as.factor(
            eval(attr(terms(formula), "variables")[[i]])
          )
          ), each = length(nn)), sep = ".")
          group <- paste(group, eval(attr(terms(formula), "variables")[[i]]), sep = ".")
        }
      }
      group <- factor(group, levels = nn)

      if(length(xlab)==0){
        n <- as.character(attr(terms(formula), "variables")[[2]])
        xlab <- paste(n[2], n[1], n[3], sep = "")
      }

    } else {

      y <- data[,as.character(attr(terms(formula), "variables")[[2]])]
      group <- data[,as.character(attr(terms(formula), "variables")[[3]])]
      nn <- levels(as.factor(group))
      if(length(attr(terms(formula), "variables"))-1 > 2){
        for(i in 4:length(attr(terms(formula), "variables"))){
          nn <- paste(nn, rep(levels(as.factor(
            data[,as.character(attr(terms(formula), "variables")[[i]])]
          )
          ), each = length(nn)), sep = ".")
          group <- paste(group, data[,as.character(attr(terms(formula), "variables")[[i]])], sep = ".")
        }
      }
      group <- factor(group, levels = nn)

      if(length(xlab)==0){
        xlab <- as.character(attr(terms(formula), "variables")[[2]])
      }

    }

    n <- length(levels(as.factor(group)))
    names <- levels(as.factor(group))

  }

  if(length(breaks) == 0){

    if (!is.formula(formula)){
      xx <- formula
    } else {
      xx <- y
    }

    l <- length(xx)

    if (l < 19){
      breaks <- seq(floor(min(xx, na.rm = T)),
                    ceiling(max(xx, na.rm = T)), 1)
    } else {
      if(l < 99){
        l <- l/3
      } else {
        if(l < 999) {
          l <- 10*floor(log10(l))
        } else {
          l <- 10*(floor(log10(l))-1)
        }
      }
      breaks <- seq(floor(min(xx, na.rm = T)),
                    ceiling(max(xx, na.rm = T)),
                    length = l)
    }
  }

  if(n > 1){
    col.fill <- rep(col.fill, length = n)
    col.bor <- rep(col.bor, length = n)
    hist.dens <- rep(hist.dens, length = n)
    hist.ang <- rep(hist.ang, length = n)
    col.line <- rep(col.line, length = n)
    col.ker <- rep(col.ker, length = n)
    ker.dens <- rep(ker.dens, length = n)
    ker.ang <- rep(ker.ang, length = n)
  }

  for (i in 1:n){

    if (!is.formula(formula)){
      xx <- formula
    } else {
      xx <- y[group == levels(as.factor(group))[i]]
    }

    if (i == 1){

      if(n == 1){

        hist(..., x = xx, las = las, cex.axis = cex.axis, ylab = ylab,
             cex.lab = cex.lab, font.lab = font.lab, xlab = xlab,
             col.axis = col, col.lab = col, main = main, lwd = lwd.hist,
             col = col.fill[1], border = col.bor[1], freq = freq, breaks = breaks,
             density = hist.dens, angle = hist.ang)

      } else {

        hist(..., x = xx, las = las, cex.axis = cex.axis, ylab = ylab,
             cex.lab = cex.lab, font.lab = font.lab, xlab = xlab,
             col.axis = col, col.lab = col, main = main, lwd = lwd.hist,
             col = col.fill[i], border = col.bor[i], freq = freq, breaks = breaks,
             density = hist.dens[i], angle = hist.ang[i])

      }

    } else {
      hist(..., x = xx, main = "", lwd = lwd.hist, ylab = "",
           col = col.fill[i], border = col.bor[i], freq = freq, breaks = breaks,
           density = hist.dens[i], angle = hist.ang[i], add = T)
    }

    if(kernel == T){
      polygon(density(xx, na.rm = T), col = col.ker[i], border = "transparent",
              density = ker.dens, angle = ker.ang)
      lines(density(xx, na.rm = T), col = col.line[i], lwd = lwd.line)
    }
  }

  box()

  if(legend == T){

    par(xpd=T)
    par.old$xpd <- F

    if (length(leg.lab) == 0){
      leg.lab <- names
    }

    if (length(pt.col.leg) == 0){
      pt.col.leg <- col.bor
    }

    if (length(pt.bg.leg) == 0){
      pt.bg.leg <- col.fill
    }

    if (length(tx.col.leg) == 0){
      tx.col.leg <- col
    }

    if (length(tit.col.leg) == 0){
      tit.col.leg <- col
    }

    if(pos.leg =="outbottom"){
      horiz <-  T
      inset <- inset*1.1
    } else {
      horiz <- F
    }

    ins <- 0

    switch (pos.leg,
            "outtopright" = eval(parse(text = "pos.leg <- 'topleft'; ins <- c(inset,0)")),
            "outright" = eval(parse(text = "pos.leg <- 'left'; ins <- c(inset,0)")),
            "outbottomright" = eval(parse(text = "pos.leg <- 'bottomleft'; ins <- c(inset,0)")),
            "outbottom" = eval(parse(text = "pos.leg <- 'bottom'; ins <- c(0,inset)"))
    )

    legend(pos.leg[1] , pos.leg[2], inset = ins,
           legend = leg.lab, col = pt.col.leg,
           pt.bg = pt.bg.leg, pch = pch.leg,
           bty = bty.leg, bg = bg.leg, text.col = tx.col.leg,
           pt.cex = pt.cex.leg, cex = tx.cex.leg, horiz = horiz,
           title = leg.title, title.col = tit.col.leg)

  }

}

#' Drawing a violinplot
#'
#' @param formula Data, e.g. numeric vector, formula, e.g. y ~ x, or other object containing analysis result
#' @param data If formula is inputted in "formula" parameter, a data.frame (or list) from which the variables in formula should be taken.
#' @param ... Argument to be passed to methods. Please see boxplot().
#' @param las las, defauls is 1
#' @param xlab x label
#' @param ylab y label
#' @param names names
#' @param xlim x limit
#' @param ylim y limit
#' @param xaxt xaxt, default is "s".
#' @param yaxt yaxt, default is "s".
#' @param adjust Adjust of density curve, default is 1.
#' @param cex.axis axis cex, default is 1.1
#' @param cex.lab label cex, default is 1.3
#' @param font.lab label font size, default is 2
#' @param pch.dot points pch, default is 16
#' @param pch.stat mean points pch, default is 21
#' @param cex.dot points cex, default is 0.5
#' @param cex.stat mean points cex, default is 1
#' @param scale Max width of density curve, "area", "width" and numeric number are able to select.
#' @param staplelwd staplelwd, default is "NA".
#' @param boxwex boxwex, default is 0.5.
#' @param notch notch, default is "F".
#' @param density Density of violin fill
#' @param angle Angle of violin fill stripe, default is 45 degree.
#' @param col.fill violin fill color
#' @param col.mar violin border color
#' @param col.box box fill color
#' @param col.bor box border color
#' @param col.stat Mean and error bar color
#' @param col.dot points color
#' @param col.bg background color
#' @param lwd.mar violin border lwd, default is 1
#' @param lwd.bor box border lwd, default is 1
#' @param lwd.stat mean and error bar lwd, default is 1
#' @param lwd.dot points lwd, default is 1
#' @param Mean If set "T", mean points are drawn. Default is "F".
#' @param SD If set "T", standard deviation is drawn. Default is "F".
#' @param SE If set "T", standard error is drawn. Default is "F".
#' @param boxplot If set "T", boxplot is also drawn. Default is "F".
#' @param outline If set "T", outliners are drawn. Default is "F".
#' @param all If set "T", all points are drawn. Default is "T".
#' @param add If set "T", boxplot is able to overdrawn on previous boxplot. Default is "F".
#' @param trim If set "T", tip of violin plot is trimmed. Default is "F".
#' @param horizontal horizontal, default is "F".
#' @param side Displayed half of violin and move direction of boxplot, "left", "both" and "right" are able to select. Default is "both".
#' @param side.sp Magnitude of move of boxplot, default is 0.05.
#' @param noise scatter level of points, default is 1
#' @param reflect If set "T", points are not drawn in next to boxplot and reflected. Default is "T".
#' @param legend If legend is needed, set "T". Default is "F".
#' @param pos.leg Legend position. In addition to position of legend(), "outtopright, "outright", "outbottomright" and "outbottom" are able to select. Default is "outright".
#' @param pch.leg Legend pch, default is 22.
#' @param bty.leg Legend box type. Default is ""n.
#' @param bg.leg Legend background
#' @param pt.cex.leg Points cex in legend, default is 2.
#' @param tx.cex.leg Text cex in legend, default is 1.1.
#' @param pt.col.leg Points color in legend.
#' @param pt.bg.leg Points background color in legend.
#' @param tx.col.leg Text color in legend.
#' @param leg.lab Legend label
#' @param leg.sp Legend space, default is 2.5.
#' @param inset Legend inset, default is 1.
#' @param leg.title Legend title
#' @param tit.col.leg Legend title color
#' @param mar mar, default is c(2,3.8,1,1).
#' @param mgp mgp, default is c(2.5,0.5,0).
#' @param tcl tcl, default is -0.2.
#' @param inversion Inversion mode. If set "T", plot is drawn with inversion color. Default is "F".
#' @param inv.col Inversion color, if set inversion = "T". Default is "#FFFFFF".
#'
#' @importFrom grDevices boxplot.stats colorRampPalette hcl rgb
#' @importFrom graphics arrows axis barplot box boxplot hist lines matplot par plot points polygon abline
#' @importFrom stats density na.omit sd terms var
#'
#' @export
#'
vioplotn <- function(formula, data = NULL,
                     ...,
                     las = 1,
                     xlab = NULL,
                     ylab = NULL,
                     names = NULL,
                     xlim = NULL,
                     ylim = NULL,
                     xaxt = "s",
                     yaxt = "s",
                     adjust = 1,
                     cex.axis = 1.1,
                     cex.lab = 1.3,
                     font.lab = 2,
                     pch.dot = 16,
                     pch.stat = 21,
                     cex.dot = 0.5,
                     cex.stat = 1,
                     scale = "area",
                     staplelwd = NA,
                     boxwex = 0.1,
                     notch = F,
                     density = NA,
                     angle = 45,
                     col.fill = c("#1F77B47F", "#FF7F0E7F", "#2CA02C7F",
                                  "#D627287F", "#9467BD7F", "#8C564B7F",
                                  "#E377C27F", "#7F7F7F7F", "#BCBD227F", "#17BECF7F"),
                     col.mar = c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF",
                                 "#D62728FF", "#9467BDFF", "#8C564BFF",
                                 "#E377C2FF", "#7F7F7FFF", "#BCBD22FF", "#17BECFFF"),
                     col.box = "#FFFFFF",
                     col.bor = "#000000",
                     col.stat = "default",
                     col.dot = c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF",
                                 "#D62728FF", "#9467BDFF", "#8C564BFF",
                                 "#E377C2FF", "#7F7F7FFF", "#BCBD22FF", "#17BECFFF"),
                     col.bg = "#FFFFFF",
                     lwd.mar = 1,
                     lwd.bor = 1,
                     lwd.stat = 1,
                     lwd.dot = 1,
                     Mean = F,
                     SE = F,
                     SD = F,
                     boxplot = T,
                     outline = F,
                     all = T,
                     add = F,
                     trim = F,
                     horizontal = F,
                     side = "both",
                     side.sp = 0.05,
                     noise = 1,
                     reflect = T,
                     legend = F,
                     pos.leg = "outright",
                     pch.leg = 22,
                     bty.leg = "n",
                     bg.leg = "transparent",
                     pt.cex.leg = 2,
                     tx.cex.leg = 1.1,
                     pt.col.leg = NULL,
                     pt.bg.leg = NULL,
                     tx.col.leg = NULL,
                     leg.lab = NULL,
                     leg.sp = 2.5,
                     inset = 1,
                     leg.title = NULL,
                     tit.col.leg = NULL,
                     mar = c(2,3.8,1,1),
                     mgp = c(2.5,0.5,0),
                     tcl = -0.2,
                     inversion = F,
                     inv.col = "#FFFFFF"){

  is.formula <- function(x){
    class(x)=="formula"
  }

  se  <-  function(x){
    y  <-  x[!is.na(x)]
    sqrt(var(as.vector(y))/length(y))
  }

  if (inversion == T){
    bg <- "transparent"
    col <- inv.col
  } else {
    bg <- "#FFFFFF"
    col <- "#000000"
  }

  if(horizontal == T){
    pos <- 2
    ls <- c(yaxt,xaxt)
    xaxt <- ls[1]
    yaxt <- ls[2]
    ls <- c(mar[2],mar[1])
    mar[1] <- ls[1]
    mar[2] <- ls[2]
  } else {
    pos <- 1
  }

  if(!is.na(col.fill[1])){
    if(col.fill[1] == "default"){
      col.fill <- bg
    }
  }
  if(!is.na(col.mar[1])){
    if(col.mar[1] == "default"){
      col.mar <- col
    }
  }
  if(!is.na(col.stat[1])){
    if(col.stat[1] == "default"){
      col.stat <- col
    }
  }
  if(!is.na(col.dot[1])){
    if(col.dot[1] == "default"){
      col.dot <- col
    }
  }

  if (!trim == T){
    cut <- 3
  } else {
    cut <- 0
  }

  if(legend == T){
    switch (pos.leg,
            "outtopright" = eval(mar[4] <- mar[4]+leg.sp),
            "outright" = eval(mar[4] <- mar[4]+leg.sp),
            "outbottomright" = eval(mar[4] <- mar[4]+leg.sp),
            "outbottom" = eval(mar[1] <- mar[1]+leg.sp)
    )
  }

  noise <- noise*10

  par.old <- par(mar = mar, mgp = mgp, tcl = tcl, bg = bg, fg = col)
  on.exit(par(par.old))

  if(add == T){
    par(new = T)
  }

  side <- switch(side,
                 "both" = side,
                 "right" = side,
                 "left" = side,
                 "both")

  g <- switch(side,
              "both" = 0,
              "right" = side.sp,
              "left" = -side.sp)

  if (!is.formula(formula)){
    nn <- "x"

    if(length(names)==0){
      names <- nn
    }

    if(horizontal == T){
      if(length(ylab)==0){
        ylab <- "group"
      }
      if(length(xlab)==0){
        xlab <- "data"
      }
    } else {
      if(length(xlab)==0){
        xlab <- "group"
      }
      if(length(ylab)==0){
        ylab <- "data"
      }
    }

    if(horizontal == T){
      ylim_t <- ylim
      if(length(xlim)==0){
        ylim <- range(y, na.rm = T)
      } else {
        ylim <- xlim
      }
      xlim <- ylim_t
    } else {
      if(length(ylim)==0){
        ylim <- range(y, na.rm = T)
      }
    }

  } else {
    if(is.null(data)){

      y <- eval(attr(terms(formula), "variables")[[2]])
      group <- eval(attr(terms(formula), "variables")[[3]])
      nn <- levels(as.factor(group))
      if(length(attr(terms(formula), "variables"))-1 > 2){
        for(i in 4:length(attr(terms(formula), "variables"))){
          nn <- paste(nn, rep(levels(as.factor(
            eval(attr(terms(formula), "variables")[[i]])
          )
          ), each = length(nn)), sep = ".")
          group <- paste(group, eval(attr(terms(formula), "variables")[[i]]), sep = ".")
        }
      }
      group <- factor(group, levels = nn)

      if(horizontal == T){
        if(length(xlab)==0){
          n <- as.character(attr(terms(formula), "variables")[[2]])
          xlab <- paste(n[2], n[1], n[3], sep = "")
        }
      } else {
        if(length(ylab)==0){
          n <- as.character(attr(terms(formula), "variables")[[2]])
          ylab <- paste(n[2], n[1], n[3], sep = "")
        }
      }

    } else {

      y <- data[,as.character(attr(terms(formula), "variables")[[2]])]
      group <- data[,as.character(attr(terms(formula), "variables")[[3]])]
      nn <- levels(as.factor(group))
      if(length(attr(terms(formula), "variables"))-1 > 2){
        for(i in 4:length(attr(terms(formula), "variables"))){
          nn <- paste(nn, rep(levels(as.factor(
            data[,as.character(attr(terms(formula), "variables")[[i]])]
          )
          ), each = length(nn)), sep = ".")
          group <- paste(group, data[,as.character(attr(terms(formula), "variables")[[i]])], sep = ".")
        }
      }
      group <- factor(group, levels = nn)

      if(horizontal == T){
        if(length(xlab)==0){
          xlab <- as.character(attr(terms(formula), "variables")[[2]])
        }
      } else {
        if(length(ylab)==0){
          ylab <- as.character(attr(terms(formula), "variables")[[2]])
        }
      }
    }

    if(length(names)==0){
      names <- nn
    }

    if(horizontal == T){
      if(length(ylab)==0){
        ylab <- "group"
      }
    } else {
      if(length(xlab)==0){
        xlab <- "group"
      }
    }

    if(horizontal == T){
      ylim_t <- ylim
      if(length(xlim)==0){
        ylim <- range(y, na.rm = T)
      } else {
        ylim <- xlim
      }
      xlim <- ylim_t
    } else {
      if(length(ylim)==0){
        ylim <- range(y, na.rm = T)
      }
    }
  }


  col.fill <- rep(col.fill, length = length(names))
  col.mar <- rep(col.mar, length = length(names))
  col.box <- rep(col.box, length = length(names))
  col.bor <- rep(col.bor, length = length(names))
  col.stat <- rep(col.stat, length = length(names))
  col.dot <- rep(col.dot, length = length(names))
  col.bg <- rep(col.bg, length = length(names))
  density <- rep(density, length = length(names))
  angle <- rep(angle, length = length(names))

  boxplot(formula, data = data ,..., xlim = xlim, ylim = ylim, las = las,
          outline = F, bty = "n", axes = F, add = F,
          col = NA, border = NA,  horizontal = horizontal)

  if((!xaxt == "n")&&(horizontal == F) || (!yaxt == "n")&&(horizontal == T)){
    axis(side = pos, at = 1:length(names), labels = names, cex.axis = cex.axis, cex.lab = cex.lab,
         col.axis = col, col.lab = col, font.lab = font.lab, las = las)
  }

  for (i in 1:length(nn)){

    if (!is.formula(formula)){
      xx <- formula
    } else {
      xx <- y[group == nn[i]]
    }

    if(i == 1){
      if (length(na.omit(xx)) > 1){
        M <- max(density(xx, cut = cut, adjust = adjust, na.rm = T)[[2]])
      } else {
        M <- NA
      }
    } else {
      if (length(na.omit(xx)) > 1){
        M <- c(M, max(density(xx, cut = cut, adjust = adjust, na.rm = T)[[2]]))
      } else {
        M <- c(M, NA)
      }
    }
  }

  if (is.numeric(scale)){
    scale <- rep(scale, length = length(nn))
  } else {
    scale <- switch(scale,
                    "area" = rep(0.45/max(M), length(nn)),
                    "width" = 0.45/M)
  }

  for (i in 1:length(nn)){

    if (!is.formula(formula)){
      xx <- formula
    } else {
      xx <- y[group == nn[i]]
    }

    if (length(na.omit(xx)) > 1){
      yd <- c(density(xx, cut = cut, adjust = adjust, na.rm = T)[[1]],
              rev(density(xx, cut = cut, adjust = adjust, na.rm = T)[[1]]))

      xd <- switch(side,
                   "both" = c(density(xx, cut = cut, adjust = adjust, na.rm = T)[[2]]*scale[i],
                              rev(-density(xx, cut = cut, adjust = adjust, na.rm = T)[[2]])*scale[i]) + i,
                   "right" = c(density(xx, cut = cut, adjust = adjust, na.rm = T)[[2]]*scale[i],
                               rep(0, length = length(yd)/2)) + i,
                   "left" = c(rep(0, length = length(yd)/2),
                              rev(-density(xx, cut = cut, adjust = adjust, na.rm = T)[[2]])*scale[i]) + i)

      if(horizontal == T){
        ls <- list(yd,xd)
        xd <- ls[[1]]
        yd <- (ls[[2]] - i) + i
      }

      polygon(xd, yd, col = col.fill[i], border = col.mar[i],
              density = density[i], angle = angle[i], lwd = lwd.mar)
    }

  }

  if(boxplot == F){
    col.box <- NA
    col.bor <- NA
  }

  if(horizontal == T){
    yaxt <- "n"
  } else {
    xaxt <- "n"
  }

  boxplot(formula, data = data, ..., xlim = xlim, ylim = ylim,
          xlab = xlab, ylab = ylab,
          lty = 1, outline = F, lwd = lwd.bor,
          cex.axis = cex.axis, cex.lab = cex.lab,
          col.axis = col, col.lab = col,
          font.lab = font.lab, las = las,
          staplelwd = staplelwd, boxwex = boxwex, horizontal = horizontal,
          col = col.box, border = col.bor, xaxt = xaxt, yaxt = yaxt,
          notch = notch, at = (1+g):(length(names)+g), add = T)

  if(all == T){

    for (i in 1:length(nn)){

      if (!is.formula(formula)){
        xx <- formula
      } else {
        xx <- y[group == nn[i]]
      }

      pos <- jitter(rep(0, length(xx)), factor = noise) + i + g

      if (reflect == T){
        pos <- switch(side,
                      "left" = eval(parse(text = "pos[pos > i] <- 2*i - pos[pos > i]; pos")),
                      "right" = eval(parse(text = "pos[pos < i] <- 2*i - pos[pos < i]; pos")),
                      pos)
      }

      al <- xx

      if(horizontal == T){
        p1 <- al
        p2 <- pos
      } else {
        p1 <- pos
        p2 <- al
      }

      points(p1, p2, pch = pch.dot,col = col.dot[i],
             bg = col.bg, cex = cex.dot, lwd = lwd.dot)
    }

  }

  if (outline == T){

    for (i in 1:length(nn)){

      if (!is.formula(formula)){
        xx <- formula
      } else {
        xx <- y[group == nn[i]]
      }

      out <- boxplot.stats(xx)$out
      pos <- rep(i+g, length(out))

      if(horizontal == T){
        p1 <- out
        p2 <- pos
      } else {
        p1 <- pos
        p2 <- out
      }

      points(p1, p2, pch = pch.stat, col = col.stat,
             bg = col.bg, cex = cex.stat, lwd = lwd.stat)
    }

  }

  if (!(!(Mean == T)&&!(SE == T)&&!(SD == T))){

    if (!is.formula(formula)){
      m <- mean(formula, na.rm = T)
    } else {
      m <- tapply(y, list(group), mean, na.rm = T)
    }

    pos <- (1+g):(length(nn)+g)

    if (!(!(SE == T)&&!(SD == T))) {
      if (SE == T){

        if (!is.formula(formula)){
          d <- se(formula)
        } else {
          d <- tapply(y, list(group), se)
        }

      } else {

        if (!is.formula(formula)){
          d <- sd(formula, na.rm = T)
        } else {
          d <- tapply(y, list(group), sd, na.rm = T)
        }

      }

      if(horizontal == T){
        p1 <- m+d
        p2 <- pos
        p3 <- m-d
        p4 <- pos
      } else {
        p1 <- pos
        p2 <- m+d
        p3 <- pos
        p4 <- m-d
      }

      arrows(p1, p2, p3, p4, col = col.stat,
             angle = 90, length = 0, lwd = lwd.stat)

    }

    if(horizontal == T){
      p1 <- m
      p2 <- pos
    } else {
      p1 <- pos
      p2 <- m
    }

    points(p1, p2, col = col.stat, pch = pch.stat,
           lwd = lwd.stat, cex = cex.stat, bg = col.bg)

  }

  box()


  if(legend == T){

    if (!add == T){
      par(xpd=T)
    }
    par.old$xpd <- F

    if (length(leg.lab) == 0){
      leg.lab <- names
    }

    if (length(pt.col.leg) == 0){
      pt.col.leg <- col.mar
    }

    if (length(pt.bg.leg) == 0){
      pt.bg.leg <- col.fill
    }

    if (length(tx.col.leg) == 0){
      tx.col.leg <- col
    }

    if (length(tit.col.leg) == 0){
      tit.col.leg <- col
    }

    if(pos.leg =="outbottom"){
      horiz <-  T
      inset <- inset*1.1
    } else {
      horiz <- F
    }

    ins <- 0

    switch (pos.leg,
            "outtopright" = eval(parse(text = "pos.leg <- 'topleft'; ins <- c(inset,0)")),
            "outright" = eval(parse(text = "pos.leg <- 'left'; ins <- c(inset,0)")),
            "outbottomright" = eval(parse(text = "pos.leg <- 'bottomleft'; ins <- c(inset,0)")),
            "outbottom" = eval(parse(text = "pos.leg <- 'bottom'; ins <- c(0,inset)"))
    )

    legend(pos.leg[1] , pos.leg[2], inset = ins,
           legend = leg.lab, col = pt.col.leg,
           pt.bg = pt.bg.leg, pch = pch.leg,
           bty = bty.leg, bg = bg.leg, text.col = tx.col.leg,
           pt.cex = pt.cex.leg, cex = tx.cex.leg, horiz = horiz,
           title = leg.title, title.col = tit.col.leg)

  }

}

#' Drawing a month labeled axis
#'
#' @param leap If set "T", a year is treated as leap year.
#' @param period Periods (years) which experiments were conducted.
#' @param year The start year which experiments were conducted (e.g. 1999, 2001...).
#' @param start The start month and day, e.g. October 15 is c(10,15).
#' @param lwd lwd
#' @param month.lab Month label, "a", "n", "i" and "f" are able to select. "a" is abbreviation, "n" is number, "i" is initial and "f" is full.
#' @param cex.axis axis cex, default is 1.1,
#' @param las las
#' @param mar mar, default is c(2,3.8,1,1).
#' @param mgp mgp, default is c(2.5,0.5,0).
#' @param tcl tcl, default is -0.2.
#' @param inversion Inversion mode. If set "T", plot is drawn with inversion color. Default is "F".
#' @param inv.col Inversion color, if set inversion = "T". Default is "#FFFFFF".
#'
#' @importFrom grDevices boxplot.stats colorRampPalette hcl rgb
#' @importFrom graphics arrows axis barplot box boxplot hist lines matplot par plot points polygon abline
#' @importFrom stats density na.omit sd terms var
#'
#' @export
#'
month.axis <- function(leap = F,
                       period = 1,
                       year = NULL,
                       start = c(1,1),
                       lwd = 1,
                       month.lab = "a",
                       cex.axis = 1.1,
                       las = 1,
                       mar = c(3.8,3.8,1,1),
                       mgp = c(2.5,0.5,0),
                       tcl = -0.2,
                       inversion = F,
                       inv.col = "#FFFFFF"){

  if (inversion == T){
    bg <- "transparent"
    col <- inv.col
  } else {
    bg <- "#FFFFFF"
    col <- "#000000"
  }

  par.old <- par(mar = mar, mgp = mgp, tcl = tcl, bg = bg, fg = col)
  on.exit(par(par.old))

  if(!length(year) == 0){
    leap <- leap.year(year)
  }

  dayls <- if(leap) {
    list(c(1:31), c(1:29) + 31, c(1:31) + 60, c(1:30) + 91,
         c(1:31) + 121, c(1:30) + 152, c(1:31) + 182, c(1:31) + 213,
         c(1:30) + 244, c(1:31) + 274, c(1:30) + 305, c(1:31) + 335)
  } else {
    list(c(1:31), c(1:28) + 31, c(1:31) + 59, c(1:30) + 90,
         c(1:31) + 120, c(1:30) + 151, c(1:31) + 181, c(1:31) + 212,
         c(1:30) + 243, c(1:31) + 273, c(1:30) + 304, c(1:31) + 334)
  }

  monthd <- if(leap) {
    c(31,29,31,30,31,30,31,31,30,31,30,31)
  } else {
    c(31,28,31,30,31,30,31,31,30,31,30,31)
  }
  if(period > 1){
    for(i in 1:(period-1)){
      leap <- leap.year(year + i)

      md <- if(leap) {
        c(31,29,31,30,31,30,31,31,30,31,30,31)
      } else {
        c(31,28,31,30,31,30,31,31,30,31,30,31)
      }
      monthd <- c(monthd, md)
    }
  }

  monthd2 <- rep(0, 12*period)
  for (i in 1:(12*period)){
    if (i == 1) {
      monthd2[i] <- -0.5
    } else {
      monthd2[i] <- sum(monthd[1:i-1]) - 0.5
    }
  }

  if (length(month.lab) > 1) {
    monthn <- month.lab
  } else {
    monthn <- switch(month.lab,
                     "a" = rep(c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec"), period),
                     "n" = rep(1:12, period),
                     "i" = rep(c("J","F","M","A","M","J","J","A","S","O","N","D"), period),
                     "f" = rep(c("January","February","March","April","May","June","July",
                                 "August","September","October","November","December"), period)
    )
  }
  monthp <- rep(0, 12*period)
  for (i in 1:(12*period)){
    if (i == 1) {
      monthp[i] <- monthd[i]/2 - 0.5
    } else {
      monthp[i] <- monthd[i]/2 + sum(monthd[1:i-1]) - 0.5
    }
  }

  monthd2 <- monthd2 - (dayls[[start[1]]][start[2]] - 1)
  monthp <- monthp - (dayls[[start[1]]][start[2]] - 1)

  axis(side=1, labels = F, at = monthd2, lwd = lwd)
  axis(side=1, lty = 0, labels = monthn, at = monthp,
       cex.axis = cex.axis, col.axis = col, col.lab = col, las = las)
}

#' Drawing mean points
#'
#' @param x Data, e.g. numeric vector, formula, e.g. y ~ x, or other object containing analysis result
#' @param data If formula is inputted in "formula" parameter, a data.frame (or list) from which the variables in formula should be taken.
#' @param at Drawing position
#' @param SD If set "T", standard deviation is drawn. Default is "F".
#' @param SE If set "T", standard error is drawn. Default is "F".
#' @param group Grouping factor
#' @param mean.column Column which means are stored.
#' @param pch pch, default is 21.
#' @param cex cex, default is 1.5.
#' @param col.mean mean points color, default is "#000000".
#' @param col.bg mean points background color, default is "#FFFFFF".
#' @param lwd.mean mean lwd, default is 1.
#' @param col.bar error bar color, default is "#000000".
#' @param lwd.bar error bar lwd, default is 1.
#' @param lty line type of error bar, default is 1.
#' @param length Length of vertical bar of tip in error bar, default is 0.5
#' @param horizontal horizontal, default is "F".
#' @param plot If set "F", calculate is only done.
#' @param mar mar, default is c(2,3.8,1,1).
#'
#' @importFrom grDevices boxplot.stats colorRampPalette hcl rgb
#' @importFrom graphics arrows axis barplot box boxplot hist lines matplot par plot points polygon abline
#' @importFrom stats density na.omit sd terms var
#'
#' @export
#'
Mean.pt <- function(x, data = NULL,
                    at = NULL,
                    SD = F,
                    SE = F,
                    group = NULL,
                    mean.column = 1,
                    pch = 21,
                    cex = 1.5,
                    col.mean = "#000000",
                    col.bg = "#FFFFFF",
                    lwd.mean = 1,
                    col.bar = "#000000",
                    lwd.bar = 1,
                    lty = 1,
                    length = 0.5,
                    horizontal = F,
                    plot = T,
                    mar = c(2,3.8,1,1)){

  is.formula <- function(x){
    class(x)=="formula"
  }

  se  <-  function(x){
    y  <-  x[!is.na(x)]
    sqrt(var(as.vector(y))/length(y))
  }


  par.old <- par(mar = mar)
  on.exit(par(par.old))

  calculate <- F
  if(is.formula(x)) {
    calculate <- T
  } else {
    if(is.vector(group)){
      calculate <- T
    }
  }

  if(calculate == T){
    if(is.formula(x)) {
      if(is.null(data)){

        xx <- eval(attr(terms(x), "variables")[[2]])
        group <- eval(attr(terms(x), "variables")[[3]])

      } else {

        xx <- data[,as.character(attr(terms(x), "variables")[[2]])]
        group <- data[,as.character(attr(terms(x), "variables")[[3]])]

      }


    } else {

      if(is.null(data)){

        xx <- as.matrix(x)[,1]

        if(length(group) == 1){
          group <- as.matrix(x)[,group]
        }

      } else {

        xx <- data[,x]

        if(length(group) == 1){
          group <- data[,group]
        }

      }

    }

    m <- tapply(xx, list(group), mean, na.rm = T)
    n <- length(unique(group))

    d <- NULL
    if (!(!(SE == T)&&!(SD == T))){

      if (SE == T){
        d <- tapply(xx, list(group), se)
      } else {
        d <- tapply(xx, list(group), sd, na.rm = T)
      }

    }

  } else {

    m <- x[,mean.column]
    d <- x[,3 - mean.column]
    n <- length(m)

  }

  if(plot == T){

    pos <- at

    if(length(pos) == 0){
      pos <- 1:n
    }

    if (!(!(SE == T)&&!(SD == T))){

      ep <- m+d
      em <- m-d

      if(horizontal == T){
        p1 <- m
        p2 <- pos
        p3 <- ep
        p4 <- pos
        p5 <- m
        p6 <- pos
        p7 <- em
        p8 <- pos
      } else {
        p1 <- pos
        p2 <- m
        p3 <- pos
        p4 <- ep
        p5 <- pos
        p6 <- m
        p7 <- pos
        p8 <- em
      }

      arrows(p1, p2, p3, p4, col = col.bar, lty = lty,
             angle = 90, length = length, lwd = lwd.bar)
      arrows(p5, p6, p7, p8, col = col.bar, lty = lty,
             angle = 90, length = length, lwd = lwd.bar)

    }

    if(horizontal == T){
      p1 <- m
      p2 <- pos
    } else {
      p1 <- pos
      p2 <- m
    }

    points(p1, p2, pch = pch, cex = cex, col = col.mean, bg = col.bg)

  }

  i <- cbind(m,d)

  name <- if(length(d) == 0){
    NULL
  } else {
    if(SD == T){
      "SD"
    } else {
      "SE"
    }
  }

  colnames(i) <- c("Mean", name)
  invisible(i)

}

#' Judging leap year
#' @param year The start year which experiments were conducted (e.g. 1999, 2001...).
#'
leap.year <- function(year){
  if(!length(year) == 0){
    if(year%%400 == 0){
      leap <- T
    } else {
      if(year%%100 == 0){
        leap <- F
      } else {
        if(year%%4 == 0){
          leap <- T
        } else {
          leap <- F
        }
      }
    }
  }
  leap
}

#' Drawing categorized axis
#'
#' @param main Main category, this is given as vector (e.g. c("S", "R"))
#' @param sub Sub category, this is given as vector (e.g. c("1", "10", "100"))
#' @param data If formula is inputted in "formula" parameter, a data.frame (or list) from which the variables in formula should be taken.
#' @param main.axis.at Drawing position of main axis
#' @param main.axis.length Bar length of main axis
#' @param sub.axis.at Drawing position of sub axis
#' @param lwd sub axis lwd, default is 1.
#' @param bar.lwd main axis lwd, default is 1.
#' @param cex.axis axis cex, default is 1.1.
#' @param las las
#' @param x.intsp Inter space of main axis bar, defauit is 0.6.
#' @param y.intsp Inter space of main and sub axis, default is 1.8,
#' @param horizontal horizontal, default is "F".
#' @param mar mar, default is c(3.8,3.8,1,1).
#' @param mgp mgp, default is c(2.5,0.5,0).
#' @param tcl tcl, default is -0.2.
#' @param inversion Inversion mode. If set "T", plot is drawn with inversion color. Default is "F".
#' @param inv.col Inversion color, if set inversion = "T". Default is "#FFFFFF".
#'
#' @importFrom grDevices boxplot.stats colorRampPalette hcl rgb
#' @importFrom graphics arrows axis barplot box boxplot hist lines matplot par plot points polygon abline
#' @importFrom stats density na.omit sd terms var
#'
#' @export
#'
category.axis <- function(main, sub, data = NULL,
                          main.axis.at = NULL,
                          main.axis.length = 3,
                          sub.axis.at = NULL,
                          lwd = 1,
                          bar.lwd = 1,
                          cex.axis = 1.1,
                          las = 1,
                          x.intsp = 0.6,
                          y.intsp = 1.8,
                          horizontal = F,
                          mar = c(3.8,3.8,1,1),
                          mgp = c(2.5,0.5,0),
                          tcl = -0.2,
                          inversion = F,
                          inv.col = "#FFFFFF"){

  if (inversion == T){
    bg <- "transparent"
    col <- inv.col
  } else {
    bg <- "#FFFFFF"
    col <- "#000000"
  }

  side <- 1
  if (horizontal == T){
    side <- 2
    mar[1:2] <- mar[2:1]
  }

  par.old <- par(mar = mar, mgp = mgp, tcl = tcl, bg = bg, fg = col)
  on.exit(par(par.old))

  if(!is.null(data)){
    x1n <- levels(data[,sub])
    x2n <- levels(data[,main])
  } else {
    x1n <- sub
    x2n <- main
  }

  nx1n <- length(x1n)
  nx2n <- length(x2n)

  x1n <- rep(x1n, nx2n)

  if (length(sub.axis.at) == 0){
    at1 <-  1:(nx1n*nx2n)
  } else {
    at1 <- sub.axis.at
  }
  axis(side = side, lty = 1, labels = x1n, at = at1,
       cex.axis = cex.axis, col.axis = col, lwd = lwd, las = las)

  if (horizontal == T){
    mar[2] <- mar[2] - y.intsp
  } else {
    mar[1] <- mar[1] - y.intsp
  }

  par(mar = mar, tcl = 0)

  for(i in 1:nx2n){

    if (length(main.axis.at) == 0){
      at2 <- c((i - 1)*nx1n + 0.5 + x.intsp/2, i*nx1n + 0.5 - x.intsp/2)
    } else {
      at2 <- c(main.axis.at[i] - main.axis.length/2 + x.intsp/2, main.axis.at[i] + main.axis.length/2 - x.intsp/2)
    }

    axis(side = side, lty = 1, lwd = bar.lwd, at = at2,
         labels = F, cex.axis = cex.axis, col.axis = col)
  }

  if (length(main.axis.at) == 0){
    at3 <- seq((nx1n+1)/2, nx1n*(nx2n - 0.5) + 0.5, length = nx2n)
  } else {
    at3 <- main.axis.at
  }

  axis(side = side, lty = 0, at = at3,
       labels = x2n, cex.axis = cex.axis, col.axis = col, las = las)

}

#' Function of overdrawing of low level plot function on plot function in plotn library
#'
#' @param x low level plot function, e.g. points()...
#' @param mar mar, default is c(3.8,3.8,1,1).
#' @param mgp mgp, default is c(2.5,0.5,0).
#' @param tcl tcl, default is -0.2.
#' @param inversion Inversion mode. If set "T", plot is drawn with inversion color. Default is "F".
#' @param inv.col Inversion color, if set inversion = "T". Default is "#FFFFFF".
#'
#' @export
#'
par.set <- function(x,
                    mar = c(3.8,3.8,1,1),
                    mgp = c(2.5,0.5,0),
                    tcl = -0.2,
                    inversion = F,
                    inv.col = "#FFFFFF"){

  if (inversion == T){
    bg <- "transparent"
    col <- inv.col
  } else {
    bg <- "#FFFFFF"
    col <- "#000000"
  }

  par.old <- par(mar = mar, mgp = mgp, tcl = tcl, bg = bg, fg = col)
  on.exit(par(par.old))
  x
}
