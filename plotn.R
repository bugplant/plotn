#https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html

col_genelator <- function (palette = "d3",
                           palette_types = NULL,
                           number = 10, 
                           alpha = 0.5){
  targetPackages <- c('ggplot2', 'ggsci', 'scales') 
  newPackages <- targetPackages[!(targetPackages %in% installed.packages()[,"Package"])]
  if(length(newPackages)) install.packages(newPackages, repos = "http://cran.us.r-project.org")
  for(package in targetPackages) library(package, character.only = T)
  
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

default_fill <- col_genelator()
default_bor <- col_genelator(alpha = 1)

plotn <- function(formula, y = NULL, data = NULL, ..., 
                  xlim = NULL,
                  ylim = NULL,
                  las = 1, 
                  cex.axis = 1.1, 
                  cex.lab = 1.3, 
                  font.lab = 2,
                  pch = 16,
                  col.dot = default_bor,
                  col.fill = default_fill,
                  col.line = default_bor,
                  col.bor = "default",
                  col.bg = "#FFFFFF",
                  legend = F,
                  pos.leg = "outright",
                  pch.leg = NULL,
                  bty.leg = "n",
                  bg.leg = "transparent",
                  lty = 1,
                  lwd = 1,
                  pt.cex.leg = 1.5,
                  tx.cex.leg = 1.1,                     
                  pt.col.leg = NULL,
                  pt.bg.leg = NULL,
                  lty.leg = NULL,
                  lwd.leg = NULL,
                  tx.col.leg = NULL,
                  leg.lab = NULL,
                  leg.sp = 2.5,
                  inset = 1,
                  leg.title = NULL,
                  tit.col.leg = NULL,
                  mode = "s",
                  group = NULL,
                  fill = F,
                  density = NA,
                  angle = 45,
                  mar = c(3.8,3.8,1,1), 
                  mgp = c(2.5,0.5,0),
                  tcl = -0.2, 
                  inversion = F,
                  inv.col = "#FFFFFF"){
  
  #内部関数の定義
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
  
  if(!is.na(col.dot[1])){
    if(col.dot[1] == "default"){
      col.dot <- col
    }
  }
  
  if(!is.na(col.fill[1])){
    if(col.fill[1] == "default"){
      col.fill <- bg
    }
  }
  
  if(!is.na(col.bor[1])){
    if(col.bor[1] == "default"){
      col.bor <- "transparent"
    }
  }
  
  if(!is.na(col.line[1])){
    if(col.line[1] == "default"){
      col.line <- col
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
  
  mode <-  switch(mode, 
                  "s" = mode,
                  "m" = mode,
                  "s")
  
  par.old <- par(mar = mar, mgp = mgp, tcl = tcl, bg = bg, fg = col)
  on.exit(par(par.old))
  
  #描画
  if ( mode == "s"){
    
    if(!is.formula(formula)) {
      
      x <- formula
      
      if(!length(y) == 0){
        plot(x = x, y = y, ..., xlim = xlim, ylim = ylim,
             las = las, cex.axis = cex.axis, col = col.dot[1],
             cex.lab = cex.lab, font.lab = font.lab,
             col.axis = col, col.lab = col, pch = pch, lty = lty, lwd = lwd)
        
      } else {
        
        error <- NULL
        error <- try(plot(x, ..., xlim = xlim, ylim = ylim,las = las, cex.axis = cex.axis, 
                          col = col.dot[1], cex.lab = cex.lab, font.lab = font.lab,
                          col.axis = col, col.lab = col, pch = pch, lty = lty, 
                          lwd = lwd), 
                     silent = T)
        
        if (class(error) == "try-error") {
          warning("Data wasn't plotted with default settings, so trying to plot with different settings.")
          plot(x, ..., las = las, cex.axis = cex.axis, 
               col = col.dot,cex.lab = cex.lab, font.lab = font.lab,
               col.axis = col, col.lab = col, pch = pch, lty = lty, 
               lwd = lwd)
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
           col.axis = col, col.lab = col, pch = pch, lty = lty, lwd = lwd)
    }
    
    if (fill == T) {
      polygon(x, y, col = col.fill, border = col.bor,
              density = density, angle = angle)
      lines(x, y, col = col.line, lty = lty, lwd = lwd)
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
                lty = lty, lwd = lwd)
      } else {
        matplot(x = x, ..., pch = pch,
                las = las, cex.axis = cex.axis, xlim = xlim, ylim = ylim,
                cex.lab = cex.lab, font.lab = font.lab, col = col.dot,
                col.axis = col, col.lab = col,
                lty = lty, lwd = lwd)
      }
      
      if (fill == T) {
        for(i in 1:n){
          if(ncol(x) > 1){
            polygon(c(1:length(x[,1])), x[,i], col = col.fill[i], border = col.bor[i],
                    density = density[i], angle = angle[i])
            lines(c(1:length(x[,1])), x[,i], col = col.line[i], lty = lty[i], lwd = lwd)
          } else {
            polygon(x, y[,i], col = col.fill[i], border = col.bor[i],
                    density = density[i], angle = angle[i])
            lines(x, y[,i], col = col.line[i], lty = lty[i], lwd = lwd)
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
             cex.lab = cex.lab, font.lab = font.lab, col = NA,
             col.axis = col, col.lab = col)
        
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
             cex.lab = cex.lab, font.lab = font.lab, col = NA,
             col.axis = col, col.lab = col)
        
      }
      
      for (i in 1:length(names)){
        xx <- x[g == names[i]]
        yy <- y[g == names[i]]
        
        par(new = T)
        
        plot(x = xx, y = yy, ..., axes = F,
             col = col.dot[i], bg = col.bg, col.lab = NA, pch = pch[i], 
             lty = lty[i], lwd = lwd, xlim = xlim, ylim = ylim)
        
        if (fill == T) {
          polygon(xx, yy, col = col.fill[i], border = col.bor[i],
                  density = density[i], angle = angle[i])
          lines(xx, yy, col = col.line[i], lty = lty[i], lwd = lwd)
          
        }
      }
      
    }
    
  }
  
  #凡例
  if(legend == T){
    
    par(xpd=T)
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
      lty.leg <- lty
    }
    
    if (length(lwd.leg) == 0){
      lwd.leg <- lwd
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
           pt.bg = pt.bg.leg, pch = pch.leg, lwd = lwd.leg, x.intersp = x.intersp,
           bty = bty.leg, bg = bg.leg, text.col = tx.col.leg,
           pt.cex = pt.cex.leg, cex = tx.cex.leg, horiz = horiz,
           title = leg.title, title.col = tit.col.leg)
  }
  
}


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
                     col.fill = default_fill,
                     col.bor = default_bor,
                     col.dot = default_bor,
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
  
  #内部関数の定義
  is.formula <- function(x){
    class(x)=="formula"
  }
  
  se  <-  function(x){
    y  <-  x[!is.na(x)]  #  remove  the  missing  values
    sqrt(var(as.vector(y))/length(y))
  }
  
  #反転させる？
  if (inversion == T){ 
    bg <- "transparent"
    col <- inv.col
  } else {
    bg <- "#FFFFFF"
    col <- "#000000"
  }
  
  #デフォルトカラーの設定
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
  
  #水平？
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
  
  #凡例つける？
  if(legend == T){
    switch (pos.leg,
            "outtopright" = eval(mar[4] <- mar[4]+leg.sp),
            "outright" = eval(mar[4] <- mar[4]+leg.sp),
            "outbottomright" = eval(mar[4] <- mar[4]+leg.sp),
            "outbottom" = eval(mar[1] <- mar[1]+leg.sp)
    )
  }
  
  #横にずらす？
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
  
  #ラベル名取得
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
  
  #軸の設定
  boxplot(formula, data = data, ..., xlim = xlim, ylim = ylim, 
          outline = F, las = las, horizontal = horizontal,
          bty = "n", axes = F, add = add, 
          col = NA, border = NA)
  
  if((!xaxt == "n")&&(horizontal == F) || (!yaxt == "n")&&(horizontal == T)){
    axis(side = pos, at = 1:length(names), labels = names, cex.axis = cex.axis, cex.lab = cex.lab, 
         col.axis = col, col.lab = col, font.lab = font.lab, las = las)
  }
  
  #描画
  
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
  
  #全点
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
  
  #外れ値
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
  
  #平均、SD、SE
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
  
  #凡例
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


barplotn <- function(formula, data = NULL, ..., 
                     las = 1, 
                     cex.axis = 1.1, 
                     cex.lab = 1.3, 
                     font.lab = 2,
                     lwd.bor = 2,
                     lwd.axis = 1,
                     lwd.stat = 1,
                     col.fill = default_fill,
                     col.bor = default_bor,
                     col.stat = "default",
                     length = "auto",
                     space = 0.5,
                     names = NULL,
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
  
  #内部関数の定義
  is.formula <- function(x){
    class(x)=="formula"
  }
  
  se  <-  function(x){
    y  <-  x[!is.na(x)]  #  remove  the  missing  values
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
  
  #水平？
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
  
  #ラベル名取得
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
                 col.axis = col, col.lab = col, xlab = xlab, ylab = ylab, beside = beside)
  box(lty=1, lwd = lwd.axis)
  
  #SD、SE
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
  
  #凡例
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


histn <- function(formula, data = NULL, ...,
                  xlab = NULL,
                  ylab = NULL,
                  las = 1, 
                  main = "",
                  cex.axis = 1.1, 
                  cex.lab = 1.3, 
                  font.lab = 2,
                  col.fill = default_fill,
                  col.bor = default_bor,
                  hist.dens = NA,
                  hist.ang = 45,
                  kernel = F,
                  freq = T,
                  col.line = default_bor,
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
  
  #breaksの定義
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
  
  #凡例
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
                     col.fill = default_fill,
                     col.mar = default_bor,
                     col.box = "#FFFFFF",
                     col.bor = "#000000",
                     col.stat = "default",
                     col.dot = default_bor,
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
  
  #内部関数の定義
  is.formula <- function(x){
    class(x)=="formula"
  }
  
  se  <-  function(x){
    y  <-  x[!is.na(x)]  #  remove  the  missing  values
    sqrt(var(as.vector(y))/length(y))
  }
  
  #反転させる？
  if (inversion == T){ 
    bg <- "transparent"
    col <- inv.col
  } else {
    bg <- "#FFFFFF"
    col <- "#000000"
  }
  
  #水平？
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
  
  #デフォルトカラーの設定
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
  
  #バイオリンをトリムする？
  if (!trim == T){
    cut <- 3
  } else {
    cut <- 0
  }
  
  #凡例つける？
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
  
  #横にずらす？
  side <- switch(side,
                 "both" = side,
                 "right" = side,
                 "left" = side,
                 "both")
  
  g <- switch(side,
              "both" = 0,
              "right" = side.sp,
              "left" = -side.sp) 
  
  
  #ラベル名取得
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
  
  #バイオリンプロット描画
  
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
  
  #ボックスプロット描画
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
  
  #全点
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
  
  #外れ値
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
  
  #平均、SD、SE
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
  
  #凡例
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
  
  #反転させる？
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
                    mar = c(2,3.8,1,1)
){
  
  #内部関数の定義
  is.formula <- function(x){
    class(x)=="formula"
  }
  
  se  <-  function(x){
    y  <-  x[!is.na(x)]  #  remove  the  missing  values
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
  
  #描画
  
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
  #反転させる？
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
  
}#https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html

col_genelator <- function (palette = "d3",
                           palette_types = NULL,
                           number = 10, 
                           alpha = 0.5){
  targetPackages <- c('ggplot2', 'ggsci', 'scales') 
  newPackages <- targetPackages[!(targetPackages %in% installed.packages()[,"Package"])]
  if(length(newPackages)) install.packages(newPackages, repos = "http://cran.us.r-project.org")
  for(package in targetPackages) library(package, character.only = T)
  
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

default_fill <- col_genelator()
default_bor <- col_genelator(alpha = 1)

plotn <- function(formula, y = NULL, data = NULL, ..., 
                  xlim = NULL,
                  ylim = NULL,
                  las = 1, 
                  cex.axis = 1.1, 
                  cex.lab = 1.3, 
                  font.lab = 2,
                  pch = 16,
                  col.dot = default_bor,
                  col.fill = default_fill,
                  col.line = default_bor,
                  col.bor = "default",
                  col.bg = "#FFFFFF",
                  legend = F,
                  pos.leg = "outright",
                  pch.leg = NULL,
                  bty.leg = "n",
                  bg.leg = "transparent",
                  lty = 1,
                  lwd = 1,
                  pt.cex.leg = 1.5,
                  tx.cex.leg = 1.1,                     
                  pt.col.leg = NULL,
                  pt.bg.leg = NULL,
                  lty.leg = NULL,
                  lwd.leg = NULL,
                  tx.col.leg = NULL,
                  leg.lab = NULL,
                  leg.sp = 2.5,
                  inset = 1,
                  leg.title = NULL,
                  tit.col.leg = NULL,
                  mode = "s",
                  group = NULL,
                  fill = F,
                  density = NA,
                  angle = 45,
                  mar = c(3.8,3.8,1,1), 
                  mgp = c(2.5,0.5,0),
                  tcl = -0.2, 
                  inversion = F,
                  inv.col = "#FFFFFF"){
  
  #内部関数の定義
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
  
  if(!is.na(col.dot[1])){
    if(col.dot[1] == "default"){
      col.dot <- col
    }
  }
  
  if(!is.na(col.fill[1])){
    if(col.fill[1] == "default"){
      col.fill <- bg
    }
  }
  
  if(!is.na(col.bor[1])){
    if(col.bor[1] == "default"){
      col.bor <- "transparent"
    }
  }
  
  if(!is.na(col.line[1])){
    if(col.line[1] == "default"){
      col.line <- col
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
  
  mode <-  switch(mode, 
                  "s" = mode,
                  "m" = mode,
                  "s")
  
  par.old <- par(mar = mar, mgp = mgp, tcl = tcl, bg = bg, fg = col)
  on.exit(par(par.old))
  
  #描画
  if ( mode == "s"){
    
    if(!is.formula(formula)) {
      
      x <- formula
      
      if(!length(y) == 0){
        plot(x = x, y = y, ..., xlim = xlim, ylim = ylim,
             las = las, cex.axis = cex.axis, col = col.dot[1],
             cex.lab = cex.lab, font.lab = font.lab,
             col.axis = col, col.lab = col, pch = pch, lty = lty, lwd = lwd)
        
      } else {
        
        error <- NULL
        error <- try(plot(x, ..., xlim = xlim, ylim = ylim,las = las, cex.axis = cex.axis, 
                          col = col.dot[1], cex.lab = cex.lab, font.lab = font.lab,
                          col.axis = col, col.lab = col, pch = pch, lty = lty, 
                          lwd = lwd), 
                     silent = T)
        
        if (class(error) == "try-error") {
          warning("Data wasn't plotted with default settings, so trying to plot with different settings.")
          plot(x, ..., las = las, cex.axis = cex.axis, 
               col = col.dot,cex.lab = cex.lab, font.lab = font.lab,
               col.axis = col, col.lab = col, pch = pch, lty = lty, 
               lwd = lwd)
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
           col.axis = col, col.lab = col, pch = pch, lty = lty, lwd = lwd)
    }
    
    if (fill == T) {
      polygon(x, y, col = col.fill, border = col.bor,
              density = density, angle = angle)
      lines(x, y, col = col.line, lty = lty, lwd = lwd)
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
                lty = lty, lwd = lwd)
      } else {
        matplot(x = x, ..., pch = pch,
                las = las, cex.axis = cex.axis, xlim = xlim, ylim = ylim,
                cex.lab = cex.lab, font.lab = font.lab, col = col.dot,
                col.axis = col, col.lab = col,
                lty = lty, lwd = lwd)
      }
      
      if (fill == T) {
        for(i in 1:n){
          if(ncol(x) > 1){
            polygon(c(1:length(x[,1])), x[,i], col = col.fill[i], border = col.bor[i],
                    density = density[i], angle = angle[i])
            lines(c(1:length(x[,1])), x[,i], col = col.line[i], lty = lty[i], lwd = lwd)
          } else {
            polygon(x, y[,i], col = col.fill[i], border = col.bor[i],
                    density = density[i], angle = angle[i])
            lines(x, y[,i], col = col.line[i], lty = lty[i], lwd = lwd)
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
             cex.lab = cex.lab, font.lab = font.lab, col = NA,
             col.axis = col, col.lab = col)
        
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
             cex.lab = cex.lab, font.lab = font.lab, col = NA,
             col.axis = col, col.lab = col)
        
      }
      
      for (i in 1:length(names)){
        xx <- x[g == names[i]]
        yy <- y[g == names[i]]
        
        par(new = T)
        
        plot(x = xx, y = yy, ..., axes = F,
             col = col.dot[i], bg = col.bg, col.lab = NA, pch = pch[i], 
             lty = lty[i], lwd = lwd, xlim = xlim, ylim = ylim)
        
        if (fill == T) {
          polygon(xx, yy, col = col.fill[i], border = col.bor[i],
                  density = density[i], angle = angle[i])
          lines(xx, yy, col = col.line[i], lty = lty[i], lwd = lwd)
          
        }
      }
      
    }
    
  }
  
  #凡例
  if(legend == T){
    
    par(xpd=T)
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
      lty.leg <- lty
    }
    
    if (length(lwd.leg) == 0){
      lwd.leg <- lwd
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
           pt.bg = pt.bg.leg, pch = pch.leg, lwd = lwd.leg, x.intersp = x.intersp,
           bty = bty.leg, bg = bg.leg, text.col = tx.col.leg,
           pt.cex = pt.cex.leg, cex = tx.cex.leg, horiz = horiz,
           title = leg.title, title.col = tit.col.leg)
  }
  
}


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
                     col.fill = default_fill,
                     col.bor = default_bor,
                     col.dot = default_bor,
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
  
  #内部関数の定義
  is.formula <- function(x){
    class(x)=="formula"
  }
  
  se  <-  function(x){
    y  <-  x[!is.na(x)]  #  remove  the  missing  values
    sqrt(var(as.vector(y))/length(y))
  }
  
  #反転させる？
  if (inversion == T){ 
    bg <- "transparent"
    col <- inv.col
  } else {
    bg <- "#FFFFFF"
    col <- "#000000"
  }
  
  #デフォルトカラーの設定
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
  
  #水平？
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
  
  #凡例つける？
  if(legend == T){
    switch (pos.leg,
            "outtopright" = eval(mar[4] <- mar[4]+leg.sp),
            "outright" = eval(mar[4] <- mar[4]+leg.sp),
            "outbottomright" = eval(mar[4] <- mar[4]+leg.sp),
            "outbottom" = eval(mar[1] <- mar[1]+leg.sp)
    )
  }
  
  #横にずらす？
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
  
  #ラベル名取得
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
  
  #軸の設定
  boxplot(formula, data = data, ..., xlim = xlim, ylim = ylim, 
          outline = F, las = las, horizontal = horizontal,
          bty = "n", axes = F, add = add, 
          col = NA, border = NA)
  
  if((!xaxt == "n")&&(horizontal == F) || (!yaxt == "n")&&(horizontal == T)){
    axis(side = pos, at = 1:length(names), labels = names, cex.axis = cex.axis, cex.lab = cex.lab, 
         col.axis = col, col.lab = col, font.lab = font.lab, las = las)
  }
  
  #描画
  
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
  
  #全点
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
  
  #外れ値
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
  
  #平均、SD、SE
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
  
  #凡例
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


barplotn <- function(formula, data = NULL, ..., 
                     las = 1, 
                     cex.axis = 1.1, 
                     cex.lab = 1.3, 
                     font.lab = 2,
                     lwd.bor = 2,
                     lwd.axis = 1,
                     lwd.stat = 1,
                     col.fill = default_fill,
                     col.bor = default_bor,
                     col.stat = "default",
                     length = "auto",
                     space = 0.5,
                     names = NULL,
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
  
  #内部関数の定義
  is.formula <- function(x){
    class(x)=="formula"
  }
  
  se  <-  function(x){
    y  <-  x[!is.na(x)]  #  remove  the  missing  values
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
  
  #水平？
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
  
  #ラベル名取得
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
                 col.axis = col, col.lab = col, xlab = xlab, ylab = ylab, beside = beside)
  box(lty=1, lwd = lwd.axis)
  
  #SD、SE
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
  
  #凡例
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


histn <- function(formula, data = NULL, ...,
                  xlab = NULL,
                  ylab = NULL,
                  las = 1, 
                  main = "",
                  cex.axis = 1.1, 
                  cex.lab = 1.3, 
                  font.lab = 2,
                  col.fill = default_fill,
                  col.bor = default_bor,
                  hist.dens = NA,
                  hist.ang = 45,
                  kernel = F,
                  freq = T,
                  col.line = default_bor,
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
  
  #breaksの定義
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
  
  #凡例
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
                     col.fill = default_fill,
                     col.mar = default_bor,
                     col.box = "#FFFFFF",
                     col.bor = "#000000",
                     col.stat = "default",
                     col.dot = default_bor,
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
  
  #内部関数の定義
  is.formula <- function(x){
    class(x)=="formula"
  }
  
  se  <-  function(x){
    y  <-  x[!is.na(x)]  #  remove  the  missing  values
    sqrt(var(as.vector(y))/length(y))
  }
  
  #反転させる？
  if (inversion == T){ 
    bg <- "transparent"
    col <- inv.col
  } else {
    bg <- "#FFFFFF"
    col <- "#000000"
  }
  
  #水平？
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
  
  #デフォルトカラーの設定
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
  
  #バイオリンをトリムする？
  if (!trim == T){
    cut <- 3
  } else {
    cut <- 0
  }
  
  #凡例つける？
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
  
  #横にずらす？
  side <- switch(side,
                 "both" = side,
                 "right" = side,
                 "left" = side,
                 "both")
  
  g <- switch(side,
              "both" = 0,
              "right" = side.sp,
              "left" = -side.sp) 
  
  
  #ラベル名取得
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
  
  #バイオリンプロット描画
  
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
  
  #ボックスプロット描画
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
  
  #全点
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
  
  #外れ値
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
  
  #平均、SD、SE
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
  
  #凡例
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
  
  #反転させる？
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
                    mar = c(2,3.8,1,1)
){
  
  #内部関数の定義
  is.formula <- function(x){
    class(x)=="formula"
  }
  
  se  <-  function(x){
    y  <-  x[!is.na(x)]  #  remove  the  missing  values
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
  
  #描画
  
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
  #反転させる？
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


par.set <- function(x, #低水準作図関数
                    mar = c(3.8,3.8,1,1), 
                    mgp = c(2.5,0.5,0),
                    tcl = -0.2,
                    inversion = F,
                    inv.col = "#FFFFFF"){
  
  #反転させる？
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
