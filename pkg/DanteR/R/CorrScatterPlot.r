CorrScatterPlot <- function(Data, N=1000, cMap="heat"){

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))    
    theCor <- abs(cor(x, y, use="pairwise.complete.obs"))
    r <- theCor
    par(usr = c(0, 1, 0, 1))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor) || !is.na(cex.cor)) cex.cor <- 1/strwidth(txt)    
  
    color.idx <- as.integer(99*(theCor - theCor.range[1])/diff(theCor.range) + 1)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = color.map(100)[color.idx])

    x = (x - min(x, na.rm=T))/diff(range(x, na.rm=T))
    y = (y - min(y, na.rm=T))/diff(range(y, na.rm=T))
    #points(x, y, pch=".")
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

panel.cor2 <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))    
    theCor <- abs(cor(x, y, use="pairwise.complete.obs"))
    r <- theCor
    par(usr = c(0, 1, 0, 1))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 1/strwidth(txt)    
  
    color.idx <- as.integer(99*(theCor - theCor.range[1])/diff(theCor.range) + 1)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = color.map(100)[color.idx])

    x = (x - min(x, na.rm=T))/diff(range(x, na.rm=T))
    y = (y - min(y, na.rm=T))/diff(range(y, na.rm=T))
    points(x, y, pch=".")
    #text(0.5, 0.5, txt, cex = cex.cor * r)
}


color.map <- list(rainbow = rainbow, heat=heat.colors, terrain=terrain.colors, topo = topo.colors, cm = cm.colors)[[cMap]]

stopifnot(!is.null(color.map))

N = 1000
Data <- Data[sample(dim(Data)[1], min(N, dim(Data)[1])),]
theCor <- cor(Data, use="pairwise.complete.obs")
theCor[theCor==1] <- NA
theCor.range <- range(theCor, na.rm=T)
pairs(Data, pch=".", gap=0, upper.panel=panel.cor, lower.panel=panel.cor2, xaxt="n", yaxt="n")
invisible(recordPlot())
}




