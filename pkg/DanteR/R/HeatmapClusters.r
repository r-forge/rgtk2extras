# Written by Ashoka D. Polpitiya
# for the Department of Energy (PNNL, Richland, WA)
# Copyright 2007, Battelle Memorial Institute
# E-mail: ashoka.polpitiya@pnl.gov
# Website: http://omics.pnl.gov/software
# -------------------------------------------------------------------------
#
# Licensed under the Apache License, Version 2.0; you may not use this file except
# in compliance with the License.  You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0
#
# R Plotting functions used in DAnTE
# -------------------------------------------------------------------------


HeatmapClusters.dialog <- list(
  label =  "A heat map is a false color image with a dendrogram\nadded to the left side and to the top.",
  x.dataframeItem="", label = "Data source",
    signal = c("default", "get.dataset.factors", "factor.column"),              
  do.factors.trueFalseItem=F, label = "Group By Factors",
    signal = c("default", "toggle.sensitive", "factor.column"),                
  factor.column.choiceItem=NULL, label="Choose Factor", indent=10,
  cMap.choiceItem = c("BlackBody" ,value="GreenRed" , "Heat", "BlueWhiteRed"), label="Color Map",
  rowscale.trueFalseItem = TRUE, label = "Scale Rows", tooltip = "Scale Rows By Z_Score",
  noxlab.trueFalseItem = TRUE, label = "Suppress Row Labels",      
  color.missing.trueFalseItem = TRUE, label = "Color Missing Values",   
  labelscale.rangeItem = c(value=1, from=0, to=2, by=0.1), label = "Label Scale",
  
  BREAK=T,
  do.clustering.trueFalseItem=FALSE, label = "Do Clustering?", 
    signal = c("default", "toggle.sensitive", "distance", "do.col.dend", "do.row.dend", "cluster.method", "agglomeration", "Kmeans", "fixSeed"),                  
  distance.choiceItem = c("Euclidean","Maximum","Manhattan","Canberra","Binary","Pearson","Correlation","Spearman","Kendall"), label="Distance Metric", by.index=T, indent=10,
  do.col.dend.trueFalseItem = FALSE,  label="Cluster Columns",  tooltip = "Draw a Column Dendrogram",indent=10,
  do.row.dend.trueFalseItem = FALSE,  label="Cluster Rows", tooltip = "Draw a Row Dendrogram - Warning! This can take a long time.",indent=10,
    #signal = c("default", "toggle.sensitive", "cluster.method", "agglomeration", "Kmeans", "fixSeed"),  
  cluster.method.radiobuttonItem = c("hier", value="kmeans"), item.labels=c("Hierarchical Clustering", "K-means clustering"), label="Clustering Method",indent=10,
  signal = c("default", function(item, agglomeration, Kmeans, fixSeed){
     sapply(c(agglomeration, Kmeans, fixSeed), gtkWidgetSetSensitive, FALSE)
     litems <- list(hier = list(agglomeration), kmeans = list(Kmeans, fixSeed))
     sapply(litems[[get.value(item)]], gtkWidgetSetSensitive, TRUE)
    }, "agglomeration", "Kmeans", "fixSeed"), 
             
    agglomeration.choiceItem = c("Single linkage","Complete linkage","Average method","McQuitty method","Ward method","Median linkage","Centroid linkage"), label="Agglomeration Method", indent=20,
    Kmeans.integerItem = 5, label="K", indent=20, tooltip = "Number of clusters to divide data into",
    fixSeed.trueFalseItem=FALSE, label="Fix Random Seed", indent=20  
)

HeatmapClusters <- function(x, do.row.dend=F, do.col.dend=F, 
                            do.clustering = FALSE,
                            cluster.method = "hier",
                            Kmeans=5,
                            do.factors=F,
                            factor.column = integer(0),
                            fixSeed=FALSE,
                            agglomeration = 1,
                            distance = 0,
                            rowscale=TRUE,
                            file="deleteme.png",
                            cMap="GreenRed",
                            bkground="white",
                            color="wheat2",
                            factor.columns=1,
                            customColors=c("green", "black", "red"),
                            colRange=NULL,
                            labelscale=1.3,
                            noxlab = FALSE,
                            stamp=NULL,
                            color.missing = FALSE,
                            ...)
{
  #png(filename=file,width=1152,height=864,pointsize=12,bg=bkground,
  #          res=600)
  #require(Cairo)
  #CairoPNG(filename=file,width=IMGwidth,height=IMGheight,pointsize=FNTsize,bg=bkground,res=600)
  #corners <- par('usr')

    # Added by TT
  rDend=NULL; cDend=NULL
  ColSides <- FALSE
  RowSides <- FALSE
  
  row_metadata <- attr(x, "Row_Metadata")
  if(!do.row.dend) rDend <- NA
  if(!do.col.dend) cDend <- NA  
  do.Kmeans <- FALSE
  if(cluster.method=="kmeans") do.Kmeans <- TRUE  

  # Factor colors
  box_color <- hsv(h = seq(0,1,1/dim(x)[2]), s=1, v=1)[-1]
  if(do.factors && length(factor.column)==1){
    box_color <- ColorByFactor(x, factor.column)$color
  }
  if(do.factors) ColSides <- TRUE  
  
    # Order by factors
  if(do.factors && length(factor.column) == 1) {
    Factors <- get.factors(x)[,factor.column]
    x <- x[,order(Factors),drop=F]
  }
  
    #
    
  #tryCatch(
  #{
      #if (length(dim(x)) != 2 || !is.numeric(x))
      #  stop("Data must be a numeric matrix")
      stopifnot(is.matrix(x) || is.data.frame(x))
      if (length(dim(x)) != 2 )
        stop("Data must be a numeric matrix or data frame")      
      if (is.matrix(x) && !is.numeric(x))
        stop("Data must be a numeric matrix or data frame")            
      if (is.data.frame(x) && any(sapply(x, class) != "numeric"))
        stop("Data must be a numeric matrix or data frame")                    

      cmap <- colorRampPalette(c("black", "red", "orange","yellow","lightyellow"),
                        space="rgb")(20)
      cmap <- switch (cMap,
            "BlackBody" = colorRampPalette(
                        c("black", "red", "orange","yellow","lightyellow"),
                        space="rgb")(20),
            "GreenRed" = colorRampPalette( c("green", "black", "red"),
                       space="rgb")(20),
            "Heat" = colorRampPalette(c("red", "orange","yellow","lightyellow"),
                        space="rgb")(20),
            "BlueWhiteRed" =  colorRampPalette( c("blue", "white", "red"),
                       space="rgb")(20),
            "Custom" = colorRampPalette(customColors, space="rgb")(20),
            colorRampPalette(c("black", "red", "orange","yellow","lightyellow"),
                        space="rgb")(20)
            )

      linkmethod <- switch(as.character(agglomeration),
            "0" = "single",
            "1" = "complete",
            "2" = "average",
            "3" = "mcquitty",
            "4" = "ward",
            "5" = "median",
            "6" = "centroid",
            "complete"  # default case for switch
            )
      distmethod <- switch(as.character(distance),
            "0" = "euclidean",
            "1" = "maximum",
            "2" = "manhattan",
            "3" = "canberra",
            "4" = "binary",
            "5" = "pearson",
            "6" = "correlation",
            "7" = "spearman",
            "8" = "kendall",
            "euclidean"  # default case for switch
            )

      clust_color <- rep(color,dim(x)[1])
      # Factor Colors
      #Factor <- Factor[,factor.column]

      #Factor <- Factor[is.element(names(Factor),colnames(Data))]
#      if(do.factors && length(factor.column)==1){
#      box_color <- ColorByFactor(x, factor.column)
#        Factor <- Factors[,factor.column,drop=F]
#        stopifnot(dim(Factor)[1] == dim(x)[2])
#        uF <- unique(Factor)
#        colStep <- nrow(uF)
#        #colorRange <- rainbow(colStep)
#        colorRange <- hsv(h = seq(0,1,1/colStep), s=.9, v=.9)
#        #colorRange <- hsv(h = seq(0,1,1/colStep), s=1, v=1)
#        for (i in 1:nrow(uF))
#        {
#            idx <- which(uF[i,]==Factor[,])
#            box_color[idx] <- colorRange[i]
#        }
#        ColSides <- TRUE
#      }
                  
#      if (length(Factor) == dim(x)[2])
#      {
#        uF <- unique(Factor)
#        colStep <- length(uF)
#        colorRange <- hsv(h = seq(0,1,1/colStep), s=1, v=1)
#        for (i in 1:length(uF))
#        {
#            idx <- which(uF[i]==Factor)
#            box_color[idx] <- colorRange[i]
#        }
#        ColSides <- TRUE
#      }
      # end Factor Colors

      clustResult <- 0
      if (do.clustering && do.Kmeans) 
      {
        if (Kmeans >= dim(x)[1])
            stop("Too many clusters requested.")
        ############impute####################
        x1 <- ImputeData(x, mode="median")
        ######################################
        if (fixSeed)
        {
            set.seed(1234)
            N <- 1
        }
        else
            N <- 10

        require(amap)
        km <- Kmeans(x1, Kmeans, iter.max=100, nstart=N)
        x1 <- cbind(km$cluster, x1)
        #############
        clustResult <- as.matrix(km$cluster)
        colnames(clustResult) <- "Km_Clusters"
        #############

        x2 <- x1[order(x1[,1]),]
        x <- x[order(x1[,1]),]
        # Cluster Colors
        clust_color <- rep(color,dim(x)[1])
        colStep <- Kmeans
        colorRange <- hsv(h = seq(0,1,1/colStep), s=1, v=1)
        for (i in 1:Kmeans)
        {
            idx <- which(x2[,1]==i)
            clust_color[idx] <- colorRange[i]
        }
        RowSides <- TRUE
        rDend = NA # why turn this off?
        # end Clust Colors
      }


      RowSideColors <- NA
      if (RowSides) RowSideColors <- clust_color
      ColSideColors <- NA
      if (ColSides) ColSideColors <- box_color
      
      OUT <- heatmap.dante(x, color=cmap, Rowv=rDend, Colv=cDend,
                scale= if (rowscale) "row" else "none",
                hclustfun=hcluster,
                distmethod=distmethod,
                linkmethod=linkmethod,
                cexCol=1*labelscale,
                cexRow=0.8*labelscale,
                ColSideColors = ColSideColors,
                RowSideColors = RowSideColors,
                colRange=colRange,
                noxlab = noxlab,
                color.missing = color.missing,
                margin=c(5,5),...)

      #browser()
      if (!do.clustering)
      {
          clustResult <- as.matrix(1:dim(x)[1])
          rownames(clustResult) <- rev(OUT$labRow)
          colnames(clustResult) <- "HC_Order"
      }
      x <- OUT$X
      # Color legend
      Min <- signif(min(x,na.rm=TRUE), digits=2)
      Max <- signif(max(x,na.rm=TRUE), digits=2)
      Mid <- signif((Min+Max)/2, digits=2)
      col.labels <- c(Min, Mid, Max)

      if (is.null(rDend) && !is.null(cDend))
      { xa <- .94; xb <- .96 }
      else
      { xa <- .87; xb <- .89 }
      color.legend(xa,.3,xb,.7, col.labels, cmap, align="rb", cex=0.8,gradient="y")
      # end color legend
      text(1,1,"a",col="white") # :-)) ????

      if (!is.null(stamp))
            mtext(paste("DAnTE : ", format(Sys.time(), "%m-%d-%Y %I:%M%p"),
                  " (", stamp, ")", sep=""),col=1,cex=.6,line=3, side=1, adj=1)
#  },
#  interrupt = function(ex)
#  {
#    cat("An interrupt was detected.\n");
#    print(ex);
#  },
#  error = function(ex)
#  {
#    plot(c(1,1),type="n",axes=F,xlab="",ylab="")
#    text(1.5,1,paste("Error:", ex),cex=.7)
#    cat("An error was detected.\n");
#    print(ex);
#  },
#  finally =
#  {
#    #cat("Releasing tempfile...");
#    #par(mfrow=c(1,1),pch=1)
#    #dev.off()
#    #cat("done\n");
#  }) # tryCatch()
#
  #return(clustResult)
    # Set metadata attributes
  attr(clustResult, "Row_Metadata") <- row_metadata
  return(list(plot=recordPlot(), clustResult=clustResult))
}
                                                                       
#------------------------------------------------------------------------------
heatmap.dante <- function (x, Rowv = NULL, Colv = if (symm) "Rowv" else NULL,
    distmethod = "euclidean", hclustfun = hcluster, linkmethod="complete",
    reorderfun = function(d, w) reorder(d, w), add.expr, symm = FALSE, 
    revC = identical(Colv, "Rowv"), scale = c("row", "column", "none"), na.rm = TRUE,
    margins = c(5, 5), ColSideColors, RowSideColors, cexRow = 0.2 +
        1/log10(nr), cexCol = 0.2 + 1/log10(nc), labRow = NULL,
    labCol = NULL, main = NULL, xlab = NULL, ylab = NULL, keep.dendro = FALSE,
    color = heat.colors(20), colRange=NULL, noxlab = FALSE, color.missing = TRUE,
    verbose = getOption("verbose"), ...)
{
    require(plotrix)
    require(amap)
    #print("a")
    
    scale <- if (symm && missing(scale))
        "none"
    else match.arg(scale)
    #if (length(di <- dim(x)) != 2 || !is.numeric(x))
    if (length(di <- dim(x)) != 2)
        stop("'x' must be a numeric matrix")
    nr <- di[1]
    nc <- di[2]
    if (nr <= 1 || nc <= 1)
        stop("'x' must have at least 2 rows and 2 columns")
    if (!is.numeric(margins) || length(margins) != 2)
        stop("'margins' must be a numeric vector of length 2")

    ############impute####################
    y <- matrix(NA, nr, nc)
    y[is.na(x)] <- 1   # NA's are stored

    x <- ImputeData(x, mode="median")
    ######################################

    doRdend <- !identical(Rowv, NA)
    doCdend <- !identical(Colv, NA)
    #print(c(doRdend, doCdend))
    #print(c(RowSideColors, ColSideColors))

    if (is.null(Rowv))
        Rowv <- rowMeans(x, na.rm = na.rm)
    if (is.null(Colv))
        Colv <- colMeans(x, na.rm = na.rm)
    if (doRdend) {
        if (inherits(Rowv, "dendrogram"))
            ddr <- Rowv
        else {
            hcr <- hclustfun(x, method=distmethod, link=linkmethod) # Find the row dendrogram
            ddr <- as.dendrogram(hcr)
            if (!is.logical(Rowv) || Rowv)
                ddr <- reorderfun(ddr, Rowv)
        }
        if (nr != length(rowInd <- order.dendrogram(ddr)))
            stop("row dendrogram ordering gave index of wrong length")
    }
    else rowInd <- 1L:nr
    if (doCdend) { 
        if (inherits(Colv, "dendrogram"))
            ddc <- Colv
        else if (identical(Colv, "Rowv")) {
            if (nr != nc)
                stop("Colv = \"Rowv\" but nrow(x) != ncol(x)")
            ddc <- ddr
        }
        else {
            hcc <- hclustfun(t(x), method=distmethod, link="complete") # Find the column dendrogram
            ddc <- as.dendrogram(hcc)
            if (!is.logical(Colv) || Colv)
                ddc <- reorderfun(ddc, Colv)
        }
        if (nc != length(colInd <- order.dendrogram(ddc)))
            stop("column dendrogram ordering gave index of wrong length")
    }
    else colInd <- 1L:nc
    #print("b")    

    x <- x[rowInd, colInd] # Here are the row and column indices
    y <- y[rowInd, colInd] ###############################

    labRow <- if (is.null(labRow))
        if (is.null(rownames(x)))
            (1L:nr)[rowInd]
        else rownames(x)
    else labRow[rowInd]
    labCol <- if (is.null(labCol))
        if (is.null(colnames(x)))
            (1L:nc)[colInd]
        else colnames(x)
    else labCol[colInd]
    
    if (scale == "row") {
        x <- sweep(x, 1, rowMeans(x, na.rm = na.rm), check.margin = FALSE)
        sx <- apply(x, 1, sd, na.rm = na.rm)
        x <- sweep(x, 1, sx, "/", check.margin = FALSE)
    }
    else if (scale == "column") {
        x <- sweep(x, 2, colMeans(x, na.rm = na.rm), check.margin = FALSE)
        sx <- apply(x, 2, sd, na.rm = na.rm)
        x <- sweep(x, 2, sx, "/", check.margin = FALSE)
    }
    #print("c")
    lmat <- rbind(c(NA, 3), 2:1)
    lwid <- c(if (doRdend) 1 else 0.05, 4)
    lhei <- c((if (doCdend) 1 else 0.05) + if (!is.null(main)) 0.2 else 0,
        4)
    if (length(ColSideColors) && !identical(ColSideColors, NA)) {
        if (length(ColSideColors) != nc)
            stop("'ColSideColors' must be a character vector of length ncol(x)")
        lmat <- rbind(lmat[1, ] + 1, c(NA, 1), lmat[2, ] + 1)
        lhei <- c(lhei[1], 0.2, lhei[2])
    }
    if (length(RowSideColors) && !identical(RowSideColors, NA)) {
        if (length(RowSideColors) != nr)
            stop("'RowSideColors' must be a character vector of length nrow(x)")
        lmat <- cbind(lmat[, 1] + 1, c(rep(NA, nrow(lmat) - 1),
            1), lmat[, 2] + 1)
        lwid <- c(lwid[1], 0.2, lwid[2])
    }
    lmat[is.na(lmat)] <- 0
    if (verbose) {
        cat("layout: widths = ", lwid, ", heights = ", lhei,
            "; lmat=\n")
        print(lmat)
    }
   # print("d")
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    layout(lmat, widths = lwid, heights = lhei, respect = TRUE)
    if (!identical(RowSideColors, NA)) {
        par(mar = c(margins[1], 0, 0, 0.5))
        image(rbind(1L:nr), col = RowSideColors[rowInd], axes = FALSE)
    }
    if (!identical(ColSideColors, NA)) {
        par(mar = c(0.5, 0, 0, margins[2]))
        image(cbind(1L:nc), col = ColSideColors[colInd], axes = FALSE)
    }
    par(mar = c(margins[1], 0, 0, margins[2]))
    if (!symm || scale != "none")
    {
        x <- t(x)
        y <- t(y)
    }
    if (revC) {
        iy <- nr:1
        ddr <- rev(ddr)
        x <- x[, iy]
        y <- y[, iy]
    }
    else iy <- 1L:nr
    ###### color range
    if (!is.null(colRange))
    {
      scaleMin <- colRange[1]
      scaleMax <- colRange[2]
      x[x > scaleMax] <- scaleMax
      x[x < scaleMin] <- scaleMin
    }
    #print("e")
    ######
    if(color.missing)
      image(1L:nc, 1L:nr, y, xlim = 0.5 + c(0, nc), col = "grey", ylim = 0.5 +
        c(0, nr), axes = FALSE, xlab = "", ylab = "", ...)     
#      image(1L:nc, 1L:nr, y, xlim = 0.5 + c(0, nc), col = "grey", ylim = 0.5 +
#        c(0, nr), axes = FALSE, xlab = "", ylab = "", add = TRUE, ...) 
    #########################################################################
    image(1L:nc, 1L:nr, x, xlim = 0.5 + c(0, nc), col = color, ylim = 0.5 +
        c(0, nr), axes = FALSE, xlab = "", ylab = "", add=TRUE, ...)
    ############################Plot the missing in grey#####################
    
    axis(1, 1L:nc, labels = labCol, las = 2, line = -0.5, tick = 0,
        cex.axis = cexCol)
    if (!is.null(xlab))
        mtext(xlab, side = 1, line = margins[1] - 1.25)
    if (!noxlab)
        axis(4, iy, labels = labRow, las = 2, line = -0.5, tick = 0,
            cex.axis = cexRow)
    if (!is.null(ylab))
        mtext(ylab, side = 4, line = margins[2] - 1.25)
    if (!missing(add.expr))
        eval(substitute(add.expr))
    par(mar = c(margins[1], 0, 0, 0))
    if (doRdend)
        plot(ddr, horiz = TRUE, axes = FALSE, yaxs = "i", leaflab = "none")
    else frame()
    par(mar = c(0, 0, if (!is.null(main)) 1 else 0, margins[2]))
    if (doCdend)
        plot(ddc, axes = FALSE, xaxs = "i", leaflab = "none")
    else if (!is.null(main))
        frame()
    if (!is.null(main))
        title(main, cex.main = 1.5 * op[["cex.main"]])

    #print("f")
    invisible(list(rowInd = rowInd, colInd = colInd, 
            labRow = labRow, labCol = labCol,
            X=x,
            Rowv = if (keep.dendro && doRdend) ddr, 
            Colv = if (keep.dendro && doCdend) ddc))
}
