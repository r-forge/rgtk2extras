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

PCAplot.dialog <- list(
  keep.open = TRUE,
  label = "Principal component analysis (PCA) involves a mathematical procedure\nthat transforms a number of (possibly) correlated variables into a (smaller) \nnumber of uncorrelated variables called principal components. The first \nprincipal component accounts for as much of the variability in the data as \npossible, and each succeeding component accounts for as much of the\nremaining variability as possible.\n",
  Type.radiobuttonItem = c(value="PCA", "PLS"), item.labels = c("Principal Component Analysis (PCA)",  "Partial Least Squares (PLS)"), label="Select Parameters For", visible = F,
  Data.dataframeItem="", label = "Data source",
#    signal = c("default", update_PCs, "Data", "transpose", "PC1", "PC2", "PC3"),
  transpose.radiobuttonItem=c(FALSE, TRUE), item.labels = c("By Columns", "By Rows"), label= "Plot Type",  
    signal = c("default", function(item, Labels) {
      set.value(Labels, TRUE)
      if(get.value(item)) set.value(Labels, FALSE)
    }, "Labels"),
#    signal = c("default", update_PCs, "Data", "transpose", "PC1", "PC2", "PC3"),    
  color.by.factors.trueFalseItem=FALSE, label="Color By Factor",
    signal = c("default", "toggle.dataset.factors", "Data", "factor.column"),              
    factor.column.choiceItem=NULL, label="Choose Factor", indent=10,
  dimensions.radiobuttonItem = c("2D", "3D", value="Interactive 3D"), label = "Plot Type",
    signal = c("default", function(item, sphereSize, labelOffset, PC3, theta, phi){
    sapply(list(sphereSize, labelOffset, PC3, theta, phi), function(x) x$setSensitive(TRUE))    
    if (get.value(item) == "2D")
      sapply(list(sphereSize, labelOffset, PC3, theta, phi), function(x) x$setSensitive(FALSE))
    }, "sphereSize", "labelOffset", "PC3", "theta", "phi"),
  sphereSize.rangeItem=c(value=2, from=0.1, to=4, by=0.1), label="Marker Size",  indent=10,
  labelOffset.rangeItem=c(value=0.5, from=-3, to=3, by=0.1), label="Label Offset", indent=10,
  BREAK=TRUE, 
  PC1.integerItem = c(value=1, from=1, to=10, by=1), label = "PCA Axis 1",   
    tooltip = "Principal component vector to plot on x-axis. This cannot exceed the number of columns",
  PC2.integerItem = c(value=2, from=1, to=10, by=1), label = "PCA Axis 2",
    tooltip = "Principal component vector to plot on y-axis. This cannot exceed the number of columns",
  PC3.integerItem = c(value=3, from=1, to=10, by=1), label = "PCA Axis 3", 
    tooltip = "Principal component vector to plot on z-axis. This cannot exceed the number of columns",
  theta.rangeItem=c(value=35, from=0, to=180, by=1), label="3D X-Y Axis Angle",
  phi.rangeItem=c(value=25, from=0, to=180, by=1), label="3D X-Z Axis Angle",
  do.identify.trueFalseItem=FALSE, label= "Interactive Peak Picking", 
    signal = c("default", function(item, Labels) set.value(Labels, !get.value(item)), "Labels"),  
  Labels.trueFalseItem=TRUE, label="Show Labels",
  drawEllipse.trueFalseItem = FALSE, label = "Draw 95% Confidence Ellipse",
  scree.trueFalseItem=FALSE, label="Show Scree Plot", tooltip = "A scree plot shows the sorted eigenvalues, from large to small, as a function of the eigenvalue index.",    
  Persp.trueFalseItem=TRUE, label="Perspective View",                    
  Lines.trueFalseItem=TRUE, label = "Draw Lines",
  biplotting.trueFalseItem=FALSE, label="Draw Biplot", tooltip = "A biplot represents the observations and variables of a matrix of multivariate data on the same plot.",
    signal = c("default", "toggle.sensitive", "biplotL", "Arrows"),        
  biplotL.trueFalseItem=FALSE, label= "Show Feature Labels", indent=10,
  Arrows.trueFalseItem=FALSE, label= "Show Arrows", indent=10
)

Plot3D.dialog <- list(
  label = "Plot variables in 3-D Space",
  M.dataframeItem="T_Data", label="Data Source",
    signal = c("default", "get.colnames", "dummy"),
    signal = c("default", "set.to", "cols", user.data = character(0)),      
  dummy.listItem = NULL, label = "Available Columns", suppress=T, show.arrows=F,
  cols.listItem=NULL, label="Selected Data Columns",  BREAK=T, max.items = 3,
    signal = c("add", "push.selection", "dummy"),
    signal = c("subtract", "pop.selection", "dummy"),        
  BREAK = T, 
  type.choiceItem = c("p", "s", "l"), item.labels = c("points", "spheres", "lines"), label = "Plot Type",
  sphereSize.rangeItem=c(value=2, from=0.1, to=4, by=0.1), label="Marker Size",  indent=10,
  labelOffset.rangeItem=c(value=0.5, from=-3, to=3, by=0.1), label="Label Offset", indent=10,
  Labels.trueFalseItem=FALSE, label="Show Labels",
  do.identify.trueFalseItem=FALSE, label= "Interactive Peak Picking", signal.on.starting = FALSE,
    signal = c("default", function(item, Labels) set.value(Labels, !get.value(item)), "Labels"),  
  drawEllipse.trueFalseItem = FALSE, label = "Draw 95% Confidence Ellipse"
)


Plot3D <- function(M, cols, txtlabels=rownames(M), 
  Xlabel=cols[1], Ylabel=cols[2], Zlabel=cols[3], 
  sphereSize=1, pca_color="blue", Labels = FALSE, 
  drawEllipse=FALSE, do.identify=FALSE, labelOffset=1, type = "s")
{
  require(rgl)
  open3d(windowRect=c(50, 50, 600, 500))

  X = M[,cols[1]]; Y= M[,cols[2]]; Z = M[,cols[3]]
  plot3d(X, Y, Z, 
                  xlab = Xlabel,
                  ylab = Ylabel,
                  zlab = Zlabel,                   
                  type = type, 
                  size=sphereSize,        
                  box=F,     
                  col=pca_color)
 # if(.Platform$OS.type == "windows") 
 #   rgl.bringtotop(stay=TRUE)   
 # else 
   rgl.bringtotop()   

  if(Labels) text3d(X, Y, Z, txtlabels, adj = c(0, labelOffset))
  if(drawEllipse) {
    require(MASS)
    ct <- cov.trob(na.omit(cbind(X, Y, Z)))
    plot3d(ellipse3d(ct$cov, level = 0.95, centre=ct$center), col='blue', alpha=0.3, add=T, box=F, axes=F)
  }
                              
  if(do.identify){
    pca.dialog <- list(
      keep.open = TRUE,
      title = "3D Options",
      label = "Click Select3D And Drag To Select Points",
      select3d.buttonItem =    "    Select3d     ",
        signal = c("clicked", function(item, labelOffset){
          keep <- select3d()(X, Y, Z)
          if(sum(keep))
            text3d(X[keep], Y[keep], Z[keep], txtlabels[keep], adj = c(0, get.value(labelOffset)))
         }, "labelOffset"),                   
         labelOffset.rangeItem=c(value=labelOffset, from=-3, to=3, by=0.1), label="Label Offset",
         takeAPicture.buttonItem = "Take A Picture", tooltip = "It'll last longer",
         signal = c("clicked", function(item, format){
                       
           fmt = get.value(format)
           fn <- my_choose_files(paste("PCAplot", fmt, sep="."))
             if(length(fn) && nchar(fn)>0){                 
               if(fmt == "png")
                 rgl.snapshot( fn, fmt=fmt, top=TRUE )
               else 
                 rgl.postscript(fn, fmt=fmt, drawText=FALSE)
              }
            }, "format"),
         format.choiceItem = c("png", "ps", "eps", "tex", "pdf", "svg", "pgf"), indent=10, label = "File Format")               
    run.dialog(list, dlg.list=pca.dialog)
  } # do.identify
} #function

# TT - assume factors are called "factors"...
PCAplot <- function(Data,
                    color.by.factors=FALSE,
                    factor.column=integer(0),
                    dimensions = "2D",
                    transpose = FALSE,
                    PC1=1,
                    PC2=2,
                    PC3=3,
                    #file="deleteme.png",
                    #bkground="white",
                    Lines=TRUE,
                    Persp=TRUE,
                    biplotting=FALSE,
                    Labels=TRUE,
                    scree=FALSE,
                    biplotL=FALSE,
                    Arrows=TRUE,
                    Type = "PCA",
                    stamp=NULL,
                    do.identify = FALSE,
                    Factors=get.factors(Data),
                    sphereSize = 2,
                    labelOffset = 1,
                    drawEllipse = FALSE,
                    theta=35, phi=25, # viewing angles
                    ...)
{
    if(dimensions=="2D") {
      PCs <- c(PC1, PC2)
    } else {
      PCs <- c(PC1, PC2, PC3)
    }           
    
    completeData <- Data[complete.cases(Data),]
    if(transpose) {
      if( Type == "PLS" ) stop("\nPLS not supported on transposes")    
      Data <- t(Data)
      completeData <- t(completeData)
    }
    stopifnot(all(PCs <= dim(Data)[2]))

    if (dim(Data)[2] < 3)
    {
      plot(c(1,1),type="n",axes=F,xlab="",ylab="")
      text(1.3,1,"Too few datasets!",cex=1.2)
    }
    else if (dim(completeData)[1] < 4)
    {
      plot(c(1,1),type="n",axes=F,xlab="",ylab="")
      text(1.3,1,"Too many missing values.\n Or too few features.",cex=1.2)
    }
    else
    {
      txtlabels <- colnames(completeData)
      Dim <- length(PCs)
      pca_color <- rainbow(dim(Data)[2]) #rep("blue",dim(Data)[2])
      if(color.by.factors && length(factor.column)==1){
        colrv <- ColorByFactor(Data, factor.column)
        pca_color <- colrv$color
        pca_legend <- colrv$legend
      }
      if(Type == "PCA"){
        pepLabels <- rownames(completeData)
        try(Object <- prcomp(t(completeData),scale=TRUE,retx=TRUE), silent=TRUE)
        if (!exists("Object"))
          Object <- prcomp(t(completeData),scale=FALSE,retx=TRUE)
         
        rot <- Object$rotation
        x <- Object$x
        eigens <- (Object$sdev)^2
        percentVar <- signif(eigens[PCs]/sum(eigens)*100,digits=3)
        percentCumVar <- signif(sum(eigens[PCs])/sum(eigens)*100,digits=3)
        mainLabel = paste("PCA Plot (", percentCumVar, "%)", sep="")
        #print('__________PCA Variation___________________________________')
        #print(summary(Object))                       
        #print('__________________________________________________________')
        screep <- summary(Object)
        screep <- screep$importance[2,]
      } else if (Type == "PLS") {
        require(pls)

        Factors <- get.factors(Data)
        rnf <- intersect(rownames(Factors), colnames(Data))
        Factor <- Factors[rnf,factor.column,drop=F]
        Data <- Data[,rnf, drop=F]
        stopifnot(dim(Factor)[1] == dim(Data)[2])                        
        plsF <- plsFactors(Factor)
                        
        pepLabels <- rownames(completeData)
        plsData <- list(y=plsF, x=t(completeData))
        Object <- plsr(y~x, data = plsData, x=TRUE, y=TRUE,
                                         na.action=na.exclude)
        x <- Object$scores
        rot <- Object$loadings
        percentVar <- signif(Object$Xvar/Object$Xtotvar * 100, digits=3)
        percentCumVar <- signif(sum(percentVar[PCs]), digits=3)
        mainLabel = paste("PLS Plot (", percentCumVar, "%)", sep="")
        screep <- percentVar
        #print('___________PLS Variation__________________________________')
        #print(signif(Object$Xvar/Object$Xtotvar * 100, digits=4))               #print('__________________________________________________________')
      } else {
        stop("Type not recognized")
      }
            
      pvals <- apply(abs(rot), 2, function(x) 1 - ecdf(x)(x))
      rownames(pvals) <- rownames(rot)

      Xlabel <- paste("PC",PCs[1]," (",percentVar[1],"%)",sep="")
      Ylabel <- paste("PC",PCs[2]," (",percentVar[2],"%)",sep="")
      Zlabel <- paste("PC",PCs[3]," (",percentVar[3],"%)",sep="")

      if (biplotting)
      {
        biplot.dante(Object, Rownames=pepLabels,
          Colnames=txtlabels, Type=Type, PCs=PCs, Labels=biplotL,
          col.obj=pca_color, Arrows=Arrows, perspective=Persp, theta=theta, phi=phi)
        if (length(factor.column) == dim(Data)[2])
          legend("topleft", names(pca_legend), col=pca_legend,pch=19, bg='transparent', inset = .02)
      } else if(scree) {
#            par(new = TRUE, fig = c(.84, .94, .8, .9), mar = c(0,0,0,0))
        barplot(screep,main="",cex.axis=.6,cex.names=.6,axisnames=F)
#          }
      }  else  { # plot PCA
      if (dimensions == "2D") {
        X <- x[,PCs[1]]
        Y <- x[,PCs[2]]                   
        plot(X, Y, cex=2, col=pca_color, pch=19, xlab=Xlabel,ylab=Ylabel,main=mainLabel)
       if (Labels)
         text(X, Y, txtlabels,cex=.6,pos=4) # text labels
       if(drawEllipse)
         data.ellipse(X, Y, add=T, levels=0.95, plot.points=F, center.pch=F, robust=T,  col="blue")                                                
       if(do.identify)
         identify(X, Y, txtlabels) 
       }  else if (dimensions == "3D") {
        if (Persp)
        {
            scatter.3D(x[,PCs],col=pca_color,pch=19,cex=sphereSize,
                xlab=Xlabel,
                ylab=Ylabel,
                zlab=Zlabel,
                main=mainLabel,
                Lines=Lines,
                txtLabels=txtlabels, Labels=Labels, theta=theta, phi=phi)
        }
        else
        {
            require(scatterplot3d)
            Type="p"
            if (Lines)
                Type="h"
            rR <- scatterplot3d(x[,PCs[1]],
                x[,PCs[2]],x[,PCs[3]],
                color=pca_color,cex.symbols=sphereSize,pch=19, xlab=Xlabel,
                ylab=Ylabel,zlab=Zlabel,
                box=FALSE,grid=TRUE,main=mainLabel,
                angle=theta,type=Type)
            if (Labels)
            {
                text(rR$xyz.convert(x[,PCs]),txtlabels,cex=.6,pos=4) #labels
            }
        }       
   
       } else if (dimensions == "Interactive 3D") {             
         Plot3D(M=x, cols=PCs[1:3], txtlabels=txtlabels, Xlabel=Xlabel, Ylabel=Ylabel, Zlabel=Zlabel, 
                sphereSize=sphereSize, pca_color=pca_color, Labels = Labels, drawEllipse=drawEllipse, do.identify=do.identify, labelOffset=labelOffset)
         }   
       } 

                }
#
#                if (length(Factor) == dim(Data)[2])
#                    legend("topleft",uF,col=colorRange[1:length(uF)],pch=19,
#                        bg='transparent', inset = .02)
#                if (scree)
#                {
#                    par(new = TRUE, fig = c(.84, .94, .8, .9), mar = c(0,0,0,0))
#                    barplot(screep,main="",cex.axis=.6,cex.names=.6,axisnames=F)
#                }
#            }
#            if (!is.null(stamp))
#                mtext(paste("DAnTE : ", format(Sys.time(), "%m-%d-%Y %I:%M%p"),
#                    " (", stamp, ")", sep=""),col=1,cex=.6,line=2, side=1,
#                    adj=1, outer=T)
#            #return(list(X=rot, P=pvals, Mode=Type))
#            invisible(list(X=x, P=pvals, Mode=Type))
#        }
}

#-------------------------------------------------------------

plsFactors <- function(factors)
{
    plsF <- rep(numeric(0),length(factors))
    uF <- unique(factors)
    ulevels <- seq(from=0, to=length(uF)-1, by=1)
    for (i in 1:length(uF))
    {
        idx <- which(uF[i] == factors)
        plsF[idx] <- ulevels[i]
    }
    return(plsF)
}

#-------------------------------------------------------------------------
scatter.3D <- function(x,xlim=NULL,ylim=NULL,zlim=NULL,
                       col=par("col"),
                       pch=par("pch"), cex=par("cex"),
                       main=NULL, Lines=TRUE,
                       xlab=NULL, ylab=NULL, zlab=NULL,
                       txtLabels=NULL, Labels=FALSE, theta=theta, phi=phi, ...)
{
   x1 <- x

   if(is.matrix(x))
   {
     z <- x[,3]
     y <- x[,2]
     x <- x[,1]
     x1[,3] <- rep(min(z,na.rm=T),length(x1[,3])) # z1
   }
   if(is.matrix(x1))
   {
     z1 <- x1[,3]
     y1 <- x1[,2]
     x1 <- x1[,1]
   }
   if(missing(zlim))
   {
     z.grid <- matrix(range(z),2,2)
   }
   else
   {
     z.grid <- matrix(zlim,2,2)
   }

   if(missing(xlim)){ xlim <- range(x) }
   if(missing(ylim)){ ylim <- range(y) }

   persp(xlim, ylim, z.grid, col = NA, border=NA,
         xlab=xlab, ylab=ylab, zlab=zlab,
         main=main, theta=theta, phi=phi, d=2,
         ticktype = "detailed",...) -> res
   #----------------------------------
   trans3d <- function(x,y,z, pmat)
   {
     tr <- cbind(x,y,z,1) %*% pmat
     list(x = tr[,1]/tr[,4], y= tr[,2]/tr[,4])
   }
   #----------------------------------
   out <- trans3d(x,y,z,pmat=res)
   out1 <- trans3d(x1,y1,z1,pmat=res)
   if (Lines)
   {
      for(i in 1:length(out$x))
      {
        lines(c(out$x[i],out1$x[i]),c(out$y[i],out1$y[i]), col="gray", ...)
      }
   }
   points(out, col=col, pch=pch, cex=cex, ...)
   if (Labels)
      text(out,txtLabels,cex=.6,pos=4) # text labels

   return(invisible(out))
}

#-----------------------------------------------------------------------

biplot.dante <- function (object,
                       Rownames,
                       Colnames,
                       Type = c('PCA','PLS'),
                       PCs=c(1,2),
                       box=FALSE,
                       Labels=TRUE,
                       col.obj=2,
                       col.var=1,
                       cex=.6,
                       Arrows=TRUE,
                       perspective=FALSE, theta=theta, phi=phi, ... )
{
    n.values <- length(PCs)
    switch(match.arg(Type),
        PCA = {
            scores <- object$x[,PCs]
            rots <- object$rotation[,PCs]
            eigens <- (object$sdev)^2
            percentVar <- signif(eigens[PCs]/sum(eigens)*100,digits=3)
            percentCumVar <- signif(sum(eigens[PCs])/sum(eigens)*100,digits=3)
            mainBiLabel = paste("PCA Bi-Plot (", percentCumVar, "%)", sep="")
            n <- NROW(object$x)
            lam <- object$sdev[PCs] * 2
            scores <- t(t(scores)/lam)
            rots <- t(t(rots) * lam)
            #scores <- customScale(scores,rots)
        },
        PLS = {
            scores <- object$scores[,PCs]
            rots <- object$loadings[,PCs]
            scores <- customScale(scores,rots)
            percentVar <- signif(object$Xvar/object$Xtotvar * 100, digits=3)
            percentCumVar <- signif(sum(percentVar[PCs]), digits=3)
            mainBiLabel = paste("PLS Bi-Plot (", percentCumVar, "%)", sep="")
        })

    Xlabel = paste("PC",PCs[1]," (",percentVar[1],"%)",sep="")
    Ylabel = paste("PC",PCs[2]," (",percentVar[2],"%)",sep="")
    Zlabel = paste("PC",PCs[3]," (",percentVar[3],"%)",sep="")

    Vars <- rbind(scores, rots, rep(0, n.values)) # scores = samples, rots = features

    if (length(col.obj) == 1)
        col.obj = rep(col.obj,nrow(scores))

    if(n.values == 2) {
      plot(Vars, xlab=Xlabel, ylab=Ylabel, main=mainBiLabel,
           type = if(Labels) 'n' else 'p',
           col=col.var, pch = 20) # samples + features
      points(scores,col=col.obj, pch = 19, type='p') # variables
      grid()

      if(Labels)
        text(x=rots[,1], y=rots[,2], labels = Rownames,
           cex=cex, col=col.var, pos=4)
      if (Arrows)
        arrows(x0=0, y0=0, x1=rots[,1], y1=rots[,2], length=0.1, angle=20,
               col=col.var)
      text(x=scores[,1], y=scores[,2], labels=Colnames,
             cex=cex, col=col.obj, pos=4)
    }
    if(n.values == 3) {
        if (perspective)
        {
            x1 <- Vars[,1]
            y1 <- Vars[,2]
            z1 <- Vars[,3]
            xlim <- range(x1)
            ylim <- range(y1)
            z.grid <- matrix(range(z1),2,2)
            persp(xlim, ylim, z.grid, col = NA, border=NA,
                xlab=Xlabel, ylab=Ylabel, zlab=Zlabel,
                main=mainBiLabel, theta=theta, phi=phi, d=2,
                ticktype = "detailed") -> res
            out <- trans3d(scores[,1],scores[,2],scores[,3],pmat=res)
            out1 <- trans3d(rots[,1],rots[,2],rots[,3],pmat=res)
            out0 <- trans3d(0,0,0,pmat=res)
            if(Labels)
                text(out1, labels=Rownames, col=1, cex=cex, pos=4)
            else
                points(out1, col=col.var, pch=20, cex=cex)
            points(out, col=col.obj, pch=19, cex=1.3)
            if (Arrows)
            {
                for(i in 1:length(out1$x))
                {
                    lines(c(out1$x[i],out0$x[1]),c(out1$y[i],out0$y[1]),
                          col=col.var)
                }
            }
            text(out,Colnames,cex=.6,col=col.obj, pos=4)
        }
        else
        {
            require(scatterplot3d)
            graph = scatterplot3d(Vars,
                                  type = if(Labels) 'n' else 'p',
                                  xlab = Xlabel,
                                  ylab = Ylabel,
                                  zlab = Zlabel,
                                  main = mainBiLabel,
                                  grid = TRUE, box = box,
                                  cex.symbols = cex, color = col.var, pch = 20)
            graph$points3d(scores[,1], scores[,2], scores[,3],
                           pch = 19, type='p', col=col.obj, cex = 1.3)
            if(Labels)
               text(graph$xyz.convert(rots), labels=Rownames,
                    col=col.var, cex=cex, pos=4)

            if (Arrows)
            {
                for(i in 1:nrow(rots))
                {
                    graph$points3d(c(0, rots[i,1]), c(0, rots[i,2]),
                             c(0, rots[i,3]), type='l', col=col.var)
                }
            }
            text(graph$xyz.convert(scores), labels=Colnames, col=col.obj,
                 cex=cex, pos=4)
        }
    }
}
