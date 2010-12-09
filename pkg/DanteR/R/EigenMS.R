#sva.id <- function(dat, mod, B=20, sv.sig=0.10, seed=NULL) {
##Input
##=============================================================================
##dat: A m genes by n arrays matrix of expression data
##mod: A model matrix for the terms included in the analysis 
##B: The number of null iterations to perform
##sv.sig: The significance cutoff for the surrogate variables
##seed: A seed value for reproducible results
##Output
##=============================================================================
##n.sv: Number of significant surrogate variables. 
##id: An indicator of the significant surrogate variables
##B: number of null iterations
##sv.sig: significance level for surrogate variables
#   print("sva.id")
#   print(dim(dat))
#   print(dim(mod))
#    if(!is.null(seed)){set.seed(seed)}
#    warn <- NULL
#    n <- ncol(dat)
#    m <- nrow(dat)
#    H <- mod %*% solve(t(mod) %*% mod) %*% t(mod) 
#    res <- dat - t(H %*% t(dat))
#    uu <- svd(res)
#    ndf <- n - ceiling(sum(diag(H)))
#    dstat <- uu$d[1:ndf]^2/sum(uu$d[1:ndf]^2)
#	dstat0 <- matrix(0,nrow=B,ncol=ndf)
#	
#	for(i in 1:B){
#        res0 <- t(apply(res, 1, sample, replace=FALSE))
#        res0 <- res0 - t(H %*% t(res0))
#        uu0 <- svd(res0)
#        dstat0[i,] <- uu0$d[1:ndf]^2/sum(uu0$d[1:ndf]^2)
#	}
#	psv <- rep(1,n)
#	for(i in 1:ndf){
#	    psv[i] <- mean(dstat0[,i] >= dstat[i])
#	}
#	for(i in 2:ndf){
#	    psv[i] <- max(psv[(i-1)],psv[i]) 
#	}
#	
#    nsv <- sum(psv <= sv.sig)
#    return(list(n.sv = nsv,p.sv=psv))
#}
#
##multiply square matrix by rectangle with NA's
#mmul <- function(A, B){
#  X <- A
#  Y <- B
#  X[is.na(A)] <- Y[is.na(B)] <- 0
#  R <- X %*% Y
#  R[is.na(B)] <- NA
#  R
#}
#
#rescale.pvals <- function(y1, y1.bar, factors, g.arr = seq(0, 3, by=0.1)){
#
#  fit1 <- lm(y1 ~ ., data=factors)
#  fit2 <- lm(y1.bar ~ ., data=factors)
#
#  ss <- summary(fit1)$fstatistic
#  p1 <- pf(ss[1], ss[2], ss[3], lower=F)
#  ss <- summary(fit2)$fstatistic
#  p2 <- pf(ss[1], ss[2], ss[3], lower=F)
#  ei <- resid(fit2)
#  sd.fit1 <- sd(resid(fit1))
#
#  gp.arr <- rep(NA, length(g.arr))
#  for(i in 1:length(g.arr)){
#    gr <- g.arr[i]
#    #eir = ei + sign(ei)*gr
#    eir = ei*gr
#    #y1.bar.mod <- y1.bar-ei + eir
#    y1.bar.mod <- y1.bar + eir
#    fit.mod <- lm(y1.bar.mod ~ ., data=factors)
#    ss <- summary(fit.mod)$fstatistic
#    p.mod <- pf(ss[1], ss[2], ss[3], lower=F)
#    gp.arr[i] <- (abs(p.mod - p1))
#    if (sd(resid(fit.mod)) > sd.fit1) break;
#  }
##  print(g.arr[which(gp.arr==min(gp.arr, na.rm=T))])
#  y.ret <- y1.bar + ei*g.arr[which(gp.arr==min(gp.arr, na.rm=T))]
#  return(list(y=y.ret, p.values = c(p1, p2, p.mod)))
#}
#
#  #datc[is.na(datc)] <- 0
#  #datc <- dat
#  # filter out everything we don't see straight off
#  #seen.datc <- !is.na(datc)
#  #ii <- rowSums((seen.datc %*% mod)==0)==0
#  #datc <- datc[ii,]
#  #seen.datc <- !is.na(datc)
#  #for incomplete data
#  #RC <- datc - t(H %*% t(datc))
#  #BC.hat <- R%*%V0
#
#  # rescale p-values
#  #my.sva <- sva(dat, mod)
#  #m <- nrow(Y)
#  #n <- ncol(Y)
#  #p.arr <- array(NA, c(m, 3))
#  #Rc <- datc - t(mmul(H, t(datc)))
#
#  #for (i in 1:nrow(Y.rescaled)) {
#  #  pp <- rescale.pvals(Y[i,], Y.bar[i,], factors)
#  #  Y.rescaled[i,] <- pp$y
#  #  p.arr[i,] <- pp$p.v
#  #}
#
#
#sva.build <- function(dat, factors){
#
#  dat <- na.omit(dat)
#  mod <- model.matrix(~., data=factors)
#  Y <- as.matrix(dat)
#  H <- mod %*% solve(t(mod) %*% mod) %*% t(mod) 
#  R <- Y - t(H %*% t(Y))
#
#  my.svd <- svd(R)
#  h <- sva.id(dat, mod, seed=1234)$n.sv
#  print("significant eigenpeptides")
#  print(h)
#  V0 <- cbind(my.svd$v[,1:h])
#  B.hat <- R%*%V0
#
#  Y.bar <- Y - B.hat %*% t(V0)
#  R.bar <- Y.bar - t(H %*% t(Y.bar))
#  R.scale <- apply(R, 1, function(x) sum(x^2))/apply(R.bar, 1, function(x) sum(x^2))
#  R.scale <- sqrt(R.scale)
#  Y.rescaled <- (t(H %*% t(Y.bar)) + R.bar*R.scale)
#
#  return(list(Y=Y.rescaled, V0=V0))
#}
#
#EigenMS <- function(dat, factors, plotflag=TRUE, reference=TRUE, folder=""){
#  header <- colnames(dat)
#  factors <- data.frame(factors)
#  my.sva <- sva.build(dat, factors)
#  Y.rescaled <- my.sva$Y
#    if (plotflag)
#    {
#        tryCatch(
#        {
#          #pic1 <- file.choose()
#          pic1 <- folder
#	  #browser()
#          png(filename=pic1, width=1024, height=768, pointsize=12, bg="white", units="px")
#          #require(Cairo)  
#          #CairoPNG(filename=pic1,width=1024, height=768,bg="white",res=600)
##          matplot(my.sva$V0, type="l", main="Eigenpeptides", xlab="Index", #ylab="Intensity", lwd=((ncol(my.sva$V0)+1):1))
#          plot.ts(my.sva$V0, main="Test eigenpeptides", xlab="Index", ylab="Intensity")
#        },
#        interrupt = function(ex)
#        {
#          cat("An interrupt was detected.\n");
#          print(ex);
#        },
#        error = function(ex)
#        {
#          cat("An error was detected.\n");
#          print(ex);
#        },
#        finally =
#        {
#          cat("Releasing figure file...");
#          dev.off()
#          cat("done\n");
#        }) # tryCatch()
#    }# if
#  Y.rescaled
#}
#
#
#
#