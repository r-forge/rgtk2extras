# EigenMS normalization
# Ref: "Normalization of peak intensities in bottom-up MS-based proteomics using
#       singular value decomposition" Karpievitch YV, Taverner T, Adkins JN,
#       Callister SJ, Anderson GA, Smith RD, Dabney AR. Bioinformatics 2009
#
# Written by Tom Taverner, Shelley Herbrich and Yuliya Karpievitch
# for Pacific Northwest National Lab.

plot.eigentrends <- function(svdr, title1){
  v <- svdr$v
  d <- svdr$d
  ss <- d^2
  Tk = signif(ss/sum(ss)* 100, 2)
  #  pe <- signif(d/sum(d, na.rm=T)*100, 2)
  titles <- paste("Trend ", 1:3, " (", Tk[1:3], "%)", sep = "")
  do.text <- function(j) mtext(titles[j], cex=0.7, padj=-0.7, adj=1)
  range.y <- range(as.numeric(v[,1:3]), na.rm=T)
  
  
  toplot1_1 <- as.numeric(v[,1])
  toplot1_2 <- as.numeric(v[,2])
  toplot1_3 <- as.numeric(v[,3])

  print("top trend: ")
  print(toplot1_1)

  plot(c(1:length(toplot1_1)), toplot1_1, type='b', ann=F, ylim=range.y)
  do.text(1)
  abline(h=0, lty=3)
  title(title1, cex.main = 1.2, font.main= 1, col.main= "purple", ylab=NULL)
  plot(c(1:length(toplot1_2)), toplot1_2, type='b', ann=F, ylim=range.y)
  do.text(2)
  abline(h=0, lty=3)
  plot(c(1:length(toplot1_3)), toplot1_3, type='b', ann=F, ylim=range.y)
  do.text(3)
  abline(h=0, lty=3)
}
  
eigen_norm1 = function(m, treatment, protein_group=2){
# Performs modified normalization and rescaling prior to imputation 
# Handles no sibling peptides with missing groups
# 
# Input:
#   m: An m x n (peptides x samples) matrix of expression data
#      peptide and protein identifiers come from the get.ProtInfo()
#   treatment:  vector indicating the treatment group of each sample i.e. [1 1 1 1 2 2 2 2...]
#  
# Output: list of:
#   final: k x m matrix of normalized data (includes no-sibling peptides with completely missing groups) 
#	  k <= m, as some peptides may have been filtered out due to low information content

  print(dim(m))
  print(treatment)

  # preprocess data; these steps only used for the stand-alone version
  prot.info <- get.ProtInfo(m)  # pepID, prID - not as in our text file data frame? here a mat 
  prot.info <- cbind(prot.info[,1], prot.info[,protein_group])  
  treatment <- get.factors(m)[,treatment] # get treatment from the facotrs in DanteR
  n.treatment <- length(treatment)

  #find peptides with no siblings
  all.proteins <- unique(prot.info[,2])
  all.proteins <- all.proteins[order(all.proteins)] 
  numb <- array(NA, length(all.proteins))
  for (k in 1:length(all.proteins)) {
    prot <- all.proteins[k]
    pmid.matches <- m[prot.info[,2]==prot,1]
    numb[k] <- length(pmid.matches)    
  }
  singular <- all.proteins[which(numb ==1)]
  singlepep <- which(prot.info[,2] %in% singular)
  nosib <- rbind(m[singlepep,])

  #filter out min.missing
  nobs <- array(NA, c(nrow(m), length(unique(treatment))))
  for(i in 1:nrow(m)) {
    for(j in 1:length(unique(treatment))) {
      nobs[i,j] <- sum(!is.na(m[i, treatment==unique(treatment)[j]]))
    }
  } 
  #remove peptides with missing groups
  present.min <- apply(nobs, 1, min)
  ii <- present.min == 0
  nmiss <- sum(present.min == 0)
  pmiss <- rbind(m[ii,])
  nosib.miss <- nosib[ii[singlepep],,drop=FALSE]
  nosib.miss.info <- prot.info[prot.info[,1] %in% rownames(nosib.miss),]
  nosib.miss.comp <- cbind(nosib.miss.info, nosib.miss)

  #create matrix for present peptides
  present <- prot.info[which(!prot.info[,1] %in% rownames(pmiss)), ]
  pres <- m[which(!rownames(m) %in% rownames(pmiss)), ]

  #select only 'complete' peptides
  nobs<- array(NA, nrow(pres))
  for (i in 1:nrow(pres)) nobs[i] <- sum(!is.na(pres[i,]))
  ii <- nobs == ncol(pres)
  complete <- rbind(pres[ii,])
  # write.table(complete, file = "complete_DanteR.txt", append = FALSE,
  #            quote = FALSE, sep = "\t",
  #            eol = "\n", na = "NaN", dec = ".", row.names = TRUE,
  #            col.names = TRUE, qmethod = c("escape", "double"))

  # compute bias with 'complete' eigenvector and 'present' residuals
  # calculate eigenpeptides for 'complete' data only
  comp <- as.vector(t(complete))
  mod.c <- model.matrix(~treatment, contrasts=list(treatment="contr.sum"))
  Y.c <- as.matrix(complete)
  H.c <- mod.c %*% solve(t(mod.c) %*% mod.c) %*% t(mod.c)  # in R we can use lm()
  R.c <- Y.c - t(H.c %*% t(Y.c))
  # compute the SVD of residual matrix R - for plotting only?  or to use?
  # center each peptide around zero (subtract its mean across samples)
  

  R.c_center <- t(scale(t(R.c), center = TRUE, scale = FALSE))
  my.svd <- svd(R.c_center)

  #identify number of eigenvalues that account for a significant amount of residual variation
  numcompletepep <- dim(complete)[1] # save to return to the user as part of the return list  
  # this is important info for papers...
  h.c <- sva.id(complete, mod.c, seed=1234)$n.sv
  print("Number of significant eigenpeptides/trends")
  print(h.c)
  # center each peptide around zero (subtract its mean across samples)
  complete_center <- t(scale(t(complete), center = TRUE, scale = FALSE))
  toplot1 <- svd(complete_center)
  par(mfcol=c(3,2))
  par(mar = c(2,2,2,2))
  plot.eigentrends(toplot1, "Raw Data")
  plot.eigentrends(my.svd, "Residual Data")
  bringToTop()
  retval <- list(m=m, treatment=treatment, my.svd=my.svd,
    pres=pres, n.treatment=n.treatment, nosib.miss=nosib.miss,
    nosib.miss.info=nosib.miss.info, h.c=h.c, present=present, toplot1=toplot1)
  return(retval)
}

eigen_norm2 <- function(rv){
#rv <<- rv
  m <- rv$m
  treatment <- rv$treatment
  my.svd <- rv$my.svd
  pres <- rv$pres
  n.treatment <- rv$n.treatment
  nosib.miss <- rv$nosib.miss
  nosib.miss.info <- rv$nosib.miss.info
  h.c <- rv$h.c
  present <- rv$present
  toplot1 <- rv$toplot1

  obj_metadata <- list(attr(m, "Row_Metadata"), attr(m, "Column_Metadata"))
  obj_dimnames <- dimnames(m)

  # ask the user if he/she wants to use estimated number of significant trends
#  st <- paste("Number of significant eigentrends = ", h.c, "\n", "Do you want to use this number of Eigentrends?")
#  markup = list(title='Eigentrends', label=st, choice.radiobuttonItem=c("Yes","No"),
#                eigen_new_val.integerItem = h.c, eigen_new_val.label="Use this number: ",
#                tooltip = "Use number of trends we identified or fewer to prevent overfitting")
#  print("after markup")
#  h.c <- run.dialog(get_eigenpep, dlg.list=markup)$retval
#

  #residual eigenpeptides
  #V0 <- cbind(my.svd$v[,1:h.c]) # drop
  V0 <- my.svd$v[,1:h.c,drop=F]

  print("Normalizing...")

  #compute residual matrix for 'present' peptides
  betahat <- matrix(NA,nrow=length(unique(treatment)),ncol=nrow(pres))
  newR <- array(NA, c(nrow(pres), n.treatment))
  norm_m <- array(NA, c(nrow(pres), n.treatment))
  numsamp = dim(m)[2]
  for (i in 1:nrow(pres)) {
    pep <- pres[i, ] 
    pos <- !is.na(pep)
    peptemp <- as.matrix(pep[pos])
    ftemp <- treatment[pos]
    modt <- model.matrix( ~ftemp, contrasts=list(ftemp="contr.sum"))
    bhat <-  solve(t(modt) %*% modt) %*% t(modt) %*% peptemp
    betahat[,i] <- bhat
    ceffects <- modt %*% bhat

    resm <- rep(NA, numsamp)
    resm[pos] <- as.numeric(pep[pos] - ceffects)
    newR[i, ] <- resm
    bias <- array(NA, numsamp)
    bias[pos] <- resm[pos] %*% V0[pos,] %*% t(V0[pos,])
    norm_m[i, ] <- as.numeric(pep - bias)
  }

############normalize with missing no-sibling peps####################
# these are proteins with 1 peptide, in which 1+ group is completely missing
# in our experience such peptides/proteins may be biomarkers and trashing
# them is nto a good idea.
if(nrow(nosib.miss) > 0){
  #(a) impute missing values
  rv_imp_elim = imp_elim_proteins(nosib.miss, treatment)  
  imp_elim = rv_imp_elim$proteins
  notblank.idx = rv_imp_elim$notblank.idx
  imp_elim = imp_elim[notblank.idx,,drop=F]
  nosib.miss.info = nosib.miss.info[notblank.idx,,drop=F]
  
  elim = cbind(nosib.miss.info, imp_elim)

  #(b) normalize?
  beta_hat <- matrix(NA,nrow=length(unique(treatment)),ncol=nrow(imp_elim))
  norm_nosib <- array(NA, c(nrow(imp_elim), length(treatment)))
#imp_elim <<- imp_elim
  for (i in 1:nrow(imp_elim)) {
    pep <- imp_elim[i, ] 
    pos <- !is.na(pep)
    peptemp <- as.matrix(pep[pos])
    ftemp <- treatment[pos]
    modt <- model.matrix( ~ftemp, contrasts=list(ftemp="contr.sum"))
    bhat <-  solve(t(modt) %*% modt) %*% t(modt) %*% peptemp
    beta_hat[,i] <- bhat
    ceffects <- modt %*% bhat
    resm <- as.numeric(pep[pos]- ceffects)
    bias <- resm[pos] %*% V0[pos,] %*% t(V0[pos,])
    norm_nosib[i, pos] <- as.numeric(pep[pos] - bias)
   }

  norm_nosib_comp <- cbind(nosib.miss.info, norm_nosib)
  rownames(norm_nosib_comp) <- norm_nosib_comp[,1]

  #norm_m_comp <- cbind(present[,(1:2)],norm_m)
  #colnames(norm_m_comp) <- colnames(norm_nosib_comp)
  #norm_comp <- rbind(norm_m_comp, norm_nosib_comp)
  }

   print("Normalizing......")

########## end of normalization with missing values ##################
################## begin rescaling values ############################

  max_e <- max(newR, na.rm = T)
  maxrange <- (round(max_e*10)/10)/2
  #try with 0.5 and 1
  # if (maxrange > 0.5) maxrange <- 0.5
  if (maxrange > 1) maxrange <- sqrt(maxrange)
  
print("rescaling upper bound")
print(maxrange)

  p_correct <- rep(NA, nrow(pres))
  p_rescaled <- rep(NA, nrow(pres))
  r_factor <- rep(NA, nrow(pres))
  y_rescaled <- matrix(NA, nrow(pres), length(treatment))

  mod <- model.matrix( ~treatment, contrasts=list(treatment="contr.sum"))
  X_0 <- cbind(cbind(c(rep(1, length(treatment)))), V0)
  X_1 <- cbind(mod, V0)

  for (t in 1:nrow(pres)){
    y1 <- as.numeric(pres[t,])
    y1 <- as.vector(na.omit(y1))
    y1.bar <- as.numeric(norm_m[t,])
    y1.bar <- as.vector(na.omit(y1.bar))
    X_null <- X_0[!is.na(pres[t,]),]
    X_unrestricted <- X_1[!is.na(pres[t,]),]
    ftemp <- treatment[!is.na(pres[t,])]
    #compute p-values for experimental factors of interest using a model that also includes the residual eigenpeptides computed by EigenMS 
    # correct p-values
    fit1 <- lm(y1 ~ftemp, contrasts=list(ftemp="contr.sum"))
    fit_null <- lm(y1~ X_null[, -1])
    fit_unrestricted <- lm(y1~ X_unrestricted[, -1])
    p_correct[t] <- anova(fit_null, fit_unrestricted)[6][2,]

    fit2 <- lm(y1.bar ~ ftemp,  contrasts=list(ftemp="contr.sum"))
    ei <- resid(fit2)
    sd.fit1 <- sd(resid(fit1))

    g.arr = seq(0, maxrange, by=0.1)
    gp.arr <- rep(NA, length(g.arr))
    p.mod <- rep(NA, length(g.arr))
    y1.temp <- rep(NA, length(g.arr))
    for(i in 1:length(g.arr)){
      gr <- g.arr[i]
      eir = ei + sign(ei)*gr
      y1.bar.mod <- y1.bar-ei + eir
      fit.mod <- lm(y1.bar.mod ~ ., data=ftemp)
      ss <- summary(fit.mod)$fstatistic
      p.mod[i] <- pf(ss[1], ss[2], ss[3], lower=F)
      gp.arr[i] <- (abs(p.mod[i] - p_correct[t]))
      if (sd(resid(fit.mod)) > sd.fit1) break;
    }
    if(!any(is.finite(gp.arr))) next; # check code tom 092710
    p_rescaled[t] <- p.mod[which(gp.arr==min(gp.arr, na.rm=T))]
    y_r <- y1.bar + sign(ei)*g.arr[which(gp.arr==min(gp.arr, na.rm=T))]
    r_factor[t] <-g.arr[which(gp.arr==min(gp.arr, na.rm=T))]

    k=1
    y_rescale <- array(NA, n.treatment)
    for (j in 1:length(pres[t,])){
      if (!is.na(pres[t,j])) {
        y_rescale[j] <- y_r[k]
        k <- k+1
      } else {
        y_rescale[j]=NA
      }
    }
  y_rescaled[t,] <- y_rescale
  }
  y_raw <- data.frame(present, y_rescaled)

  if(nrow(nosib.miss) > 0){
    colnames(norm_nosib_comp) <- colnames(y_raw)
    final = rbind(y_raw, norm_nosib_comp)
  } else{
    final = y_raw
  }
  rownames(final) <- final[,1] # add row names to the matrix

  # Faster that the code we had before
  complete_all <- y_rescaled[rowSums(is.na(y_rescaled))==0,,drop=F]
  
  x11() # make R open new figure window
  par(mfcol=c(3,2))
  par(mar = c(2,2,2,2))
  # center each peptide around zero (subtract its mean across samples)
  # note: we are not changing matrix itself, only centerig what we pass to svd
  complete_all_center <- t(scale(t(complete_all), center = TRUE, scale = FALSE))
  toplot3 <- svd(complete_all_center)
  plot.eigentrends(toplot1, "Raw Data")
  plot.eigentrends(toplot3, "Normalized Data")
  bringToTop() # bring current figure to front

  print("done with normalization!!!")
  final <- final[,-c(1,2),drop=F]
  
  # copy metadata and row and column names
  # holdrownames <- rownames(final)
  # holdcolnames <- colnames(final)
  
  final <- data.matrix(final) # as.matrix destroys the row/col info, so do that 1st
# browser()

  colnames(final) <-  obj_dimnames[[2]]
  colnames(V0) <-  paste("Trend", 1:ncol(V0), sep="_")
  rownames(V0) <-  obj_dimnames[[2]]

  attr(final, "Row_Metadata") <- obj_metadata[[1]]
  attr(final, "Column_Metadata") <- attr(V0, "Row_Metadata") <- obj_metadata[[2]]

  maxrange <- data.matrix(maxrange)
  return(list(normalized=final, eigentrends=V0, rescrange=maxrange))
}





######## end of EigenMS Normalization ###########
### EigenMS helper functions
sva.id <- function(dat, mod, B=20, sv.sig=0.10, seed=NULL) {
# Executes Surrogate Variable Analysis
# Input:
#   dat: A m peptides/genes by n samples matrix of expression data
#   mod: A model matrix for the terms included in the analysis 
#   B: The number of null iterations to perform
#   sv.sig: The significance cutoff for the surrogate variables
#   seed: A seed value for reproducible results
# Output
#    n.sv: Number of significant surrogate variables. 
#    id: An indicator of the significant surrogate variables
#    B: number of permutation to do
#    sv.sig: significance level for surrogate variables
  print("Number of complete peptides (and samples) used in SVD")
  print(dim(dat))

  if(!is.null(seed))  { set.seed(seed) }
  warn <- NULL
  n <- ncol(dat)
  m <- nrow(dat)
  H <- mod %*% solve(t(mod) %*% mod) %*% t(mod) # regression y = (X'X)-1X'B
  res <- dat - t(H %*% t(dat))
  
  # centering was not done before...
  # center each peptide around zero (subtract its mean across samples)
  # note: we are not changing matrix itself, only centerig what we pass to svd
  res_center <- t(scale(t(res), center = TRUE, scale = FALSE))
  uu <- svd(res_center) # the diag is min(n, m)
  
  # tom - this looks like a bug
  ndf <- min(n, m) - ceiling(sum(diag(H)))  
  #ndf <- m - ceiling(sum(diag(H)))
  dstat <- uu$d[1:ndf]^2/sum(uu$d[1:ndf]^2)
	dstat0 <- matrix(0,nrow=B,ncol=ndf)
	
	for(i in 1:B){
    res0 <- t(apply(res, 1, sample, replace=FALSE)) # regression
    res0 <- res0 - t(H %*% t(res0))
    # center each peptide around zero (subtract its mean across samples)
    # note: we are not changing matrix itself, only centerig what we pass to svd
    res0_center <- t(scale(t(res0), center = TRUE, scale = FALSE))
    uu0 <- svd(res0_center)
    dstat0[i,] <- uu0$d[1:ndf]^2/sum(uu0$d[1:ndf]^2)
	}
	psv <- rep(1,n)
	for(i in 1:ndf){
	    psv[i] <- mean(dstat0[,i] >= dstat[i])
	}
	for(i in 2:ndf){
	    psv[i] <- max(psv[(i-1)],psv[i]) 
	}
	
  nsv <- sum(psv <= sv.sig)
    # tom - this should be at least 1
  #nsv <- min(sum(psv <= sv.sig), 1, na.rm=T)
  return(list(n.sv = nsv,p.sv=psv))
}
# End sva.id
 
mmul <- function(A, B){
#multiply square matrix by rectangle with NA's
  X <- A
  Y <- B
  X[is.na(A)] <- Y[is.na(B)] <- 0
  R <- X %*% Y
  R[is.na(B)] <- NA
  R
}
# End mmul

#rescale.pvals <- function(y, y.bar, factors, g.arr = seq(0, range, by=0.1)){
## Replaces the systematic variability removed by EigenMS with
## just enough random variability to approximately achieve the correct number
## of degrees of freedom
#
#p_correct <- rep(NA, nrow(factors))
#p_rescaled <- rep(NA, nrow(factors))
#y_rescaled <- matrix(NA, 200, 25)
#for (t in 1:nrow(y)){
#  y1 <- as.numeric(y[t,])
#  y1.bar <- as.numeric(y.bar[t,])
## Compute p-values for experimental factors of interest using a model that also includes the residual eigenpeptides computed by EigenMS
#  ftry <- factor(rep(1:5, each=5)) 
#  fit1 <- lm(y1 ~ftry, contrasts=list(ftry="contr.sum"))
#  X_null <- cbind(cbind(c(rep(1, 25))), V0)
#  X_unrestricted <- cbind(model.matrix(fit1), V0)
#  fit_null <- lm(y1~ X_null[, -1])
#  fit_unrestricted <- lm(y1~ X_unrestricted[, -1])
#  ## YAY!! correct pvalues match Yuliyas results!!
#  p_correct[t] <- anova(fit_null, fit_unrestricted)[6][2,]
#
#  fit2 <- lm(y1.bar ~ ftry,  contrasts=list(ftry="contr.sum"))
#
#  ss <- summary(fit1)$fstatistic
#  p1 <- pf(ss[1], ss[2], ss[3], lower=F)
#  ss <- summary(fit2)$fstatistic
#  p2 <- pf(ss[1], ss[2], ss[3], lower=F)
#  ei <- resid(fit2)
#  sd.fit1 <- sd(resid(fit1))
#
#  gp.arr <- rep(NA, length(g.arr))
#  p.mod <- rep(NA, length(g.arr))
#  y1.temp <- rep(NA, length(g.arr))
#  for(i in 1:length(g.arr)){
#    gr <- g.arr[i]
#    eir = ei + sign(ei)*gr
#    #eir = ei*gr
#    y1.bar.mod <- y1.bar-ei + eir
#    #y1.bar.mod <- y1.bar + eir
#    fit.mod <- lm(y1.bar.mod ~ ., data=factors)
#    ss <- summary(fit.mod)$fstatistic
#    p.mod[i] <- pf(ss[1], ss[2], ss[3], lower=F)
#    gp.arr[i] <- (abs(p.mod[i] - p_correct[t]))
#    if (sd(resid(fit.mod)) > sd.fit1) break;
#  }
#
#  p_rescaled[t] <- (p.mod[which(gp.arr==min(gp.arr, na.rm=T))])
#  y_rescaled[t,] <- y1.bar + sign(ei)*g.arr[which(gp.arr==min(gp.arr, na.rm=T))]
#} 
#  return(y=y_rescaled)
#}

#######################################################
## This may not be needed
#protein_var1 <- function(Y_raw, treatment){
#  n.peptide <- nrow(Y_raw)
#  y <- as.vector(t(Y_raw))
#  n <- length(y)
#  n.treatment <- length(treatment)
#  n.u.treatment <- length(unique(treatment))
#
#  n.present <- array(NA, c(n.peptide, n.u.treatment))
#  colnames(n.present) <- unique(treatment)
#  for(i in 1:n.peptide) for(j in 1:n.u.treatment) n.present[i,j] <- sum(!is.na(y[peptide==i & treatment==unique(treatment)[j]]))
#  peptides.missing <- rowSums(is.na(Y_raw))
#
#  f.treatment <- factor(rep(treatment, n.peptide))
#  f.peptide <- factor(peptide)
#
#  # estimate rough model parameters
#  # create model matrix for each protein and
#  # remove any peptides with missing values
#  ii <- (1:n)[is.na(y)]
#  if (n.peptide != 1){
#    X  <- model.matrix(~f.peptide + f.treatment, contrasts = list(f.treatment="contr.sum", f.peptide="contr.sum") )
#  } else {
#    X <- model.matrix(~f.treatment, contrasts=list(f.treatment="contr.sum"))
#  }
#  if(length(ii) > 0){
#    y.c <- y[-ii]
#    X.c <- X[-ii,]
#  } else {
#    y.c <- y
#    X.c <- X
#  }
#
#  # calculate initial beta values and residuals
#  beta <- drop(solve(t(X.c) %*% X.c) %*% t(X.c) %*% y.c)
#  Y_hat <- X.c %*% beta
#  Y_temp <- Y_raw
#  Y_temp <- as.numeric(t(Y_temp))
#  Y_temp[!is.na(Y_temp)] <- Y_hat
#  Y_temp <- matrix(Y_temp, nrow = n.peptide, byrow = T)
#  Y_hat <- Y_temp
#  ee <- Y_raw - Y_hat
#
#  effects <- X.c %*% beta
#  resid <- y.c - effects
#  overall_var <- var(resid)
#  return(list(overall_var=det(overall_var)))
#}

#######################################################################
imp_elim_proteins<- function(nosib.miss, treatment){
# Impute 1 peptide proteins with 1 grp completely missing
# 
# INPUT: proteins - 1 peptide proteins with 1 grp missing completely (NaNs)
#        grps - grouping information for observations, can be 2+ grps here, 
#               but not sure what to do with more then 2 groups if 2+ gs are
#               missing completely. So only 1 grp is completely missing.
# OUTPUT: prs - imputed proteins, same size as proteins parameter
#nosib.miss <<- nosib.miss
#treatment <<- treatment 
  tlvls <- unique(treatment)
  proteins <- nosib.miss
  ng = length(unique(treatment))
  for (i in 1:nrow(proteins)) {
  	pr <- as.vector(proteins[i,])
    #% find number of missing values in ALL groups
    miss <- array(NA, c(1, ng))
    for (j in 1:ng) miss[j] <- sum(is.na(pr[treatment==unique(treatment)[j]]))
#    pos <- miss==0
    pos <- miss==min(miss)  # Tom 092510
    present_groups <- pr[treatment==unique(treatment)[pos]]
	
      #% compute mean and stdev from one of the present groups, make sure no NaNs are used
	pospres = !is.na(present_groups)
	presvals = present_groups[pospres]
	pepmean = mean(presvals)
	pepstd =  sd(presvals)	
	if(is.na(pepstd)) next;
	
      #% imputing only COMPLETELY missing peptides here
	for (j in 1:ng) {
		if   (!pos[j]) { #% imute only the ones not at pos complete
			imppos = is.na(pr[treatment==tlvls[j]])  #% should be all in this group, but double check
			imppepmean = pepmean - 6* pepstd
			imppepstd = pepstd
			tt = imppepmean - 3 * imppepstd 
			kk = 0
      while (tt < 0 && kk < 10){  # added kk counter - tom 092510
				offset = .25
				gap = imppepstd * offset
				imppepstd = imppepstd * (1- offset)
				imppepmean = imppepmean + 3 * gap
				tt = imppepmean - 3 * imppepstd
				kk <- kk + 1
			}
			imp_tmp = rnorm(length(imppos), imppepmean, pepstd)
			pr[treatment==tlvls[j]] <- imp_tmp
		}
	}
  proteins[i,] <- pr	
  }
  
  # tom - this routine gives some nearly-blank rows
  # to avoid singularities, i'm going to scan this to see which protein rows
  # have all blanks in one row and remove them
#  proteins <<- proteins
  xx <- (!is.na(proteins)) %*% model.matrix(~treatment-1)
  notblank.idx <- rep(TRUE, nrow(xx))
  for(jj in 1:ncol(xx)) notblank.idx <- notblank.idx & xx[,jj]
#  proteins <- proteins[blank.idx,,drop=FALSE]
  
  return(list(proteins=proteins, notblank.idx=notblank.idx))
}

#######################################################################
#######################################################################
my.Psi = function(x, my.pi){
# calculates Psi
exp(log(1-my.pi)  + dnorm(x, 0, 1, log=T) - log(my.pi + (1 - my.pi) * pnorm(x, 0, 1) ))
}
# end my.Psi

my.Psi.dash = function(x, my.pi){
# calculates the derivative of Psi
-my.Psi(x, my.pi) * (x + my.Psi(x, my.pi))
}
# end my.Psi.dash

phi = function(x){dnorm(x)}

rnorm.trunc <- function (n, mu, sigma, lo=-Inf, hi=Inf){
# Calculates truncated noraml
  p.lo <- pnorm (lo, mu, sigma)
  p.hi <- pnorm (hi, mu, sigma)
  u <- runif (n, p.lo, p.hi)
  return (qnorm (u, mu, sigma))
}
# End rnorm.truncf


####################################
# dialog for DanteR
####################################
  # long.running=TRUE,
#eigen_norm.dialog = list(
#  title='EigenMS Normalization',
#  m.dataframeItem=NULL, label='Raw Data',
#    signal = c("default", DanteR:::get.dataset.factors, "treatment"),
#  treatment.choiceItem=NULL, label='Factors'
#)
#
eigen_norm.dialog = list(
  title='EigenMS Normalization',
  m.dataframeItem=NULL, label='Raw Data',
    signal = c("default", get.dataset.factors, "treatment"),
    signal = c("default", "get.dataset.row.metadata.fields", "protein_group"),        
  treatment.choiceItem=NULL, label='Factors',
  protein_group.choiceItem = NULL, label = "Field Containing Proteins"
)

eigen_norm <- function(m, treatment, protein_group=2){

    # First step
    # Second step
  rv <- eigen_norm1(m, treatment, protein_group)
  #stop("what")
  
  st <- paste("Found ", rv$h.c, " significant eigentrend", ifelse(rv$h.c==1, "", "s"), sep="")
  print(rv$h.c)
  print(rv$n.treatment)
  markup = list(title='Eigentrends', label=st,
#    choice.trueFalseItemItem=c("Yes","No"),
#      label = "Automatic Eigentrends",
    eigen_new_val.integerItem = c(value=rv$h.c, from=1, to=rv$n.treatment, by=1),
      label="Number of Eigentrends to Use",
    tooltip = "Use number of trends we identified or fewer to prevent overfitting")
  user_choice <- run.dialog(list, dlg.list=markup, envir=environment(), auto.assign=FALSE)$retval
  if(!is.null(user_choice)){
#    w <- gtkWindowNew("", show=F); w$setDecorated(FALSE); w$add(gtkLabelNew("  Calculating, please wait...  "))
#    w$setPosition(GtkWindowPosition["center"]); w$setModal(TRUE); w$showAll();
#    on.exit(w$destroy())
#
    #return(user_choice)
    #if(identical(user_choice$retval$choice, "No"))
    rv$h.c <- user_choice$eigen_new_val
    rv2 <- eigen_norm2(rv)
    return(rv2)
  }
}


# eigen_norm.dialog = list( title='EigenMS Normalization', m.dataframeItem='m',
#  label='Raw Data', signal = c("default", DanteR:::get.dataset.factors, "treatment"),
#  treatment.choiceItem=NULL, label='Factors')

#eigen_norm.dialog = list( title='EigenMS Normalization', m.dataframeItem='m',label='Raw Data',
#treatment.dataframeItem='treatment', label='Factors')

# dialog for interactive eigentrends
get_eigenpep = function (choice,eigen_new_val){
  if (choice=="Yes") {
		h.c = h.c
	} else {
    h.c = eigen_new_val
		# h.c = run.dialog(enter_ep)$retval
	}
  return(h.c)
}

# enter_ep = function(new_val){
#	h.c = new_val
#	return(h.c)
# }
# enter_ep.dialog=list(title='Modify Eigentrends', new_val.numericItem=NULL, label='Enter the number of Eigentrends:')

