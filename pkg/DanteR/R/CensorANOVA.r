IADJ_pvals = function(mm, treatment, protein_group=2) {
# IADJ_pvals = function(mm, treatment, plot_hist=FALSE, plot_volc=FALSE, p=0.05){
# computes adjusted pvalues for imputed data
# here we assume that p-value distribution for the Null peptides
# (non-differentially expressed) are uniformly distributed. So we will adjust all
# p-values to assure that the tail of the distribution is uniformly distributed.
# For most label-free data this will be a correct assumption. But if this
# assumption is not valid for a dataset this adjusted computation shold nto be used.
#
# Input:
#   input: An m x n (peptides x (n+2 samples)) matrix of expression data
#          where first column contains peptide identifier and second column
#          contains protein identifier
#   treatment: vector indicating the treatment group of each sample ie [1 1 1 1 2 2 2 2...]
#   plot_hist: T/F plot histogram of pvalues
#   plot_vol: T/F volcano plot of pvalues
#   p: level of significance for volcano plot
# 
# Output: list of:
#   protein_effect: matrix of effect sizes and p-values of differential 
#      expression for each protein. Null hypothesis:
#              no group difference/differential expression
#   peptide_effect: as above for all individual peptides.
#


  #  data<- input[,-(1:2)]
  #  rownames(data) <- input[,1]
  #  prot.info <- data.frame(Pep=input[,1], Prot=input[,2])
  #  treatment = as.factor(treatment)

  prot.info <- get.ProtInfo(mm)  # pepID, prID - not as in our text file data frame? here a mat
  row.key <- colnames(prot.info)[1]
  prot.info <- cbind(prot.info[,1], prot.info[,protein_group])
  colnames(prot.info) <- c(row.key, protein_group)
    
  formula.y_temp = as.formula(paste("y_temp ~", treatment))    
  formula.1pep = as.formula(paste("Y ~", treatment))
  contrasts.1pep = list(f.peptide="contr.sum")
  contrasts.Npep = list(.DanteR_block_name="contr.sum", f.peptide="contr.sum")  
  names(contrasts.Npep)[2] <- names(contrasts.Npep)[1] <- treatment
  formula.Npep = as.formula(paste("Y ~ .DanteR_block_name +", treatment))
 
  treatment.name <- treatment
  treatment <- get.factors(mm)[,treatment,drop=F]
  treatment.vector <- treatment[,1]
#  print(treatment.vector)

  # treatment factors
  n.treatment <- length(treatment)
  n.u.treatment <- length(unique(treatment))

  # Match to protein
  all.proteins <- unique(prot.info[,2])

  betahat_prot <- array(NA, c(length(all.proteins), n.u.treatment-1))
  betahat_pep <- NULL
  pval_prot_imp <- array(NA, c(length(all.proteins), 501))
  pval_pep_imp <- NULL
  
    # Pre-fill the return array with names
  names.df <- data.frame(Y = rnorm(length(treatment)), treatment)
  
  fit1 <- lm(formula.1pep, data=names.df)  
  nv <- DanteR:::names.vector(fit1, 1)  
  prot.eff <- array(NA, c(length(all.proteins), length(nv)))  
  rownames(prot.eff) <- all.proteins
  colnames(prot.eff) <- names(nv)
#  print(prot.eff)
  
  pep.eff <- array(NA, c(nrow(mm), length(nv)))
  rownames(pep.eff) <- rownames(mm)
  colnames(pep.eff) <- names(nv)    

  for (k in 1:length(all.proteins)) {
    prot <- all.proteins[k]
    pmid.matches <- prot.info[prot.info[,2]==prot,1]
    
    # idx.prot <- which(rownames(data) %in% pmid.matches)
    idx.prot <- which(rownames(mm) %in% pmid.matches)
    #    y_raw <- rbind(data[idx.prot,])
    y_raw <- mm[idx.prot,,drop=F]
    
    y <- as.vector(t(y_raw))

    #rownames(y_raw) <- rownames(mm)[idx.prot]
    n.peptide = nrow(y_raw)
    
    if (nrow(y_raw) == 0) next

    peptide <-rep(rep(1:n.peptide, each=n.treatment))
    #f.treatment <- factor(rep(treatment, n.peptide))
    f.peptide <- factor(peptide)

    # adjusted pvalues for protein level
    #stop("what")
     
    imp <- protlevel_pvals(y_raw, treatment.vector)
    pval_prot_imp[k,] <- imp$pval
    
  
     # browser()
    # calculate betahat for each peptide
    beta_pep <- array(NA, n.peptide)
    for (i in 1:n.peptide){
	    y_temp = as.numeric(y_raw[i,])
      my.df = data.frame(y_temp=y_temp, treatment)    
	    refit = lm(formula.y_temp, data = my.df)#, contrasts=contr.sum)
	    beta_pep[i] = coef(refit)[2]

	    nv <- DanteR:::names.vector(refit)
	    pep.eff[rownames(pep.eff)%in%rownames(y_raw)[i], names(nv)] <- nv
    }
 

    betahat_pep = c(betahat_pep, beta_pep)

    
    # calculate betahat for each protein
    if (n.peptide == 1){
        my.df = data.frame(Y=y, treatment.vector)    
        colnames(my.df)[2] <- treatment.name
        refit_prot <- lm(formula.1pep, data=my.df)#, contrasts=contrasts.1pep)
#       refit_prot <- lm(y ~f.treatment, contrasts=list(f.treatment="contr.sum"))
    } else {   
        my.df = data.frame(Y=y, .DanteR_block_name=f.peptide, treatment.vector)    
        colnames(my.df)[3] <- treatment.name                 
        refit_prot <- lm(formula.Npep, data=my.df)#, contrasts=contrasts.Npep)    
#       refit_prot <- lm(y ~f.peptide+f.treatment,contrasts = list(f.treatment="contr.sum", f.peptide="contr.sum"))
    }
    nv <- DanteR:::names.vector(refit_prot)
#    print(nv)
#    print(prot.eff)
    prot.eff[k,names(nv)] <- nv

    betahat_prot[k,] <- coef(refit)[2]

  } #end for each protein


  # adjusted pvalues for protein level
  #pval_prot_imp[is.na(pval_prot_imp)] <- 0
  pval_prot_imp[is.na(pval_prot_imp)] <- NA # set to 0  
  prot_calc <- select_pvals(pval_prot_imp)
  pval_prot <- array(prot_calc$select_pvals, c(length(all.proteins), 1))
  rownames(pval_prot) <- all.proteins

  # adjsuted pvalues fot peptide level
  pval_pep_imp <- array(NA, c(nrow(mm), 501))

  for (j in 1:nrow(mm)){
    temp <- array(mm[j,], c(1, length(mm[j,])))
    pepimp <- protlevel_pvals(temp, treatment.vector)
    pval_pep_imp[j,] <- pepimp$pvals
  }

  # I assume pval will be NA if we were unable to estimate the grp difference
  # ahouls not be an issue in imputed data
  
  pval_pep_imp[is.na(pval_pep_imp)] <- 0
  pep_calc <- select_pvals(pval_pep_imp)  
  pval_pep <- array(pep_calc$select_pvals, c(nrow(mm), 1))
  rownames(pval_pep) <- rownames(mm)

  ADJfactor <- as.matrix(pep_calc$ADJfactor)
  
  attr(pep.eff, "Row_Metadata") <- c(key = colnames(prot.info)[1], table = attr(mm, "Row_Metadata")[["table"]])   
  attr(prot.eff, "Row_Metadata") <- c(key = colnames(prot.info)[2], table = attr(mm, "Row_Metadata")[["table"]])  
  
  return(list(peptide_effects = pep.eff, 
              protein_effects = prot.eff,
              ADJfactor=ADJfactor))
#  return(list(pval_pep=pval_pep, pval_prot=pval_prot,
#              prot_betahat=betahat_prot, pep_betahat=betahat_pep,
#              ADJfactor=ADJfactor))
#
}


######################################################################
# helper functions below
######################################################################
protlevel_pvals = function(pr, treatment){
# function [pvals, betahat, prnames] = urpvals_imp(dname, grps, nheadercols, nheaderrows)
# compute p-values for imputed proteins using adjusted (fudged) likelihood ratio statistics. 
# 
# Ours is model-based imputation as described in: "A Statistical Framework for Protein
# Quantification in Botton-Up MS-Based Proteomcs", Bioinformatics 2009.
# 
# INPUT: pr - one protein with all peptides passed in. npep x nsamp (?)
#        grps  - vector of group assignments for each sample: [1 1 1 1 1 2 2 2 2 2 3 3 3 3 3]
# 
# OUTPUT: pvals - vector of adjusted p-values (501 values) for thsi protein. From here
#         the values will need to be selected to fit the null distribution.
#

  ugrps = unique(treatment)
  ng = length(ugrps)

  fudgeFs = seq(1, 10, length=501) #this makes 500 factors, no other significance in 0.018
  lf = length(fudgeFs)
  
  nsamps = ncol(pr)
  npep = nrow(pr)
 
  allY <- c(as.matrix(t(pr)))

  if (npep >=2){
   	grps2 <- factor(rep(treatment, npep))
	  peps <-factor(rep(rep(1:npep, each=nsamps)))
    X2 <- model.matrix(~peps, contrasts = list(peps="contr.sum") )
	  X  <- model.matrix(~grps2 + peps, contrasts = list(grps2="contr.sum", peps="contr.sum") )
	  fit_null <- lm(allY~ X2 -1)
    fit_unrestricted <- lm(allY~ X -1)
    results <- anova(fit_null, fit_unrestricted)
    ff <- results[5][2,]
 } else{
	  grps2 <- factor(rep(treatment, npep))
    X <- model.matrix(~grps2, contrasts=list(grps2="contr.sum"))
	  X2 <- array(1, c(nrow(X),1))
	  fit_null <- lm(allY~ X2 -1)
    fit_unrestricted <- lm(allY~ X -1)
    results <- anova(fit_null, fit_unrestricted)
    ff <- results[5][2,]
  }

  DFtr = ncol(X)- ncol(X2)  # tr - tretment group
  DFres =  nrow(X2) - ncol(X)  # sum(ngrps(varPreserve)) + 1
  

  pvals <- array(NA, lf)
  # adjust the test statistic by [1, 1.18, ... 10] factors
  for (j in 1:lf){
    pvals[j] = 1-pf(ff/fudgeFs[j],DFtr,DFres)
  }
  rv = list(pvals=pvals)
  return(rv)
}


#########################################################
select_pvals = function(pvals_impute){
# choose adjust pvalues using regression
# input:
#    pvals_impute- an m x length(fudge factor) matrix of adjusted pvalues
#
# output:
#    select_pval- column of pvalues corresponding to the smallest fudge 
#                 factor that produced a positive slope
#    k - fudge factor position
#

  fudgeFs = seq(1, 10, by=0.018) #this makes 500 factors, no other significance in 0.018
  intervals = array(NA, c(501, 6))
 
 N_FUDGE = 501
  for (k in 1:N_FUDGE){ #% 501 fudge factors for likelihood test statistic
  	intervals[k,1] = sum(pvals_impute[,k] >= 0.7 & pvals_impute[,k] <0.75) #[.7 .75]
  	intervals[k,2] = sum(pvals_impute[,k] >= 0.75 & pvals_impute[,k] <0.8) #[.75 .8]
  	intervals[k,3] = sum(pvals_impute[,k] >= 0.8 & pvals_impute[,k] <0.85) #[.8 .85]
  	intervals[k,4] = sum(pvals_impute[,k] >= 0.85 & pvals_impute[,k] <0.9) #[.85 .9]
  	intervals[k,5] = sum(pvals_impute[,k] >= 0.9 & pvals_impute[,k] <0.95) #[.9 .95]
  	intervals[k,6] = sum(pvals_impute[,k] >= 0.95 & pvals_impute[,k] <=1) #[.95 1]
   

#count_pvals <<- (pvals_impute[,k] >= 0.7 & pvals_impute[,k] <0.75)

#   print("k: ")
#   print(k)
#   print("intervals:")
#   print(intervals[1:6,])
   #Sys.sleep(4)
   
   
  	# compute regression line between [1 2 3 4 5 6...] & counts
  	# select the min fudge factor for which slope is positive
  	counts = factor(c(1:6))
  	X = model.matrix(~counts, contrasts=list(counts="contr.sum"))
    
    y <- as.vector(t(intervals[k,]))
	  
  	betas = drop(solve(t(X) %*% X) %*% t(X) %*% y)
 
	  effects = X%*%betas
  	m = (effects[1] - effects[6]) / (-5)

  	if (m > 0){
    		select_pvals = pvals_impute[,k]
    		select_fudgeF = fudgeFs[k]
    		# select_fundgeF = k
    		break
  	}

  	# if we do not get a pos slope in 501 iteration, then use the lagest
    # adjustment factor we got
 		if (m <= 0){
 		  select_pvals = pvals_impute[,k]
    	select_fudgeF = fudgeFs[k]
  	}
  }
  
  return(list(select_pvals=select_pvals, ADJfactor=select_fudgeF))
}




##########################################################################
# dialog for DanteR
##########################################################################
IADJ_pvals.dialog = list(title='Compute p-values adjusted for imputation',
  mm.dataframeItem=NULL, label='Data matrix',
    signal = list("default", "get.dataset.factors", "treatment", user.data=list(include.primary=FALSE)),
    signal = list("default", "get.dataset.row.metadata.fields", "protein_group", user.data=list(include.primary=FALSE)),    
  treatment.choiceItem=NULL, label='Factors',
  protein_group.choiceItem = NULL, label = "Field Containing Proteins"
)

  ##signal = c("default", "toggle.sensitive", "plot_hist"),
#  plot_hist.trueFalseItem = TRUE, label='Plot p-value histogram',
#
#  # signal = c("default", "toggle.sensitive", "plot_volc"),
#  plot_volc.trueFalseItem = TRUE, label='Plot volcano Plots',
#
#  signal = c("default", "toggle.sensitive", "p"),
#  p.numericItem=0.05, label='Significance level in volcano plots'
# )
  
                                                                        
CensorANOVA.dialog <- list(
 title = "Model Based Impute/Filter/ANOVA",
 label = "Filtering, imputation and ANOVA probability calculation with rescaling",
  m.dataframeItem=NULL, label='Data matrix',
    signal = c("default", "get.dataset.factors", "treatment"),
    signal = c("default", "get.dataset.row.metadata.fields", "protein_group"),    
  treatment.choiceItem=NULL, label='Factors',
  protein_group.choiceItem = NULL, label = "Field Containing Proteins",
  BREAK=TRUE,
  compute_pi.trueFalseItem=FALSE, label='Manually Estimate PI',
   tooltip = "If this is checked, the user can set the estimated random missingness probability",
  signal = c("default", "toggle.sensitive", "my.pi"),
    my.pi.numericItem="0.05", label='PI', indent=10,
  estimatePiOnly.trueFalseItem = FALSE, label = "Estimate Pi Only", tooltip = "Users should check this value before proceeding with filtering and imputation",
    signal = c("default", function(item, userChoice) {
      userChoice$setSensitive(TRUE)
      if(get.value(item)) userChoice$setSensitive(FALSE)
    }, "userChoice"),
  userChoice.radiobuttonItem = c("Filter Only", value="Filter, Impute, ANOVA"), label ="Choose Task", tooltip = "Choose to do filtering, then imputation and adjust p-values", indent=10
)

CensorANOVA <- function(m, treatment, protein_group, compute_pi=TRUE, 
  my.pi = 0.05, topNPep=1E6,  estimatePiOnly = FALSE,
  userChoice = "Filter, Impute, Report P-values"){
  
  if (userChoice == "Filter Only"){
    doFilter=TRUE; doImpute=FALSE; doAdjustPvals=FALSE;
  } else {
    doFilter=TRUE; doImpute=TRUE; doAdjustPvals=TRUE;  
  }

  protein_effects <- peptide_effects <- imputed <- filtered <- NULL
  output_values <- array(NA, c(3,1))
  colnames(output_values) <- deparse(substitute(m))
  rownames(output_values) <- c("Pi_from_filter", "Pi_from_impute", "P_Adjfactor")

  if(doFilter){
    rv_filter <- MBfilter(m, treatment, protein_group=protein_group, 
      my.pi=my.pi, compute_pi=compute_pi, topNPep=topNPep, compute_pi_only=estimatePiOnly)
    m <- filtered <- rv_filter$y_filtered
    output_values[1,1] <- rv_filter$pi[1,1]
  }
  if(estimatePiOnly){
    return(list(pi=rv_filter$pi))
  }

  if(doImpute){
    rv_impute <- MBimpute(m, treatment, protein_group=protein_group, my.pi=my.pi, compute_pi=compute_pi)
    m <- imputed <- rv_impute$y_imputed
    output_values[2,1] <- rv_impute$pi[1,1]
  }

  if(doAdjustPvals){
    rv_adjusted_pvals <- IADJ_pvals(m, treatment, protein_group=protein_group)
    peptide_effects <- rv_adjusted_pvals$peptide_effects
    protein_effects <- rv_adjusted_pvals$protein_effects    
    output_values[3,1] <- rv_adjusted_pvals$ADJfactor[1,1]    
  }

  rv <- list(
    filtered = filtered, 
    imputed = imputed, 
    peptide_effects = peptide_effects,
    protein_effects = protein_effects,
    output_values = output_values
  )
  
  return(rv)
}

