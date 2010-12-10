\name{ANOVA}
\alias{ANOVA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data set ANOVA
}
\description{
Perform ANOVA on peptide crosstab data, possibly using additional row-wise factors.
}
\usage{
ANOVA(dataset, split.by.row.metadata = FALSE, split.row.metadata.field = NULL, column.metadata.fields = NULL, do.interactions = FALSE, block.order.function.name = "median", nrow.block.min = 1, nrow.block.max = 5, useglm = "lm", use.weight = FALSE, weight.function = "NULL", weight.par = 0, formula.string = ".", do.residuals = FALSE, first.level = character(0), first.level.contrasts = character(0), progressbar = NULL, progresslabel = NULL, return.all.fits = FALSE, return.residuals = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{
  A data frame or array containing numerical data.
}
  \item{split.by.row.metadata}{
  Do ANOVA row by row, or 2-factor ANOVA on entire blocks specified by row metadata?
}
  \item{split.row.metadata.field}{
  Field corresponding to proteins
}
  \item{column.metadata.fields}{
  Factors to use in ANOVA
}
  \item{do.interactions}{
  Use interactions between factors
}
  \item{block.order.function.name}{
  How to order blocks of peptides within a protein
}
  \item{nrow.block.min}{
  Min number of rows per block (peptides per protein). Turn this to 2 to exclude
  one hit wonders.
}
  \item{nrow.block.max}{
  Max number of rows per block (peptides per protein)
}
  \item{useglm}{
  Type of model to fit the data to
}
  \item{use.weight}{
  Weight the model or not?
}
  \item{weight.function}{
  String containing the weighting function  
}
  \item{weight.par}{
  String containing the weighting parameter
}
  \item{formula.string}{
  String containing the formula
}
  \item{do.residuals}{
  Return a list of all residuals for the fit
}
  \item{first.level}{
  Specify first level of factor - for one factor only
}
  \item{first.level.contrasts}{
  Contrast scheme for first factor
}
  \item{progressbar}{
  Internal argument for DanteR
}
  \item{progresslabel}{
  Internal argument for DanteR
}
  \item{return.all.fits}{
   Return a list of all fitted models
}
  \item{return.residuals}{
   Return an array of residuals for the fit
}
  \item{\dots}{
     Additional arguments
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
There is a comprehensive ANOVA scheme included in DAnTE. This model can account
for unbalanced data using marginal sums of squares in the case of 2-way ANOVA 
(or n-Way) and can account for random effects such as LC column effects etc 
through a REML based multi level model. User can also check interactions in a 
higher order (n-Way) ANOVA. For N peptides or proteins, depending on whether
protein-level ANOVA is selected or not, the output will be a Nx2K column 
array containing estimate sizes and p-values for each factor comparison.

The ANOVA function is performed according to Oberg et al. 

For non protein level ANOVA and one factor the result is identical to a 2-sided
t-test performed on each row.

For protein level ANOVA and one factor, the following steps are 
  performed for each protein:

1. Take the N most abundant peptides by their median or mean abundance 
  (N is defaulted to 5)
2. Fit the following linear 2-factor ANOVA model to the data:

y_ij = alpha_i + beta_(Pr[i], Tr[j]) + delta_ij

Associated with each peptide intensity is a protein identity Pr, so the 
i'th peptide belongs to protein Pr[i]. The recorded log-intensity of the 
i'th peptide belonging to the Pr[i]'th protein in the j'th experiment is y_ij. 
This is expected to depend on  a peptide-dependent ionization efficiency  i; 
a treatment effect  beta_(Pr[i], Tr[j]) depending on the peptide's originating 
protein Pr[i] and the experimental treatment group Tr[j]; i.i.d. random noise 
delta_ij. Thus all data is fit to the linear model. 

The weight function optionally weights the noise delta_ij. 
For LTQ LC-MS data a moderate exponential weighting y_ij ~ exp(-0.25y) fits 
instrument noise well.

}
\references{
Oberg, A. L.; Mahoney, D. W.; Eckel-Passow, J. E.; Malone, C. J.; 
Wolfinger, R. D.; Hill, E. G.; Cooper, L. T.; Onuma, O. K.; Spiro, C.; 
Therneau, T. M.; Bergen, H. R., 3rd J Proteome Res 2008, 7, 225.

}