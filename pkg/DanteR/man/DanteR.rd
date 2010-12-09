\name{dante}
\Rdversion{1.1}
\alias{dante}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
DanteR
}
\description{
DAnTE is a front-end tool for downstream proteomic data analysis. 
}
\usage{
dante()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 No arguments
}
\details{

dante extends the capabilities of the data pipeline by allowing the user to 
apply many data analysis algorithms.

}
\value{
NULL
}
\author{
Tom Taverner <Thomas.Taverner@pnl.gov>
}
\note{

Functions Within DanteR

\strong{File Menu}

DAnTE has an advanced data loading mechanism. It can load either abundance data 
(or expression ratios) or Sequest output files to process Peptide count data.

Data tables are treated as crosstabs where columns generally correspond to data
sets and rows correspond to an observation class - for example, peptide 
intensities, or gene observation.

Data tables may be linked to tables containing metadata (data about data) for
their row and column names.

\strong{Abundance Data}

DAnTE can preserve the peptide-protein mapping information if the user decides 
to load protein information. The data file can be in the following formats: Excel,
Excel 2007, Access, CSV, TSV and SQLite DB.

The data file must have

* A header row
* A unique row ID column (ex: Mass Tag or Probeset ID)
* Abundance values for each row in the next columns with each column 
corresponding to a dataset
* Optional Columns containing row metadata IDs to create a separate row metadata 
table.

\strong{Peptide Count Data}
File -> Open -> Spectral Count Data will bring an interface to load sequest 
output files. There are three
methods to load the SEQUEST output files:
1. Concatenated *.out files (*_out.txt) or Synopsis files (*_syn.txt) from a 
local folder. These files are generated using Peptide File Extractor 
(available at http://omics.pnl.gov/software/.

2. SEQUEST *.out files residing in a local folder. Dataset names are obtained 
using the file names (dataset names are extracted from the file names truncated 
at the first "." Ex: File name: dataset1_0305-07.5.5.1.out -> Dataset name:
dataset1_0305-07).

3. Directly from the PNNL DMS archive, either *_out.txt or *_syn.txt files 
(on PNNL computers only). You need to have at least two unique SEQUEST results 
sets to build a table of spectral counts. Loading from *_syn.txt files
will be the fastest.


\strong{Metadata}
Metadata, or data about data, is important for keeping track of data 
organization and analysis. Much like Access, DanteR defines links between the 
crosstab of interest and tables containing row metadata and column metadata. 

\strong{Column Metadata (Factors)}
Factors capture the underlying grouping structure in the data. For example, 
gender can be treated as a factor with two enumeration levels Male/Female. 
Treatment can be treated as a factor with as many enumeration levels (Burn/Sham).
Factors can either be loaded as a csv file or defined in the 'Metadata' menu. 

\strong{Pre-Process Menu}

\strong{Log Transform}
User can log transform the data (either base 2 or 10) with options of having a 
additive or multiplicative bias.

\strong{Linear Regression Based Normalization}

Linear regression method tries to fit a regression line for each dataset within 
a selected factor (ex. Replicate) against a reference (see 'Define Factors' 
for more information on factors).

\strong{Loess Normalization}
Loess normalization (local regression estimates) tries to fit simple models to 
localized subsets of the data. Everything else follows as in the linear 
regression method. The size of the local subset is controlled by the span value 
(Window width) and Callister et.al.(2006) report that a value of 0.4 works best 
for LC-MS proteomics data.

\strong{Quantile Normalization}
This method implements the Quantile Normalization method proposed by Bolstad BM, 
Irizarry RA, Astrand M, Speed TP. (See Bioinformatics. 2003 Jan 22;19(2):185-93)

\strong{Median Absolute Deviation (MAD) Adjustment}
This method tries to adjust the MAD of each dataset so that all the data sets 
will have the same spread of abundance intensity (i.e. comparable box plots). 
This is achieved by choosing a suitable scaling factor for each dataset to 
adjust its abundance values. See Yang, Y. H., S. Dudoit, et al. (2002). Nucleic 
Acids Res 30(4): e15.

\strong{Central Tendancy Adjustment}
Mean/Median of the dataset (in a column) is subtracted from (or divided by) each,
resulting datasets which will have mean/median centered at zero. User can choose 
to subtract/divide either mean or the median and also to set the new mean/median 
at zero. If the checkbox 'New Center at Zero' is unchecked, all means/medians are 
set to the maximum in all the datasets.

\strong{Imputing Missing Values}
There are some simple ways of imputing missing values and also some advanced
methods. The screenshot above shows some of the simplest ways one can impute 
missing data.

\strong{Mean/Median}
Mean or the median of the entire dataset is used to replace all missing values 
in the dataset.

\strong{Substitue a Constant}
Fill all the missing data with the same constant value such as half of the 
maximum detected value.

\strong{Row Mean within a Factor}
This method fills the missing values within a row with the mean value of row 
observations by the selected factor. This approach is recommended when 
significant difference are expected among factor levels.

\strong{k-Nearest Neighbor (kNN) Method}
Replaces missing data with the mean of the k-nearest neighbor peptides
(See: Troyanskaya O. and Cantor M. and Sherlock G. and Brown P. and Hastie T. 
and Tibshirani R. and Botstein D. and Altman RB. - Missing value estimation 
methods for DNA microarrays. Bioinformatics. 2001 Jun;17(6):520-5.)

\strong{Weighted kNN Method}
Replaces missing data with a weighted mean of the k-nearest neighbor peptides. 
The weights are inversely proportional to the distances from the neighboring 
peptides.

\strong{SVDimpute method}
SVDimpute algorithm works iteratively until the change in the estimated solution 
falls below a certain threshold. Each step the eigenpeptides of the current 
estimate are calculated and used to determine a new estimate. An optimal linear
combination is found by regressing the incomplete variable against the k most 
significant eigenpeptides. See: Troyanskaya O. and Cantor M. and Sherlock G. 
and Brown P. and Hastie T. and Tibshirani R. and Botstein D. and Altman RB. - 
Missing value estimation methods for DNA microarrays. Bioinformatics. 2001 
Jun;17(6):520-5.

\strong{Rollup Menu}

\strong{RRollup - Reference Peptide Based Scaling}
Note that this method correctly works only with log transformed data.

A reference peptide which has the most presence across all the datasets, is 
chosen from the group of peptides that belong to a protein. If there are 
multiple candidates, the most abundant one is chosen. Then the ratios of peptide 
abundances with respect to the reference are computed (since the data is assumed 
to be in log scale, the differences are used) and their median is used as a 
scaling factor. Protein abundance is obtained as the median of the resulting 
peptide abundances.

Minimum Presence of at least one Peptide for a Protein: Peptides that have too 
many missing values below this percentage are dropped.

Exclude peptides from scaling if they are at least not present in this many 
datasets: Within a group of peptides for a specific protein, the ones that do 
not overlap well (controlled by this value) are not scaled but they are kept to 
calculate the final protein abundance.

Include Single peptide/protein matches (i.e. 'One-Hit-Wonders'): Protein with 
only one observed peptide will be included in the final list of proteins. The 
rational behind this is that if a particular protein may have only one peptide 
but it may be quite abundant and present throughout giving some strong 
confidence on the presence of the protein.

\strong{Plot Menu}
\strong{Histograms}
Blue curve shows the density distribution if shifted to mean zero and the red 
curve shows the density distribution in the data.

\strong{QQ Plot}
A normal Qquantile-Quantile Plot is a graphical method for diagnosing how well 
the data fits to a comparison distribution (a normal distribution is used here). 
In the case of substantial deviations from linearity, one can assume that the 
normality assumption is violated.

\strong{Correlation Plots}
Correlations between the column data can be plotted in many different ways. 
For heatmap style and 2D box style, the correlation range can be adjusted for 
display purposes.

\strong{Correlation Plots}

\strong{Box Plots}

\strong{Principal Component Analysis (PCA) and Partial Least Squares Analysis 
(PLS)}

You can select a factor for the PCA/PLS plot to be colored accordingly. There 
are options to plot either scores plot or loadings plot (see biplot option). 
User can also choose to plot the screeplot.

The resulting weights of the PCA and PLS analysis on each of the orthogonal 
component are also reported in a Data Table. The weights reported correspond to 
a p-value obtained by fitting an empirical distribution function to the original 
weights. A low p-value denotes a significant feature.

\strong{MA Plot}
MA plot is a scatterplot with transformed axes. The X-axis represents the 
average log intensity from 2 datasets while Y-axis represents the log-ratios. 
MA plot is especially useful for the detection of the intensity-dependent 
effects in datasets.

\strong{Protein Rollup Plots}
 These are similar to the ones shown in 'Peptides and Protein Rollup Methods'
 
\strong{Cluster Heatmaps} 
A simple heatmap display control is added with hierachical and k-means 
clustering options. The data rows to be used for the plot can be either 
specified in the dialog box below or can be selected in the datatable. If a 
factor is selected, a colorbar will be added on top of the heatmap denoting the 
grouping information of datasets. Cluster ordering for Hierarchical clusters and
the cluster assignments for k-means clustering will be displayed in a separate 
data table "Clusters".

\strong{Statistics Menu}
\strong{Define Factors}
Factors will identify the groups within the datasets. For an example, the 
experiment may have Male and Female individuals, or the samples are from 
healthy individuals and cancerous patients, or the order they were run in 
batches (blocking factors) can be used as factors. See File Menu for loading 
factors from a file.

\strong{Shapiro-Wilks Test for Normality}
Shapiro-Wilks Test is used to check the normality of the data. This is supposed 
to be better than the Kolmogorov-Smirnov test and other similar goodness of fit 
tests. Royston (1995) reports that the null hypothesis (i.e. data is normally 
distributed) can be rejected for p-value < 0.1 (see: Patrick Royston (1995) 
Remark AS R94: A remark on Algorithm AS 181: The W test for normality. Applied 
Statistics, 44, 547--551).

\strong{ANOVA}
There is a comprehensive ANOVA scheme included in DAnTE. This model can account
for unbalanced data using marginal sums of squares in the case of 2-way ANOVA 
(or n-Way) and can account for random effects such as LC column effects etc 
through a REML based multi level model. User can also check interactions in a 
higher order (n-Way) ANOVA. The output will be the p-values and the corresponding 
q-values that denote the FDR thresholds (see: Storey JD. (2003) The positive
false discovery rate: A Bayesian interpretation and the q-value. Annals of 
Statistics, 31: 2013-2035).

\strong{Non-parametric Tests}
\strong{Wilcoxon Rank Sum Test}
This test is equivalent to the Mann-Whitney test and can be applied in place of 
the t-test when the normality assumption does not hold. This test can be applied 
only for a factor with two levels. For factors with more than two levels the 
Kruskal-Walis test (see below) should be used instead.

\strong{Kruskal-Walis Test}
This test can be applied in place of the One-way ANOVA when the normality 
assumption does not hold.

\strong{Fold Changes}

Fold changes can be calculated among two levels of a Factor. Data is assumed to 
be in log scale and the fold change is calculated by first avaraging the values 
in datasets for each factor level and subtracting one from the other noting that 
log(x/y) = log(x) - log(y).

If you uncheck the check box to denote that the data is in normal scale the 
usual ratios are obtained by division. For ratios smaller than 1, -1/ratio is 
reported as a negative fold change. Absolute fold change value is also reported.
Menu location: Statistics -> Calculate Fold Changes.

\strong{p/q-value Filters}
Based on the resulting p-values or q-values the data can be filtered. The dialog 
box is shown below.

\strong{Miscellaneous Features}
\strong{Merging columns of a data table}
Columns can be merged based on a factor. Merging can be done as a sum, median, 
or as mean. If this table to be used for subsequent analysis, it has to be 
save and loaded to a new analysis session since the old factor definitions 
would no longer be valid. However the statistical plots can be done on this table.

\strong{Plot rows of a data table}
Select Rollup->Plot Rows and select rows or row metadata to plot.

\strong{Save data table with protein information}
If the data table needs to be saved with protein ID columns appended, right 
click the table from the tree view on the left to bring up the context menu.

\strong{Save/Open Session}
Choose File -> Save Session to save the entire analysis session so that it can 
be later retrieved by File -> Open Session.

\strong{Step-by-step Analysis Summary}
Abundance Data
1. First step would be to log transform the data.
2. You can investigate the various plots available in DAnTE to study the 
variation and to decide on the type of normalization method to apply.
3. Define factors so that the normalization methods can be applied to groups of 
datasets categorized by factors.
4. Apply normalization. There are few major methods available: Linear 
Regression, Loess and Quantile. These can be thought of as applying locally 
within a selected factor. In addition a global methods can also be applied. 
These are the Median Absolute Deviation (MAD) correction and the mean/median 
centering. Mean centering is recommended as the final step after MAD adjustment.
5. If the protein information is loaded, data can be rolled up to protein 
abundances using one of the three methods available. Note that the RRollup 
method assumes that the data is in log scale.

Peptide Count Data
1. Load the data using the wizard.
2. Investigate using the scatter plots available under the correlation plots to 
see for any systematic shift in the data and correct if any, using the linear 
regression method.
3. Log transform the data using an appropriate bias.
4. Use a non-parametric test (ex: Kruskal-Walis test) to select the significant 
features.

}

\examples{
  dante()
}
