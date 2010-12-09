# VennDiagrams.R

VennDiagrams2.dialog <- list(
  n.components.radiobuttonItem = 2:5, label = "Number of Components",
  c1.dataframeItem = "", label = "Component 1",
  c2.dataframeItem = "", label = "Component 2",
  c3.dataframeItem = "", label = "Component 3",
  c4.dataframeItem = "", label = "Component 4",
  c5.dataframeItem = "", label = "Component 5"
)

VennDiagrams2 <- function(n.components, c1=NULL, c2=NULL, c3=NULL, c4=NULL, c5=NULL)
{
  setList = list()
  for(i in 1:n.components) {
    cx <- paste("c", i, sep="")
    names.cx <- deparse(substitute(get(cx)))
    stopifnot(!is.null(rownames(get(cx))))
    setList[[names.cx]] <- rownames(get(cx))
  }

  setlist <- list(A=sample(letters, 18), B=sample(letters, 16), C=sample(letters, 20), D=sample(letters, 22), E=sample(letters, 18), F=sample(letters, 22, replace=T))

  source("http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/overLapper.R")
  setlist <- setlist[1:n.components]
  OLlist <- overLapper(setlist=setlist, sep="_", type="vennsets");
  counts <- sapply(OLlist$Venn_List, length);
  vennPlot(counts=counts, ccol=c(rep(1,30),2), lcex=1.5, ccex=c(rep(1.5,5), rep(0.6,25),1.5))
}
