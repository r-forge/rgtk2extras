                                                                                   
VennPlot.dialog <- list(
  label = "Venn Diagram Using Table Row Names",
  keep.open = TRUE,
  dummy.listItem = substitute(get_all_tables()), suppress=T, show.arrows = FALSE, label = "All Tables",
  table_list.listItem = character(0), label = "Choose More Than One Table",
    signal = c("add", "push.selection", "dummy"),
    signal = c("subtract", "pop.selection", "dummy"),
  type.radiobuttonItem = c(value="vennPlot", "olBarPlot"), item.labels = c("Venn Diagram", "Venn Bar Plot"), label = "Venn diagram type", tooltip = "Venn Diagrams work with up to 5 data sets",
  textSize.rangeItem = c(value=0.4, from=0, to=2, by=0.1), label = "Text Size"
)

VennPlot <- function(table_list, type = "vennPlot", textSize=0.6) {
  setlist <- sapply(table_list, function(x) rownames(safe.eval(x)))
#  print(setlist)
     
  if (length(setlist) > 5 && type == "vennPlot")
    stop("Can't plot Venn diagram for more than 5 data sets")
  OLlist <- overLapper(setlist=setlist, sep="", type="vennsets")
  OLlist$Venn_List <- sapply(OLlist$Venn_List, function(x) setdiff(x, "")) # remove ""
  if (type == "vennPlot") {
    counts <- sapply(OLlist$Venn_List, length);
    vennPlot(counts=counts) #
  } else {
    olBarplot(OLlist=OLlist, horiz=T, las=1, cex.names=textSize, main="Venn Bar Plot")
  }
  return(list(plot=recordPlot(), set_list = ragged.cbind(OLlist$Venn_List)))
}
