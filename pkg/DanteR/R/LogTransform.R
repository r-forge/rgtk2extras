#LogTransform <- function(Data, log.type=1){
#  stopifnot(log.type%in%(1:3))
#  log.base <- c(10, 2, exp(1))[log.type]
#  return(log(Data, log.base))
#}


LogT.dialog = list(
  title = "Scaling",
  label="Data Transformation: x <- log(Ax + B)",
  df.dataframeItem = "T_Data", label="Dataset",
  doLog.trueFalseItem = TRUE, label = "Do Log",
    signal = c("default", "toggle.sensitive", "logBase", "designateBase", "designateBase", "logScale"),
    logBase.radiobuttonItem = c(value=2, exp(1), 10), item.labels = c("2", "Natural Log", "10"), label = "Log Base", indent=10,
    designateBase.trueFalseItem = FALSE, label = "Custom Log Base", indent=10,
      signal = c("default", "toggle.sensitive", "logScale"),
      logScale.numericItem = 10, label = "Log Scale", indent=20, BREAK = TRUE,
  mul.numericItem = 1, label="Multiplier Bias A", tooltip="Multiplier will be performed before log transformation",
  plus.numericItem = 0, label="Addition Bias B",  tooltip="Addition will be performed before log transformation"
)

## Joe's new log transform function
LogT <- function(df, theColumns=NULL, mul=1, plus=0, doLog = FALSE, logScale = 10, designateBase=FALSE, logBase=2){
  old_attr <- attributes(df)    

  df <- data.matrix(df)
  if(!is.null(theColumns)) df<- df[,theColumns]
  df<- df*mul + plus
  if(doLog) {
    if(designateBase) {
       df <- log(df, logScale)
    } else {
      df <- log(df, logBase)
    }
  }
  df[!is.finite(df)] <- NA
  df <- copy.metadata(df, old_attr)  
  return(df)
}