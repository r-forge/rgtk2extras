\name{get.ProtInfo}
\alias{get.ProtInfo}
\title{Get row metadata}
\usage{get.ProtInfo(dataset)}
\description{Get linked row metadata from a matrix or data frame}
\details{IF set to NULL, no handler is called.}
\arguments{
\item{dataset}{Matrix or data frame}
}
\note{
When called with an M-row dataset that has an attribute called "Row_Metadata" 
  set to c(table = table_name, key = key_name), where table_name is the name of 
  an existing row metadata table and key_name is the name of the primary ID key,
  this function will return the M x N "ProtInfo" table, where the first column
  is identical to the rownames of the dataset, and the other columns are the 
  other row metadata fields.
}
