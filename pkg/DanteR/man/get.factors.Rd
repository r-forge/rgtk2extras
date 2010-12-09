\name{get.factors}
\alias{get.factors}
\title{Get row metadata}
\usage{get.factors(dataset)}
\description{Get linked column metadata from a matrix or data frame}
\details{IF set to NULL, no handler is called.}
\arguments{
\item{dataset}{Matrix or data frame}
}
\note{
When called with an N-column dataset that has an attribute called 
  "Column_Metadata" set to c(table = table_name, key = key_name), where 
  table_name is the name of a column metadata table and key_name is the name 
  of the primary ID key, this function will return the N x M "ProtInfo" table, 
  where the first column is identical to the column names of the dataset, and 
  the other columns are the other column metadata fields.
}
