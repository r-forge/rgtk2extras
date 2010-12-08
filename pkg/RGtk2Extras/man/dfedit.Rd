\name{dfedit}
\alias{dfedit}
\title{Convenience function for editing a data frame in its own window}
\usage{dfedit(items, dataset.name = deparse(substitute(items)), 
size=c(500, 300), col.width=64, pretty_print=FALSE, sprintf_format = "\%.6G")}
\description{Convenience function for editing a data frame in a window.}
\details{IF set to NULL, is the name of the dataset.}
\value{Returns the edited data frame after running modally}
\arguments{\item{items}{The data frame to edit}
\item{dataset.name}{Optional name to assign the dataset when updating}
\item{size}{Optional window size request}
\item{col.width}{Optional column size}
\item{pretty_print}{Optional pretty_print}
\item{sprintf_format}{Pretty print formatting for numeric data}}        

