% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMotifFromMEME.R
\name{getMotifFromMEME}
\alias{getMotifFromMEME}
\title{Extract and Visualize Motif Information from MEME Software}
\arguments{
\item{data}{A txt file from MEME software.}

\item{format}{The result format from MEME, txt or xml.}
}
\value{
Return a datafram
}
\description{
\code{getMotifFromMEME} Extract motif information from the MEME software results.
}
\examples{
filepath <- system.file("examples", "meme.txt", package = "ggmotif")
motif.info <- getMotifFromMEME(data = filepath, format = "txt")

filepath <- system.file("examples", "meme.xml", package = "ggmotif")
motif.info <- getMotifFromMEME(data = filepath, format = "xml")
}
\author{
Xiang LI <lixiang117423@foxmail.com>
}
