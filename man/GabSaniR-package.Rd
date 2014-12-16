\name{GabSaniR-package}
\alias{GabSaniR-package}
\alias{GabSaniR}
\docType{package}
\title{Extraction, checking and dynamic report based on InfoSani data in region of Gabon}
\description{
This package is a tool that contains three main functions (InfoSaniR_extract, InfoSaniR_check, InfoSaniR_report) that facilitates faster extraction, checking the InfoSani data for each year, next generates automatically a statistical annual report of health for region of Gabon 
 }
\details{
\tabular{ll}{
Package: \tab GabSaniR\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-12-02\cr
License: \tab GPL\cr
}
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~
}
\author{Regis Obiang <regis.obiang@lambarene.org>, Bertrand Lell 
        <bertrand.lell@gmail.com>
Maintainer: Regis Obiang <regis.obiang@lambarene.org>

}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the ~~
~~ R documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{#Chargement des donnees de pathologie dans la session R
data("Data_patho_2012")
#Production du rapport annuel les pathologies 
InfoSaniR_report(data='patho', Year=2012, file=FALSE)}
