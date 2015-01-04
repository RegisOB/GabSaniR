\name{InfoSaniR_extract}
\alias{InfoSaniR_extract}
\title{Extraction de tous les fichiers InfoSani annuel}
\description{Extrait automatiquement toutes les donnees brutes du fichier InfoSani dans toutes les structures sanitaires d'une province pour une annnee. Cette fonction neccessite tous les fichiers xlsx InfoSani annuel.}
\usage{
InfoSaniR_extract(health = 'patho', Year=2000)
}

\arguments{
\item{health}{Donnee InfoSani brute. Soit pathologie (patho), sante maternite et infantile (accouch1, accouch2 ou malnu), vaccination (vac) ou toutes les donnees (all).}
  \item{Year}{ Annee de recueil des donnees InfoSani. Annee doit etre numerique et compris entre 2000 et 2099.}
}
\details{Vous devez specifier correctement le dossier contenant des fichiers xlsx et le dossier ou les donnees extraites seront exportees.}
\value{
La fonction retourne automatiquement des donnees extraites dans le dossier specifie pour exportation sur les pathologies, sante maternite infantile et vaccination:
\item{data_health_year.xlsx }{Donnees extraites sous format excel (xlsx)}
\item{data_health_year.RData }{Donnees extraites sous format image (RData) lisible par R}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Regis Obiang <regis.obiang@lambarene.org>, 
Bertrand Lell <bertrand.lell@gmail.com>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##Convertir prealablement les extensions xls en xlsx 
##les fichiers InfoSani excel

##Extraire automatiquement les donnees de pathologies en 2012
##de toutes les structures de sante
InfoSaniR_extract(health = 'patho', Year=2012)

##Extraire automatiquement les donnees sante de maternite infantile en 2012
##de toutes les structures de sante
InfoSaniR_extract(health = 'accouch1', Year=2012)
InfoSaniR_extract(health = 'accouch2', Year=2012)
InfoSaniR_extract(health = 'malnu', Year=2012)

##Extraire automatiquement les donnees sante de vaccination en 2012
##de toutes les structures de sante
InfoSaniR_extract(health = 'vac', Year=2012)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
