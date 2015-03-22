\name{InfoSaniR_check}
\alias{InfoSaniR_check}

\title{
Verification des erreurs de donnees receuillies
}
\description{Detection automatique des erreurs de saisies dans tous les structures de sante. Cette fonction necessite les fichiers de donnees Rdata. 
}
\usage{
InfoSaniR_check(data = "patho", Year = 2000, file = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ Donnee InfoSani extraite. Soit pathologie (patho), sante maternite et infantile (accouch1, accouch2 ou malnu), vaccination (vac) ou toutes les donnees (all).}
\item{file}{ Si FALSE alors un dossier 'Verification' sera cree automatiquement dans le dossier mes documents de votre PC. Si TRUE alors vous devriez specifier interactivement les dossiers des donnees et de verification.}
  \item{Year}{Annee de recueil des donnees InfoSani. Annee doit etre numerique et compris entre 2000 et 2099.}
}
}
\details{Vous devez specifier correctement le dossier contenant des fichiers RData et le dossier ou les verifications seront exportes.Par defaut, les verifactions seront dans un dossier appele verification dans 'mes documents' de votre PC.
}
\value{La fonction retourne automatiquement des fichiers xlsx avec les erreurs probables de saisie et cookies.

}
\references{
%% ~put references to the literature/web site here ~
}

\author{Regis Obiang <regis.obiang@lambarene.org>}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
