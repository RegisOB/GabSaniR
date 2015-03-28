\name{InfoSaniR_report}
\alias{InfoSaniR_report}
\title{Rapport dynamique statistiques annuel de sante}
\description{Produit automatiquement un rapport dynamique statistiques annuel de sante pour une province du Gabon. Cette fonction necessite les donnees RData extraites des fichiers InfoSani de toutes structures sanitaires dans une province.}
\usage{
InfoSaniR_report(data, Year, file=FALSE)
}

\arguments{
\item{data}{ Donnee InfoSani extraite. Soit pathologie (patho), sante maternite et infantile (accouch1, accouch2 ou malnu), vaccination (vac) ou toutes les donnees (all).}
\item{file}{ Si FALSE alors le dossier du rapport sera cree automatiquement dans le dossier mes documents de votre PC. Si TRUE alors vous devriez specifier interactivement les dossiers des donnees et du rapport final.}
  \item{Year}{ Annee de recueil des donnees InfoSani. Annee doit etre numerique et compris entre 2000 et 2099.}
}
\details{Vous devez specifier correctement le dossier contenant des fichiers RData et le dossier ou le rapport final sera exporte.Par defaut, le rapport sera dans un dossier appele Rapport dans 'mes documents' de votre PC.}
\value{La fonction retourne automatiquement un rapport annuel nomme 'report.pdf' dans le dossier specifie pour exportation sur les pathologies, sante maternite infantile et vaccination.}
\references{
%% ~put references to the literature/web site here ~
}
\author{Regis Obiang <regis.obiang@lambarene.org>}


\examples{
#Chargement des donnees de pathologie dans la session R
data("Data_patho_2012")

#Production du rapport annuel les pathologies 
InfoSaniR_report(data='patho', Year=2012, file=FALSE)}
\keyword{report }
