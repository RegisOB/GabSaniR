InfoSaniR_report <- function(data = "patho", Year = 2000, file = FALSE) {
    
    ## Checking inputs
    if (missing(data) && file == FALSE) {
        stop("data doit etre specifie car manquant")
    }
    if (!is.character(data) && file != FALSE) {
        stop("data doit etre un caractere")
    }
    
    if (missing(Year)) {
        stop("Year doit etre specifie car manquant")
    }
    if (!is.numeric(Year)) {
        stop("Year doit etre de type numerique")
    }
    if (Year < 2000 | Year > 2099) {
        stop("Year doit etre compris entre 2000 et 2099")
    }
    
    if (exists(data, envir = .GlobalEnv) && file == FALSE) {
        setwd("~")
        if (is.element(paste("Report", data, as.character(Year), 
            sep = "_"), list.files(getwd()))) {
            fileReport <- normalizePath(file.path(getwd(), 
                paste("Report", data, as.character(Year), sep = "_")))
        } else {
            dir.create(file.path(getwd(), paste("Report", data, 
                as.character(Year), sep = "_")))
            fileReport <- normalizePath(file.path(getwd(), 
                paste("Report", data, as.character(Year), sep = "_")))
        }
    } else {
        # Choosing a folder interactively
        fileExtract <- choose.dir(default = "", caption = "Le dossier donnees .RData InfoSani")
        fileReport <- choose.dir(default = "", caption = "Le dossier du rapport final")
        
        ## Checking extracted data files specifier la recherche
        ## selon data mis en entree (patho,....All)
        FileData <- list.files(path = fileExtract, pattern = ".RData$")
        DirFileData <- as.list(dir(path = fileExtract, pattern = ".RData$", 
            full.names = T))
        if (length(FileData) < 1L) {
            stop("le dossier ne contient pas les fichiers format .RData ou mal specifie")
        }
        
        # Checking the year
        year <- str_extract(FileData, "20[0-9]{2}")
        year_bad <- FileData[!is.element(year, as.character(Year))]
        if (length(year_bad) > 0) {
            cat("Le(s) annee(s) de(s) fichier(s) ci-dessous est(sont) mal specifie(s) \n          ou l'annee d'extraction ne corresponds pas a ceux des fichiers:", 
                "\n")
            print(year_bad)
            stop("Veuillez corriger avant de continuer svp")
        }
        
        # Loading all Data.RData
        lapply(DirFileData, load, envir = .GlobalEnv)
    }
    
    ## Reporting
    rep <- system.file("templates", "patho.Rnw", package = "GabSaniR")
    min <- system.file("templates", "min.png", package = "GabSaniR")
    file.copy(from = rep, to = fileReport, overwrite = T)
    file.copy(from = min, to = fileReport, overwrite = T)
    setwd(fileReport)
    knit("patho.Rnw", encoding = "UTF-8", quiet = F)
    texi2pdf("patho.tex", clean = T)
} 
