InfoSaniR_extract<-function(health = "patho", Year = 2000){
  
  if (missing(Year)) {
    stop("Year doit etre specicifie car manquant")
  }
  if (!is.numeric(Year)) {
    stop("Year doit etre de type numerique")
  }
  if (Year<2000 | Year>2099) {
    stop("Year doit etre compris entre 2000 et 2099")
  }
  
  if (missing(health)) {
    stop("Year doit etre specicifie car manquant")}
  if (!is.character(health)) {
    stop("health doit etre un caractere")
  }    
  if (!is.element(health, c("patho", "accouch1", "accouch2", "malnu", "vac"))) {
    stop("l'argument health mal specifie reverifier son orthographe")
  }
  
  # Choosing a folder interactively
  fileInput <- choose.dir(default = "", 
    caption = "Le dossier donnees xlsx InfoSani")
  
  fileOutput <- choose.dir(default = "", 
    caption = "Le dossier donnees exportees") 
  
  # Extraction de la liste fichiers InfoSani 
  FileCenter <- list.files(path=fileInput)
  # Vérification des fichiers dans le fichier
  if (length(FileCenter) < 1L) {
    stop("le dossier contenant les données est vide ou mal spécifié")
  } 
  
  #Vérification des fichiers Infosani (formats, structure du nom)
  #Structure du nom:Province_RegionSanitaire_Deptement_TypedeStructure_Nom_Annee.xls
  if (length(FileCenter) != 0) {
    ext.fichier <- str_detect(FileCenter, ".xlsx")
    struc.fichier <- str_count(FileCenter, "_")
    if (sum(ext.fichier) != length(FileCenter)) {
      cat("Le(s) fichier(s) ci-dessous n est(sont) pas au format excel version 2007 minimum requis:", "\n")
      print(FileCenter[!str_detect(FileCenter, ".xlsx")])
      stop("Veuillez retirer ce(s) fichier(s) du dossier des donnees avant de continuer svp")
    } else {
      if (sum(struc.fichier == 5 | struc.fichier == 6) != length(FileCenter)) {
        cat("Le(s) nom(s) de(s) fichier(s) ci-dessous n'est(sont) pas structuré(s) correctement", "\n")
        print(FileCenter[sum(struc.fichier == 5 | struc.fichier == 6) != length(FileCenter)])
        stop("Veuillez structurer correctement le(les) noms(s) de ce(s) fichier(s) sous le format: 
          Province_RegionSanitaire_Departement_TypedeStructure_Nom_Annee.xlsx")
      }
      }
    } 
  
  # Liste des provinces
  prov <- c("EST", "HOG", "MOG", "NYA", "NGO", "OGL", "OGI", "OGM", "WON")
  nameprov <- str_sub(FileCenter, start = 1, end = str_locate(FileCenter, "_")[, "start"] -  1)
  # Vérification des noms des provinces
  prov_bad <- FileCenter[!is.element(nameprov, prov)]
  if (length(prov_bad) > 0) {
    cat("Le(s) nom(s) de provinces de(s) fichier(s) ci-dessous sont mal orthographies:", "\n")
    print(prov_bad)
    stop("Veuillez corriger avant de continuer svp")
  }
  # Definition provinces
  nameprov2 <- rep("Estuaire", length(nameprov))
  nameprov2[str_detect(nameprov, "HOG")] <- "Haut-Ogooué"
  nameprov2[str_detect(nameprov, "MOG")] <- "Moyen-Ogooué"
  nameprov2[str_detect(nameprov, "NYA")] <- "Nyanga"
  nameprov2[str_detect(nameprov, "NGO")] <- "Ngounie"
  nameprov2[str_detect(nameprov, "OGL")] <- "Ogooué-Lolo"
  nameprov2[str_detect(nameprov, "OGI")] <- "Ogooué-Ivindo"
  nameprov2[str_detect(nameprov, "OGM")] <- "Ogooué-Maritime"
  nameprov2[str_detect(nameprov, "WON")] <- "Woleu-Ntem" 
  #Liste des regions sanitaires
  reg<-c('LBVOW','OUE','SUDE','CEN','CENS','SUD','EST','CENE','MARI','NOR')
  namereg<-c()
  for (i in 1:length(FileCenter)) {
    namereg[i] <- str_sub(FileCenter[i], start = str_locate_all(FileCenter, "_")[[i]][1, "end"][[1]] + 
        1, end = str_locate_all(FileCenter, "_")[[i]][2, "start"][[1]] - 1)
  }
  # Vérification des noms des provinces
  reg_bad <- FileCenter[!is.element(namereg, reg)]
  if (length(reg_bad) > 0) {
    cat("Le(s) nom(s) de(s) region(s) sanitaire(s) de(s) fichier(s) ci-dessous sont mal orthographies:", "\n")
    print(reg_bad)
    stop("Veuillez corriger avant de continuer svp")
  } 
  
  #Definition regions sanitaires
  namereg2 <- rep("Libreville-Owendo", length(namereg))
  namereg2[str_detect(namereg, "OUE")] <- "Ouest"
  namereg2[str_detect(namereg, "SUDE")] <- "Sud-Est"
  namereg2[str_detect(namereg, "CEN")] <- "Centre"
  namereg2[str_detect(namereg, "CENS")] <- "Centre-Sud"
  namereg2[str_detect(namereg, "SUD")] <- "Sud"
  namereg2[str_detect(namereg, "Est")] <- "Est"
  namereg2[str_detect(namereg, "CENE")] <- "Centre-Est"
  namereg2[str_detect(namereg, "MARI")] <- "Maritime"
  namereg2[str_detect(namereg, "NOR")] <- "Nord"  
  
  #Listes des departements
  Dept <- c("LBV1", "LBV2", "LBV3", "KOMOM", "KOMO", 
    "NOYA", "MPA", "DJOUA", "LEBL", "LEKL", "LEKO", 
    "PLAT", "DJOUO", "OGLE", "SEB", "BAYB", "LECL", 
    "OGLA", "ABAB", "BOUML", "DOLA", "LOUB", "LOUW", 
    "MOUG", "NDOL", "TSAMBM", "OGOU", "DOUYO", 
    "MOUG", "HAUB", "BASB", "DOUT", "DOUI", "MONG", 
    "IVIN", "LOPE", "ZAD", "MVOU", "LOBOU", "LOMB", 
    "MULUN", "OFOUO", "BEND", "ETIM", "NDOU", "WOLE", 
    "NTE", "HAUN", "OKA", "HAUC")
  nameDept <- c()
  for (i in 1:length(FileCenter)) {
    nameDept[i] <- str_sub(FileCenter[i], start = str_locate_all(FileCenter, "_")[[i]]
      [2, "end"][[1]] + 1, end = str_locate_all(FileCenter, "_")[[i]][3, "start"][[1]] - 1)
  }
  Dept_bad <- FileCenter[!is.element(nameDept, Dept)]
  if (length(Dept_bad) > 0) {
    cat("Le(s) nom(s) de(s) departement(s) de(s) fichier(s) ci-dessous sont mal orthographiés:", "\n")
    print(Dept_bad)
    stop("Veuillez corriger avant de continuer svp")
  } 
  
  # Definition departements
  nameDept2 <- rep("Libreville 1", length(nameDept))
  nameDept2[str_detect(nameDept, "LBV2")] <- "Libreville 2"
  nameDept2[str_detect(nameDept, "LBV3")] <- "Libreville 3"
  
  nameDept2[str_detect(nameDept, "KOMOM")] <- "Komo-Mondah"
  nameDept2[str_detect(nameDept, "KOMO")] <- "Komo"
  nameDept2[str_detect(nameDept, "NOYA")] <- "Noya"
  
  nameDept2[str_detect(nameDept, "MPA")] <- "Mpassa"
  nameDept2[str_detect(nameDept, "DJOUA")] <- "DJouri-Agnili"
  nameDept2[str_detect(nameDept, "LEBL")] <- "Lebombi-Leyou"
  nameDept2[str_detect(nameDept, "LEKL")] <- "Lekabi-Lewolo"
  nameDept2[str_detect(nameDept, "LEKO")] <- "Lekoko"
  nameDept2[str_detect(nameDept, "PLAT")] <- "Les plateaux"
  nameDept2[str_detect(nameDept, "DJOUO")] <- "Djoue-Onga"
  nameDept2[str_detect(nameDept, "OGLE")] <- "Ogooué-Letili"
  nameDept2[str_detect(nameDept, "SEB")] <- "Sebe-Brikolo"
  nameDept2[str_detect(nameDept, "BAYB")] <- "Bayi-Brikolo"
  nameDept2[str_detect(nameDept, "LECL")] <- "Leconi-lekori"
  
  nameDept2[str_detect(nameDept, "OGLA")] <- "Ogooué-Lacs"
  nameDept2[str_detect(nameDept, "ABAB")] <- "Abanga-Bigne"
  
  nameDept2[str_detect(nameDept, "BOUML")] <- "Boumi-Louetsi"
  nameDept2[str_detect(nameDept, "DOLA")] <- "Dola"
  nameDept2[str_detect(nameDept, "LOUB")] <- "Louetsi-Bikala"
  nameDept2[str_detect(nameDept, "LOUW")] <- "Louetsi-Wano"
  nameDept2[str_detect(nameDept, "MOUG")] <- "Mougalaba"
  nameDept2[str_detect(nameDept, "NDOL")] <- "Ndolou"
  nameDept2[str_detect(nameDept, "TSAMBM")] <- "Tsamba-magotsi"
  nameDept2[str_detect(nameDept, "OGOU")] <- "Ogoulou"
  nameDept2[str_detect(nameDept, "DJOUO")] <- "Douya-Onoye"
  
  nameDept2[str_detect(nameDept, "MOUG")] <- "Mougoutsi"
  nameDept2[str_detect(nameDept, "HAUB")] <- "Haute-Banio"
  nameDept2[str_detect(nameDept, "BASB")] <- "Basse-Banio"
  nameDept2[str_detect(nameDept, "DOUT")] <- "Doutsila"
  nameDept2[str_detect(nameDept, "DOUI")] <- "Douigny"
  nameDept2[str_detect(nameDept, "MONG")] <- "Mongo"
  
  nameDept2[str_detect(nameDept, "IVIN")] <- "Ivindo"
  nameDept2[str_detect(nameDept, "LOPE")] <- "Lope"
  nameDept2[str_detect(nameDept, "ZAD")] <- "Zadie"
  nameDept2[str_detect(nameDept, "MVOU")] <- "Mvoung"
  
  nameDept2[str_detect(nameDept, "LOBOU")] <- "Lolo-Bouenguidi"
  nameDept2[str_detect(nameDept, "LOMB")] <- "Lombo-Bouenguidi"
  nameDept2[str_detect(nameDept, "MULUN")] <- "Mulundi"
  nameDept2[str_detect(nameDept, "OFOUO")] <- "Ofoue-Onoye"
  
  nameDept2[str_detect(nameDept, "BEND")] <- "Bendje"
  nameDept2[str_detect(nameDept, "ETIM")] <- "Etimboue"
  nameDept2[str_detect(nameDept, "NDOU")] <- "Ndougou"
  
  nameDept2[str_detect(nameDept, "WOLE")] <- "Woleu"
  nameDept2[str_detect(nameDept, "NTEM")] <- "Ntem"
  nameDept2[str_detect(nameDept, "HAUN")] <- "Haut-Ntem"
  nameDept2[str_detect(nameDept, "OKA")] <- "Okano"
  nameDept2[str_detect(nameDept, "HAUC")] <- "Haut-Como"
  
  #Récupération de type strucutres
  typestruc <- c("DISP", "INF", "CS", "CM", "HOP", "AUT")
  nametypestruc <- c()
  for (i in 1:length(FileCenter)) {
    nametypestruc[i] <- str_sub(FileCenter[i], start = str_locate_all(FileCenter, "_")[[i]]
      [3, "start"][[1]] + 1, end = str_locate_all(FileCenter, "_")[[i]][4, "start"][[1]] - 1)
  }
  typestruc_bad <- FileCenter[!is.element(nametypestruc, typestruc)]
  if (length(typestruc_bad) > 0) {
    cat("Le(s) nom(s) de(s) type(s) de strucutre(s) de(s) fichier(s) ci-dessous sont mal orthographiés:", 
      "\n")
    print(reg_Dept)
    stop("Veuillez corriger avant de continuer svp")
  }
  #Définition les types d'établissements de santé
  Centertype <- rep("Autre", length(nametypestruc))
  Centertype[str_detect(nametypestruc, "DISP")] <- "Dispensaire"
  Centertype[str_detect(nametypestruc, "INF")] <- "Infirmerie"
  Centertype[str_detect(nametypestruc, "CS")] <- "Centre de Santé"
  Centertype[str_detect(nametypestruc, "CM")] <- "Centre Médical"
  Centertype[str_detect(nametypestruc, "HOP")] <- "Hopital" 
  
  #Récupération des noms de structures
  #Region sanitaire CENTRE:
  struc4 <- c("St Martin Zilé", "Isaac", "Lycée Charles Mefane", 
    "Makouke", "Atsie", "CES Lambaréné", "Sunly", 
    "College Adiwa", "Georges Rawiri", "Magnang", 
    "Ngomo", "Urbain de Lambaréné", "Albert Schweitzer", 
    "Regionale Lambaréné", "Kery", "Biweni", 
    "Tchad 1", "Nzoghe Bang", "Koungoule", "Adanhe", 
    "Medang Nkoghe", "Benguie 2", "Benguie 4", 
    "Paris Bifoun", "Bindo", "Belle Vue", "Allonha", 
    "Siat Zilé", "Odimba", "Ntchatanga", "Aschouka", 
    "Gome Dakar", "Siat Lambaréné", "Nombedouma", 
    "Nombakélé", "Makouke", "Ndjolé", "Darlot", 
    "Ebel Abanga Rive Droite", "Minkok Messeng", 
    "SMI Régionale Lambaréné", "CES Modjeckou") 
  namestruc <- c()
  for (i in 1:length(FileCenter)) {
    namestruc[i] <- str_sub(FileCenter[i], start = str_locate_all(FileCenter, "_")[[i]]
      [4, "start"][[1]] + 1, end = str_locate_all(FileCenter, "_")[[i]][5, "start"][[1]] - 1)
  }
  struc4_bad <- FileCenter[!is.element(namestruc, struc4)]
  if (length(struc4_bad) > 0) {
    cat("Le(s) nom(s) de(s) établissement(s) de santé de(s) fichier(s) ci-dessous sont mal orthographiés:", 
      "\n")
    print(struc4_bad)
    stop("Veuillez corriger avant de continuer svp")
  } 
  
  # Définition des soins médicaux (consultations ou hospitalisations)
  soin <- rep("Consultation", length(FileCenter))
  soin[str_detect(FileCenter, "HOSP")] <- "Hospitalisation"
  
  # Recupération des années
  year <- str_extract(FileCenter, "20[0-9]{2}")
  year_bad <- FileCenter[!is.element(year, as.character(Year))]
  if (length(year_bad) > 0) {
    cat("Le(s) année(s) de(s) établissement(s) de santé de(s) fichier(s) ci-dessous est(sont)
      mal spécifié(s) ou l'année d'extraction ne corresponds pas à ceux des fichiers:", 
      "\n")
    print(year_bad)
    stop("Veuillez corriger avant de continuer svp")
  } 
  
  # Définition des mois en francais
  month.french <- c("Janvier", "Fevrier", "Mars", "Avril", "Mai", "Juin", 
    "Juillet", "Aout", "Septembre", "Octobre", "Novembre", "Decembre") 
  
  
  ##########################################################################################################
  if (health == 'patho'){
    
    ## Extraction des données de pathologies Extraction des données de tous les
    ## établissements de Santé
    sheetlist <- c(2, 3, 4, 6, 7, 8, 10, 11, 12, 14, 15, 16)
    indextab <- data.frame(strow = c(1, 52, 69, 74,  120, 121, 127, 151, 176, 199, 210,
      227, 264, 281, 301, 319, 329, 340, 344, 345, 350, 370, 403, 409))
    indextab$enrow <- c(50, 63, 72, 115, 120, 126, 149, 170, 197, 208, 221,
      262, 275, 299, 317, 323, 339, 343, 344, 348, 364, 401, 407, 415)
    indextab$coldeas <- c(2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 
      2, 3, 2, 2, 2, 2)
    
    tab0 <- vector("list", length(FileCenter))
    tab1 <- vector("list", length(sheetlist) * dim(indextab)[1])
    start <- Sys.time()
    
    for (k in 1:length(FileCenter)) {
      cat("...Extraction des données de pathologies:", namestruc[k], "\n")
      
      for (i in 1:12) {
        sheet<- read.xlsx(file.path(fileInput, FileCenter[k]), sheet = sheetlist[i], 
          startRow=556, colNames=F)
        for (j in 1:24) {
          tab1[[((i - 1) * 24 + j)]] <- sheet[c(indextab[j, 1]:indextab[j, 2]),
            c(1, indextab[j, 3],4:23)]
          names(tab1[[((i - 1) * 24 + j)]]) <- c("Code_Maladie", "Maladies", "0_CAS_M", 
            "0_CAS_F", "0_DCD_M", "0_DCD_F", "1_CAS_M", "1_CAS_F", "1_DCD_M", "1_DCD_F",
            "5_CAS_M", "5_CAS_F", "5_DCD_M", "5_DCD_F", "15_CAS_M", "15_CAS_F", "15_DCD_M", 
            "15_DCD_F", "50_CAS_M", "50_CAS_F", "50_DCD_M", "50_DCD_F")          
        }
        rmtm <- round((length(FileCenter) * 12 - (k - 1) * 12 - i) * (difftime(Sys.time(), start, 
          units = "mins"))/((k - 1) * 12 + i), 1)
        cat("... Extraction des données du mois: ", month.french[i], "...estimation du temps restant:", 
          rmtm, "minutes \n")
      }
      tab11 <- do.call(rbind, tab1)
      tab11$Code_Domaine <- rep(c("I", "N", "H", "D", "V", "R", "M", "S", "U", "L", "E", "T",
        "O", "C", "E'", "G", "P", "Y"), c(50, 12, 4, 72, 20, 22, 10, 12, 36, 12, 19, 17, 5, 
          20, 15, 32, 5, 7))
      tab11$Domaine <- rep(c("Infect", "Metab", "Haemato", "Gastro", "Cardio", "Resp", "Psych",
        "Neuro", "Uro", "Ortho", "Trauma", "Dermato", "Opthalmo", "ORL", "Stomato", "Gyn",
        "Perinat", "Autres"), c(50, 12, 4, 72, 20, 22, 10, 12, 36, 12, 19, 17, 5, 20, 15, 32, 5, 7))
      tab11$Mois <- rep(month.french, each = 370)
      tab12 <- melt(tab11, id.vars = c("Code_Maladie", "Code_Domaine", "Domaine", "Maladies", "Mois"))
      tab12$Province <- nameprov2[k]
      tab12$Region_Sanitaire <- namereg2[k]
      tab12$Departement <- nameDept2[k]
      tab12$Etablissement <- Centertype[k]
      tab12$Annee <- year[k]
      tab12$Nom <- namestruc[k]
      tab12$Soin <- soin[k]
      tab12$Age <- str_extract(tab12$variable, "[0-5]{1,2}")
      tab12$Statut_Vital <- str_extract(tab12$variable, "[A-S]{3}")
      tab12$Sexe <- str_extract(tab12$variable, "[M,F]")
      tab12 <- tab12[, c(8, 9, 10, 11, 13, 12, 5, 3, 1, 4, 14, 17, 15, 16, 7)]
      tab0[[k]] <- tab12
      cat("Fin d'extraction des données!\n")
    } 
    tabfinal <- do.call(rbind,tab0)
    tabfinal1 <- tabfinal[!is.na(tabfinal$value),]
    
    #Exportation des donées vers une table Excel/RData
    ##################################################
    patho <- tabfinal1
    patho$Age <- factor(patho$Age)
    patho$Maladies <- as.character(patho$Maladies)
    levels(patho$Age) <- c("0-11 mois", "1-4 ans", "15-49 ans", "5-14 ans", ">49 ans")
    patho$Age <- factor(patho$Age, 
      levels=c('0-11 mois','1-4 ans','5-14 ans','15-49 ans','>49 ans'))
    names(patho)[15] <- "Effectif"
    save(ls = "patho", file = file.path(fileOutput,paste(paste('data_patho', year[1], sep='_'),'RData',sep='.')))
    
    write.xlsx(patho, file = file.path(fileOutput,paste(paste('data_patho', year[1], sep='_'),'xlsx',sep='.')),
      row.names = F)
    return(patho) 
  }
  
  ##########################################################################################################
  if (health == "accouch1") {
    
    ## Extraction des données d'accouchement
    data_final <- vector("list", 12 * length(FileCenter))
    IndexFeuil <- c(2, 3, 4, 6, 7, 8, 10, 11, 12, 14, 15, 16)
    k <- 1
    for (i in 1:length(FileCenter)) {
      cat("...Extraction des données de Santé Maternité Infantile:", namestruc[i], "\n")
      j <- 1
      while (j <= 12) {
        time.j <- Sys.time()
        data <- read.xlsx(file.path(fileInput, FileCenter[i]), colNames = F, sheet = IndexFeuil[j], 
          , startRow = 206)
        data <- data[c(1, 2, 6:8, 10:19), c(1, 8, 14, 20)]
        colnames(data) <- c("Type_Accouchement", "Domicile", "Infrastructure", "Autre")
        data_bis <- melt(data, id = "Type_Accouchement")
        colnames(data_bis) <- c("Type_Accouchement", "Lieu", "Effectif")
        data_bis$Province <- nameprov2[i]
        data_bis$Region_Sanitaire <- namereg2[i]
        data_bis$Annee <- year[i]
        data_bis$Mois <- month.french[j]
        data_bis$Nom <- namestruc[i]
        data_bis$Departement <- nameDept2[i]
        data_bis$Etablissement <- Centertype[i]
        data_final[[k]] <- data_bis[, c(4, 5, 9, 10, 8, 6, 7, 2, 1, 3)]
        #time.j <- c(time.j, Sys.time())
        rmtm <- round((length(FileCenter) * 12 - (i - 1) * 12 - j) * (difftime(Sys.time(), time.j, units = "mins")), 
          1)
        cat("... Extraction des données du mois: ", month.french[j], "...estimation du temps restant:", rmtm, 
          "minutes \n")
        k <- k + 1
        j <- j + 1       
      }      
    }
    cat("Fin d'extraction des données!\n")
    
    # Exportation des donées vers une table Excel/RData
    data_merge = do.call("rbind", data_final)
    last_data <- na.omit(data_merge)
    accouch1 <- last_data
    save(accouch1, file = file.path(fileOutput,paste(paste('data_accouch1', year[1], sep='_'),'RData',sep='.')))
    write.xlsx(accouch1, file = file.path(fileOutput,paste(paste('accouch1', year[1], sep='_'),'xlsx',sep='.')),
      row.names = F)
    return(last_data)    
  }
  
  ##########################################################################################################
  if (health == "accouch2") {
    
    ## Extraction des données d'accouchement
    data_final <- vector("list", 12 * length(FileCenter))
    IndexFeuil <- c(2, 3, 4, 6, 7, 8, 10, 11, 12, 14, 15, 16)
    k <- 1
    
    for (i in 1:length(FileCenter)) {
      cat("...Extraction des données de Santé Maternité Infantile:", namestruc[i], "\n")
      j <- 1
      while (j <= 12) {
        time.j <- Sys.time()
        data <- read.xlsx(file.path(fileInput, FileCenter[i]), colNames = F, sheet = IndexFeuil[j],
          startRow = 231)
        data <- data[c(1:5, 7:10), c(1, 9, 11, 15, 17, 21, 23)]
        colnames(data) <- c("Naissance", "Dom_M", "Dom_F", "Inf_M", "Inf_F", "Aut_M", "Aut_F")
        data_bis <- melt(data, id = "Naissance")
        colnames(data_bis)[3] <- "Effectif"
        data_bis$Lieu <- str_extract(data_bis$variable, "[A-z]{3}")
        data_bis$Lieu[data_bis$Lieu == "Dom"] <- "Domicile"
        data_bis$Lieu[data_bis$Lieu == "Inf"] <- "Infrastructure"
        data_bis$Lieu[data_bis$Lieu == "Aut"] <- "Autre"
        data_bis$Sexe <- str_extract(data_bis$variable, "[M,F]")
        data_bis$Province <- nameprov2[i]
        data_bis$Region_Sanitaire <- namereg2[i]
        data_bis$Annee <- year[i]
        data_bis$Mois <- month.french[j]
        data_bis$Nom <- namestruc[i]
        data_bis$Departement <- nameDept2[i]
        data_bis$Etablissement <- Centertype[i]
        data_final[[k]] <- data_bis[, c("Province", "Region_Sanitaire", "Departement", "Etablissement", "Nom", "Annee", 
          "Mois", "Naissance", "Lieu", "Sexe", "Effectif")]
        rmtm <- round((length(FileCenter) * 12 - (i - 1) * 12 - j) * (difftime(Sys.time() , time.j, units = "mins")), 
          1)
        cat("... Extraction des données du mois: ", month.french[j], "...estimation du temps restant:", rmtm, "minutes \n")
        k <- k + 1
        j <- j + 1
      }
    }
    cat("Fin d'extraction des données!\n")
    
    # Exportation des données vers une table Excel/RData
    data_merge = do.call("rbind", data_final)
    last_data <- na.omit(data_merge)
    accouch2 <-last_data
    save(accouch2, file = file.path(fileOutput,paste(paste('data_accouch2', year[1], sep='_'),'RData',sep='.')))
    write.xlsx(accouch1, file = file.path(fileOutput,paste(paste('accouch2', year[1], sep='_'),'xlsx',sep='.')),
      row.names = F)
    return(last_data)
  } 
  
  
  ##########################################################################################################
  if (health == "malnu") {
    
    ## Extraction des données malnutrition
    data_final <- vector("list", 12 * length(FileCenter))
    IndexFeuil <- c(2, 3, 4, 6, 7, 8, 10, 11, 12, 14, 15, 16)
    k <- 1
    
    for (i in 1:length(FileCenter)) {
      cat("...Extraction des données de malnutrition:", namestruc[i], "\n")
      j <- 1
      while (j <= 12) {
        time.j <- Sys.time()
        data <- read.xlsx(file.path(fileInput, FileCenter[i]), colNames = F, sheet = IndexFeuil[j], startRow = 311)
        data <- data[1:7, c(1, 12, 17, 22)]
        colnames(data) <- c("Depistage", "0-11", "12-23", "24-59")
        data_bis <- melt(data, id = "Depistage")
        colnames(data_bis) <- c("Depistage", "Age_Mois", "Effectif")
        data_bis$Province <- nameprov2[i]
        data_bis$Region_Sanitaire <- namereg2[i]
        data_bis$Annee <- year[i]
        data_bis$Mois <- month.french[j]
        data_bis$Nom <- namestruc[i]
        data_bis$Departement <- nameDept2[i]
        data_bis$Etablissement <- Centertype[i]
        data_final[[k]] <- data_bis[, c("Province", "Region_Sanitaire", "Departement", "Etablissement", 
          "Nom", "Annee", "Mois", "Depistage", 
          "Age_Mois", "Effectif")]
        rmtm <- round((length(FileCenter) * 12 - (i - 1) * 12 - j) * (difftime(Sys.time(), time.j, units = "mins")), 1)
        cat("... Extraction des données du mois: ", month.french[j], "...estimation du temps restant:", rmtm, "minutes \n")
        k <- k + 1
        j <- j + 1
      }
    }
    cat("Fin d'extraction des données!\n")
    
    
    # Exportation des données vers une table Excel/RData
    data_merge = do.call("rbind", data_final)
    last_data <- na.omit(data_merge)
    malnu <- last_data
    save(malnu, file = paste(fileOutput,paste(paste('/data_malnu', year[1], sep='_'),'RData',sep='.'),sep=''))
    write.xlsx(malnu, file = file.path(fileOutput,paste(paste('malnu', year[1], sep='_'),'xlsx',sep='.')),
      row.names = F)
    return(last_data)
  } 
  ##########################################################################################################
  if (health == "vac") {
    
    ## Extraction des donn?es de vaccination
    data_final <- vector("list", 12 * length(FileCenter))
    IndexFeuil <- c(2, 3, 4, 6, 7, 8, 10, 11, 12, 14, 15, 16)
    k <- 1
    
    for (i in 1:length(FileCenter)) {
      cat("...Extraction des données de vaccination:", namestruc[i], "\n")
      j <- 1  
      while (j <= 12) {
        time.j <- Sys.time()
        data <- read.xlsx(file.path(fileInput, FileCenter[i]), colNames = F, sheet = IndexFeuil[j], 
          startRow = 320)
        data <- data[1:28, c(1, 8, 12, 16, 20, 24)]
        colnames(data) <- c("Antigene", "0-11", "12-23", "24-59", "Femmes enceintes", "Femmes non enceintes")
        data_bis <- melt(data, id = "Antigene")
        colnames(data_bis) <- c("Antigene", "Age", "Effectif")
        data_bis$Province <- nameprov2[i]
        data_bis$Region_Sanitaire <- namereg2[i]
        data_bis$Annee <- year[i]
        data_bis$Mois <- month.french[j]
        data_bis$Nom <- namestruc[i]
        data_bis$Departement <- nameDept2[i]
        data_bis$Etablissement <- Centertype[i]
        data_final[[k]] <- data_bis[, c("Province", "Region_Sanitaire", "Departement", "Etablissement", "Nom", "Annee", "Mois", 
          "Antigene", "Age", "Effectif")]
        rmtm <- round((length(FileCenter) * 12 - (i - 1) * 12 - j) * (difftime(Sys.time(), time.j, units = "mins")), 1)
        cat("... Extraction des données du mois: ", month.french[j], "...estimation du temps restant:", rmtm, "minutes \n")
        k <- k + 1
        j <- j + 1
      }
    }
    cat("Fin d'extraction des données!\n")
    
    # Exportation des don?es vers une table Excel/RData
    data_merge = do.call("rbind", data_final)
    last_data <- na.omit(data_merge)
    vac <- last_data
    save(vac, file = paste(fileOutput,paste(paste('/data_vac', year[1], sep='_'),'RData',sep='.'),sep=''))
    write.xlsx(vac, file = file.path(fileOutput,paste(paste('vac', year[1], sep='_'),'xlsx',sep='.')),
      row.names = F)
    
    return(last_data)
  }
  }
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



