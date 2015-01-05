##Data Extraction 
sub_accouch11<-subset(accouch1,grepl('eutociques$|dystociques$',Type_Accouchement))
sub_accouch112<-ddply(sub_accouch11,.(Mois), summarise,
                     freq=sum(as.numeric(as.character(Effectif)),na.rm=TRUE))
sub_accouch12 <- subset(accouch1,grepl('^Nombre de (.*?) maternels|^Accouchements assist.es',Type_Accouchement))
sub_accouch13 <- subset(accouch2,grepl('^Enfants n.s vivants|pr.matur.s$',Naissance))

##Data manipulation
sub_accouch112$Mois <- factor(sub_accouch112$Mois,levels = month.french)
sub_accouch12$Mois <- factor(sub_accouch12$Mois,levels = month.french)
sub_accouch13$Mois <- factor(sub_accouch13$Mois,levels = month.french)
sub_accouch12$Effectif <- as.numeric(as.character(sub_accouch12$Effectif))
sub_accouch13$Effectif <- as.numeric(as.character(sub_accouch13$Effectif))

##Description monthly
sub_accouch11a<-cast(sub_accouch112, .~Mois,value="freq")
sub_accouch12a<-cast(sub_accouch12, Type_Accouchement~Mois,value="Effectif", 
                     sum,margins=F,na.rm=TRUE)
sub_accouch13a<-cast(sub_accouch13, Naissance~Mois,value="Effectif", 
                     sum,margins=F,na.rm=TRUE)
colnames(sub_accouch11a)[1]<-'Indicateurs'
colnames(sub_accouch12a)[1]<-'Indicateurs'
colnames(sub_accouch13a)[1]<-'Indicateurs'

data_health <- merge(sub_accouch11a, sub_accouch12a, all=T)
data_health <- merge(data_health, sub_accouch13a, all=T)
data_health[is.na(data_health)] <- 0

##Indicator tables
tx1000<-function(x){round(1000*x/x[1],2)}
tx100<-function(x){round(100*x/x[1],2)}
Indicator_health1<-apply(data_health[c(1,3:5),2:13],2,tx1000)
Indicator_health2<-apply(data_health[c(1,2),2:13],2,tx100)
Indicator_health <- rbind (Indicator_health1[2:4,], Indicator_health2[2,])
Indicator_health <- rbind (sub_accouch11a[,2:13], Indicator_health)
Indicator_health <- as.data.frame(Indicator_health)
Indicator_names <- data.frame(Indicateurs=c('Nbre accouchements','TMM pour 1000','TEV2500 pour 1000', 'TEVP pour 1000','PAA pour 100'))
Indicator_table <- cbind(Indicator_names, Indicator_health)



