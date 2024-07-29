# BE_n_ug_imputation.R
# Julian Bischof
# 05.03.2021
# Abhänigkeiten: BE_imputation.R

#-----------------------------------------
# n_ug: Anazahl unterirdische vollgeschosse. Die Fläche der UGs ist 
# in NRF und EBF bereits enthalten. Die BGF der UGs ergibt sich aus 
# GEB_FLAECHE * n_UG, die NRF der UGs folgt durch Multiplikation mit 
# dem Verhältnis der gesamten NRF/BGF.
#-----------------------------------------

#............................................................................
#
# 1
#Daten in Auswertepakete unterteilen####
#
#............................................................................

#kein clean set benötigt. DB_BE genügt

# #............................................................................
# #
# # 2
# #Auswertung####
# #
# #............................................................................
# 

# n_ug ist vollständig, nur -8 zu 0 Korrektur! Die Modalwert Imputationen wurden vorher erstellt. 
# Diese sind unten weiter zur Info noch gelistet, werden aber bei vollem Datensatz nicht umgesetzt.

#------------
#Einzelklasse

# #Modalwert der verschieden hk_geb klassen
# dat <- data.frame(factor=as.factor(DB_BE$hk_geb), value=DB_BE$n_ug) #factor=as.factor(DB_BE_clean$bak_grob), 
# mode_n_ug_based_on_hk_geb <- with(dat, tapply(value, factor, Mode))
# mode_n_ug_based_on_hk_geb #zeige Liste der Mittelwerte von n_ug in Abhänigkeit von hk_geb
# mode_n_ug_based_on_hk_geb[[1]] #zeige Mittelwert von hk_geb = 1
# 
# #Modalwert der verschieden bak_grob klassen
# dat <- data.frame(factor=as.factor(DB_BE$bak_grob), value=DB_BE$n_ug) #factor=as.factor(DB_BE_clean$bak_grob), 
# mode_n_ug_based_on_hk_geb <- with(dat, tapply(value, factor, Mode))
# mode_n_ug_based_on_hk_geb #zeige Liste der Mittelwerte von n_ug in Abhänigkeit von hk_geb
# mode_n_ug_based_on_hk_geb[[1]] #zeige Mittelwert von hk_geb = 1

#-----------
#Kombiklasse

# #Modalwert der verschieden hk_geb_bak_grob klassen
# # table(DB_BE$hk_geb)
# # table(DB_BE$bak_grob)
# hk_geb_bak_grob <- paste(as.character(DB_BE$hk_geb), as.character(DB_BE$bak_grob), sep=".") # Kombiniere hk_geb und bak_grob zu character
# dat <- data.frame(factor=as.factor(hk_geb_bak_grob), value=DB_BE$n_ug) #factor=as.factor(DB_BE_clean$bak_grob), 
# mode_n_ug_based_on_hk_geb_bak_grob <- with(dat, tapply(value, factor, Mode))
# mode_n_ug_based_on_hk_geb_bak_grob #zeige Liste der Mittelwerte von n_ug in Abhänigkeit von hk_geb
# mode_n_ug_based_on_hk_geb_bak_grob[[1]] #zeige Mittelwert von hk_geb = 1

# #Modalwert der verschieden hk_geb_bak klassen
# # table(DB_BE$hk_geb)
# # table(DB_BE$bak)
# hk_geb_bak <- paste(as.character(DB_BE$hk_geb), as.character(DB_BE$bak), sep=".") # Kombiniere hk_geb und bak zu character
# dat <- data.frame(factor=as.factor(hk_geb_bak), value=DB_BE$n_ug) #factor=as.factor(DB_BE_clean$bak_grob), 
# mode_n_ug_based_on_hk_geb_bak <- with(dat, tapply(value, factor, Mode))
# mode_n_ug_based_on_hk_geb_bak #zeige Liste der Mittelwerte von n_ug in Abhänigkeit von hk_geb
# mode_n_ug_based_on_hk_geb_bak[["5.12"]] #zeige Modalwert von hk_geb_bak = 5.12

#Modalwert der verschieden hk_geb_bak klassen
# table(DB_BE$hk_geb)
# table(DB_BE$bak)
DB_BE$n_ug[DB_BE$n_ug==(-8)] <- 0 # -8 bedeutet das es kein Untergeschoss gibt. D.h. -8 wird zu 0
hk_geb_bak <- paste(as.character(DB_BE$hk_geb), as.character(DB_BE$bak), sep=".") # Kombiniere hk_geb und bak zu character
dat <- data.frame(factor=as.factor(hk_geb_bak), value=DB_BE$n_ug) #factor=as.factor(DB_BE_clean$bak_grob),

dat_clean <- subset(dat, value>=0)
mode_n_ug_based_on_hk_geb_bak <- with(dat_clean, tapply(value, factor, mean))
mode_n_ug_based_on_hk_geb_bak #zeige Liste der Mittelwerte von n_ug in Abhänigkeit von hk_geb
mode_n_ug_based_on_hk_geb_bak[["5.12"]] #zeige Modalwert von hk_geb_bak = 5.12


#Modelwertimputation nach hk_geb und bak für alle n_ug mit -7
# class(DB_BE$hk_geb) #Check the data class of a vector
# class(DB_BE$bak) #Check the data class of a vector
class(hk_geb_bak)
# levels_hk_geb <- levels(as.factor(DB_BE$hk_geb)) #liste die Ausprägunguen von factor vektoren auf
# levels_bak_grob <- levels(as.factor(DB_BE$bak_grob))
levels_hk_geb_bak <- levels(as.factor(hk_geb_bak))
for (i in levels_hk_geb_bak){
  DB_BE$n_ug[DB_BE$n_ug==(-7) & hk_geb_bak==i] <- mode_n_ug_based_on_hk_geb_bak[[i]]
}
# table(DB_BE$n_ug)


#-----------
# -8 bedeutet das es kein Untergeschoss gibt. D.h. -8 wird zu 0
DB_BE$n_ug[DB_BE$n_ug==(-8)] <- 0




#............................................................................
#
# 3
#Speichern####
#
#............................................................................

# Imputierte Variable zum Speichern vorbereiten

n_ug <- DB_BE$n_ug
Imputierte_Variable <- as.data.frame(cbind(scr_gebaeude_id, n_ug)) # mit Geb.ID verbinden, damit klar zuweisbar

# write it as a csv file
write.csv(Imputierte_Variable, 'DB_BE_n_ug_imputiert.csv')


