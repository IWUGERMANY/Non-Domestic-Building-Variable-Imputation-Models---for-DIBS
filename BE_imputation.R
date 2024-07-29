# BE Imputationen
# Imputation of missing values in the large data set (BRE) of ENOB:DataNWG, requiered for DIBS simulation
#
# Author/Developer: Julian Bischof
#
# 19.08.2021
# Updated 11.12.2023
#
# Cleaned up 12.07.2024: Removed of most unnecessary comments. Only final version of imputation process is presented. 
# Any development steps to get to the final impuation script version (e.g. tested alternatives) are removed.
#
# UTF 8
#
# Dependencies: JBs_R_Functions.R; BE_q25_imputation.R; Einzelskripte im Ordner einzel_imputationen


#............................................................................
#
# SET UP ####
#
#............................................................................
#Sys.setlocale(category = "LC_ALL", locale = "German") # sets Language Envrionment

rm(list=ls()) # cleans Environment

#set working directory
wd <- getwd()

sink()  # returns output to the console in case it was not there already



#............................................................................
#
# Load Libraries ####
#
#............................................................................

#install.packages("dplyr")
library(dplyr)
library(tidyr)
library(nnet)
library(downloader)

#............................................................................
#
# Implement Functions ####
#
#............................................................................

# calling my function R-Script on GitHub!
SHA <- sha_url('https://raw.githubusercontent.com/julianbischof/R---JBs-Data-Cleaning-Function-Collection/main/JBs_R_Functions.R')
SOURCE <- 'https://raw.githubusercontent.com/julianbischof/R---JBs-Data-Cleaning-Function-Collection/main/JBs_R_Functions.R'
downloader::source_url(SOURCE, sha = SHA, prompt=FALSE)
#downloader::source_url(SOURCE, prompt=FALSE)
print('Downloaded and using JBs_R_Functions')



#............................................................................
#
# Read Data ####
#
#............................................................................

#Read CSV DB-Breitenerhebung####
DB_BE = read.csv("S:/GitHub_Data_Access/Zugriff_BRE_gebview_HRF.csv", header = TRUE, sep = ";", dec = ",", fill = TRUE) # 11.12.2023 -> updated file path

#Read CSV DB-Screening####
DB_SCR = read.csv("S:/GitHub_Data_Access/Zugriff_Screening_Gebaeude.csv", header = TRUE, sep = ";", dec = ",", fill = TRUE) # 11.12.2023 -> updated file path

#Read CSV DB-Verbrauchsdaten in order to check how many full building data sets for DIBS are available, that also have the measured consumption available für Check, 
DB_TE_Verbrauch = read.csv("S:/GitHub_Data_Access/data_BE_TE_TEK_Verbrauch.csv", header = TRUE, sep = ";", dec = ".", fill = TRUE) # 11.12.2023 -> updated file path



#............................................................................
#
# Clean up Data ####
#
#............................................................................

# Falls scr_gebaeude_id komisch benannt.
colname <- colnames(DB_BE)
colname[1] <- "scr_gebaeude_id"
colnames(DB_BE) <- colname


#DB_BE auf die beschriebenen Zeilen (erste Spalte mit GebäudeID Eintrag) reduzieren
DB_BE <- filter(DB_BE, scr_gebaeude_id != "") 

      #Auszug Summe Hochrechungsfaktorn bevor DB_BE weiter reduziert wird
      HRF <- DB_BE$HRF                                   # HRF der DB_BE!
      Sum_HRF_BE <- sum(HRF)

#DB_BE auf EnEV-Relevante Gebäude Reduzieren
DB_BE <- filter(DB_BE, enev_rel_nwg_be != "0") # ENEV relevante Gebäude = 1 bzw. != 0   enev_rel_nwg_be sollten 5107 Gebäude sein

# Gebäude (n=4) mit extrem hohen Hochrechnungsfaktoren entfernen (-555)
DB_BE <- filter(DB_BE, hk_geb != "-555")


#............................................................................
#
# Screening PLZ der DB_BE zuweisen #####
#
#............................................................................

rows_DB_BE <- nrow(DB_BE)

# i = 2
for(i in 1:rows_DB_BE){
   DB_BE$plz[i] <- DB_SCR$plz[DB_SCR$scr_gebaeude_id == DB_BE$scr_gebaeude_id[i]]
}

# # Analyse PLZ
# table(DB_BE$plz)
# min(DB_BE$plz)
plz <- DB_BE$plz



#............................................................................
#
# Ermitteln Anzahl Voller Datensätze in BE (ohne Missings = -7)#####
#
#............................................................................

# Calling Determination_of_full_Datasets.R
source("C:/Users/Julian/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/Determination-of-full-datasets/Determination_of_full_Datasets.R")

#............................................................................
#
#Variablen welche zu imputieren sind auf Missings prüfen####
#Wenn es keine Missings gibt, muss nicht imputierte werden
#
#............................................................................


      #----------------------------------------------------------------
      # Hochrechnungsfaktoren
      table(DB_BE$HRF)
      HRF <- DB_BE$HRF # HRF der DB_BE EnEV/GEG relevant!

      #----------------------------------------------------------------


      #----------------------------------------------------------------
      #-----------------------------
      #Per Definition keine Missings und damit keine Imputationen
      table(DB_BE$scr_gebaeude_id)
      scr_gebaeude_id <- DB_BE$scr_gebaeude_id
      min(scr_gebaeude_id) # No Missings
      
      #-----------------------------
      #Keine Missings und daher keine Imputation
      
          # HILFSVARIABLE
          table(DB_BE$geb_flaeche) # net_room_area_base # -555 # 
          geb_flaeche <- DB_BE$geb_flaeche
          min(geb_flaeche) # No Missings
      
      table(DB_BE$geb_f_hoehe_mittel_iwu) # building_height # -555 # 
      geb_f_hoehe_mittel_iwu <- DB_BE$geb_f_hoehe_mittel_iwu
      min(geb_f_hoehe_mittel_iwu) # No Missings
      
          # HILFSVARIABLE
          table(DB_BE$f_ant_beheizt) # Flächenanteil der beheizten Fläche # -555, -66 und -8 # 
          f_ant_beheizt <- DB_BE$f_ant_beheizt
          min(f_ant_beheizt) # No Missing
      
          # HILFSVARIABLE
          table(DB_BE$bak) # Baualter # -555 und -66 # 
          bak <- DB_BE$bak
          min(bak) # No Missing
      
      table(DB_BE$bak_grob) # Baualter GROB # -555 und -66 # 
      bak_grob <- DB_BE$bak_grob
      min(bak_grob) #No Missing
      
      table(DB_BE$hk_geb) # hk_geb # -555 und -66
      hk_geb <- DB_BE$hk_geb
      min(hk_geb) # No Missing
      
      table(DB_BE$uk_geb) # uk_geb # -555 und -66
      # Excel -> csv -> R Import Formatfehler korrigieren: In Excel werde die uk_geb letzte Nullen einfach gelöscht. Diese sind aber wichtig für die eindeutige Zuweisung!
      DB_BE$uk_geb[DB_BE$uk_geb==1] <- "1.00"
      DB_BE$uk_geb[DB_BE$uk_geb==2] <- "2.00"
      DB_BE$uk_geb[DB_BE$uk_geb==3] <- "3.00"
      DB_BE$uk_geb[DB_BE$uk_geb==3.1] <- "3.10"
      DB_BE$uk_geb[DB_BE$uk_geb==4] <- "4.00"
      DB_BE$uk_geb[DB_BE$uk_geb==4.1] <- "4.10"
      DB_BE$uk_geb[DB_BE$uk_geb==5] <- "5.00"
      DB_BE$uk_geb[DB_BE$uk_geb==6] <- "6.00"
      DB_BE$uk_geb[DB_BE$uk_geb==6.1] <- "6.10"
      DB_BE$uk_geb[DB_BE$uk_geb==7] <- "7.00"
      DB_BE$uk_geb[DB_BE$uk_geb==8] <- "8.00"
      DB_BE$uk_geb[DB_BE$uk_geb==8.1] <- "8.10"
      DB_BE$uk_geb[DB_BE$uk_geb==9] <- "9.00"
      DB_BE$uk_geb[DB_BE$uk_geb==10] <- "10.00"
      uk_geb <- as.character(DB_BE$uk_geb)
      min(uk_geb) # No Missing
      
          # HILFSVARIABLE
          # Gebäudeabicklungen der Fassaden
          
          table(DB_BE$geb_f_laenge_n) # geb_f_laenge_n # -555 und -999
          geb_f_laenge_n <- DB_BE$geb_f_laenge_n
          min(geb_f_laenge_n) # No Missing
          
          table(DB_BE$geb_f_laenge_o) # geb_f_laenge_o # -555 und -999
          geb_f_laenge_o <- DB_BE$geb_f_laenge_o
          min(geb_f_laenge_o) # No Missing      
          
          table(DB_BE$geb_f_laenge_s) # geb_f_laenge_s # -555 und -999
          geb_f_laenge_s <- DB_BE$geb_f_laenge_s
          min(geb_f_laenge_s) # No Missing           
          
          table(DB_BE$geb_f_laenge_w) # geb_f_laenge_w # -555 und -999
          geb_f_laenge_w <- DB_BE$geb_f_laenge_w
          min(geb_f_laenge_w) # No Missing      
          
      DB_BE$geb_f_umfang <- geb_f_laenge_n + geb_f_laenge_o + geb_f_laenge_s + geb_f_laenge_w

      
      # Gebäudefassadenflächen
      
      table(DB_BE$geb_f_flaeche_n_iwu) # geb_f_flaeche_n_iwu # -555 und -999
      geb_f_flaeche_n_iwu <- DB_BE$geb_f_flaeche_n_iwu
      min(geb_f_flaeche_n_iwu) # No Missing      
      
      table(DB_BE$geb_f_flaeche_o_iwu) # geb_f_flaeche_o # -555 und -999
      geb_f_flaeche_o_iwu <- DB_BE$geb_f_flaeche_o_iwu
      min(geb_f_flaeche_o_iwu) # No Missing      
      
      table(DB_BE$geb_f_flaeche_s_iwu) # geb_f_flaeche_s # -555 und -999
      geb_f_flaeche_s_iwu <- DB_BE$geb_f_flaeche_s_iwu
      min(geb_f_flaeche_s_iwu) # No Missing      
      
      table(DB_BE$geb_f_flaeche_w_iwu) # geb_f_flaeche_w # -555 und -999
      geb_f_flaeche_w_iwu <- DB_BE$geb_f_flaeche_w_iwu
      min(geb_f_flaeche_w_iwu) # No Missing    
      
      table(DB_BE$f_fl_geo_iwu) # geb_f_flaeche_SUMME # -555 und -999
      f_fl_geo_iwu <- DB_BE$f_fl_geo_iwu
      min(f_fl_geo_iwu) # No Missing          
      
      
      
      
      table(DB_BE$qd1) # Fensterflächenanteil qd1 primäre Bauweise# 
      qd1 <- DB_BE$qd1
      min(qd1) # No Missing
          # Decodiert qd1 
          table(DB_BE$fen_flant_1) # Fensterflächenanteil qd1 primäre Bauweise# 
          fen_flant_1 <- DB_BE$fen_flant_1
          min(fen_flant_1) # No Missing
      
      
      # Alle hier gelisteten Variablen ohne Missings werden bereits in einen temporären dataframe gepackt, 
      # damit langsam ein dataframe Datensatz zum späteren speichern aufgebaut werden kann.
      # Das ist notwendig, da in den einzelnen Imputationen einige der obigen Variablen noch einmal
      # neu angeleget und ggf. verändert (andere Filterung) und in das Environment geladen werden. 
      # Damit würde es beim abschließenden speichern sonst zu Problemen kommen.
      
      #Initierung DB_DIBS
      DB_DIBS <- as.data.frame(cbind(scr_gebaeude_id, geb_flaeche, geb_f_hoehe_mittel_iwu, f_ant_beheizt, bak, bak_grob, hk_geb, uk_geb, geb_f_laenge_n, geb_f_laenge_o, geb_f_laenge_s, geb_f_laenge_w, geb_f_flaeche_n_iwu, geb_f_flaeche_o_iwu, geb_f_flaeche_s_iwu, geb_f_flaeche_w_iwu, f_fl_geo_iwu, qd1))

      # PLZ aus Screening zu DB_DIBS hinzufügen
      DB_DIBS <- as.data.frame(cbind(DB_DIBS, plz))

#----------------------------------------------------------------
#Prüfung manuall über table # DIBS-Variable # Missings # Imputationscode 

    #Missings Bedeutungen:
    # -999
    # -555 extrem hohe Hochrechnungsfaktoren, werden gelöscht, da diese ansonsten zu starken Verzerrungen der Bestandsergebnissen führen würden.
    # -55 durch Fragenänderung während der Interviewphase entstanden, nicht in Variablen Version abgefragt... sollte durch Erstellung einer Editierten Variablen elinminiert worden sein 
    # -66 Fehlt aufgrund kurz oder ultrakurz Interview (z.B. bei nicht GEG relevanten NWG)
    # -88 Trifft nicht zu (TNZ) da falsche Filterführung in vorheriger Fragebogen Version, wie -7 zu behandeln
    # -8 TNZ
    # -7 Missing

    # Nur -7 (Missing) wird imputiert, -8 (TNZ) nicht. Es kann sein, dass -8 aber zu 
    # DIBS verständlichem Code z.B. 0 geändert werden muss (das ist für jede Variable einzeln zu prüfen).

    # EnEV/GEG relevante Gebäude sollte nur -8 und -7 enthalten 
    # (die unten aufgezählten Missings zu den "table()" sind für alle Gebäude auch nicht EnEV)
        
    # Gewichtung und Normierung:
    # In der Imputation werden Modelle für die Teilgruppe der GEG-Relevanten (ohne die extrem hohen Hochrechnungsfaktoren (n=4)) entwickelt. 
    # D.h. es wird für die Gewichtung die NWG-GEGrelevante-Anzahl (N = 1,797,241) zur Normierung herangezogen, da der Fokus  
    # die Verteilung in dieser Gruppe ist und da auf diese Gruppe geschätzt werden soll.
    # Bzw. wenn eine Teilmenge (ohne Missings) herangezogen wirde, z.B. für ein Würfel oder ein Regressionsmodel, 
    # dann wird auf die Teilmenge (ohne Missing) normieren, da nur so die Probabilities zusammen 100% ergeben und 
    # damit die Teilmenge korrekt abbilden. 
      
      
      
      
      
#----------------------------------------------------------------
# Imputationsansätze:
# Für die Imputation der Breitenerhebungsdaten, zur Bereitstellung für den DIBS, wurden 6 Imputationstechniken angewandt.
      
      # Imputationstechnik 1 - Imputationstechnik 1: Calculation based on physical or known statistical relations [a = b + c] --- metric
      # physical e.g. ebf; statistical e.g. nrf_2
      
      # Imputationstechnik 2 - Imputationstechnik 2: Linear (singular and multiple) weighted regression estimation [lm(x~a:b, weights = Y)] --- metric
            # Beschreibung
            # Begründung
            # Anwendung an Variablen (am Schluß ergänzen)

      # Imputationstechnik 3 - Imputationstechnik 3: Individual - quasi-binominal (quasi-logistic) - regression based - dice - weighted [gml() + dice] --- non-metric
      
      # Imputationstechnik 4 - Imputationstechnik 4: Individual - multinominal - binominal (logistic) - regression based - dice - weighted --- non-metric

      # Imputationstechnik 5 - Imputationstechnik 5: Cleaning up -8 if non existent --- metric

      # Imputationstechnik 6 - Imputationstechnik 6: Assumption of values based on related variable ---  metric and non-metric
      # sub-terrain room height equals above ground room heights
      
      
      #Tested techniques, but not finally used:
               # I2: REPLACED BY Imputationstechnik 4: Weighted dice based on single main building attribute --- non-metric 
      
               # I4: TO BE REPLACED BY Imputationstechnik 2 - Average value based on main variable(s) --- metric
                  #NUR WENN NOCH IMMER MISSINGS IN DER VARIABLE n_ug VORHANDEN SIND!!!!!

      
      
      

      

      
      #---------------------
      #---------------------
      #
      # Korrekturen vereinfachte Annahmen aus Forschungsdatenbank (-7 zu -8 Problem)
      #
      # Die folgenden Variablen wurden im Rahmen der finalen Erstellung der Forschungsdatenbank von HC abgeändert:
      # n_ug, d_fl_be und f_ant_belueftet.
      # 
      #
      # Die vorherigen -7 wurden hierbei zu -8 geändert, um konsistent mit dem Fragebogen zu bleiben. 
      # Für die Simulation ist die Aussage -7 (weiß nicht) aber in der Regel anders zu werten als -8 (TNZ). 
                  
      # n_ug -> bgf, nrf_2 und ebf
      # Nicht geändert, da die geschätzten Flächen auf Basis eines geschätzten n_ug wahrscheinlich genauso falsch sind, wie die Annhame n_ug = 0
      # Um konsistent zu Forschungsdatenbank zu bleiben behalten wir die Annahme n_ug = -7 = -8 = 0 für die 25 Fälle bei.
      # table(DB_BE$q65b0) (Ursprungsvariable von n_ug)
      
      # f_ant_belueftet
      # Daher werden im Folgenden f_ant_belueftet vor der imputation korrigiert. Dabei wird in den Fällen 
      # von qh1 (Ursprungsvariable von f_ant_belueftet) = -7 f_ant_belueftet (-8) mit -7 überschrieben und anschließend imputiert.
      # table(DB_BE$qh1)
      # f_ant_belueftet wird auf Basis von qh1 (Ursprungsvariable von f_ant_belueftet) korrigiert:
      # Korrekturen vereinfachte Annahmen aus Forschungsdatenbank (-7 zu -8 Problem)
      DB_BE$f_ant_belueftet[DB_BE$qh1==(-7)] <- -7
      
      # d_fl_be
      # Auch bei d_fl_be wurde angenommen, dass die dachform_be -7 gleich 1 (Steildach) ist. Damit weißen die 31 Fälle von 
      # dachform_be = -7 nun in d_fl_be nun Werte auf Basis der Annahme dachform_be = 1 auf.
      # Um konsistent zu Forschungsdatenbank zu bleiben behalten wir die Annahme bei.
      # table(DB_BE$dachform_be)
      # table(DB_BE$d_fl_be)
      
      #---------------------
      #---------------------
      
            
# DB_BE wird in den externen Imputationskripts bereits geupdatet. d.h. hier sind nach jeder Variablenimputation auch die 
# Imputierten Variablen vorhanden. Dadurch können nachfolgende Imputation auf Basis von imputieren Variablen erfolgen!

      
    # B.1 _HILFSVARIABLE - Imputationstechnik 2 # NOT USED with final Dataset
    #---------------------
    # 23.07.2021: !!!! ACHTUNG NICHT MEHR -7 mit neustem Datensatz... Siehe oben: Korrekturen vereinfachte Annahmen aus Forschungsdatenbank (-7 zu -8 Problem)
    table(DB_BE$bgf) # gross_room_area_base # -555, -66 und -7 #
    bgf <- DB_BE$bgf
    min(bgf) # -7
    source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_bgf_imputation.R") # lm(bgf ~ as.factor(hk_geb):geb_f_hoehe_mittel_iwu:geb_flaeche, weights = (HRF/Sum_HRF_BE)*N_Clean)
    DB_DIBS <- as.data.frame(cbind(DB_DIBS, bgf)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
    # Due to full final dataset of DataNWG-BRE not used for imputation. However, the Regression might be of interest anyway.
    DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS, bgf_imputated))


# B.2 - Imputationstechnik 4
#---------------------
table(DB_BE$lampenart) # überwiegenden Lampenart (Benennung prüfen) # -555, -66 und -7 # 
lampenart <- DB_BE$lampenart
min(lampenart) # -7
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_lampenart_imputation.R") # Idividueller-mulitnominaler-binominal-glm()Würfel auf Basis von multinom(lampenart ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000)
DB_DIBS <- as.data.frame(cbind(DB_DIBS, lampenart)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, lampenart, lampenart_imputated))

# B.3 - Imputationstechnik 1 # NOT USED with final Dataset
#---------------------
# 23.07.2021: !!!! ACHTUNG NICHT MEHR -7 mit neuem Datensatz... WAS ist passiert? Siehe oben: Korrekturen vereinfachte Annahmen aus Forschungsdatenbank (-7 zu -8 Problem)
table(DB_BE$nrf_2) # net_room_area auf Basis Geodaten # -555, -66 und -7 #
nrf_2 <- DB_BE$nrf_2
min(nrf_2) # -7
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_nrf_2_imputation.R") # nrf_2 <- bgf * f_flaeche
DB_DIBS <- as.data.frame(cbind(DB_DIBS, nrf_2)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, nrf_2))

# B.4 - Imputationstechnik 1 # NOT USED with final Dataset
#---------------------
# 23.07.2021: !!!! ACHTUNG NICHT MEHR -7 mit neuem Datensatz... WAS ist passiert? Siehe oben: Korrekturen vereinfachte Annahmen aus Forschungsdatenbank (-7 zu -8 Problem)
table(DB_BE$ebf) # energy_ref_area # -555, -66 und -7 #
ebf <- DB_BE$ebf
min(ebf) # -7
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_ebf_imputation.R") # ebf <- nrf_2 * f_ant_beheizt
DB_DIBS <- as.data.frame(cbind(DB_DIBS, ebf)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, ebf))

# B.5 - Imputationstechnik 2
#---------------------
table(DB_BE$q25_1) # max_occupancy # -555, -66 und -7 # BE_q25_imputation.R
q25_1 <- DB_BE$q25_1
min(q25_1) # -7
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_q25_1_imputation.R") # lm(q25_1 ~ as.factor(hk_geb) : as.factor(bak_grob) : bgf, weights = (HRF/Sum_HRF_BE)*N_Clean)      und nicht mehr(UND Mittelwert nach hk_geb)
DB_DIBS <- as.data.frame(cbind(DB_DIBS, q25_1)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, q25_1, q25_1_imputated))

# # B.X - Imputationstechnik 2
# #---------------------
# table(DB_BE$fen_flantges) # max_occupancy # -555, -66 und -7 # BE_q25_imputation.R
# q25_1 <- DB_BE$fen_flantges
# min(fen_flantges) # -7


    # B.6 HILFSVARIABLE - Imputationstechnik 2
    #---------------------
    # window_area_north
    # window_area_south
    # window_area_east
    # window_area_west
    # zusammengefasst in
    table(DB_BE$fen_fl)  # window_area # -555, -66 und -7 # 
    fen_fl <- DB_BE$fen_fl
    min(fen_fl) # -7
    source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_fen_fl_imputation.R") # lm(fen_fl ~ as.factor(hk_geb) : as.factor(bak_grob) : fen_flant_1 + geb_f_hoehe_mittel_iwu + nrf_2, weights = (HRF/Sum_HRF_BE)*N_Clean)
    DB_DIBS <- as.data.frame(cbind(DB_DIBS, fen_fl)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
    DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, fen_fl, fen_fl_imputated))

# B.7 - Imputationstechnik 1
#---------------------
table(DB_BE$aw_fl) # wall_area_og # -555, -66 und -7 # 
aw_fl <- DB_BE$aw_fl
min(aw_fl) # -7
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_aw_fl_imputation.R") # Fassendfläche(f_fl_geo_iwu) - Fensterfläche(fen_fl) = aw_fl
DB_DIBS <- as.data.frame(cbind(DB_DIBS, aw_fl)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, aw_fl, aw_fl_imputated))

    # B.8 - Imputationstechnik 5              ALT: I4
    #---------------------
    # 23.07.2021: !!!! ACHTUNG NICHT MEHR -7 mit neuem Datensatz... WAS ist passiert? Siehe oben: Korrekturen vereinfachte Annahmen aus Forschungsdatenbank (-7 zu -8 Problem)
    table(DB_BE$n_ug) # Anzahl der Untergeschosse (Bennenung prüfen) # -555, -66, -8 und -7 # 
    n_ug <- DB_BE$n_ug
    min(n_ug) # -8 und -7
    # -8 = 0 d.h. 0 Untergeschosse 
    source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_n_ug_imputation.R") #        ALT: Mittelwert nach bak und hk_geb
    DB_DIBS <- as.data.frame(cbind(DB_DIBS, n_ug)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
    # n_ug ist vollständig, nur -8 zu 0 Korrektur! Die Modalwert Imputationen wurden vorher erstellt. 
    # Diese sind unten weiter zur Info noch gelistet, werden aber bei vollem Datensatz nicht umgesetzt.
    DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, n_ug))

    # B.9 HILFSVARIABLE - Imputationstechnik 3
    #---------------------
    table(DB_BE$dachform_be) # roof_form # -555, -66 und -7 # 
    dachform_be <- DB_BE$dachform_be
    min(dachform_be) # -7
    source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_dachform_be_imputation.R") # Idividueller-binominal-glm()Würfel auf Basis von glm(dachform_be ~ as.factor(uk_geb), family=binomial)
    DB_DIBS <- as.data.frame(cbind(DB_DIBS, dachform_be)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
    DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, dachform_be, dachform_be_imputated))

    # B.10 - Imputationstechnik 1 # NOT USED with final Dataset
    #---------------------
    # 23.07.2021: !!!! ACHTUNG NICHT MEHR -7 mit neuem Datensatz... WAS ist passiert? Siehe oben: Korrekturen vereinfachte Annahmen aus Forschungsdatenbank (-7 zu -8 Problem)
    table(DB_BE$d_fl_be) # roof_area # -555, -66 und -7 # 
    d_fl_be <- DB_BE$d_fl_be
    min(d_fl_be) # -7
    source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_d_fl_be_imputation.R") # Berechnet nach Flachdach und Steildach
    DB_DIBS <- as.data.frame(cbind(DB_DIBS, d_fl_be)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
    DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, d_fl_be))

# B.11 - Imputationstechnik 2
#---------------------
table(DB_BE$n_og) # Anzahl OGs # -555, -66 und -7 #
n_og <- DB_BE$n_og
min(n_og) # -7
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_n_og_imputation.R") # lm(n_og ~ as.factor(hk_geb) : geb_f_hoehe_mittel_iwu : as.factor(dachform_be), weights = (HRF/Sum_HRF_BE)*N_Clean)
DB_DIBS <- as.data.frame(cbind(DB_DIBS, n_og)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, n_og, n_og_imputated))

    # B.12 HILFSVARIABLE - Imputationstechnik 2
    #---------------------
    table(DB_BE$q66a_1) # room_height # -555, -66 und -7 #
    q66a_1 <- DB_BE$q66a_1
    min(q66a_1) # -7
    source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_q66a_1_imputation.R") # lm(q66a_1 ~ as.factor(hk_geb) * geb_f_hoehe_mittel_iwu, weights = (HRF/Sum_HRF_BE)*N_Clean)
    DB_DIBS <- as.data.frame(cbind(DB_DIBS, q66a_1)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
    DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, q66a_1, q66a_1_imputated))
    
    # B.13 - HILFSVARIABLE Imputationstechnik 6
    #---------------------
    table(DB_BE$unter_hoehe) # wall_area_ug Summe der Außenwandflächen unterirdisch # -555, -66, -8 und -7 #
    unter_hoehe <- DB_BE$unter_hoehe
    min(unter_hoehe) # -8 # -7
    source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_unter_hoehe_imputation.R") # Zuweisung auf Basis bereits imputierter Werte von q66a_1
    DB_DIBS <- as.data.frame(cbind(DB_DIBS, unter_hoehe)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
    DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, unter_hoehe, unter_hoehe_imputated))

# B.14 - Imputationstechnik 1
#---------------------
table(DB_BE$unteraw_fl) # wall_area_ug Summe der Außenwandflächen unterirdisch # -555, -66, -8 und -7 #
unteraw_fl <- DB_BE$unteraw_fl
min(unteraw_fl) # -8 # -7
# -8 = 0, d.h. es gibt keine unterirdischen Geschosse und daher auch keine unterirdische Außenwandfläche
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_unteraw_fl_imputation.R") # unteraw_fl = n_ug * ((unter_hoehe)+0.3) * geb_f_umfang
DB_DIBS <- as.data.frame(cbind(DB_DIBS, unteraw_fl)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, unteraw_fl, unteraw_fl_imputated))

# B.15 - Imputationstechnik 4
#---------------------
table(DB_BE$qf1) # überwiegende Beleuchtungsart (Benennung prüfen) # -555, -66 und -7 # 
qf1 <- DB_BE$qf1
min(qf1) # -7
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_qf1_imputation.R") # Idividueller-mulitnominaler-binominal-glm()Würfel auf Basis von multinom(qf1 ~ as.factor(hk_geb) * q66a_1, data = DB_BE_clean, weights = (HRF/Sum_HRF_BE)*N_Clean)
DB_DIBS <- as.data.frame(cbind(DB_DIBS, qf1)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, qf1, qf1_imputated))

# B.16 - Imputationstechnik 5
#---------------------
# table(DB_BE$fen_glasart_1) # überwiegende Art der Verglasung (Bennenung prüfen) # -555, -66 und -8 #
# fen_glasart_1 <- DB_BE$fen_glasart_1
# min(fen_glasart_1) # -8
# source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_fen_glasart_1_imputation.R") # Da keine -7 vorkommen wird nur -8 zu 0 geändert.
# DB_DIBS <- as.data.frame(cbind(DB_DIBS, fen_glasart_1)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
table(DB_BE$glasart_1) # überwiegende Art der Verglasung (Bennenung prüfen) # -555, -66 und -8 # 
glasart_1 <- DB_BE$glasart_1
min(glasart_1) # -8
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_glasart_1_imputation.R") # Da keine -7 vorkommen wird nur -8 zu 0 geändert.
DB_DIBS <- as.data.frame(cbind(DB_DIBS, glasart_1)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, glasart_1))

# B.18 - Imputationstechnik 4
#---------------------
table(DB_BE$aw_konstr_1) # Fassadenbauweise des Gebäudes (Bennenung prüfen) # -555, -66 und -7 #  1 massiv, 2 leicht, 3 Fassadensystem
aw_konstr_1 <- DB_BE$aw_konstr_1
min(aw_konstr_1) # -7
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_aw_konstr_1_imputation.R") # Idividueller-mulitnominaler-binominal-glm()Würfel auf Basis von multinom(aw_konstr_1 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE)*N_Clean)
DB_DIBS <- as.data.frame(cbind(DB_DIBS, aw_konstr_1)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, aw_konstr_1, aw_konstr_1_imputated))

    # B.19 HILFSVARIABLE - Imputationstechnik 2
    #---------------------
    table(DB_BE$aw_daemm_staerke_1) # Außenwanddämmstärke des überwiegenden Außenwandaufbaus
    aw_daemm_staerke_1 <- DB_BE$aw_daemm_staerke_1
    min(DB_BE$aw_daemm_staerke_1) # -7
    source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_aw_daemm_staerke_1_imputation.R") # lm(aw_daemm_staerke_1 ~ as.factor(hk_geb) * as.factor(bak), weights = (HRF/Sum_HRF_BE)*N_Clean)
    DB_DIBS <- as.data.frame(cbind(DB_DIBS, aw_daemm_staerke_1)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
    # -8 -> keine Dämmung zu 0
    DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, aw_daemm_staerke_1, aw_daemm_staerke_1_imputated))
    
    # B.20 HILFSVARIABLE - Imputationstechnik 2
    #---------------------
    table(DB_BE$aw_flantgedges) # Außenwandflächeanteil gedämmt gesammt
    aw_flantgedges <- DB_BE$aw_flantgedges
    min(aw_flantgedges) # -7
    source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_aw_flantgedges_imputation.R") # lm(aw_daemm_staerke_1 ~ as.factor(hk_geb) * as.factor(bak), weights = (HRF/Sum_HRF_BE)*N_Clean)
    DB_DIBS <- as.data.frame(cbind(DB_DIBS, aw_flantgedges)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
    # -8 -> keine Dämmung zu 0
    DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, aw_flantgedges, aw_flantgedges_imputated))

# B.21 - Imputationstechnik 2
#---------------------
table(DB_BE$u_aw) # u_walls # -555, -66 und -7 # 
u_aw <- DB_BE$u_aw
min(u_aw) # -7
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_u_aw_imputation.R") # lm(u_aw ~ as.factor(bak_grob) * aw_flantgedges * aw_daemm_staerke_1 * as.factor(aw_konstr_1), weights = (HRF/Sum_HRF_BE)*N_Clean) #  Predicted Valus <0 are replaced by the min value predicted >0
DB_DIBS <- as.data.frame(cbind(DB_DIBS, u_aw)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, u_aw, u_aw_imputated))

# B.17 - Imputationstechnik 2
#---------------------
table(DB_BE$u_fen) # u_windows # -555, -66, -8 und -7 # 
u_fen <- DB_BE$u_fen
min(u_fen) # -8 und -7
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_u_fen_imputation.R") # lm(u_fen ~ as.factor(hk_geb) + as.factor(glasart_1) + as.factor(bak), weights = (HRF/Sum_HRF_BE)*N_Clean)
DB_DIBS <- as.data.frame(cbind(DB_DIBS, u_fen)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, u_fen, u_fen_imputated))
# -8 -> keine Fenster. Die Fensterfläche fen_fl = 0 siehe test in nächste Zeile.
# DB_BE$fen_fl[DB_BE$u_fen==-8]
# !
# Im DIBS sollte von daher die -8 keine Probleme machen, da mit 0 Multipliziert. 
# Zur sicherheit werden die u_fen = -8 aber mit u_aw (U-Wert der Außenwand) überschrieben
# !

    # B.22 HILFSVARIABLE - Imputationstechnik 2
    #---------------------
    # Not used anymore
    table(DB_BE$d_fl_wueoa) # wärmeübertragende Fläche des oberen Gebäudeabschlusses
    d_fl_wueoa <- DB_BE$d_fl_wueoa
    min(d_fl_wueoa) # -7
    source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_d_fl_wueoa_imputation.R") # lm(d_fl_wueoa ~ as.factor(hk_geb) * as.factor(dachform_be) * geb_flaeche, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)
    DB_DIBS <- as.data.frame(cbind(DB_DIBS, d_fl_wueoa)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
    DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, d_fl_wueoa, d_fl_wueoa_imputated))


            # B.23 - Imputationstechnik 4
            #---------------------
            table(DB_BE$qh1) # Gibt es zentrale raumlufttechnische Anlagen zur mechanischen Lüftung von Nutzungsflächen im Gebäude? # -555, -66 und -7 #
            # 1 Nein, Fensterlüftung
            # 2 Nein, nur dezentrale Anlage(n) vorhanden
            # 3 Ja, zentrale Anlage(n) vorhanden
            qh1 <- DB_BE$qh1
            min(qh1) # -7
            source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_qh1_imputation.R") # Idividueller-mulitnominaler-binominal-glm()Würfel auf Basis von multinom(qh1 ~ as.factor(hk_geb) : as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)
            DB_DIBS <- as.data.frame(cbind(DB_DIBS, qh1)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
            DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, qh1, qh1_imputated))
            
            # Damit die Zuweisung von f_ant_belueftet auch zu qh3_1 bis qh3_9 passt bassieren diese auf der Zuweisung von qh1!
            
            # B.24 HILFSVARIABLE - Imputationstechnik 2
            #---------------------
            # Nicht in Benutzung
            # 23.07.2021: !!!! ACHTUNG NICHT MEHR -7 mit neuem Datensatz... Wurde oben korrigiert, da hier -7 nicht gleich -8 ist
            table(DB_BE$f_ant_belueftet) # Anteil Nutzungsfläche zentral mechanisch belüftet (Bennenung prüfen) # -555, -66, -8 und -7 # 
            f_ant_belueftet <- DB_BE$f_ant_belueftet
            min(f_ant_belueftet) # -8 und -7
            source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_f_ant_belueftet_imputation.R") # Wenn qh1 RLT ist vorhanden qh1==3, wenn ja lm(f_ant_belueftet ~ as.factor(bak) : as.factor(uk_geb), weights = (HRF/Sum_HRF_BE_clean)*N_Clean)
            DB_DIBS <- as.data.frame(cbind(DB_DIBS, f_ant_belueftet)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
            # -8 = 0 d.h. 0% mechanisch belüftete Fläche, da in diesem Fall keine zentrale mechanische Belüftung vorhanden
            DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, f_ant_belueftet))
      
               #-----------------
               # qh3_1 bis qh3_9
               # -7 nur aus Teilmenge RLT vorhanden qH1 = 3 (Ja, zentrale Anlage(n) vorhanden) imputieren! Auf basis von qh1 (siehe oben). 
               # Nur Anlagen welche qh1 = 3 sind bekommen auch hier Eigenschaften zugewiesen!
               #
               # Fälle identifizieren, welche qh1==3 und qh3_1 bis qh3_9==-8. Diese Anlagen müssen zu qh3_1 bis qh3_9==-7 abgeändert werden, 
               # da es sich bei diesen Gebäude um diejenigen handelt, welche unter qh1 eine 3 zugewürfelt bekommen haben. In qh3_1 bis qh3_9==-8
               # waren diese, da vorher qh1==-7 war konsequent als -8 eingetragen. In der qh3_1 bis qh3_9==-7 Imputation müssen diese nun aber
               # Berücksichtig werden, um Konsistenz inerhalb eines Gebäudedatensatzes zu gewährleisten.
               
               
               # B.25 - Imputationstechnik 3
               #---------------------
               table(DB_BE$qh3_1) # überwiegende Funktionen der RLTs Wärmerückgewinnung (Bennenung prüfen) # -555, -66, -8 und -7 # 
               qh3_1 <- DB_BE$qh3_1
               min(qh3_1) # -8 und -7
               source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_qh3_1_imputation.R") # Wenn qh1 RLT ist vorhanden qh1==3, wenn ja Idividueller-binominal-glm()Würfel auf Basis von glm(qh3_1 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, family=binomial)
               DB_DIBS <- as.data.frame(cbind(DB_DIBS, qh3_1)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
               # -8 bleibt -8, Es gibt keine RLT!
               DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, qh3_1, qh3_1_imputated))
               
               
                   #......................................
                   # qh3_2 bis qh3_8 not used in DIBS BE
                   # |
                   # V
                   
                   # B.26 - Imputationstechnik 3
                   #---------------------
                   table(DB_BE$qh3_2) # überwiegende Funktionen der RLTs Heizen (zusätzlich zur Wärmerückgewinnung) (Bennenung prüfen) # -555, -66, -8 und -7 # 
                   qh3_2 <- DB_BE$qh3_2
                   min(qh3_2) #  -8 und -7
                   source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_qh3_2_imputation.R") # Wenn qh1 RLT ist vorhanden qh1==3, wenn ja Idividueller-binominal-glm()Würfel auf Basis von glm(qh3_2 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, family=binomial)
                   DB_DIBS <- as.data.frame(cbind(DB_DIBS, qh3_2)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
                   # -8 bleibt -8, Es gibt keine RLT!
                   DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, qh3_2, qh3_2_imputated))
                   
                   # B.27 - Imputationstechnik 3
                   #---------------------
                   table(DB_BE$qh3_3) # überwiegende Funktionen der RLTs Kühlen (Bennenung prüfen) # -555, -66, -8 und -7 # 
                   qh3_3 <- DB_BE$qh3_3
                   min(qh3_3) #  -8 und -7
                   source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_qh3_3_imputation.R") # Wenn qh1 RLT ist vorhanden qh1==3, wenn ja Idividueller-binominal-glm()Würfel auf Basis von glm(qh3_3 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, family=binomial)
                   DB_DIBS <- as.data.frame(cbind(DB_DIBS, qh3_3)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
                   # -8 bleibt -8, Es gibt keine RLT! 
                   DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, qh3_3, qh3_3_imputated))
                   
                   # B.28 - Imputationstechnik 3
                   #---------------------
                   table(DB_BE$qh3_4) # überwiegende Funktionen der RLTs Befeuchten (Bennenung prüfen) # -555, -66, -8 und -7 # 
                   qh3_4 <- DB_BE$qh3_4
                   min(qh3_4) #  -8 und -7
                   source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_qh3_4_imputation.R") # Wenn qh1 RLT ist vorhanden qh1==3, wenn ja Idividueller-binominal-glm()Würfel auf Basis von glm(qh3_4 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, family=binomial)
                   DB_DIBS <- as.data.frame(cbind(DB_DIBS, qh3_4)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
                   # -8 bleibt -8, Es gibt keine RLT! 
                   DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, qh3_4, qh3_4_imputated))
                   
                   # B.29 - Imputationstechnik 3
                   #---------------------
                   table(DB_BE$qh3_5) # überwiegende Funktionen der RLTs Entfeuchten (Bennenung prüfen) # -555, -66, -8 und -7 # 
                   qh3_5 <- DB_BE$qh3_5
                   min(qh3_5) #  -8 und -7
                   source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_qh3_5_imputation.R") # Wenn qh1 RLT ist vorhanden qh1==3, wenn ja Idividueller-binominal-glm()Würfel auf Basis von glm(qh3_5 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, family=binomial)
                   DB_DIBS <- as.data.frame(cbind(DB_DIBS, qh3_5)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
                   # -8 bleibt -8, Es gibt keine RLT! 
                   DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, qh3_5, qh3_5_imputated))
                   
                   # B.30 - Imputationstechnik 3
                   #---------------------
                   table(DB_BE$qh3_6) # überwiegende Funktionen der RLTs Abluft (Bennenung prüfen) # -555, -66, -8 und -7 # 
                   qh3_6 <- DB_BE$qh3_6
                   min(qh3_6) #  -8 und -7
                   source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_qh3_6_imputation.R") # Wenn qh1 RLT ist vorhanden qh1==3, wenn ja Idividueller-binominal-glm()Würfel auf Basis von glm(qh3_6 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, family=binomial)
                   DB_DIBS <- as.data.frame(cbind(DB_DIBS, qh3_6)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
                   # -8 bleibt -8, Es gibt keine RLT! 
                   DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, qh3_6, qh3_6_imputated))
                   
                   # B.31 - Imputationstechnik 3
                   #---------------------
                   table(DB_BE$qh3_7) # überwiegende Funktionen der RLTs Zuluft (Bennenung prüfen) # -555, -66, -8 und -7 # 
                   qh3_7 <- DB_BE$qh3_7
                   min(qh3_7) #  -8 und -7
                   source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_qh3_7_imputation.R") # Wenn qh1 RLT ist vorhanden qh1==3, wenn ja Idividueller-binominal-glm()Würfel auf Basis von glm(qh3_7 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, family=binomial)
                   DB_DIBS <- as.data.frame(cbind(DB_DIBS, qh3_7)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
                   # -8 bleibt -8, Es gibt keine RLT! 
                   DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, qh3_7, qh3_7_imputated))
                   
                   # B.32 - Imputationstechnik 3
                   #---------------------
                   table(DB_BE$qh3_8) # überwiegende Funktionen der RLTs Umluft (Bennenung prüfen) # -555, -66, -8 und -7 # 
                   qh3_8 <- DB_BE$qh3_8
                   min(qh3_8) #  -8 und -7
                   source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_qh3_8_imputation.R") # Wenn qh1 RLT ist vorhanden qh1==3, wenn ja Idividueller-binominal-glm()Würfel auf Basis von glm(qh3_8 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, family=binomial)
                   DB_DIBS <- as.data.frame(cbind(DB_DIBS, qh3_8)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
                   # -8 bleibt -8, Es gibt keine RLT!
                   DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, qh3_8, qh3_8_imputated))
                   # 
                   #-----------------
               
      
         # B.33 HILFSVARIABLE - Imputationstechnik 4
         #---------------------
         # qi1 ist unabhängig von qh1==3
         # Wenn qh3_3 = 1 dann muss auch qi1 = 3 sein!? Annahme, dass in diesem Fall (Zentrale RLT mit Kühlung) auch eine Zentrale Kälteanlage existiert
         # Anzahl(N) qi1 = 3 in Teilmenge mit qi1 = -7 würfeln. qh3_3=1 fälle(O) in qi1=-7 mit 3 überschreiben. Verbleibene Anzahl(N-O) noch einmal zufällig in unter verbleibenden qi1=-7 auswürfeln?
         table(DB_BE$qi1) # Wird im Gebäude mit zentralen mechanischen Kälteanlagen gekühlt?
         qi1 <- DB_BE$qi1
         min(qi1) # -7
         source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_qi1_imputation.R") # Wenn mechanisch Belüftet und über RLT gekühlt wird qi1==1 & 2 und -7 zu 3, über Idividueller-mulitnominaler-binominal-glm()Würfel auf Basis von multinom(qi1 ~ as.factor(hk_geb) : as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)
         DB_DIBS <- as.data.frame(cbind(DB_DIBS, qi1)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
         DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, qi1, qi1_imputated))
      
         # B.34 HILFSVARIABLE - Imputationstechnik 2
         #---------------------
         # Damit die Zuweisung der Kälteanlagen passt, muss hier nur im Fall f_ant_gekuehlt==-7 & qi1==3 imputiert werden
         table(DB_BE$f_ant_gekuehlt) # Anteil Nutzungsfläche zentral mechanisch gekühlt (Bennenung prüfen) # -555, -66, -8 und -7 #f_ant_gekuehlt <- DB_BE$f_ant_gekuehlt
         f_ant_gekuehlt <- DB_BE$f_ant_gekuehlt
         min(f_ant_gekuehlt) # -8 und -7
         source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_f_ant_gekuehlt_imputation.R") # Würfeln ob mechanisch Belüftet, wenn ja lm(f_ant_gekuehlt ~ as.factor(bak) : as.factor(hk_geb), weights = (HRF/Sum_HRF_BE)*N_Clean)
         DB_DIBS <- as.data.frame(cbind(DB_DIBS, f_ant_gekuehlt)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
         DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, f_ant_gekuehlt, f_ant_gekuehlt_imputated))
   
# B.35 - Imputationstechnik 4
#--------------------- 
#table(qi1a)
#table(DB_BE$qi1av6)
table(DB_BE$freie_kuehlung) # night_flushing_flow # -555, -66, -55 und -7 # 
freie_kuehlung <- DB_BE$freie_kuehlung
min(freie_kuehlung) # -7
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_freie_kuehlung_imputation.R") # Idividueller-mulitnominaler-binominal-glm()Würfel auf Basis von multinom(qi1av6 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean)
DB_DIBS <- as.data.frame(cbind(DB_DIBS, freie_kuehlung)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, freie_kuehlung, freie_kuehlung_imputated))
# Auch Anlagen ohne RLT können eine Nachtlüftung oder freie Kühlung haben! 
   

# B.36 - Imputationstechnik 4
#---------------------
table(DB_BE$w_erz_art) # Haupt-Wärmeerzeuger im Gebäude # -7 # 
w_erz_art <- DB_BE$w_erz_art
min(w_erz_art) # -7
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_w_erz_art_imputation.R") # Idividueller-mulitnominaler-binominal-glm()Würfel auf Basis von multinom(w_erz_art ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000)
# DB_DIBS <- as.data.frame(cbind(DB_DIBS, w_erz_art)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben

# B.37 - Imputationstechnik 4
#---------------------
table(DB_BE$energietraeger) # überwiegend verwendeter Energieträger im Gebäude # -7 # 
energietraeger <- DB_BE$energietraeger
min(energietraeger) # -7   
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_energietraeger_imputation.R") # Idividueller-mulitnominaler-binominal-glm()Würfel auf Basis von multinom(energietraeger ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000)
# DB_DIBS <- as.data.frame(cbind(DB_DIBS, energietraeger)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben


# B.38 Imputationstechnik 4
#---------------------
table(DB_BE$w_erz_art_et) # heating_supply_system # -555, -66 und -7 # 
w_erz_art_et <- DB_BE$w_erz_art_et
min(w_erz_art_et) # -7
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_w_erz_art_et_imputation.R") # Idividueller-mulitnominaler-binominal-glm()Würfel auf Basis von multinom(w_erz_art_et ~ as.factor(hk_geb), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000)
DB_DIBS <- as.data.frame(cbind(DB_DIBS, w_erz_art_et)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, w_erz_art_et, w_erz_art_et_imputated))

# B.39 - Imputationstechnik 4
#---------------------
table(DB_BE$qg13) # heating_emission_system # -555, -88, -66 und -7 # 
qg13 <- DB_BE$qg13
min(qg13) # -88 und -7
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_qg13_imputation.R") # Idividueller-mulitnominaler-binominal-glm()Würfel auf Basis von multinom(qg13 ~ as.factor(hk_geb) * as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 2000)
DB_DIBS <- as.data.frame(cbind(DB_DIBS, qg13)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, qg13, qg13_imputated))

# B.40 - Imputationstechnik 4
#---------------------
table(DB_BE$k_erz_art_rk) # cooling_supply_system # -555, -66, -8 und -7 # 
k_erz_art_rk <- DB_BE$k_erz_art_rk
min(k_erz_art_rk) #-8 und -7
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_k_erz_art_rk_imputation.R") # multinom(k_erz_art_rk ~ as.factor(bak), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 1223.488 # NAs
DB_DIBS <- as.data.frame(cbind(DB_DIBS, k_erz_art_rk)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, k_erz_art_rk, k_erz_art_rk_imputated))
# -8 bleibt -8, d.h. -8 bedeutet es wird nicht gekühlt--> daher kein Kühlsystem könnte auch Null gesetzt werden.


# B.41 - Imputationstechnik 4
#---------------------
table(DB_BE$qi11) # cooling_emission_system  # -555, -66, -8 und -7 # 
qi11 <- DB_BE$qi11
min(qi11) # -8 und -7
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_qi11_imputation.R") # multinom(qi11 ~ as.factor(bak) * f_ant_gekuehlt, data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 603.4769 # NAs
DB_DIBS <- as.data.frame(cbind(DB_DIBS, qi11)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, qi11, qi11_imputated))
# -8 bleibt -8, d.h. -8 bedeutet es wird nicht gekühlt--> daher kein Kälteüberträger könnte auch Null gesetzt werden.


# B.42 - Imputationstechnik 2
#---------------------
table(DB_BE$u_ug) # u_base # NOCH IN DB_BE EINBAUEN; DANN PRÜFEN
u_ug <- DB_BE$u_ug
min(u_ug) # -8 und -7
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_u_ug_imputation.R") # lm(u_ug ~ as.factor(hk_geb) + as.factor(bak), weights = (HRF/Sum_HRF_BE_clean))
DB_DIBS <- as.data.frame(cbind(DB_DIBS, u_ug)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, u_ug, u_ug_imputated))
   
# B.43 - Imputationstechnik 4
#---------------------
table(DB_BE$qd8) # Überwiegende art des installierten Sonnenschutzes (zusammengefasst für die Haupthimmelsrichtungen O-S-W)
qd8 <- DB_BE$qd8
min(qd8) # -88 und -7
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_qd8_imputation.R") # Idividueller-mulitnominaler-binominal-glm()Würfel auf Basis von multinom(qd8 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 11123 # 
DB_DIBS <- as.data.frame(cbind(DB_DIBS, qd8)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, qd8, qd8_imputated))

# B.44 - Imputationstechnik 2
#---------------------
table(DB_BE$d_u_ges) # u-wert Dach opak+transparent
d_u_ges <- DB_BE$d_u_ges
min(d_u_ges) # -8 und -7
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_d_u_ges_imputation.R") # lm(u_ug ~ as.factor(hk_geb) + as.factor(bak), weights = (HRF/Sum_HRF_BE_clean))
DB_DIBS <- as.data.frame(cbind(DB_DIBS, d_u_ges)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, d_u_ges, d_u_ges_imputated))
  

# B.45 - Imputationstechnik 4
#---------------------
table(DB_BE$qg21) # dhw_system # Warmwasser System
qg21 <- DB_BE$qg21
min(qg21)
source("D:/OneDrive/OneDrive - Technological University Dublin/GitHub/DataNWG---BRE-Imputation-for-DIBS/einzel_imputationen/BE_qg21_imputation.R") # Idividueller-mulitnominaler-binominal-glm()Würfel auf Basis von multinom(qd8 ~ as.factor(hk_geb) * as.factor(bak_grob), data = DB_BE_clean, weights = (HRF/Sum_HRF_BE_clean)*N_Clean, maxit = 1000, MaxNWts = 1000) # AIC: 11123 # 
DB_DIBS <- as.data.frame(cbind(DB_DIBS, qg21)) #Imputierte Variable aus externem Skript in DB_DIBS schreiben
DB_DIBS_flagged <- as.data.frame(cbind(DB_DIBS_flagged, qg21, qg21_imputated))




#............................................................................
#
# 
#Speichern von DB_DIBS ####
#
#............................................................................

# write it as a csv file
write.csv(DB_DIBS, 'E:/GitHub_Data_Access/DB_DIBS.csv')
write.csv(DB_DIBS_flagged, 'E:/GitHub_Data_Access/DB_DIBS_flagged.csv')

# # speichert die input daten als .csv (für mögliche externe Auwertungen)
# con<-file('DB_DIBS.csv',encoding="UTF-8") # Definiert die Speichercodierung, sodass Umlaute auch richtig gespeichert werden!
# write.csv(DB_DIBS, file=con) #Speichert csv unter Berücksichtigung der Definition der Speichercodierung

# install.packages("writexl")
library("writexl")
write_xlsx(DB_DIBS,"E:/GitHub_Data_Access/DB_DIBS.xlsx") # 
write_xlsx(DB_DIBS,"E:/GitHub_Data_Access/BE_BuildingData.xlsx") # For DIBS use, just use
write_xlsx(DB_DIBS_flagged,"E:/GitHub_Data_Access/DB_DIBS_flagged.xlsx") # For DIBS use, just use

      #------------------------------------------------------------------
      #------------------------------------------------------------------
      # DIBS dataPreprocessingBE.py uses only the following 35 Variables:
      #------------------------------------------------------------------
      #
      # scr_gebaeude_id
      # plz
      # hk_geb
      # uk_geb
      # q25_1
      # aw_fl
      # unteraw_fl
      # qd1
      # geb_f_flaeche_n_iwu
      # geb_f_flaeche_o_iwu
      # geb_f_flaeche_s_iwu
      # geb_f_flaeche_w_iwu
      # d_fl_be
      # nrf_2
      # ebf
      # n_og
      # geb_f_hoehe_mittel_iwu
      # qf1
      # lampenart
      # glasart_1
      # qd8
      # u_fen
      # u_aw
      # d_u_ges
      # u_ug
      # n_ug
      # bak_grob
      # qh1
      # qh3_1
      # aw_konstr_1
      # freie_kuehlung
      # w_erz_art_et
      # k_erz_art_rk
      # qg13
      # qi11
      # qg21 # added 05.05.2022
      #------------------------------------------------------------------
      #------------------------------------------------------------------



#----------------------------------------------------------------
# Prüfung ob alle für DIBS relevanten Variablen abgedeckt sind!!!

# PRÜFEN:
# [glass_solar_transmittance]
# [glass_solar_shading_transmittance]
# [temp_adj_base]
# [ach_infl]
# Luftdichtheit des Gebäudes
# [ach_win]
# [ach_vent]
# [heat_recovery_efficiency]
# [thermal_capacitance]
#----------------------------------------------------------------




