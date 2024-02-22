#extrahieren und variabel
titanic$anrede <- sapply(strsplit(titanic$Name, ", "), function(x) strsplit(x[2], "\\.")[[1]][1])

#variablen als factor umwandeln
titanic$Survived<-as.factor(titanic$Survived)
titanic$Sex<-as.factor(titanic$Sex)
titanic$Embarked<-as.factor(titanic$Embarked)


#ordered factor
titanic$Pclass <- factor(titanic$Pclass, ordered = TRUE, levels = c(1, 2, 3))
print(titanic)

#fehlene werte imputieren 

# Berechne den Median des Alters für jede Anredegruppe
median_age_by_title <- tapply(titanic$Age, anrede, median, na.rm = TRUE)

# Imputiere fehlende Werte in der Variable "Age" basierend auf der Anrede
for (title in unique(anrede)) 
  titanic$Age[anrede == title & is.na(titanic$Age)] <- median_age_by_title[[title]]



#Extrahieren 

# Extrahiere Informationen aus der Kabinenklasse
deck <- substr(titanic$Cabin, 1, 1)  # Deck ist der erste Buchstabe der Kabinennummer
location <- ifelse(as.integer(substring(titanic$Cabin, 2, nchar(titanic$Cabin))) %% 2 == 0, "backbord", "steuerbord")
# Prüfe, ob die Nummer gerade ist (backbord) oder ungerade (steuerbord)
# Wenn die Nummer nicht bekannt ist (NA), wird NA beibehalten

# Erstelle neue Variablen für Deck und Lage
titanic$Deck <- deck
titanic$Location <- location

# Ergebnis anzeigen
print(titanic)


#Löschen
titanic <- subset(titanic, select = -c(PassengerId, Name, Ticket, Cabin))
print(titanic)


# Speicherpfad und Dateiname für die exportierte CSV-Datei festlegen
export_pfad <- "/Users/melanieoraca/Downloads/wisa"
dateiname <- "titanic_geändert.csv"

# Den veränderten Datensatz "titanic" als CSV-Datei exportieren
write.csv(titanic, file = paste0(export_pfad, dateiname), row.names = FALSE)

