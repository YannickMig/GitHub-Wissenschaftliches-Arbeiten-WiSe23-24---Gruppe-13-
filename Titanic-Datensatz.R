###############################################################################
###																			                                    ###
###							      Wissenschaftliches Arbeiten							              ###
###																			                                    ###
###							           Melanie & Jerry  						                    ###
###																			                                    ###
###############################################################################
##                        Titanic Datensatz                                  ##
###############################################################################
###					              	  Aufgabe 1                                     ###

##### Extrahiere aus dem Namen eine Variable mit der Anrede der Person

titanic$Name #Namen extrahieren

titanic$Anrede <- sapply(strsplit(titanic$Name, ", "), function(x) strsplit(x[2], "\\.")[[1]][1])
#Anrede <- gsub("^.*, (.*?)\\..*$", "\\1", titanic$Name)
print(titanic$Anrede)

##### Codiert die Variablen „Survived“, „Sex“, „Embarked“ als factor um

titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

## Überführt die Variable „Pclass“ in einen ordered-factor

titanic$Pclass <- factor(titanic$Pclass, ordered = TRUE, levels = c(1, 2, 3))

##### Imputiert fehlende Werte in der Variable „Age“

# Berechne den Median des Alters für jede Anredegruppe
median_age_by_title <- tapply(titanic$Age, titanic$Anrede, median, na.rm = TRUE)


# Imputiere fehlende Werte in der Variable "Age" basierend auf der Anrede
for (title in unique(titanic$Anrede)) 
  titanic$Age[titanic$Anrede == title & is.na(titanic$Age)] <- median_age_by_title[[title]]

##### Extrahiert „Cabin“ die folgenden Informationen und erzeugt neue Variablen 

# Extrahiere Informationen aus der Kabinenklasse
deck <- substr(titanic$Cabin, 1, 1)  # Deck ist der erste Buchstabe der Kabinennummer
location <- ifelse(as.integer(substring(titanic$Cabin, 2, nchar(titanic$Cabin))) %% 2 == 0, "backbord", "steuerbord")
# Prüfe, ob die Nummer gerade ist (backbord) oder ungerade (steuerbord)
# Wenn die Nummer nicht bekannt ist (NA), wird NA beibehalten

# Erstelle neue Variablen für Deck und Location
titanic$Deck <- deck
titanic$Location <- location

# Ergebnis anzeigen
print(titanic)

##### Entferne „PassengerID“, „Name“, „Ticket“ und „Cabin“ aus dem Datensatz

# Entferne die Variablen "PassengerID", "Name", "Ticket" und "Cabin" aus dem Datensatz
titanic <- subset(titanic, select = -c(PassengerId, Name, Ticket, Cabin))

# Zeige den aktualisierten Datensatz an
print(titanic)

# Speicherpfad und Dateiname für die exportierte CSV-Datei festlegen
export_pfad <- "C:\\Users\\jciyk\\Downloads\\"
dateiname <- "titanic_geändert.csv"

# Den veränderten Datensatz "titanic" als CSV-Datei exportieren
write.csv(titanic, file = paste0(export_pfad, dateiname), row.names = FALSE)
View(titanic_geändert)
