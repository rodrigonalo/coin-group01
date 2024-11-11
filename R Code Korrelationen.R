install.packages("readxl")       # Zum Einlesen von Excel-Dateien
install.packages("factoextra")   # Für die Visualisierung der PCA
install.packages("ggplot2")      # Für ansprechende Plots
install.packages("openxlsx")

#Schritt 1 öffen der excel datei
library("readxl")
library("factoextra")
library("ggplot2")
library("openxlsx")

#4 Datei einlesen (Pfad anpassen)
file_path <- "C:\\Users\\hanna\\Desktop\\Studium\\2 Master\\2. Semester\\M_Projekt Online Social Networks\\Daten von Alvaro\\3_individual_features_extended.xlsx"
daten <- read_excel(file_path)

#5 Nur numerische Spalten auswählen
numerische_daten <- daten[sapply(daten, is.numeric)]

#6 Daten standardisieren
skalierte_daten <- scale(numerische_daten)

#7.1 PCA durchführen
pca_ergebnis <- prcomp(skalierte_daten, center = TRUE, scale. = TRUE)

#7.2 Zusammenfassung der PCA
summary(pca_ergebnis)

#8.1 Scree-Plot
fviz_eig(pca_ergebnis, addlabels = TRUE, ylim = c(0, 50))

#8.2 Biplot
fviz_pca_biplot(pca_ergebnis, repel = TRUE, col.var = "blue", col.ind = "red")

#9.1 Scores der Hauptkomponenten
pca_scores <- as.data.frame(pca_ergebnis$x)

#9.2 Ladungen der Variablen
pca_loadings <- as.data.frame(pca_ergebnis$rotation)

#10.1 PCA-Scores zu den Daten hinzufügen
finale_daten <- cbind(daten, pca_scores)

#10.2 Ergebnisse speichern
write.xlsx(finale_daten, "pca_ergebnisse.xlsx")
write.xlsx(finale_daten, "C:\\Users\\hanna\\Desktop\\Studium\\2 Master\\2. Semester\\M_Projekt Online Social Networks\\Daten von Alvaro\\pca_ergebnisse.xlsx")

# PCA-Ergebnisse extrahieren (Scores)
pca_scores <- as.data.frame(pca_ergebnis$x)

# Ergebnisse speichern - nur PCA-Scores
write.xlsx(pca_scores, "C:\\Users\\hanna\\Desktop\\Studium\\2 Master\\2. Semester\\M_Projekt Online Social Networks\\Daten von Alvaro\\pca_ergebnisse2.xlsx")


# Ladungen anzeigen
pca_loadings <- as.data.frame(pca_ergebnis$rotation)

# Sortierte Ladungen für PC1
print(sort(pca_loadings$PC1, decreasing = TRUE))

# Sortierte Ladungen für PC2
print(sort(pca_loadings$PC2, decreasing = TRUE))



########


library(factoextra)

# Biplot für die ersten beiden Hauptkomponenten
fviz_pca_biplot(pca_ergebnis, repel = TRUE,
                col.var = "blue", # Farbe für Variablen (Pfeile)
                col.ind = "red")  # Farbe für Datenpunkte

# Scree-Plot
fviz_eig(pca_ergebnis, addlabels = TRUE, ylim = c(0, 50))


# Beitrag der Variablen zu PC1
fviz_contrib(pca_ergebnis, choice = "var", axes = 1, top = 10)

# Beitrag der Variablen zu PC2
fviz_contrib(pca_ergebnis, choice = "var", axes = 2, top = 10)


#Headmap erstellen
install.packages("corrplot")
library(corrplot)

# Korrelationsmatrix berechnen
korrelationsmatrix <- cor(numerische_daten, use = "complete.obs")
print(korrelationsmatrix)  # Zeigt die Matrix zur Überprüfung an

# Heatmap der Korrelationsmatrix
corrplot(korrelationsmatrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, col = colorRampPalette(c("blue", "white", "red"))(200))


install.packages("qgraph")
library(qgraph)

# Netzwerk der Korrelationsmatrix
qgraph(korrelationsmatrix, layout = "spring", 
       minimum = 0.3,  # Nur Korrelationen > 0.3 darstellen
       cut = 0.3,      # Schwellenwert für Verbindungen
       labels = colnames(korrelationsmatrix), 
       color = c("skyblue", "pink"))


##########

#Darstellung aller posititiven Korrelationen und deren Stärke zueinander als Tabelle

# Erstelle die Korrelationsmatrix
numerische_daten <- daten[sapply(daten, is.numeric)]
korrelationsmatrix <- cor(numerische_daten, use = "complete.obs")

# Korrelationsmatrix in ein Datenrahmenformat umwandeln
korrelations_df <- as.data.frame(as.table(korrelationsmatrix))

# Umbenennen der Spalten für bessere Lesbarkeit
colnames(korrelations_df) <- c("Variable1", "Variable2", "Korrelation")

# Nur die positiven Korrelationen extrahieren und dabei die Hauptdiagonale (1) und Duplikate ausschließen
positive_korrelations <- subset(korrelations_df, Korrelation > 0 & Variable1 != Variable2)

# Duplikate entfernen (weil jede Korrelation doppelt auftritt, z.B. A-B und B-A)
positive_korrelations <- positive_korrelations[!duplicated(t(apply(positive_korrelations[,1:2], 1, sort))), ]

# Tabelle nach Stärke der Korrelation sortieren (absteigend)
positive_korrelations <- positive_korrelations[order(-positive_korrelations$Korrelation), ]

# Ergebnis anzeigen
print(positive_korrelations)

# Speichern der Ergebnisse als Excel-Datei
install.packages("openxlsx")
library(openxlsx)
write.xlsx(positive_korrelations, "C:\\Users\\hanna\\Desktop\\Studium\\2 Master\\2. Semester\\M_Projekt Online Social Networks\\Daten von Alvaro\\pca_ergebnisse2.xlsx")


#######


# Nur die negativen Korrelationen extrahieren und dabei die Hauptdiagonale (1) und Duplikate ausschließen
negative_korrelations <- subset(korrelations_df, Korrelation < 0 & Variable1 != Variable2)

# Duplikate entfernen (weil jede Korrelation doppelt auftritt, z.B. A-B und B-A)
negative_korrelations <- negative_korrelations[!duplicated(t(apply(negative_korrelations[,1:2], 1, sort))), ]

# Tabelle nach Stärke der negativen Korrelation sortieren (aufsteigend, um die stärksten negativen Korrelationen oben zu haben)
negative_korrelations <- negative_korrelations[order(negative_korrelations$Korrelation), ]

# Ergebnis anzeigen
print(negative_korrelations)
write.xlsx(negative_korrelations,  "C:\\Users\\hanna\\Desktop\\Studium\\2 Master\\2. Semester\\M_Projekt Online Social Networks\\Daten von Alvaro\\negative_korrelations.xlsx")


#########

# Top 10 stärkste positive Korrelationen
top_positive <- head(positive_korrelations, 10)
print(top_positive)

# Top 10 stärkste negative Korrelationen
top_negative <- head(negative_korrelations, 10)
print(top_negative)


#########

# Häufigkeit der Variablen in positiven Korrelationen
positive_vars <- table(c(positive_korrelations$Variable1, positive_korrelations$Variable2))
print(positive_vars)

# Häufigkeit der Variablen in negativen Korrelationen
negative_vars <- table(c(negative_korrelations$Variable1, negative_korrelations$Variable2))
print(negative_vars)

########

library(qgraph)

# Netzwerkgraph nur für starke positive und negative Korrelationen (z.B. |Korrelation| > 0.7)
starke_korrelations <- subset(korrelations_df, abs(Korrelation) > 0.7 & Variable1 != Variable2)
qgraph(as.matrix(xtabs(Korrelation ~ Variable1 + Variable2, data = starke_korrelations)), layout = "spring")


####

# 1. Korrelationsmatrix berechnen
# Stellen Sie sicher, dass 'daten' nur numerische Spalten enthält
numerische_daten <- daten[sapply(daten, is.numeric)]
korrelationsmatrix <- cor(numerische_daten, use = "complete.obs")

# 2. Korrelationsmatrix in ein Datenrahmenformat umwandeln
korrelations_df <- as.data.frame(as.table(korrelationsmatrix))
colnames(korrelations_df) <- c("Variable1", "Variable2", "Korrelation")

# 3. Starke positive und negative Korrelationen extrahieren (z.B. |Korrelation| > 0.7)
starke_korrelations <- subset(korrelations_df, abs(Korrelation) > 0.7 & Variable1 != Variable2)

# 4. Liste der eindeutigen Variablen erstellen
# Falls Variablen spezielle Zeichen enthalten, die Probleme verursachen könnten, werden sie hier bereinigt
variablen <- unique(c(starke_korrelations$Variable1, starke_korrelations$Variable2))

# 5. Leere Matrix erstellen für die starken Korrelationen
korrelations_matrix <- matrix(0, nrow = length(variablen), ncol = length(variablen))
rownames(korrelations_matrix) <- variablen
colnames(korrelations_matrix) <- variablen

# 6. Füllen der Matrix mit den starken Korrelationen
for (i in 1:nrow(starke_korrelations)) {
  var1 <- starke_korrelations$Variable1[i]
  var2 <- starke_korrelations$Variable2[i]
  
  # Füge die Korrelation in die Matrix ein, symmetrisch
  if (var1 %in% variablen && var2 %in% variablen) {  # Überprüfung, ob beide Variablen in der Matrix vorhanden sind
    korrelations_matrix[var1, var2] <- starke_korrelations$Korrelation[i]
    korrelations_matrix[var2, var1] <- starke_korrelations$Korrelation[i]
  }
}

# 7. Visualisierung des Netzwerks der stärksten Korrelationen
library(qgraph)

# Zeichne den Netzwerkgraphen
qgraph(korrelations_matrix, layout = "spring", labels = colnames(korrelations_matrix), 
       color = c("skyblue", "pink"), minimum = 0.7)



###
# 1. Korrelationsmatrix berechnen
numerische_daten <- daten[sapply(daten, is.numeric)]
korrelationsmatrix <- cor(numerische_daten, use = "complete.obs")

# 2. Korrelationsmatrix in ein Datenrahmenformat umwandeln
korrelations_df <- as.data.frame(as.table(korrelationsmatrix))
colnames(korrelations_df) <- c("Variable1", "Variable2", "Korrelation")

# 3. Starke positive und negative Korrelationen extrahieren (z.B. |Korrelation| > 0.7)
starke_korrelations <- subset(korrelations_df, abs(Korrelation) > 0.7 & Variable1 != Variable2)

# 4. Liste der eindeutigen Variablen erstellen
variablen <- unique(c(starke_korrelations$Variable1, starke_korrelations$Variable2))

# 5. Überprüfen, ob alle Variablennamen korrekt sind
# Hier entfernen wir alle Zeilen, in denen die Variablen nicht in `variablen` vorkommen
starke_korrelations <- starke_korrelations[starke_korrelations$Variable1 %in% variablen & 
                                             starke_korrelations$Variable2 %in% variablen, ]

# 6. Leere Matrix erstellen für die starken Korrelationen
korrelations_matrix <- matrix(0, nrow = length(variablen), ncol = length(variablen))
rownames(korrelations_matrix) <- variablen
colnames(korrelations_matrix) <- variablen

# 7. Füllen der Matrix mit den starken Korrelationen
for (i in 1:nrow(starke_korrelations)) {
  var1 <- starke_korrelations$Variable1[i]
  var2 <- starke_korrelations$Variable2[i]
  
  # Füge die Korrelation in die Matrix ein, symmetrisch
  korrelations_matrix[var1, var2] <- starke_korrelations$Korrelation[i]
  korrelations_matrix[var2, var1] <- starke_korrelations$Korrelation[i]
}

# 8. Visualisierung des Netzwerks der stärksten Korrelationen
library(qgraph)

# Zeichne den Netzwerkgraphen
qgraph(korrelations_matrix, layout = "spring", labels = colnames(korrelations_matrix), 
       color = c("skyblue", "pink"), minimum = 0.7)

