# Install dan load library jika diperlukan
library(tidyverse)
library(ggplot2)


# Import Dataset
setwd("C:/Users/Damar/Documents/R Programming/UAS SEM 1")
data <- read.csv("schizophrenia_dataset.csv", stringsAsFactors = FALSE)
head(data)


# Rename kolom dari bahasa Turki ke bahasa Inggris
colnames(data) <- c(
  "Patient_ID", "Age", "Gender", "Education_Level", "Marital_Status",
  "Occupation", "Income_Level", "Living_Area", "Diagnosis", "Illness_Duration",
  "Hospital_Admissions", "Family_History", "Substance_Use", "Suicide_Attempt",
  "Positive_Symptom_Score", "Negative_Symptom_Score", "GAF_Score",
  "Social_Support", "Stress_Factors", "Medication_Adherence"
)


# Cek Struktur dan Statistik Deskriptif Dasar
str(data)                         # Menampilkan struktur data
summary(data)                     # Menampilkan ringkasan statistik 
# Jumlah pasien skizofrenia vs non-skizofrenia
table(data$Diagnosis)
prop.table(table(data$Diagnosis))
# Diagnosis: 0 = Non-skizofrenia, 1 = Skizofrenia


# Keterangan
# Age : Usia
# GAF_score : Penilaian fungsi psikologis
# Positive_Symptom_Score : Gejala Positif (Halusinasi, delusi)
# Negative_Symptom_Score : Gejala Negatif (menarik diri, apatis)


# Statistik Deskriptif Pasien Skizofrenia
# Analisis Rata-Rata Berdasarkan Diagnosis
mean(data$Age[data$Diagnosis == 1])                         # Rata-rata usia pasien skizofrenia
mean(data$GAF_Score[data$Diagnosis == 1])                   # Rata-rata skor GAF pasien skizofrenia
mean(data$Positive_Symptom_Score[data$Diagnosis == 1])      # Rata-rata skor gejala positif
mean(data$Negative_Symptom_Score[data$Diagnosis == 1])      # Rata-rata skor gejala negatif


# Median dan Standar Deviasi
median(data$Age[data$Diagnosis == 1])
sd(data$Age[data$Diagnosis == 1])

median(data$GAF_Score[data$Diagnosis == 1])
sd(data$GAF_Score[data$Diagnosis == 1])

median(data$Positive_Symptom_Score[data$Diagnosis == 1])
sd(data$Positive_Symptom_Score[data$Diagnosis == 1])

median(data$Negative_Symptom_Score[data$Diagnosis == 1])
sd(data$Negative_Symptom_Score[data$Diagnosis == 1])


# Analisis Gender dan Riwayat Keluarga
prop.table(table(data$Gender[data$Diagnosis == 1]))         # Proporsi gender di antara pasien skizofernia 
table(data$Family_History, data$Diagnosis)                  # Apakah ada hubungan antara Family History dan Diagnosis
prop.table(table(data$Diagnosis, data$Family_History), 2)


# Analisis Penggunaan Zat dan Percobaan Bunuh Diri
prop.table(table(data$Substance_Use, data$Diagnosis), 2)    # Proporsi penggunaan narkoba antara pasien skizofrenia dan bukan
prop.table(table(data$Suicide_Attempt, data$Diagnosis), 2)  # Proporsi percobaan bunuh diri antara pasien skizofrenia dan bukan


# Korelasi dan Regresi
# Korelasi antara gejala negatif dan GAF
cor.test(data$Negative_Symptom_Score, data$GAF_Score)

# Model regresi linier
model <- lm(GAF_Score ~ Negative_Symptom_Score, data = data)
summary(model)


# Uji Hipotesis
# Apakah skor GAF berbeda berdasarkan gender (pada pasien skizofrenia)
t.test(GAF_Score ~ Gender, data = data)


# Visualisasi 
# Histogram usia berdasarkan diagnosis
ggplot(data, aes(x = Age, fill = as.factor(Diagnosis))) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
  scale_fill_manual(
    values = c("0" = "#F8766D", "1" = "#00BFC4"),
    labels = c("0" = "Non-skizofrenia", "1" = "Skizofrenia")
  ) +
  labs(title = "Distribusi Usia berdasarkan Diagnosis", fill = "Diagnosis") +
  theme_minimal()


# Barplot penggunaan narkoba berdasarkan diagnosis
ggplot(data, aes(x = as.factor(Substance_Use), fill = as.factor(Diagnosis))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("0" = "#F8766D", "1" = "#00BFC4"),
    labels = c("0" = "Non-skizofrenia", "1" = "Skizofrenia")
  ) +
  labs(
    title = "Penggunaan Zat berdasarkan Diagnosis",
    x = "Penggunaan Zat (0=Tidak, 1=Ya)",
    fill = "Diagnosis"
  ) +
  theme_minimal()


# Scatterplot gejala negatif vs skor GAF
ggplot(data, aes(x = Negative_Symptom_Score, y = GAF_Score)) +
  geom_point(color = "#00BFC4", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "#F8766D") +
  labs(
    title = "Hubungan Gejala Negatif dan Fungsi Psikologis (GAF)",
    x = "Skor Gejala Negatif",
    y = "Skor GAF"
  ) +
  theme_minimal()
