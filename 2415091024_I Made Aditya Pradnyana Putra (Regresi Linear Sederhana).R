# Nama: I Made Aditya Pradnyana Putra/NIM: 2415091024/Kelas: 1 DPS

# Analisis Hubungan Hari Hujan dengan Curah Hujan menggunakan Regresi Linear Sederhana
library(readxl)

# 1. Data
data <- read_excel("C:\\Users\\adity\\Downloads\\Jumlah Curah Hujan dan Hari Hujan.xlsx")
head(data) # Menampilkan beberapa baris data

# variabel_independen = Hari_hujan (x)
# variabel_dependen = Curah_hujan_mm3 (y)


# 2. Uji asumsi

# 2.1. Linearitas

# Plot data dan garis regresi
plot(data$Hari_Hujan, data$Curah_Hujan_mm3, 
     main = "Hubungan Curah Hujan dengan Hari Hujan di Provinsi Jawa Barat tahun 2017",
     xlab = "Hari Hujan", ylab = "Curah Hujan (mm3)",
     pch = 19, col = "blue")

# Menambahkan garis regresi
abline(lm(Curah_Hujan_mm3 ~ Hari_Hujan, data = data), col = "red", lwd = 3)  # Garis regresi


# 2.2. Normalitas
# Jika p-value > 0.05, residual berdistribusi normal.

# Model regresi linear
model <- lm(Curah_Hujan_mm3 ~ Hari_Hujan, data = data)

# Mengambil residual dari model
residuals <- residuals(model)

# Histogram Residual
hist(residuals, main = "Histogram Residu", xlab = "Residu", col = "blue", breaks = 8)

# Q-Q Plot Residual
qqnorm(residuals, main = "Plot Q-Q Residu")
qqline(residuals, col = "red")

# Uji normalitas residual dengan Shapiro-Wilk
normalitas <- shapiro.test(residuals)
print(normalitas) 

# 2.3. Homogenitas

# Plot Residual vs Fitted
plot(fitted(model), residuals, 
     main = "Residuals vs Fitted", 
     xlab = "Fitted Values", 
     ylab = "Residu", 
     pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 3)

library(lmtest)

# Uji Breusch-Pagan
bp_test <- bptest(model)
print(bp_test) 

# 3. Analisis

# Membangun Model Regresi
model <- lm(Curah_Hujan_mm3 ~ Hari_Hujan, data = data)
plot(model, which = 1)

summary(model)

# 4. Visualisasi (menggunakan ggplot)

library(ggplot2)
ggplot(data, aes(x = Hari_Hujan, y = Curah_Hujan_mm3)) +
  geom_point(color = "blue") +  
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  labs(title = "Hubungan Curah Hujan dengan Hari Hujan di Provinsi Jawa Barat tahun 2017",
       x = "Hari Hujan",
       y = "Curah Hujan (mm³)") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))


# 5. Interpretasi

# Pada analisis regresi yang telah dilakukan terhadap data jumlah curah hujan dan hari hujan 
# di Provinsi Jawa Barat tahun 2017, terdapat hubungan yang jelas antara kedua variabel, 
# baik variabel independen (Hari Hujan) dan variabel dependen (Curah Hujan). 
# Hasil analisis menunjukkan bahwa semakin banyak hari hujan dalam sebulan, 
# maka semakin tinggi jumlah curah hujan yang tercatat. Uji linearitas mengonfirmasikan bahwa 
# hubungan antara jumlah hari hujan dan curah hujan bersifat linear. Selain itu, 
# uji normalitas terhadap residual menunjukkan bahwa kesalahan model terdistribusi secara normal, 
# yang mengindikasikan bahwa model yang dibangun valid. Uji homogenitas juga memperlihatkan bahwa 
# variasi kesalahan perhitungan tetap konsisten di seluruh data.
# Secara keseluruhan, hasil ini memberikan gambaran yang jelas bahwa jumlah hari hujan berperan penting
# dalam menentukan curah hujan.

