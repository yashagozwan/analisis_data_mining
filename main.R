# Install libraries if not already installed
required_packages <- c("dplyr", "ggplot2", "zoo")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

# Load libraries
library(dplyr)
library(ggplot2)
library(zoo)

# 1. Preprocessing Data
# asumsikan file creditcard.csv berada di directory datasets
# jika file belum ada silakan unduh dari kaggle: https://www.kaggle.com/datasets/mlg-ulb/creditcardfraud
cat("Membaca dataset...\n")
df <- read.csv("datasets/creditcard.csv")

# Mengonversi waktu (Time) dalam satuan detik menjadi jam
# 1 jam = 3600 detik
cat("Melakukan agregasi time series per jam...\n")
df_ts <- df %>%
    mutate(Hour = floor(Time / 3600)) %>%
    group_by(Hour) %>%
    summarise(
        Total_Transactions = n(),
        Total_Amount = sum(Amount),
        Total_Fraud = sum(Class == 1)
    )

# 2. Transformasi & Analisis Time Series Dasar
cat("Membuat visualisasi dasar dari transaksi per jam...\n")
plot_tx <- ggplot(df_ts, aes(x = Hour, y = Total_Transactions)) +
    geom_line(color = "blue") +
    labs(
        title = "Volume Transaksi Kartu Kredit per Jam",
        x = "Jam ke-",
        y = "Jumlah Transaksi"
    ) +
    theme_minimal()
print(plot_tx)

# 3. Smoothing Menggunakan Moving Average
# Digunakan Moving Average dengan window = 3 jam
cat("Melakukan smoothing dengan Moving Average...\n")
window_size <- 3
df_ts <- df_ts %>%
    mutate(MA_Transactions = rollmean(Total_Transactions, k = window_size, fill = NA, align = "right"))

# Visualisasi perbandingan asil dengan smoothing
plot_ma <- ggplot(df_ts, aes(x = Hour)) +
    geom_line(aes(y = Total_Transactions, color = "Aktual"), alpha = 0.5) +
    geom_line(aes(y = MA_Transactions, color = "Moving Average (k=3)"), linewidth = 1) +
    labs(
        title = "Smoothing Volume Transaksi Menggunakan Moving Average",
        x = "Jam ke-",
        y = "Jumlah Transaksi",
        color = "Legenda"
    ) +
    theme_minimal()
print(plot_ma)

# 4. Deteksi Anomali dengan Z-Score
cat("Melakukan deteksi anomali dengan metode Z-Score pada volume transaksi...\n")
# Menghitung Z-Score berdasarkan kolom jumlah transaksi per jam
mean_tx <- mean(df_ts$Total_Transactions, na.rm = TRUE)
sd_tx <- sd(df_ts$Total_Transactions, na.rm = TRUE)

df_ts <- df_ts %>%
    mutate(
        Z_Score = (Total_Transactions - mean_tx) / sd_tx,
        Is_Anomaly = ifelse(Z_Score > 1.3 | Z_Score < -1.3, "Anomali", "Normal")
    )

# Menghitung batas jumlah transaksi di Z-Score = 1.3 untuk digambarkan sebagai garis
upper_bound <- mean_tx + 1.3 * sd_tx
lower_bound <- mean_tx - 1.3 * sd_tx

# Visualisasi Anomali dengan Z-score
plot_anomaly <- ggplot(df_ts, aes(x = Hour, y = Total_Transactions)) +
    geom_line(color = "darkgray") +
    geom_point(aes(color = Is_Anomaly, size = Is_Anomaly)) +
    # Tambahkan garis merah putus-putus untuk menandakan batas anomali atas
    geom_hline(yintercept = upper_bound, color = "red", linetype = "dashed", alpha = 0.6) +
    # Tambahkan garis merah putus-putus untuk menandakan batas anomali bawah
    geom_hline(yintercept = lower_bound, color = "red", linetype = "dashed", alpha = 0.6) +
    # Tambahkan teks tepat di atas titik yang terdeteksi Anomali
    geom_text(data = filter(df_ts, Is_Anomaly == "Anomali"),
              aes(label = paste0("Jam ", Hour)), 
              vjust = -1.5, color = "red", size = 3.5, fontface = "bold") +
    scale_color_manual(values = c("Anomali" = "red", "Normal" = "black")) +
    scale_size_manual(values = c("Anomali" = 3, "Normal" = 1)) +
    labs(
        title = "Deteksi Anomali Volume Transaksi",
        subtitle = "Garis putus-putus merah melambangkan batas anomali (Z-Score = ±1.3)",
        x = "Jam ke-",
        y = "Jumlah Transaksi"
    ) +
    theme_minimal()
print(plot_anomaly)

# 5. Keterkaitan Anomali dengan Fraud
cat("Menganalisis irisan dan relasi keterkaitan...\n")
# Mengecek jam mana saja yang dianggap anomali dan apakah menampung kejadian fraud
anomaly_fraud_summary <- df_ts %>%
    filter(Is_Anomaly == "Anomali") %>%
    select(Hour, Total_Transactions, Z_Score, Total_Fraud)

print("Detail Jam Anomali Beserta Jumlah Fraud:")
print(anomaly_fraud_summary)

# Visualisasi Spesifik Distribusi Kasus Fraud
cat("Membuat visualisasi spesifik penyebaran kasus fraud...\n")
plot_fraud <- ggplot(df_ts, aes(x = Hour, y = Total_Fraud)) +
    geom_col(aes(fill = Is_Anomaly)) +
    scale_fill_manual(values = c("Anomali" = "red", "Normal" = "darkgray")) +
    labs(
        title = "Sebaran Jumlah Kasus Fraud (Per Jam)",
        subtitle = "Batang berwarna MERAH menandakan jam di mana terjadi Anomali lonjakan transaksi",
        x = "Jam ke-",
        y = "Frekuensi Kejadian Fraud",
        fill = "Status Deteksi Anomali"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
print(plot_fraud)

cat("Proses analisis selesai. Export grafik atau gunakan hasil ringkasan di atas untuk Bab 4.\n")
