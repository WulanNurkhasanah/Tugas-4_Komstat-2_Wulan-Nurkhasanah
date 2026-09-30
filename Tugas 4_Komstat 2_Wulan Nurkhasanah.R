# TUGAS DISTRIBUSI PROBABILITAS DISKRIT

# Soal 1: Sebaran Poisson

# Soal: Jika rata-rata pelanggan datang ke toko adalah 3 orang per jam, 
# modelkan dengan Poisson dan hitung P(X >= 5).

lambda <- 3

# Menghitung P(X >= 5) = 1 - P(X <= 4) menggunakan fungsi cdf (ppois)
prob_1 <- 1 - ppois(4, lambda = lambda)
# Atau menggunakan lower.tail = FALSE: ppois(4, lambda = lambda, lower.tail = FALSE)

cat("--- Soal 1 (Poisson) ---\n")
cat("P(X >= 5) =", prob_1, "\n\n")

# Opsional: Plot PMF untuk visualisasi
x_pois <- 0:12
pmf_pois <- dpois(x_pois, lambda = lambda)
plot(x_pois, pmf_pois, type = "h", lwd = 3, 
     main = "Poisson(λ = 3)", xlab = "k", ylab = "P(X=k)")


# Soal 2: Sebaran Hipergeometrik

# Soal: Dari 100 bola (20 berwarna merah), diambil 10 tanpa pengembalian.
# Modelkan jumlah bola merah yang diambil dengan distribusi yang tepat.

# Parameter Hipergeometrik:
N <- 100  # Ukuran populasi total
K <- 20   # Jumlah sukses dalam populasi (bola merah)
n <- 10   # Ukuran sampel yang diambil

# Domain k (banyak bola merah yang mungkin terambil)
k <- seq(from = max(0, n + K - N), to = min(n, K))

# Menghitung PMF P(X = k)
# Dalam R: dhyper(x, m, n, k) di mana:
# m = K (sukses), n = N - K (gagal), k = n_sampel
pmf_hyper <- dhyper(k, m = K, n = N - K, k = n)

# Menampilkan tabel PMF
tabel_hyper <- data.frame(k = k, P = pmf_hyper)

cat("--- Soal 2 (Hipergeometrik) ---\n")
print(tabel_hyper)
cat("\n")

# Plot PMF Hipergeometrik
plot(k, pmf_hyper, type = "h", lwd = 3,
     main = paste0("Hypergeometric(N=", N, ", K=", K, ", n=", n, ")"),
     xlab = "k (banyak bola merah dalam sampel)", ylab = "P(X=k)")


# Soal 3: Sebaran Binomial (Simulasi vs Teoretis)

# Soal: Simulasikan 1.000 percobaan Binomial (n = 15, p = 0.4) dan bandingkan 
# histogram hasil simulasi dengan PMF teoretis.

# Parameter Binomial
n_trials <- 15
p_prob <- 0.4
m_sim <- 1000  # Jumlah simulasi

# Set seed agar hasil simulasi dapat direproduksi
set.seed(123)

# 1. Mambangkitkan data simulasi
samp_binom <- rbinom(m_sim, size = n_trials, prob = p_prob)

# 2. Menghitung PMF teoretis
x_binom <- 0:n_trials
pmf_binom_teoretis <- dbinom(x_binom, size = n_trials, prob = p_prob)

# 3. Menampilkan Perbandingan Grafik (Histogram Simulasi vs PMF Teoretis)
# Membuat histogram peluang (prob=TRUE agar total area/proporsi = 1)
hist(samp_binom, breaks = seq(-0.5, n_trials + 0.5, by = 1), prob = TRUE,
     main = "Bandingan Simulasi vs PMF Teoretis Binomial(n=15, p=0.4)",
     xlab = "k (jumlah sukses)", ylab = "Peluang / Frekuensi Relatif",
     col = "lightblue", border = "white")

# Menambahkan titik/garis PMF teoretis di atas histogram
lines(x_binom, pmf_binom_teoretis, type = "h", lwd = 3, col = "red")
points(x_binom, pmf_binom_teoretis, pch = 16, col = "red")

# Menambahkan legenda
legend("topright", legend = c("Simulasi (1.000 x)", "Teoretis (PMF)"),
       fill = c("lightblue", NA), border = c("white", NA),
       lty = c(NA, 1), lwd = c(NA, 3), col = c(NA, "red"), pch = c(NA, 16))