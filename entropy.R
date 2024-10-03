# Set Windows fonts (optional, uncomment to use)
# windowsFonts(f1 = windowsFont("Constantia"),
#              f2 = windowsFont("Book Antiqua"),
#              f3 = windowsFont("Cambria"),
#              f4 = windowsFont("Segoe UI Symbol"))
# par(family="f2")

# Amino acid alphabet
AA <- c("L", "I", "M", "V", "F", "A", "Y", "W", "K", "R", "H", "E", "D", "Q", "N", "T", "C", "S", "P", "G", "-")

# Define a reduced alphabet
redalph <- list(
  h = c("L", "I", "M", "V", "A"),
  a = c("F", "Y", "W"),
  b = c("K", "R", "H"),
  n = c("E", "D"),
  p = c("Q", "N"),
  o = c("T", "C", "S"),
  t = c("P", "G"),
  d = c("-")
)

ral <- names(redalph)  # Reduced alphabet names

# Function to calculate Shannon entropy
shanent <- function(x) {
  s <- unlist(strsplit(x, ""))
  l <- length(s)
  p <- table(s) / l
  H <- -sum(sapply(p, function(x) x * log2(x)))
  return(H)
}

# Function for Shannon entropy with binomial distribution
shanent.b <- function(x) {
  s <- unlist(strsplit(x, ""))
  l1 <- length(s)
  freq <- table(s)
  prob <- freq / l1
  p <- qbinom(p = prob, size = l1, prob = prob) / l1
  p[which(p == 0)] <- prob[which(p == 0)]
  H <- -sum(p * log2(p))
  return(H)
}

# Function to produce a random string from an alphabet
randstr <- function(len, alph) {
  paste(alph[round(runif(len, min = 1, max = length(alph)))], collapse = "")
}

# Function to convert a string to a reduced alphabet
inredalph <- function(s, redalph) {
  s <- unlist(strsplit(s, ""))
  paste(sapply(s, function(y) names(which(sapply(redalph, function(x) y %in% x)))), collapse = "")
}

# Read sequence data from a file
test <- read_table2("~/file.seqrows", col_names = FALSE)

# Make alignment to matrix
aln_nam <- test$X1
seq_dat <- test$X2
seq_dat <- strsplit(seq_dat, "")
m <- length(seq_dat)
n <- length(seq_dat[[1]])
seq_dat <- matrix(data = unlist(seq_dat), ncol = n, nrow = m, byrow = TRUE)

# Calculate random background entropies
n <- nrow(seq_dat)
mrredent <- mean(replicate(100, shanent(randstr(n, ral))))
mrent <- mean(replicate(100, shanent(randstr(n, AA))))
rtot <- mrredent + mrent

# Entropies in two alphabets
H2 <- sapply(1:ncol(seq_dat), function(x) shanent(paste(seq_dat[, x], collapse = "")))
H1 <- sapply(1:ncol(seq_dat), function(x) shanent(inredalph(seq_dat[, x], redalph)))
indx <- (H1 + H2) / rtot  # Normalized index

# Calculate binomial entropies (optional)
H2b <- sapply(1:ncol(seq_dat), function(x) shanent.b(paste(seq_dat[, x], collapse = "")))
H1b <- sapply(1:ncol(seq_dat), function(x) shanent.b(inredalph(seq_dat[, x], redalph)))

# Consensus sequence (3 most frequent residues)
consen <- sapply(1:length(seq_dat[1, ]), function(x) paste(names(tail(sort(table(seq_dat[, x])), 3)), collapse = ""))

# PSSM calculations
backmod <- table(seq_dat) / length(seq_dat)  # Background model per position
backmod <- backmod[order(match(names(backmod), AA))]
backmod <- backmod[names(backmod) != "X"]  # Remove unwanted character (if present)

# Initialize PSSM arrays
pssm <- array(data = 0, dim = c(21, length(seq_dat[1, ])))  # PSSM array
pssm.lo <- pssm  # Log odds PSSM
pssm.e <- pssm  # Kullback-Leibler entropy
rownames(pssm) <- AA
rownames(pssm.lo) <- AA
rownames(pssm.e) <- AA

# Calculate PSSM values
te <- sapply(1:length(seq_dat[1, ]), function(x) table(seq_dat[, x]) / n)
te <- lapply(te, function(x) x[names(x) != "X"])  # Remove unwanted character (if present)

# Update PSSM with counts and background model
for (j in 1:length(pssm[1, ])) {
  pssm[match(names(te[[j]]), AA), j] <- te[[j]]
  pssm[, j] <- ((n - 1) * pssm[, j] + backmod) / n
  pssm.lo[, j] <- -log(pssm[, j] / (1 - pssm[, j]))  # Log odds PSSM
  pssm.e[, j] <- pssm[, j] * log2(pssm[, j] / backmod)  # Kullback-Leibler entropy
  pssm[, j] <- -log2(pssm[, j] / backmod)  # Regular PSSM
}

# Empirical bounds of conservation
cbounds <- array(data = NA, dim = c(12, 3))
colnames(cbounds) <- c("H1", "H2", "indx")

# Calculate bounds using random strings from reduced alphabets
te <- paste(sapply(redalph[c("a", "h")], function(x) randstr(n / 2, x)), collapse = "")
cbounds[1, ] <- c(shanent(inredalph(te, redalph)), shanent(te), (shanent(inredalph(te, redalph)) + shanent(te)) / rtot)

# Repeat for other combinations
te <- paste(sapply(redalph[c("p", "o")], function(x) randstr(n / 2, x)), collapse = "")
cbounds[2, ] <- c(shanent(inredalph(te, redalph)), shanent(te), (shanent(inredalph(te, redalph)) + shanent(te)) / rtot)

te <- paste(sapply(redalph[c("p", "n")], function(x) randstr(n / 2, x)), collapse = "")
cbounds[3, ] <- c(shanent(inredalph(te, redalph)), shanent(te), (shanent(inredalph(te, redalph)) + shanent(te)) / rtot)

te <- paste(sapply(redalph[c("n", "a", "h")], function(x) randstr(n / 3, x)), collapse = "")
cbounds[4, ] <- c(shanent(inredalph(te, redalph)), shanent(te), (shanent(inredalph(te, redalph)) + shanent(te)) / rtot)

te <- paste(sapply(redalph[c("b", "a", "h")], function(x) randstr(n / 3, x)), collapse = "")
cbounds[5, ] <- c(shanent(inredalph(te, redalph)), shanent(te), (shanent(inredalph(te, redalph)) + shanent(te)) / rtot)

te <- paste(sapply(redalph[c("n", "b")], function(x) randstr(n / 3, x)), collapse = "")
cbounds[6, ] <- c(shanent(inredalph(te, redalph)), shanent(te), (shanent(inredalph(te, redalph)) + shanent(te)) / rtot)

te <- paste(sapply(redalph[c("b")], function(x) randstr(n, x)), collapse = "")
cbounds[7, ] <- c(shanent(inredalph(te, redalph)), shanent(te), (shanent(inredalph(te, redalph)) + shanent(te)) / rtot)

te <- paste(sapply(redalph[c("a")], function(x) randstr(n / 2, x)), collapse = "")
cbounds[8, ] <- c(shanent(inredalph(te, redalph)), shanent(te), (shanent(inredalph(te, redalph)) + shanent(te)) / rtot)

te <- paste(sapply(redalph[c("h")], function(x) randstr(n / 2, x)), collapse = "")
cbounds[9, ] <- c(shanent(inredalph(te, redalph)), shanent(te), (shanent(inredalph(te, redalph)) + shanent(te)) / rtot)

te <- paste(sapply(redalph[c("n")], function(x) randstr(n / 2, x)), collapse = "")
cbounds[10, ] <- c(shanent(inredalph(te, redalph)), shanent(te), (shanent(inredalph(te, redalph)) + shanent(te)) / rtot)

te <- paste(sapply(redalph[c("t")], function(x) randstr(n / 3, x)), collapse = "")
cbounds[11, ] <- c(shanent(inredalph(te, redalph)), shanent(te), (shanent(inredalph(te, redalph)) + shanent(te)) / rtot)

te <- paste(sapply(redalph[c("o")], function(x) randstr(n, x)), collapse = "")
cbounds[12, ] <- c(shanent(inredalph(te, redalph)), shanent(te), (shanent(inredalph(te, redalph)) + shanent(te)) / rtot)

# Set row names for cbounds
rownames(cbounds) <- c("a+h", "p+o", "p+n", "n+a+h", "b+a+h", "n+b", "b", "a", "h", "n", "t", "o")

# Stacked plot normalized entropies
par(pty = "m", mar = c(3, 2, 1, 1))  # Set plot margins
barplot(rbind(H1 / rtot, H2 / rtot), 
        names.arg = paste(sprintf("%3s", consen), sprintf("%04d", c(1:length(seq_dat[1, ])), sep = ""), sep = ""), 
        col = c("lightblue", "gray"), 
        las = 2)
abline(h = c(mean(cbounds[1:3, 3]), mean(cbounds[4:6, 3]), mean(cbounds[7:12, 3])), col = "gray30", lwd = 2, lty = 2)
box()

# Plot bidirectional entropy
cl2 <- round(((H2 - min(H2)) / (max(H2) - min(H2)) * 4 + 1), 0)  # Color index for H2
cl1 <- round(((H1 - min(H1)) / (max(H1) - min(H1)) * 2 + 1), 0)  # Color index for H1

# Define color palettes
col2 <- brewer.pal(5, "YlOrRd")  # Color palette for H2
col1 <- brewer.pal(3, "Blues")    # Color palette for H1

# Plot PSSM and entropy values
par(pty = "m", mar = c(2, 2, 1, 1), mgp = c(1, .5, 0))
barplot(-H1, col = col1[cl1], ylim = c(-3, 4), 
        names.arg = paste(sprintf("%3s", consen), sprintf("%04d", c(1:length(seq_dat[1, ])), sep = ""), sep = ""), 
        main = "TM7.merged1", las = 2, cex.names = .3, family = "f2")
barplot(H2, add = TRUE, axes = FALSE, col = col2[cl2])
abline(h = -cbounds[c("a+h", "n+a+h"), "H1"], col = "gray30", lwd = 2, lty = 2)
abline(h = cbounds[c("a+h", "n+b", "b+a+h", "t"), "H2"], col = "gray30", lwd = 2, lty = 2)
abline(h = 0, col = "black")  # Add horizontal line at zero
box()

# Save plot as PNG file
dev.copy(png, file = "~/plot.png", height = 6, width = 12, units = "in", res = 300)
dev.off()
