#Week 5 Class work

# Test data frame
df <- data.frame(A = c('T', 'T', 'T', 'T', 'T', 'T', 'F', 'F', 'F', 'T'), 
                 B = c('T', 'T', 'T', 'F', 'T', 'F', 'F', 'F', 'F', 'T'), 
                 Class = c('+', '+', '+', '-', '+', '-', '-', '-', '-', '+'))

# Calculate frequency and entropy 
freqsA <- table(df$A)/length(df$A)
freqsB <- table(df$B)/length(df$B)
entA <- -sum(freqs * log2(freqs))
entB <- -sum(freqsB * log2(freqsB))

# Information gain
igAT <- -(5/7)*log2(5/7) -(2/7)*log2(2/7)
igAF <- 0

ig <- 1 - (.7 * igAT + .3 * igAF)
ig
