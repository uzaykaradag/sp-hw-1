# 3. Reading the file into R
a <- scan("4300-0.txt", what = "character", skip = 73, nlines = 32858-73)
a <- gsub("_(", "", a, fixed = TRUE)

###

split_punct <- function(words, punct) {
  # Escape the punctuation if it's a special character in regex
  punct_escaped <- gsub("([.\\|()[{^$*+?])", "\\\\\\1", punct)

  # Identify which words contain the punctuation
  indices <- grep(punct_escaped, words)

  # Split the words containing the punctuation
  split_words <- strsplit(words[indices], punct_escaped)

  # Reconstruct the vector
  new_words <- rep(NA, length(words) + length(indices))
  j <- 1
  for (i in seq_along(words)) {
    if (i %in% indices) {
      new_words[j] <- split_words[[which(indices == i)]][1]
      new_words[j + 1] <- punct
      j <- j + 2
    } else {
      new_words[j] <- words[i]
      j <- j + 1
    }
  }

  return(new_words)
}

punctuations <- c(",", ".", ";", "!", ":", "?")
for (punct in punctuations) {
  a <- split_punct(a, punct)
}

###

a_lower <- tolower(a)
unique_words <- unique(a_lower)

indices <- match(a_lower, unique_words)

word_counts <- tabulate(indices, length(unique_words))
names(word_counts) <- unique_words

word_counts_sorted <- sort(word_counts, decreasing=TRUE)
threshold <- word_counts_sorted[1000]

b <- names(word_counts[word_counts >= threshold])

###

b_indices <- match(a_lower, b)

# Shift the index vectors
index_1 <- b_indices[1:(length(b_indices)-2)]
index_2 <- b_indices[2:(length(b_indices)-1)]
index_3 <- b_indices[3:length(b_indices)]

# Combine into a matrix
triplet_matrix <- cbind(index_1, index_2, index_3)

# Identify rows without NA
valid_triplets <- rowSums(is.na(triplet_matrix)) == 0

# Filter the matrix
T <- triplet_matrix[valid_triplets, ]

# Shift the index vectors for pairs
pair_index_1 <- b_indices[-length(b_indices)]
pair_index_2 <- b_indices[-1]

# Combine into a matrix
pair_matrix <- cbind(pair_index_1, pair_index_2)

# Identify rows without NA for pairs
valid_pairs <- rowSums(is.na(pair_matrix)) == 0

# Filter the matrix for pairs
P <- pair_matrix[valid_pairs, ]

###
# Get the words that start with a capital letter
capital_words <- a[grepl("^[A-Z]", a)]

# Convert to lowercase to match with 'b'
capital_words_lower <- tolower(capital_words)

# Count the occurrences of each word in 'capital_words_lower' and in 'a_lower'
capital_counts <- table(capital_words_lower)
total_counts <- table(a_lower)

# Ensure that both tables have the same names
all_words <- union(names(capital_counts), names(total_counts))
capital_counts <- capital_counts[all_words]
total_counts <- total_counts[all_words]

# Replace NAs with 0
capital_counts[is.na(capital_counts)] <- 0
total_counts[is.na(total_counts)] <- 0

# Identify words that most often start with a capital letter
often_capitalized <- names(which(capital_counts / total_counts > 0.5))

# Create a modified version of 'b' for printing
b_print <- ifelse(b %in% often_capitalized, tools::toTitleCase(b), b)

###

simulate_words <- function(T, P, b, b_print, n=50) {
  # Initialize the word sequence with two random words
  k <- sample(seq_along(b), 2, replace=TRUE)
  simulated_words <- b_print[k]

  for (i in 3:n) {
    # Extract sub-matrix from T
    sub_matrix <- T[T[,1] == k[i-2] & T[,2] == k[i-1], , drop=FALSE]

    # If sub-matrix has no rows, use P
    if (nrow(sub_matrix) == 0) {
      sub_matrix <- P[P[,1] == k[i-1], , drop=FALSE]
      if (nrow(sub_matrix) == 0) {
        # If still no rows, simulate according to common word frequencies
        next_word_index <- sample(seq_along(b), 1, prob=table(b_indices)/length(b_indices), replace=TRUE)
      } else {
        next_word_index <- sample(sub_matrix[,2], 1)
      }
    } else {
      next_word_index <- sample(sub_matrix[,3], 1)
    }

    # If next_word_index is NA, replace with a random word from b
    if (is.na(next_word_index)) {
      next_word_index <- sample(seq_along(b), 1)
    }

    # Append the next word to the sequence
    simulated_words <- c(simulated_words, b_print[next_word_index])
    k <- c(k, next_word_index)
  }

  # Print the simulated word sequence
  print(cat(simulated_words, sep=" "))
  print("\n\n\n")
}


simulate_words(T, P, b, b_print)


###

simulate_words_by_frequency <- function(b, b_indices, b_print, n=50) {
  # Calculate word frequencies
  word_frequencies <- table(b_indices) / length(b_indices)

  # Simulate words based on their frequencies
  simulated_indices <- sample(seq_along(b), n, replace=TRUE, prob=word_frequencies)

  # Check for NA values and replace them with a random index from b
  simulated_indices[is.na(simulated_indices)] <- sample(seq_along(b), sum(is.na(simulated_indices)), replace=TRUE)

  simulated_words <- b_print[simulated_indices]

  # Print the simulated word sequence
  print(cat(simulated_words, sep=" "))
  print("\n")
}


# Test the function
simulate_words_by_frequency(b, b_indices, b_print)


###