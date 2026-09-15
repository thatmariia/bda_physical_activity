# Feature functions (frequency domain)

#' Compute the mean frequency of a signal given its frequency and spectrum
#' @param freq A numeric vector of frequencies corresponding to the spectrum
#' @param spec A numeric vector representing the spectrum (power spectral density) of the signal
#' @return The mean frequency of the signal
mean_frequency <- function(freq, spec) {
  delta_f <- freq[2] - freq[1] # gap size between frequencies
  normalizing_constant <- sum(spec * delta_f) # ≈ ∫S(f)df
  mean_freq <- sum(freq * spec * delta_f) / normalizing_constant
  return(mean_freq)
}

#' Extract frequency domain features from a spectrum data frame,
#' which is assumed to be already segmented into epochs
#' @param spectrum_df A data frame containing the spectrum data with columns: epoch, freq, spec1, spec2, ...
#' @return A data frame containing the extracted frequency domain features for each epoch
get_frequency_domain_features <- function(spectrum_df) {
  userfreqdom <- spectrum_df %>%
    group_by(epoch) %>%
    summarise(
      # Dominant frequency of signal 1
      domfreq1 = freq[which.max(spec1)],

      # Mean frequency of signal 2
      meanfreq2 = mean_frequency(freq, spec2),

      # Example of other possible features
      # (you can define your own functions like before)
      # spectral_entropy = ...
      # bandpower_low   = ...
      # bandpower_high  = ...
  )
}