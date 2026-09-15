# Feature functions (frequency domain)

mean_frequency <- function(freq, spec) {
  delta_f <- freq[2] - freq[1] # gap size between frequencies
  normalizing_constant <- sum(spec * delta_f) # ≈ ∫S(f)df
  mean_freq <- sum(freq * spec * delta_f) / normalizing_constant
  return(mean_freq)
}

#' Convert a signal data frame into a segmented frequency domain representation
#' @param signal_df A data frame containing the signal data
#' @param n_samples_per_epoch The number of samples per epoch (default is 128, corresponding to 2.56 seconds at 50 Hz)
#' @param sample_rate The sample rate of the signal in Hz (default is 50 Hz)
#' @return A data frame containing the frequency domain representation for each epoch,
#' including the frequency bins and corresponding spectral densities for each signal.
convert_signal_to_spectrum <- function(signal_df, n_samples_per_epoch = 128, sample_rate = 50) {
  spectrum_df <- signal_df |>
    mutate(epoch = sampleid %/% n_samples_per_epoch) |>
    reframe(
      {
        sp <- spectrum(cbind(X1, X2, X3), span = 15, plot = FALSE)
        tibble(
          freq = sp$freq, # cycles per sample
          freq_hz = sp$freq * sample_rate, # cycles per second (Hz)
          spec1 = sp$spec[, 1],
          spec2 = sp$spec[, 2],
          spec3 = sp$spec[, 3]
        )
      },
      .by = epoch
    )
  return(spectrum_df)
}