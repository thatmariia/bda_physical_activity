
#' Apply Gaussian smoothing to a numeric vector
#' @param x A numeric vector to be smoothed
#' @param sigma The standard deviation of the Gaussian kernel (default is 2)
#' @return A numeric vector containing the smoothed values
gaussian_smooth <- function(x, sigma = 2) {
  radius <- ceiling(3 * sigma)
  positions <- -radius:radius

  weights <- exp(-(positions^2) / (2 * sigma^2))
  weights <- weights / sum(weights)

  smooth_x <- stats::filter(x, filter = weights, sides = 2)
  return(smooth_x)
}

#   apply(
#     x,
#     2,
#     \(column) stats::filter(
#       column,
#       filter = weights,
#       sides = 2
#     )
#   )
# }

#' Compute the spectrum of a signal and optionally apply Gaussian smoothing
#' @param signal A numeric vector representing the signal
#' @param gaussian_sigma The standard deviation of the Gaussian kernel for smoothing (default is 3; set to 0 to disable smoothing)
#' @return A data frame containing the frequency bins and corresponding spectral densities of the signal
compute_spectrum <- function(signal, gaussian_sigma = 3) {
  # Compute the spectrum of the signal
  spec <- spectrum(signal, plot = FALSE)
  spec_df <- data.frame(freq = spec$freq, spec = spec$spec)
  if (gaussian_sigma <= 0) {
    return(spec_df)
  }
  spec_df$spec <- gaussian_smooth(spec$spec, sigma = gaussian_sigma)
  return(spec_df)
}