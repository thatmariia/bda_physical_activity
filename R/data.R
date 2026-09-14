# Helper functions

#' Create a data frame with signal data and corresponding activity labels
#' @param exp_id The experiment ID (integer)
#' @param user_id The user ID (integer)
#' @param signal_data A data frame containing the signal data
#' @param sample_labels A data frame containing the sample labels with columns: userid, trial, sampleid, act_code, activity, segment
#' @return A data frame combining the signal data with the corresponding activity labels
get_signal_df <- function(exp_id, user_id, signal_data, sample_labels) {
  user_df <-
    tibble(userid = user_id, trial = exp_id, sampleid = seq.int(0, nrow(signal_data) - 1)) |>
    bind_cols(signal_data) |>
    left_join(sample_labels, by = c("userid", "trial", "sampleid"))
  return(user_df)
}

#' Get the file path for a specific sensor type, experiment ID, and user ID
#' @param data_dir The directory where the data files are located
#' @param sensor_type The type of sensor (e.g., "acc", "gyro")
#' @param exp_id The experiment ID (integer)
#' @param user_id The user ID (integer)
#' @return The constructed file path as a string
get_file_path <- function(data_dir, sensor_type, exp_id, user_id) {
  # Construct the filename based on the provided parameters
  exp_id_str <- sprintf("%02d", exp_id)
  user_id_str <- sprintf("%02d", user_id)
  file_path <- file.path(data_dir, paste0(sensor_type, "_exp", exp_id_str, "_user", user_id_str, ".txt"))
  return(file_path)
}

#' Load signal data from a file
#' @param file_path The path to the signal file
#' @return A data frame containing the signal data
load_signal <- function(file_path) {
  signal_data <- read_delim(file_path, delim = " ", col_names = FALSE, col_types = "ddd")
  return(signal_data)
}

most_common_value <- function(x) {
  counts <- table(x, useNA = "no")
  most_frequent <- which.max(counts)
  return(names(most_frequent))
}
