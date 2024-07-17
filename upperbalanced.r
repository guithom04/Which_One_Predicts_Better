upperbalance <- function(df) {
  df <- df[, colSums(is.na(df)) < nrow(df)]
  first_valid_indices <- apply(df, 2, function(col) which(!is.na(col))[1])
  latest_index <- max(first_valid_indices, na.rm = TRUE)
  df_cut <- df[latest_index:nrow(df), ]
  return(df_cut)
}
head = upperbalance(df)
head(head)
tail(head)
