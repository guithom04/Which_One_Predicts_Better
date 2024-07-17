balanced <- function(df){
  df %>% 
    mutate_all(function(col) {
      if(all(is.na(col))) {
        return(col)
      } else if(any(is.na(col))) {
        col <- na.approx(col, na.rm = FALSE) # interpola as séries
        fit <- auto.arima(na.omit(col)) # omite NA's antes de fitar
        col[is.na(col)] <- forecast(fit, h = sum(is.na(col)))$mean
      }
      return(col)
    })
}
balanced = as_tibble(balanced(head))
tail(balanced)
