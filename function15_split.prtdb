###########################################################################
###                NOWCASTING: WHICH ONE PREDICTS BETTER                ###
###                    LARGE FUNCTION 6: split.prtdb                    ###
###########################################################################
split.prtdb <- function(df, mode = c("1s","ts")){
  library(dplyr)
  # balance panel
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
  df.b <- balanced(df)
  if (mode == "1s") {
    filtered.in  = df.b[1:(nrow(df.b)-3),]
    filtered.out = df.b[-c(1:(nrow(df.b)-3)),]
  }
  if (mode == "ts") {
    filtered.in  = df.b %>%
                      filter(date <= "2019-12-01")
    filtered.out  = df.b %>%
      filter(date > "2019-12-01")
  }
  dfs <- list(df_in  = filtered.in,
              df_out = filtered.out)
  return(dfs)
}
# Usage
# split_prtdb.1s = split.prtdb(prtdb, mode = "1s")
# split_prtdb.ts = split.prtdb(prtdb, mode = "ts")
