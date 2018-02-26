parse_month_year <- function (input) {
  # input as an integer, such as 200310 --> year = 2003 and moth = 10
  month <- input %% 100
  year <- floor(input / 100)
  c(month, year)
}

merge_datasets <- function(x,y)
{
  merge(x,y, by=intersect(names(x), names(y)))
}

accuracy_score <- function (y_true, y_pred)  {
  # calculate metric
  1 - sum (abs(y_true - y_pred)) / sum (y_true)
}

recommendation_score <- function (sku1, sku2, sku_true) {
  score <- 0
  if (is.element(sku1, sku_true)) {
    score <- score + 0.5
  }
  if (is.element(sku2, sku_true)) {
    score <- score + 0.5
  }
  score
}

library(stringr)
numextract <- function(string){ 
  as.numeric(str_extract(string, "\\-*\\d+\\.*\\d*"))
} 