

## todo
## make population breakdown in prevalence according to risk
## (918 numbers of infected over population from poststrat)
## see MRP tutorial
##
## introduce probability of "response" varying by age, sex, region
## voir Blanchoz 2018, fig 7
## 58 % des femmes ont réalisé une analyse contre 42 % des hommes
## c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
## from webdigitizer
if (FALSE){
  femme <- read.csv2("data-raw/blanchoz_fig7_femmes.csv")
  homme <- read.csv2("data-raw/blanchoz_fig7_hommes.csv")
  femme$cage <- cut(femme[,1], breaks = c( seq(0, 80, 10), 200 ), right = FALSE)
  f <- tapply(femme[,2], list(femme[,3]), mean)
  homme$cage <- cut(homme[,1], breaks = c( seq(0, 80, 10), 200 ), right = FALSE)
  h <- tapply(homme[,2], list(homme[,3]), mean)
  # recours <- data.frame(Homme = c(0.16, 0.18, 0.24, 0.30, 0.45, 0.60, 0.76, 0.85, 0.70),
  #                       Femme = c(0.15, 0.30, 0.54, 0.60, 0.62, 0.67, 0.72, 0.83, 0.65) )
  a <- data.frame(h, f)
  # a <- recours # / colSums(recours)
  poststrat <- readRDS("data/poststrat.rds")
  b <- tapply(poststrat$n, list(poststrat$age, poststrat$sex), function(x) sum(x))# / sum(poststrat$n))
  colSums(a * b) / colSums(b)
  # h        f
  # 44.69958 57.72409
  
  poststrat2 <- data.frame( lapply(poststrat[poststrat$code != "6",], as.numeric) )
}
