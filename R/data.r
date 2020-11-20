##---- poststrat ----
#' poststrat
#'
#' Population counts by strata of sex, age, region.
#' Use insee pop counts as of 1/1/2020
#'
#' @format A data frame of 324 categories of 4 variables:
#' \describe{
#'   \item{code}{INSEE code of region (18)}
#'   \item{sex}{Sex: "Homme" "Femme"}
#'   \item{age}{Age class: "0-9"   "10-19" "20-29" "30-39" "40-49" "50-59" "60-69" "70-79" "80+"}
#'   \item{n}{Population size}
#'   }
#' @source \url{https://www.insee.fr/fr/statistiques/1893198}
"poststrat"

##---- code_region ----
#' code_region
#' 
#' Code and labels of French region 
#' (Code officiel géographique au 1er janvier 2016)
#'
#' @format A data frame of 324 categories of 4 variables:
#' \describe{
#'   \item{REGION}{code region INSEE (18)}
#'   \item{CHEFLIEU}{Code de la commune chef-lieu}
#'   \item{TNCC}{Type de nom (article)}
#'   \item{NCC}{Nom en clair (majuscules)}
#'   \item{NCCENR}{Nom en clair (typographie riche)}
#'   \item{reg_spf, REG_SPF1, REG_SPF2, reg_spf2}{Variations on region labels with lower/upper case and accents}
#'   \item{abb}{abbreviated labels}
#'   }
#' @source \url{https://www.data.gouv.fr/fr/datasets/r/6585c878-4e2f-4ceb-a219-75f6631d1f14}
"code_region"
