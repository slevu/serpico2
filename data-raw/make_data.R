if (TRUE){
  ##---- population counts ----
  ## population counts by stratification sex * age * region
  ## use insee pop counts, as of 1/1/2020
  ## https://www.insee.fr/fr/statistiques/1893198
  # https://www.data.gouv.fr/fr/datasets/r/6585c878-4e2f-4ceb-a219-75f6631d1f14
  
  ##- files
  FNPOP <- system.file("extdata/estim-pop-nreg-sexe-aq-1975-2020.xls", 
                       package = "serpico2", mustWork = TRUE)
  FNCODE <- system.file("extdata/code_region_spf.csv",
                        package = "serpico2", mustWork = TRUE)
  
  {
    ##---- utils
    ageband <- function(mi = 0, ma = 95, n = 5){
      a <- seq(mi, ma, n)
      b <- c(paste0("-", a[-1]-1), "+")
      print(length(a))
      paste0(a, b)
    }
    # ageband(0, 80, 10)
    ##- regroup age bands
    reagba <- function(df, mi = 0, ma = 80, n = 10){
      x <-  ageband(mi, ma, n)
      lapply(setNames(1:length(x), x), function(i){
        if (i < length(x)){
          k <- c(i*2-1, i*2)} else {
            k <- (i*2-1):ncol(df)
          }
        rowSums(df[,k])
      })
    }
  }

  ##- region
  code_region <- read.csv(FNCODE, stringsAsFactors = FALSE)
  code_region <- df_to_utf8(code_region)

  ##- pop
  insee <- as.data.frame(readxl::read_excel(FNPOP,
                                            sheet = "2020", range = "A5:BL26") )
  ## "A5:V26" ensemble , hommes : "W5:AQ26" , femmes : "AR5:BL26"
  
  ## get rid of subtotal
  insee <- insee[ -grep("France métro|DOM", insee[,1]),
                  -grep("Total", colnames(insee))]
  ## check ensemble = hommes + femmes
  identical( sum(insee[, 2:21]), sum(insee[, 22:61]) ) # 67063703
  insee[,1] <- toupper(gsub("-", " ", iconv(insee[,1],
                                            "UTF-8", "ASCII//TRANSLIT//IGNORE")))
  # colnames(insee)[1] <- "REG"
  .i <- data.frame(rep(insee[,1], 2), # reg
                   factor(rep(1:2, each = nrow(insee)),
                          levels = 1:2,
                          labels = c("Male", "Female")), # sex
                   rbind(as.matrix(insee[, 22:41]),
                         as.matrix(insee[, 42:61]) )
  )
  # str(.i)
  pop0 <- data.frame(region = .i[,1],
                     sex = .i[,2],
                     reagba(df = .i[,-(1:2)],
                            mi = 0, ma = 80, n = 10),
                     stringsAsFactors = TRUE,
                     check.names = FALSE  )
  # sum(pop0[,-(1:2)]) # 67063703
  pop0$code <- factor( code_region$REGION[ match(pop0[,1],code_region$REG_SPF2) ] )
  # expand.grid(pop0[,-1])
  poststrat <- reshape2::melt(pop0[,-1],
                              id.vars = c("code", "sex"),
                              variable.name = "age",
                              value.name = "n")
  
  ## add abbreviated regions
  # code_region <- readRD S("data/code_region.rds")
  abb <- c(GUA = 1, MAR = 2, GUY = 3, RUN = 4, MAY = 6, IDF = 11, CVL = 24,
           BFC = 27, NOR = 28, HDF = 32, GES = 44, PDL = 52, BRE = 53, NAQ = 75,
           OCC = 76, ARA = 84, PAC = 93, COR = 94)
  code_region$abb <- names(abb)[order(abb)]
  
  ##- save
  save(poststrat, file = "data/poststrat.rda") # 9 * 2 *18 = 324 cells
  # usethis::use_data(poststrat, code_region, overwrite = TRUE)
}
