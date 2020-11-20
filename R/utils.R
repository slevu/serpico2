##---- unfactor ----
unfactorDataFrame <- function( x ) {
  x <- data.frame( lapply(x, as.character), stringsAsFactors = FALSE)
  x <- data.frame( lapply(x, utils::type.convert, as.is = TRUE),
                   stringsAsFactors = FALSE)
  return(x)
}

##---- df_to_utf8 ----
df_to_utf8 <- function(d){
  d <- unfactorDataFrame(d)
  d[,] <- lapply(d, function(x){
    if (class(x) == "character"){
      Encoding(x) <- "latin1"
      iconv(x, "latin1", "UTF-8")
    } else x
  } )
  d
}

##---- guess encoding ----
if(FALSE){
  readr::guess_encoding(FN, n_max = 1000)
}
