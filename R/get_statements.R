


get_statements <- function(symbol, year, fmpkey){
  
  #symbol <- "ECL"
  #year <- 2024
  #fmpkey <- "gZGlUkYsnnPTy7mrJ4zJnCMoAjHGC3x3"
  
  y <- base::paste0(
    "https://financialmodelingprep.com/stable/financial-reports-json?symbol=",
    symbol, "&year=",year,"&period=FY&apikey=", fmpkey
  ) |>
    rvest::read_html() |>
    rvest::html_text() |>
    jsonlite::fromJSON()
  
  z <- y[stringr::str_detect(tolower(names(y)), "consolidated")]
  
  for (i in base::seq_len(base::length(z))){
    tmp <- z[[i]] |>
      tidyr::pivot_longer(cols = dplyr::everything()) |>
      dplyr::mutate(keep = purrr::map_lgl(value, function(x){
        !base::is.null(x) & base::length(x) > 1
      })) |>
      dplyr::filter(keep == TRUE) |>
      dplyr::select(-keep)
    
    tmp$value[[1]] <- lubridate::mdy(tmp$value[[1]])
    tmp <- tmp |>
      dplyr::mutate(value = purrr::map(value, function(x){
        x |> base::as.character() |>
          base::as.data.frame() |>
          t() |>
          base::as.data.frame()
      })) |>
      tidyr::unnest(value)
    
    base::names(tmp) <- base::as.character(base::unlist(tmp[1,]))
    tmp <- tmp[-1,]
    for (j in 2:base::length(tmp)) tmp[,j] <- base::as.numeric(base::unlist(tmp[,j]))
    z[[i]] <- tmp
  }
  
}