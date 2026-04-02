fmpkey <- "gZGlUkYsnnPTy7mrJ4zJnCMoAjHGC3x3"
start <- "2022-01-01"
end <- "2025-12-31"






htmlpath <- "C:/Users/nicol/Dropbox/4-Projects/Financial analysis/collect_sec_data/data/html"
collectedhtml <- base::list.files(htmlpath, full.names = FALSE, pattern = ".html$") |>
  stringr::str_remove_all(".html$")


csvpath <- "C:/Users/nicol/Dropbox/4-Projects/Financial analysis/collect_sec_data/data/csv"
collectedcsv <- base::list.files(csvpath, full.names = FALSE, pattern = ".csv$") |>
  stringr::str_remove_all(".csv$")






