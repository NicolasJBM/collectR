#' @name get_esg_sustainalytics
#' @title Retreive ESG data
#' @author Nicolas Mangin
#' @description Retreive Sustainalytics' ESG scores from Yahoo Finance.
#' @param companies Tibble List of companies with one column called "ticker"
#' @return Tibble with ESG scores and areas of involvement
#' @importFrom anytime anydate
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr mutate
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr user_agent
#' @importFrom robotstxt paths_allowed
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
#' @importFrom rvest html_text
#' @importFrom stringr str_replace_all
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr unnest
#' @importFrom urltools domain
#' @importFrom urltools url_parse
#' @export


get_esg_sustainalytics <- function(companies = NA){

  conAreas <- NULL

  base::stopifnot(!base::is.na(companies))

  # Source: https://www.kylerudden.com/blog/scraping-esg-scores/
  parse_esg_data <- function(xpath, xmldoc = page.i) {
    x <- xmldoc |>
      rvest::html_nodes(xpath = xpath) |>
      rvest::html_text(trim = TRUE)
    if (base::length(x) == 0 & xpath == '//*[@id="Col1-0-Sustainability-Proxy"]/section/div[2]/div[2]/div[2]/div/div[2]/div[1]/span/span/span') {
      return("None")
    }
    if (base::grepl("% AUM", x)) {
      return(base::as.numeric(base::sub("% AUM", "", base::sub("based on ", "", x))) / 100)
    }
    if (!base::grepl("\\d", x)) {
      return(base::trimws(x))
    } else {
      if (base::grepl("percentile", x)) {
        return(x |> stringr::str_replace_all("[^0-9\\.]", "") |> base::as.numeric() / 100)
      } else {
        if (base::grepl("updated on", x)) {
          r <- base::sub("Last updated on ", "", x)
          r <- base::paste(base::unlist(base::strsplit(r, "/"))[2], base::unlist(base::strsplit(r, "/"))[1], sep = "-")
          return(anytime::anydate(r))
        } else {
          return(base::as.numeric(x))
        }
      }
    }
  }

  get_involvement_areas <- function() {
    page.i |>
      rvest::html_nodes(xpath = '//*[@id="Col2-3-InvolvementAreas-Proxy"]/section/table') |>
      rvest::html_table() |>
      base::data.frame() |>
      dplyr::mutate(Significant.Involvement = dplyr::case_when(
        Significant.Involvement == "Yes" ~ 1,
        TRUE ~ 0
      )) |>
      tidyr::pivot_wider(names_from = "Products.and.Activities", values_from = "Significant.Involvement")
  }

  check_availability <- function(url = link.i) {
    base_url <- base::paste0(urltools::url_parse(url)$scheme, "://", urltools::domain(url))
    robotstxt::paths_allowed(
      paths = base::sub(base_url, "", link.i),
      domain = urltools::domain(url),
      bot = "*"
    )
  }

  var_agent <- "Nicolas Mangin (njbmangin@outlook.com). Collecting data for teaching purposes."
  nbrtickers <- base::nrow(companies)
  esg_data <- base::list(nbrtickers)

  for (i in base::seq_len(nbrtickers)) {
    base::message(base::paste0(i, " of ", base::nrow(companies)))
    tryCatch({
      tick.i <- companies$ticker[i]
      link.i <- base::paste0("https://finance.yahoo.com/quote/", tick.i, "/sustainability")
      bots.i <- base::suppressMessages(check_availability(link.i))
      if (bots.i) {
        base::Sys.sleep(stats::runif(1, 0.5, 5.0))
        page.i <- httr::GET(link.i, httr::user_agent(var_agent)) |> httr::content()
        esg_data[[i]] <- tibble::tibble(
          ticker = tick.i,
          SA_ESG = parse_esg_data('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[1]/div/div[2]/div[1]'),
          SA_E = parse_esg_data('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[2]/div/div[2]/div[1]'),
          SA_S = parse_esg_data('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[3]/div/div[2]/div[1]'),
          SA_G = parse_esg_data('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[1]/div/div[4]/div/div[2]/div[1]'),
          SA_CL =  parse_esg_data('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[2]/div[2]/div[2]/div/div[2]/div[1]/span/span/span'),
          conAreas = base::list(get_involvement_areas()),
          date = parse_esg_data('//*[@id="Col1-0-Sustainability-Proxy"]/section/div[3]/span[2]/span')
        ) |>
          tidyr::unnest(conAreas)
      }
    }, error=function(e){})
  }

  esg_data <- dplyr::bind_rows(esg_data) |>
    dplyr::mutate(date = dplyr::case_when(
      base::is.na(SA_ESG) ~ NA,
      TRUE ~ date
    ))

  return(esg_data)
}
