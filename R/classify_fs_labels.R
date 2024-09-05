#' @name classify_fs_labels
#' @title Classify labels in financial statements
#' @author Nicolas Mangin
#' @description Assign an account ID to financial statements accounts based on a preliminary classification and the terms appearing in the label.
#' @param label Character. Label to classify.
#' @param section Character. Preliminary classification of the label.
#' @return Character. ID for the account.
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect
#' @export


classify_fs_labels <- function(label, section){
  if (base::is.na(section) | section == "") {
    id <- ""
  } else if (section == "CA") {
    id <- dplyr::case_when(
      stringr::str_detect(base::tolower(label), "cash") ~ "CASH",
      stringr::str_detect(base::tolower(label), "tax") ~ "CDT",
      stringr::str_detect(base::tolower(label), "(?=.*account)(?=.*receiv)") ~ "AR",
      stringr::str_detect(base::tolower(label), "invest|market|treasur|note|financ|discontin") ~ "CIA",
      stringr::str_detect(base::tolower(label), "for sale") ~ "CIA",
      stringr::str_detect(base::tolower(label), "non-trade") ~ "CIA",
      stringr::str_detect(base::tolower(label), "material|leaf") ~ "RM",
      stringr::str_detect(base::tolower(label), "merchandis") ~ "MERCH",
      stringr::str_detect(base::tolower(label), "product") ~ "FP",
      stringr::str_detect(base::tolower(label), "(?=.*work)(?=.*process)") ~ "WIP",
      stringr::str_detect(base::tolower(label), "(?=.*work)(?=.*progress)") ~ "WIP",
      stringr::str_detect(base::tolower(label), "supplies|fuel") ~ "SUPPL",
      stringr::str_detect(base::tolower(label), "inventor") ~ "INV",
      stringr::str_detect(base::tolower(label), "prepaid|defer|advance") ~ "PDE",
      stringr::str_detect(base::tolower(label), "customer|receivable") ~ "AR",
      stringr::str_detect(base::tolower(label), "other") ~ "COA",
      TRUE ~ "COA"
    )
  } else if (section == "NCA") {
    id <- dplyr::case_when(
      stringr::str_detect(base::tolower(label), "cash") ~ "CASH",
      stringr::str_detect(base::tolower(label), "leas") ~ "ALTL",
      stringr::str_detect(base::tolower(label), "goodwill") ~ "GW",
      stringr::str_detect(base::tolower(label), "tax") ~ "NCDT",
      stringr::str_detect(base::tolower(label), "land") ~ "LAND",
      stringr::str_detect(base::tolower(label), "intangi|trademark|patent|softw|franchise|licen|content|techno|brand|digital") ~ "IA",
      stringr::str_detect(base::tolower(label), "propert|plant|equip|vehicle|facilities|building|system|construct|furtniture|fixture") ~ "PPE",
      stringr::str_detect(base::tolower(label), "invest|equity|market|financ|note|discontin|loan") ~ "NCIA",
      stringr::str_detect(base::tolower(label), "for sale") ~ "NCIA",
      stringr::str_detect(base::tolower(label), "other") ~ "NCOA",
      stringr::str_detect(base::tolower(label), "depr|amort") ~ "PPE",
      TRUE ~ "NCOA"
    )
  } else if (section == "CL") {
    id <- dplyr::case_when(
      stringr::str_detect(base::tolower(label), "(?=.*account)(?=.*payable)") ~ "AP",
      stringr::str_detect(base::tolower(label), "(?=.*supplier)(?=.*payable)") ~ "AP",
      stringr::str_detect(base::tolower(label), "salar|employ|compens|payroll|benefits") ~ "ESP",
      stringr::str_detect(base::tolower(label), "pension|retire") ~ "CP_PL",
      stringr::str_detect(base::tolower(label), "unearn|deposit|revenue|loyalty|customer|advance|sale") ~ "UDR",
      stringr::str_detect(base::tolower(label), "interest") ~ "IP",
      stringr::str_detect(base::tolower(label), "tax") ~ "TP",
      stringr::str_detect(base::tolower(label), "(?=.*leas)(?=.*operat)") ~ "CP_OLL",
      stringr::str_detect(base::tolower(label), "(?=.*leas)(?=.*financ)") ~ "CP_FLL",
      stringr::str_detect(base::tolower(label), "(?=.*leas)(?=.*capita)") ~ "CP_FLL",
      stringr::str_detect(base::tolower(label), "leas") ~ "CP_OLL",
      stringr::str_detect(base::tolower(label), "dividend") ~ "DP",
      stringr::str_detect(base::tolower(label), "(?=.*current)(?=.*mortgage)") ~ "CP_MRTG",
      stringr::str_detect(base::tolower(label), "(?=.*current)(?=.*debt)") ~ "CP_LTD",
      stringr::str_detect(base::tolower(label), "(?=.*current)(?=.*loan)") ~ "CP_LTD",
      stringr::str_detect(base::tolower(label), "(?=.*current)(?=.*borrow)") ~ "CP_LTD",
      stringr::str_detect(base::tolower(label), "(?=.*current)(?=.*note)") ~ "CP_LTD",
      stringr::str_detect(base::tolower(label), "(?=.*current)(?=.*bond)") ~ "CP_BOND",
      stringr::str_detect(base::tolower(label), "note|loan") ~ "STD",
      stringr::str_detect(base::tolower(label), "(?=.*short)(?=.*debt)") ~ "STD",
      stringr::str_detect(base::tolower(label), "(?=.*short)(?=.*loan)") ~ "STD",
      stringr::str_detect(base::tolower(label), "(?=.*short)(?=.*borrow)") ~ "STD",
      stringr::str_detect(base::tolower(label), "(?=.*short)(?=.*note)") ~ "STD",
      stringr::str_detect(base::tolower(label), "(?=.*commercial)(?=.*paper)") ~ "STD",
      stringr::str_detect(base::tolower(label), "(?=.*credit)(?=.*facility)") ~ "STD",
      stringr::str_detect(base::tolower(label), "debt") ~ "STD",
      stringr::str_detect(base::tolower(label), "liabil|warrant|accrued|expense|cost") ~ "OAL",
      stringr::str_detect(base::tolower(label), "advance|sale") ~ "UDR",
      TRUE ~ "COL"
    )
  } else if (section == "NCL") {
    id <- dplyr::case_when(
      stringr::str_detect(base::tolower(label), "mortgage") ~ "MRTG",
      stringr::str_detect(base::tolower(label), "bond") ~ "BOND",
      stringr::str_detect(base::tolower(label), "(?=.*financ)(?=.*oblig)") ~ "LTD",
      stringr::str_detect(base::tolower(label), "(?=.*leas)(?=.*operat)") ~ "OLL",
      stringr::str_detect(base::tolower(label), "(?=.*leas)(?=.*financ)") ~ "FLL",
      stringr::str_detect(base::tolower(label), "(?=.*leas)(?=.*capita)") ~ "FLL",
      stringr::str_detect(base::tolower(label), "tax") ~ "NCTP",
      stringr::str_detect(base::tolower(label), "employ|pension|retire") ~ "PL",
      stringr::str_detect(base::tolower(label), "debt|loan|borrow|note") ~ "LTD",
      TRUE ~ "NCOL"
    )
  } else if (section == "SE") {
    id <- dplyr::case_when(
      stringr::str_detect(base::tolower(label), "treasur") ~ "TRSTK",
      stringr::str_detect(base::tolower(label), "(?=.*repurch)(?=.*stock)") ~ "RE",
      stringr::str_detect(base::tolower(label), "(?=.*other)(?=.*comprehensive)") ~ "OCIL",
      stringr::str_detect(base::tolower(label), "(?=.*earning)(?=.*retain)") ~ "RE",
      stringr::str_detect(base::tolower(label), "(?=.*earning)(?=.*employ)") ~ "RE",
      stringr::str_detect(base::tolower(label), "(?=.*earning)(?=.*reinvest)") ~ "RE",
      stringr::str_detect(base::tolower(label), "accumulated deficit") ~ "RE",
      stringr::str_detect(base::tolower(label), "prefer") ~ "PRFSTK",
      stringr::str_detect(base::tolower(label), "common") ~ "COMSTK",
      stringr::str_detect(base::tolower(label), "paid|capital") ~ "APIC",
      stringr::str_detect(base::tolower(label), "accumul") ~ "OCIL",
      stringr::str_detect(base::tolower(label), "unreali") ~ "OCIL",
      stringr::str_detect(base::tolower(label), "control|minority") ~ "NCI",
      stringr::str_detect(base::tolower(label), "share|stock") ~ "COMSTK",
      stringr::str_detect(base::tolower(label), "earning") ~ "RE",
      TRUE ~ "SE"
    )
  } else if (section == "REV") {
    id <- "REV"
  } else if (section == "COS") {
    id <- "COS"
  } else if (section == "OPEX") {
    id <- dplyr::case_when(
      stringr::str_detect(base::tolower(label), "(sell|sale)(?=.*admin)") ~ "SGNA",
      stringr::str_detect(base::tolower(label), "sell|market|advert") ~ "SME",
      stringr::str_detect(base::tolower(label), "research|develop") ~ "RND",
      stringr::str_detect(base::tolower(label), "admin") ~ "ADM",
      TRUE ~ "SGNA"
    )
  } else if (section == "DEPR") {
    id <- "DEPR"
  } else if (section %in% c("LOSS","GAIN")) {
    id <- dplyr::case_when(
      stringr::str_detect(base::tolower(label), "dispos") ~ "GLAD",
      stringr::str_detect(base::tolower(label), "discont") ~ "GLAD",
      stringr::str_detect(base::tolower(label), "impai") ~ "IMPAIR",
      stringr::str_detect(base::tolower(label), "interest") ~ "FINGAIN",
      stringr::str_detect(base::tolower(label), "tax") ~ "TAXGAIN",
      TRUE ~ "NOGL"
    )
  } else if (section == "INT") {
    id <- "INT"
  } else if (section == "TAX") {
    id <- "TAX"
  } else if (section == "CFOA") {
    id <- dplyr::case_when(
      stringr::str_detect(base::tolower(label), "net income") ~ "NI_CFS",
      stringr::str_detect(base::tolower(label), "net earning") ~ "NI_CFS",
      stringr::str_detect(base::tolower(label), "(?=.*defer)(?=.*revenue)") ~ "CHG_UDR",
      stringr::str_detect(base::tolower(label), "unearn") ~ "CHG_UDR",
      stringr::str_detect(base::tolower(label), "uncollect") ~ "CHG_AR",
      stringr::str_detect(base::tolower(label), "(?=.*non)(?=.*cash)") ~ "CNCL_NCT",
      stringr::str_detect(base::tolower(label), "depr|amort") ~ "CNCL_DEPR",
      stringr::str_detect(base::tolower(label), "impair|write-off") ~ "CNCL_IMPAIR",
      stringr::str_detect(base::tolower(label), "dispos") ~ "CNCL_AD",
      stringr::str_detect(base::tolower(label), "derivative|fair|gain|loss") ~ "CNCL_NOGL",
      stringr::str_detect(base::tolower(label), "(?=.*non-current)(?=.*asset)(?=.*liab)") ~ "CHG_NCOL",
      stringr::str_detect(base::tolower(label), "leas") ~ "CHG_NCOL",
      stringr::str_detect(base::tolower(label), "(?=.*asset)(?=.*liab)") ~ "CHG_NFWC",
      stringr::str_detect(base::tolower(label), "tax") ~ "CHG_TP",
      stringr::str_detect(base::tolower(label), "non-trade") ~ "CHG_NFWC",
      stringr::str_detect(base::tolower(label), "receivable") ~ "CHG_AR",
      stringr::str_detect(base::tolower(label), "customer|loyal|unearn") ~ "CHG_UDR",
      stringr::str_detect(base::tolower(label), "invent|merch|product|material") ~ "CHG_INV",
      stringr::str_detect(base::tolower(label), "paid|defer") ~ "CHG_PDE",
      stringr::str_detect(base::tolower(label), "account|suppl") ~ "CHG_AP",
      stringr::str_detect(base::tolower(label), "employe|pension|retire") ~ "CHG_PL",
      stringr::str_detect(base::tolower(label), "tax|content") ~ "CHG_NFWC",
      stringr::str_detect(base::tolower(label), "asset") ~ "CHG_COA",
      stringr::str_detect(base::tolower(label), "accrued") ~ "CHG_OAL",
      stringr::str_detect(base::tolower(label), "leas") ~ "CHG_OLL",
      stringr::str_detect(base::tolower(label), "(?=.*long)(?=.*term)(?=.*liab)") ~ "CHG_NCOL",
      stringr::str_detect(base::tolower(label), "liabil|expense|charge|employe|pension|retire|") ~ "CHG_COL",
      TRUE ~ "RECON_NICFO"
    )
  } else if (section == "CFIA") {
    id <- dplyr::case_when(
      stringr::str_detect(base::tolower(label), "acqui|business") ~ "NET_CHG_GW",
      stringr::str_detect(base::tolower(label), "nonmarket|equity") ~ "CHG_NCIA",
      stringr::str_detect(base::tolower(label), "market|secur") ~ "CHG_CIA",
      stringr::str_detect(base::tolower(label), "(?=.*short)(?=.*invest)") ~ "CHG_CIA",
      stringr::str_detect(base::tolower(label), "invest") ~ "CHG_NCIA",
      stringr::str_detect(base::tolower(label), "intang") ~ "NET_CHG_IA",
      stringr::str_detect(base::tolower(label), "(?=.*cap)(?=.*expend)") ~ "NET_CHG_PPE",
      stringr::str_detect(base::tolower(label), "propert|plant|equip") ~ "NET_CHG_PPE",
      stringr::str_detect(base::tolower(label), "dispos|sale") ~ "NET_CHG_PPE",
      stringr::str_detect(base::tolower(label), "LAND") ~ "NET_CHG_LAND",
      TRUE ~ "CFIA"
    )
  } else if (section == "CFFA") {
    id <- dplyr::case_when(
      stringr::str_detect(base::tolower(label), "employee|option|award|compens") ~ "CFFA",
      stringr::str_detect(base::tolower(label), "dividend") ~ "PAIDIV",
      stringr::str_detect(base::tolower(label), "leas") ~ "CHG_FLL",
      stringr::str_detect(base::tolower(label), "credit") ~ "CHG_STD",
      stringr::str_detect(base::tolower(label), "matur") ~ "CHG_STD",
      stringr::str_detect(base::tolower(label), "(?=.*short)(?=.*debt)") ~ "CHG_STD",
      stringr::str_detect(base::tolower(label), "(?=.*short)(?=.*repay)") ~ "CHG_STD",
      stringr::str_detect(base::tolower(label), "(?=.*short)(?=.*reimb)") ~ "CHG_STD",
      stringr::str_detect(base::tolower(label), "(?=.*short)(?=.*borrow)") ~ "CHG_STD",
      stringr::str_detect(base::tolower(label), "note|commercial") ~ "CHG_STD",
      stringr::str_detect(base::tolower(label), "mortgage") ~ "CHG_MRTG",
      stringr::str_detect(base::tolower(label), "bond") ~ "CHG_BOND",
      stringr::str_detect(base::tolower(label), "borrow|reimb|repay|debt") ~ "CHG_LTD",
      stringr::str_detect(base::tolower(label), "share|stock") ~ "CHG_PIC",
      stringr::str_detect(base::tolower(label), "control|minority") ~ "CHG_NCI",
      TRUE ~ "CFFA"
    )
  } else if (section == "OCF") {
    id <- "OCF"
  } else if (section == "ADDINFO"){
    id <- dplyr::case_when(
      stringr::str_detect(base::tolower(label), "(?=.*interest)(?=.*[pay|pai])") ~ "PAIDINT",
      stringr::str_detect(base::tolower(label), "(?=.*tax)(?=.*[pay|pai])") ~ "PAIDTAX",
      TRUE ~ ""
    )
  }
  return(id)
}
