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
      stringr::str_detect(base::tolower(label), "invest|market|treasur|note|financ|discontin") ~ "CIA",
      stringr::str_detect(base::tolower(label), "other") ~ "OCOA",
      TRUE ~ "OCOA"
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
      stringr::str_detect(base::tolower(label), "other") ~ "ONCOA",
      stringr::str_detect(base::tolower(label), "depr|amort") ~ "PPE",
      TRUE ~ "ONCOA"
    )
  } else if (section == "CL") {
    id <- dplyr::case_when(
      stringr::str_detect(base::tolower(label), "(?=.*account)(?=.*payable)") ~ "AP",
      stringr::str_detect(base::tolower(label), "(?=.*supplier)(?=.*payable)") ~ "AP",
      stringr::str_detect(base::tolower(label), "salar|employ|compens|payroll|benefits") ~ "ESP",
      stringr::str_detect(base::tolower(label), "pension|retire") ~ "CP.PL",
      stringr::str_detect(base::tolower(label), "unearn|deposit|revenue|loyalty|customer|advance|sale") ~ "UDR",
      stringr::str_detect(base::tolower(label), "interest") ~ "IP",
      stringr::str_detect(base::tolower(label), "tax") ~ "TP",
      stringr::str_detect(base::tolower(label), "(?=.*leas)(?=.*operat)") ~ "CP.OLL",
      stringr::str_detect(base::tolower(label), "(?=.*leas)(?=.*financ)") ~ "CP.FLL",
      stringr::str_detect(base::tolower(label), "(?=.*leas)(?=.*capita)") ~ "CP.FLL",
      stringr::str_detect(base::tolower(label), "leas") ~ "CP.FLL",
      stringr::str_detect(base::tolower(label), "dividend") ~ "DP",
      stringr::str_detect(base::tolower(label), "(?=.*current)(?=.*mortgage)") ~ "CP.MRTG",
      stringr::str_detect(base::tolower(label), "(?=.*current)(?=.*debt)") ~ "CP.LTD",
      stringr::str_detect(base::tolower(label), "(?=.*current)(?=.*loan)") ~ "CP.LTD",
      stringr::str_detect(base::tolower(label), "(?=.*current)(?=.*borrow)") ~ "CP.LTD",
      stringr::str_detect(base::tolower(label), "(?=.*current)(?=.*note)") ~ "CP.LTD",
      stringr::str_detect(base::tolower(label), "(?=.*current)(?=.*bond)") ~ "CP.BOND",
      stringr::str_detect(base::tolower(label), "note|loan") ~ "STD",
      stringr::str_detect(base::tolower(label), "(?=.*short)(?=.*debt)") ~ "STD",
      stringr::str_detect(base::tolower(label), "(?=.*short)(?=.*loan)") ~ "STD",
      stringr::str_detect(base::tolower(label), "(?=.*short)(?=.*borrow)") ~ "STD",
      stringr::str_detect(base::tolower(label), "(?=.*short)(?=.*note)") ~ "STD",
      stringr::str_detect(base::tolower(label), "(?=.*commercial)(?=.*paper)") ~ "STD",
      stringr::str_detect(base::tolower(label), "(?=.*credit)(?=.*facility)") ~ "STD",
      stringr::str_detect(base::tolower(label), "debt") ~ "STD",
      stringr::str_detect(base::tolower(label), "payable") ~ "AP",
      stringr::str_detect(base::tolower(label), "liabil|warrant|accrued|expense|cost") ~ "OAL",
      stringr::str_detect(base::tolower(label), "advance|sale") ~ "UDR",
      stringr::str_detect(base::tolower(label), "borrow") ~ "STD",
      stringr::str_detect(base::tolower(label), "financial") ~ "STD",
      stringr::str_detect(base::tolower(label), "credit") ~ "STD",
      stringr::str_detect(base::tolower(label), "controllling") ~ "OCFL",
      stringr::str_detect(base::tolower(label), "loyalty") ~ "UDR",
      stringr::str_detect(base::tolower(label), "deferred") ~ "UDR",
      stringr::str_detect(base::tolower(label), "accrued") ~ "OAL",
      stringr::str_detect(base::tolower(label), "accrual") ~ "OAL",
      stringr::str_detect(base::tolower(label), "obligation") ~ "OCFL",
      stringr::str_detect(base::tolower(label), "marketing|selling") ~ "OAL",
      stringr::str_detect(base::tolower(label), "parties") ~ "OCOL",
      stringr::str_detect(base::tolower(label), "client") ~ "UDR",
      stringr::str_detect(base::tolower(label), "benefit|compensation|incentive") ~ "ESP",
      stringr::str_detect(base::tolower(label), "government") ~ "OCOL",
      stringr::str_detect(base::tolower(label), "derivative") ~ "OCOL",
      stringr::str_detect(base::tolower(label), "revenue") ~ "UDR",
      stringr::str_detect(base::tolower(label), "manager") ~ "ESP",
      stringr::str_detect(base::tolower(label), "insurance") ~ "OCOL",
      stringr::str_detect(base::tolower(label), "trade") ~ "OCOL",
      stringr::str_detect(base::tolower(label), "liabilit") ~ "OCOL",
      stringr::str_detect(base::tolower(label), "other") ~ "OCOL",
      TRUE ~ "OCFL"
    )
  } else if (section == "NCL") {
    id <- dplyr::case_when(
      stringr::str_detect(base::tolower(label), "mortgage") ~ "MRTG",
      stringr::str_detect(base::tolower(label), "bond") ~ "BOND",
      stringr::str_detect(base::tolower(label), "(?=.*financ)(?=.*oblig)") ~ "LTD",
      stringr::str_detect(base::tolower(label), "(?=.*leas)(?=.*operat)") ~ "OLL",
      stringr::str_detect(base::tolower(label), "(?=.*leas)(?=.*financ)") ~ "FLL",
      stringr::str_detect(base::tolower(label), "(?=.*leas)(?=.*capita)") ~ "FLL",
      stringr::str_detect(base::tolower(label), "leas") ~ "FLL",
      stringr::str_detect(base::tolower(label), "tax") ~ "NCTP",
      stringr::str_detect(base::tolower(label), "employ|pension|retire") ~ "PL",
      stringr::str_detect(base::tolower(label), "debt|loan|borrow|note") ~ "LTD",
      stringr::str_detect(base::tolower(label), "borrow") ~ "LTD",
      stringr::str_detect(base::tolower(label), "financial") ~ "ONCFL",
      stringr::str_detect(base::tolower(label), "credit") ~ "LTD",
      stringr::str_detect(base::tolower(label), "controllling") ~ "ONCFL",
      stringr::str_detect(base::tolower(label), "payable") ~ "ONCOL",
      stringr::str_detect(base::tolower(label), "loyalty") ~ "ONCOL",
      stringr::str_detect(base::tolower(label), "deferred") ~ "ONCOL",
      stringr::str_detect(base::tolower(label), "accrued") ~ "ONCOL",
      stringr::str_detect(base::tolower(label), "accrual") ~ "ONCOL",
      stringr::str_detect(base::tolower(label), "obligation") ~ "ONCFL",
      stringr::str_detect(base::tolower(label), "marketing|selling") ~ "ONCOL",
      stringr::str_detect(base::tolower(label), "parties") ~ "ONCOL",
      stringr::str_detect(base::tolower(label), "client") ~ "ONCOL",
      stringr::str_detect(base::tolower(label), "benefit|compensation|incentive") ~ "ONCOL",
      stringr::str_detect(base::tolower(label), "government") ~ "ONCOL",
      stringr::str_detect(base::tolower(label), "derivative") ~ "ONCOL",
      stringr::str_detect(base::tolower(label), "revenue") ~ "ONCOL",
      stringr::str_detect(base::tolower(label), "manager") ~ "ONCOL",
      stringr::str_detect(base::tolower(label), "insurance") ~ "ONCOL",
      stringr::str_detect(base::tolower(label), "trade") ~ "ONCOL",
      stringr::str_detect(base::tolower(label), "liabilit") ~ "ONCOL",
      stringr::str_detect(base::tolower(label), "other") ~ "ONCOL",
      TRUE ~ "ONCFL"
    )
  } else if (section == "SE") {
    id <- dplyr::case_when(
      stringr::str_detect(base::tolower(label), "^prefer") ~ "PRFSTK",
      stringr::str_detect(base::tolower(label), "^common") ~ "COMSTK",
      stringr::str_detect(base::tolower(label), "treasur") ~ "TRSTK",
      stringr::str_detect(base::tolower(label), "(?=.*repurch)(?=.*stock)") ~ "TRSTK",
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
      TRUE ~ "OTHEQ"
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
      TRUE ~ "ONOGL"
    )
  } else if (section == "INT") {
    id <- "INT"
  } else if (section == "TAX") {
    id <- "TAX"
  } else if (section == "CFOA") {
    id <- dplyr::case_when(
      stringr::str_detect(base::tolower(label), "(?=.*(gain|loss))(?=.*(invest|dispos|sale|impair|divest|discontin|propert|asset|other))") ~ "CNCL.NOGL",
      stringr::str_detect(base::tolower(label), "(?=.*(gain|loss))(?=.*(debt|financ|equity|credit|foreign|currency|deriv|secur|warrant))") ~ "CNCL.NOGL",
      stringr::str_detect(base::tolower(label), "(?=.*(gain|loss))(?=.*(subsid|unreal|cumul|pension|impair|franchis|transac))") ~ "CNCL.NOGL",
      stringr::str_detect(base::tolower(label), "(?=.*profit)(?=.*shar)") ~ "CNCL.NOGL",
      stringr::str_detect(base::tolower(label), "^net .?(income|loss|earning)") ~ "NI.CFS",
      stringr::str_detect(base::tolower(label), "profit") ~ "NI.CFS",
      stringr::str_detect(base::tolower(label), "(?=.*defer)(?=.*revenue)") ~ "CHG.UDR",
      stringr::str_detect(base::tolower(label), "unearn") ~ "CHG.UDR",
      stringr::str_detect(base::tolower(label), "uncollect") ~ "CHG.AR",
      stringr::str_detect(base::tolower(label), "(?=.*non)(?=.*cash)") ~ "CNCL.NCT",
      stringr::str_detect(base::tolower(label), "depr|amort") ~ "CNCL.DEPR",
      stringr::str_detect(base::tolower(label), "impair|write-off") ~ "CNCL.IMPAIR",
      stringr::str_detect(base::tolower(label), "dispos") ~ "CNCL.AD",
      stringr::str_detect(base::tolower(label), "derivative|fair") ~ "CNCL.NOGL",
      stringr::str_detect(base::tolower(label), "(?=.*non-current)(?=.*asset)(?=.*liab)") ~ "CHG.NCOL",
      stringr::str_detect(base::tolower(label), "leas") ~ "CHG.NCOL",
      stringr::str_detect(base::tolower(label), "(?=.*asset)(?=.*liab)") ~ "CHG.NFWC",
      stringr::str_detect(base::tolower(label), "tax") ~ "CHG.TP",
      stringr::str_detect(base::tolower(label), "non-trade") ~ "CHG.NFWC",
      stringr::str_detect(base::tolower(label), "receivable") ~ "CHG.AR",
      stringr::str_detect(base::tolower(label), "customer|loyal|unearn") ~ "CHG.UDR",
      stringr::str_detect(base::tolower(label), "invent|merch|product|material") ~ "CHG.INV",
      stringr::str_detect(base::tolower(label), "paid|defer") ~ "CHG.PDE",
      stringr::str_detect(base::tolower(label), "account|suppl") ~ "CHG.AP",
      stringr::str_detect(base::tolower(label), "employe|pension|retire") ~ "CHG.PL",
      stringr::str_detect(base::tolower(label), "tax|content") ~ "CHG.NFWC",
      stringr::str_detect(base::tolower(label), "asset") ~ "CHG.COA",
      stringr::str_detect(base::tolower(label), "accrued") ~ "CHG.OAL",
      stringr::str_detect(base::tolower(label), "leas") ~ "CHG.OLL",
      stringr::str_detect(base::tolower(label), "(?=.*long)(?=.*term)(?=.*liab)") ~ "CHG.NCOL",
      stringr::str_detect(base::tolower(label), "liabil|expense|charge|pension|retire|") ~ "CHG.COL",
      stringr::str_detect(base::tolower(label), "stock") ~ "CNCL.NOGL",
      TRUE ~ "OCFOA"
    )
  } else if (section == "CFIA") {
    id <- dplyr::case_when(
      stringr::str_detect(base::tolower(label), "acqui|business") ~ "NET.CHG.GW",
      stringr::str_detect(base::tolower(label), "nonmarket|equity") ~ "CHG.NCIA",
      stringr::str_detect(base::tolower(label), "market|secur") ~ "CHG.CIA",
      stringr::str_detect(base::tolower(label), "(?=.*short)(?=.*invest)") ~ "CHG.CIA",
      stringr::str_detect(base::tolower(label), "invest") ~ "CHG.NCIA",
      stringr::str_detect(base::tolower(label), "intang") ~ "NET.CHG.IA",
      stringr::str_detect(base::tolower(label), "(?=.*cap)(?=.*expend)") ~ "NET.CHG.PPE",
      stringr::str_detect(base::tolower(label), "propert|plant|equip") ~ "NET.CHG.PPE",
      stringr::str_detect(base::tolower(label), "dispos|sale") ~ "NET.CHG.PPE",
      stringr::str_detect(base::tolower(label), "LAND") ~ "NET.CHG.LAND",
      TRUE ~ "OCFIA"
    )
  } else if (section == "CFFA") {
    id <- dplyr::case_when(
      stringr::str_detect(base::tolower(label), "employee|option|award|compens") ~ "OCFFA",
      stringr::str_detect(base::tolower(label), "dividend") ~ "PAIDIV",
      stringr::str_detect(base::tolower(label), "leas") ~ "CHG.FLL",
      stringr::str_detect(base::tolower(label), "credit") ~ "CHG.STD",
      stringr::str_detect(base::tolower(label), "matur") ~ "CHG.STD",
      stringr::str_detect(base::tolower(label), "(?=.*short)(?=.*debt)") ~ "CHG.STD",
      stringr::str_detect(base::tolower(label), "(?=.*short)(?=.*repay)") ~ "CHG.STD",
      stringr::str_detect(base::tolower(label), "(?=.*short)(?=.*reimb)") ~ "CHG.STD",
      stringr::str_detect(base::tolower(label), "(?=.*short)(?=.*borrow)") ~ "CHG.STD",
      stringr::str_detect(base::tolower(label), "note|commercial") ~ "CHG.STD",
      stringr::str_detect(base::tolower(label), "mortgage") ~ "CHG.MRTG",
      stringr::str_detect(base::tolower(label), "bond") ~ "CHG.BOND",
      stringr::str_detect(base::tolower(label), "borrow|reimb|repay|debt") ~ "CHG.LTD",
      stringr::str_detect(base::tolower(label), "share|stock") ~ "CHG.PIC",
      stringr::str_detect(base::tolower(label), "control|minority") ~ "CHG.NCI",
      TRUE ~ "OCFFA"
    )
  } else if (section == "OCF") {
    id <- dplyr::case_when(
      stringr::str_detect(base::tolower(label), "foreign") ~ "EOER",
      stringr::str_detect(base::tolower(label), "exchange") ~ "EOER",
      stringr::str_detect(base::tolower(label), "currency") ~ "EOER",
      stringr::str_detect(base::tolower(label), "forex") ~ "EOER",
      TRUE ~ "OCFC"
    )
  } else if (section == "ADDINFO"){
    id <- dplyr::case_when(
      stringr::str_detect(base::tolower(label), "(?=.*interest)(?=.*[pay|pai])") ~ "PAIDINT",
      stringr::str_detect(base::tolower(label), "(?=.*tax)(?=.*[pay|pai])") ~ "PAIDTAX",
      TRUE ~ ""
    )
  }
  return(id)
}
