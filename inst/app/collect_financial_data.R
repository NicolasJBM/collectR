
collect_financial_data <- shiny::shinyApp(

  # INTERFACE ##################################################################

  ui = shinydashboardPlus::dashboardPage(

    options = base::list(sidebarExpandOnHover = TRUE),

    # HEADER ###################################################################

    header = shinydashboardPlus::dashboardHeader(
      fixed = FALSE,
      leftUi = shiny::tagList(

        shiny::tags$button(
          id = "exit", type = "button", class = "btn action-button",
          onclick = "setTimeout(function(){window.close();},100);",
          style = "background-color:#660033;color:#FFF;width:250px;",
          shiny::icon("power-off"),
          shiny::span("Exit", title = "Exit the application without saving your unsaved work.")
        )

      )
    ),

    # SIDEBAR ##################################################################

    # Menus are only displayed if the corresponding header switch is TRUE
    sidebar = shinydashboardPlus::dashboardSidebar(
      minified = TRUE, collapsed = TRUE, width = 230,
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          shiny::span("overview", title = "Check the financial data already collected and classified."),
          tabName = "Overview", icon = shiny::icon("eye")
        ),

        shinydashboard::menuItem(
          shiny::span("statements", title = "Retrieve, classify and export financial statements"),
          tabName = "Financial statements", icon = shiny::icon("table")
        ),

        shinydashboard::menuItem(
          shiny::span("market", title = "Retrieve and export market data."),
          tabName = "Market data", icon = shiny::icon("chart-line")
        )

      )
    ),

    # BODY #####################################################################

    body = shinydashboard::dashboardBody(
      shiny::tags$head(
        shiny::tags$head(
          shiny::tags$style(shiny::HTML('
            .jstree-rename-input {
              max-height:25px !important;
            }
          ')),
          shiny::tags$style(".modal-dialog{width:90%}")
        )
      ),

      shiny::tags$script(shiny::HTML("$('body').addClass('fixed');")),

      shinydashboard::tabItems(

        # Tooling ##############################################################

        shinydashboard::tabItem(
          tabName = "overview",  shiny::tags$br(),
          shiny::fluidRow(
            shiny::column(1, shiny::textInput("afterdate", "After date", value = "2020-01-01"))
            # allow filter per company, data siource, and year
            # see collected and uncollected data: IS, BS, CFS, and market data, per year (separate files)
          )
        ),

        shinydashboard::tabItem(
          tabName = "statements",  shiny::tags$br(),
          shiny::fluidRow(
            shiny::column(
              4,
              # Select data source
              # Define year
              
            ),
            shiny::column(
              8,
              shiny::textOutput("tables_found"),
              shiny::fluidRow(
                shiny::column(
                  4,
                  shinyWidgets::prettyCheckbox("rightorder", "Decreasing years", value = TRUE)
                ),
                shiny::column(
                  4,
                  shiny::numericInput("units", "Units:", value = 1000000)
                ),
                shiny::column(
                  4,
                  shiny::selectInput(
                    "slctcontent", "Select the statement:",
                    choices = c("BS","IS","CFS")
                  )
                )
              ),
              DT::dataTableOutput("display_statement")
            )
          )
        ),

        shinydashboard::tabItem(
          tabName = "market",  shiny::tags$br(),
          shiny::actionButton(
            "savecsv", "Save CSV",
            style = "background-color:#009900;color:#FFF;width:100%;margin-bottom:10px;",
            shiny::icon("floppy-disk")
          ),
          rhandsontable::rHandsontableOutput("classification"),
          shiny::fluidRow(
            shiny::column(6, DT::dataTableOutput("check_section_totals")),
            shiny::column(6, DT::dataTableOutput("check_aggregates"))
          )
        )

      )
    ),

    # CONTROLS #################################################################

    controlbar = shinydashboardPlus::dashboardControlbar(
      id = "rightsidebar", width = 200, collapsed = FALSE, overlay = FALSE,
      shinydashboardPlus::controlbarMenu(
        id = "controlbar",

        shinydashboardPlus::controlbarItem(
          title = shiny::span("Actions", title = "Actions"),
          shiny::actionButton(
            "refresh", "Refresh",
            style = "background-color:#000099;color:#FFF;width:100%;margin-bottom:10px;",
            shiny::icon("rotate")
          ),
          shiny::radioButtons("slcttype", "Type", choices = c("JSON","HTML","CSV"), selected = "CSV"),
          shiny::uiOutput("filter_symbol"),
          shiny::uiOutput("filter_date"),
          shiny::actionButton(
            "openjson", "Open JSON",
            style = "background-color:#006633;color:#FFF;width:100%;margin-bottom:10px;",
            shiny::icon("download")
          ),
          shiny::actionButton(
            "openhtml", "Open HTML",
            style = "background-color:#006633;color:#FFF;width:100%;margin-bottom:10px;",
            shiny::icon("file-code")
          ),
          shiny::actionButton(
            "getstatement", "Get statements",
            style = "background-color:#990000;color:#FFF;width:100%;margin-bottom:10px;",
            shiny::icon("upload")
          ),
          shiny::actionButton(
            "getmarket", "Get market",
            style = "background-color:#990000;color:#FFF;width:100%;margin-bottom:10px;",
            shiny::icon("upload")
          ),
          shiny::actionButton(
            "getindexes", "Get indexes",
            style = "background-color:#990000;color:#FFF;width:100%;margin-bottom:10px;",
            shiny::icon("upload")
          ),
          shiny::actionButton(
            "getrates", "Get rates",
            style = "background-color:#990000;color:#FFF;width:100%;margin-bottom:10px;",
            shiny::icon("upload")
          ),
          shiny::actionButton(
            "getcorp", "Export corporations",
            style = "background-color:#990000;color:#FFF;width:100%;margin-bottom:10px;",
            shiny::icon("upload")
          )
        )

      )
    )
  ),

  # SERVER #####################################################################

  server = function(session, input, output) {

    base::options(
      scipen = 100,
      shiny.maxRequestSize=300*1024^2
    )


    # Collection ###############################################################
    
    future::plan(strategy = "multisession", workers = 8)
    
    focus <- shiny::reactive({
      utils::read.csv("base/focus.csv")
    })

    fmpfiles <- shiny::reactive({
      
      input$refresh
      
      shinybusy::show_modal_progress_line(value = 1/8, text = "Get FMP filings")
      
      utils::read.csv("base/filings.csv") |>
        dplyr::filter(type == "10-K") |>
        dplyr::rename(filingDate = fillingDate, form = type) |>
        dplyr::select(-link) |>
        dplyr::mutate(cik = purrr::map_chr(cik, collectR::complete_cik)) |>
        dplyr::mutate(
          filingDate = lubridate::ymd(base::as.Date(filingDate)),
          acceptedDate = lubridate::ymd(base::as.Date(acceptedDate))
        )
    })
    
    companies <- shiny::reactive({
      shiny::req(!base::is.null(fmpfiles()))
      fmpfiles() |>
        dplyr::select(symbol, cik) |>
        base::unique()
    })
    
    jsonfiles <- shiny::reactive({
      
      shiny::req(!base::is.null(companies()))
      
      shinybusy::update_modal_progress(value = 2/8, text = "Get SEC filings")
      
      collectR::get_filings("data/json") |>
        dplyr::left_join(companies(), by = "cik", relationship = "many-to-many") |>
        dplyr::mutate(
          filingDate = lubridate::ymd(base::as.Date(filingDate)),
          acceptedDate = lubridate::ymd(base::as.Date(acceptedDate)),
          reportDate = lubridate::ymd(base::as.Date(reportDate))
        ) |>
          dplyr::select(symbol, cik, form, filingDate, acceptedDate, reportDate, link)
    })
    
    htmlfiles <- shiny::reactive({
      
      shiny::req(!base::is.null(jsonfiles()))
      
      shinybusy::update_modal_progress(value = 3/8, text = "List HTML files")
      
      tibble::tibble(
        paths = base::list.files("data/html", full.names = TRUE, pattern = "html$"),
        files = base::list.files("data/html", full.names = FALSE, pattern = "html$")
      ) |>
        tidyr::separate(files, into = c("cik","form","reportDate"), sep = "_") |>
        dplyr::mutate(
          reportDate = stringr::str_remove_all(reportDate, ".html"),
          reportDate = lubridate::ymd(base::as.Date(reportDate))
        ) |>
        dplyr::left_join(companies(), by = "cik", relationship = "many-to-many") |>
        dplyr::select(symbol, cik, form, reportDate, paths)
    })
    
    csvfiles <- shiny::reactive({
      
      shiny::req(!base::is.null(htmlfiles()))
      
      shinybusy::update_modal_progress(value = 4/8, text = "List csv files")
      
      checkclabal <- function(path){
        file <- path |>
          readr::read_csv(col_types = "cDcDccn")
        y <- file |>
          dplyr::select(section) |>
          dplyr::group_by(section) |>
          dplyr::count() |>
          dplyr::filter(n > 2)
        if (base::nrow(y) > 8) cla <- "Y" else cla <- "N"
        if (cla == "Y"){
          z <- file |>
            dplyr::filter(section %in% c("CA","NCA","CL","NCL","SE")) |>
            dplyr::mutate(section = dplyr::case_when(
              section %in% c("CA","NCA") ~ "A",
              TRUE ~ "LE"
            )) |>
            dplyr::group_by(section) |>
            dplyr::summarise(amount = base::sum(amount, na.rm = TRUE), .groups = "drop")
          
          if (base::round(z$amount[[1]],1) == base::round(z$amount[[2]],1))
            bal <- "Y" else bal <- "N"
        } else bal <- "N"
        tibble::tibble(classified = cla, balanced = bal)
      }
      
      tibble::tibble(
        paths = base::list.files("data/csv", full.names = TRUE, pattern = "csv$"),
        files = base::list.files("data/csv", full.names = FALSE, pattern = "csv$")
      ) |>
        dplyr::mutate(checks = purrr::map(paths, checkclabal)) |>
        tidyr::unnest(checks) |>
        tidyr::separate(files, into = c("cik","form","reportDate"), sep = "_") |>
        dplyr::mutate(
          reportDate = stringr::str_remove_all(reportDate, ".csv"),
          reportDate = lubridate::ymd(base::as.Date(reportDate))
        ) |>
        dplyr::right_join(companies(), by = "cik", relationship = "many-to-many") |>
        dplyr::select(symbol, cik, form, reportDate, paths, classified, balanced)
    })
    
    
    
    finstat <- shiny::reactive({
      
      shiny::req(!base::is.null(csvfiles()))
      
      shinybusy::update_modal_progress(value = 5/8, text = "Gather statements")
      
      keep <- csvfiles() |>
        dplyr::filter(balanced == "Y")
      
      collectR::get_fs_files("data/csv") |>
        dplyr::filter(
          !base::is.na(section),
          cik %in% base::unique(keep$cik)
        ) |>
        dplyr::left_join(companies(), by = "cik", relationship = "many-to-many") |>
        dplyr::group_by(symbol, cik, date, statement) |>
        dplyr::slice_max(reportDate) |>
        dplyr::ungroup() |>
        dplyr::select(-reportDate) |>
        dplyr::mutate(amount = dplyr::case_when(
          section == "REV" ~ amount,
          section == "COS" ~ base::abs(amount),
          section == "OPEX" ~ base::abs(amount),
          section == "DEPR" ~ base::abs(amount),
          section == "GAIN" ~ -base::abs(amount),
          section == "LOSS" ~ base::abs(amount),
          section == "INT" ~ base::abs(amount),
          section == "TAX" ~ base::abs(amount),
          TRUE ~ amount
        )) |>
        dplyr::mutate(id = furrr::future_map2_chr(label, section, collectR::classify_fs_labels)) |>
        dplyr::select(symbol, cik, date, statement, section, id, label, amount) |>
        base::unique()
    })
    
    
    totals <- shiny::reactive({
      shiny::req(!base::is.null(finstat()))
      
      shinybusy::update_modal_progress(value = 6/8, text = "Compute totals")
      
      aggregations <- readxl::read_excel("base/aggregations.xlsx")
      finstat() |>
        dplyr::left_join(aggregations, by = "id") |>
        tidyr::pivot_longer(
          cols = base::names(aggregations)[-1],
          names_to = "aggregate",
          values_to = "coeff"
        ) |>
        dplyr::filter(coeff != 0) |>
        dplyr::mutate(value = amount * coeff) |>
        dplyr::group_by(symbol, date, aggregate) |>
        dplyr::summarise(value = base::sum(value, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(value = base::round(value/1000,0)*1000) |>
        dplyr::rename(id = aggregate)
    })
    
    
    checks <- shiny::reactive({
      
      shiny::req(!base::is.null(totals()))
      
      shinybusy::update_modal_progress(value = 7/8, text = "Make additional checks")
      
      # Cash variations (BS/CFS)
      
      cashbs <- totals() |>
        dplyr::filter(id == "CASH") |>
        dplyr::select(symbol, date, value) |>
        dplyr::group_by(symbol, date) |>
        dplyr::summarise(ending = base::sum(value, na.rm = TRUE), .groups = "drop") |>
        dplyr::group_by(symbol) |>
        dplyr::mutate(
          beginning = dplyr::lag(ending),
          CASHVAR.BS = ending - beginning
        ) |>
        dplyr::ungroup() |>
        stats::na.omit()
      
      cashis <- totals() |>
        dplyr::filter(id == "CASHVAR") |>
        dplyr::select(symbol, date, value) |>
        dplyr::group_by(symbol, date) |>
        dplyr::summarise(CASHVAR.CFS = base::sum(value, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(symbol, date) |>
        stats::na.omit()
      
      cashdiff <- cashbs |>
        dplyr::full_join(cashis, by = c("symbol", "date")) |>
        dplyr::mutate(cashdiff = CASHVAR.CFS - CASHVAR.BS) |>
        dplyr::select(symbol, date, CASHVAR.CFS, CASHVAR.BS, cashdiff)
      
      base::rm(cashbs, cashis)
      
      
      # Interest (IS/CFS)
      
      intdiff <- totals() |>
        dplyr::filter(id %in% c("INT","PAIDINT","IBD")) |>
        dplyr::select(symbol, date, id, value) |>
        dplyr::group_by(symbol, date, id) |>
        dplyr::summarise(value = base::sum(value, na.rm = TRUE), .groups = "drop") |>
        tidyr::pivot_wider(names_from = "id", values_from = "value", values_fill = 0) |>
        dplyr::arrange(symbol, date) |>
        stats::na.omit() |>
        dplyr::mutate(
          intrate = base::round(100 * INT / IBD,2),
          intdiff = dplyr::case_when(
            INT == 0 & IBD == 0 ~ "Y",
            INT == 0 & IBD > 0 ~ "N",
            intrate < 0.1 ~ "N",
            intrate > 25 ~ "N",
            TRUE ~ "Y"
          )
        ) |>
        dplyr::select(symbol, date, INT, PAIDINT, IBD, intdiff, intrate)
      
      # Taxes (IS)
      
      taxdiff <- totals() |>
        dplyr::filter(id %in% c("TAX","EBT")) |>
        dplyr::select(symbol, date, id, value) |>
        dplyr::group_by(symbol, date, id) |>
        dplyr::summarise(value = base::sum(value, na.rm = TRUE), .groups = "drop") |>
        tidyr::pivot_wider(names_from = "id", values_from = "value", values_fill = 0) |>
        dplyr::arrange(symbol, date) |>
        stats::na.omit() |>
        dplyr::mutate(
          taxrate = base::round(100 * TAX / EBT, 2),
          taxdiff = dplyr::case_when(
            taxrate >= 0 & taxrate <= 100 ~ "Y",
            TRUE ~ "N"
          )
        ) |>
        dplyr::select(symbol, date, taxdiff, taxrate)
      
      
      # Net Incomes (IS/CFS)
      
      nidiff <- totals() |>
        dplyr::filter(id %in% c("NI","CFOA.NI")) |>
        dplyr::select(symbol, date, id, value) |>
        dplyr::group_by(symbol, date, id) |>
        dplyr::summarise(value = base::sum(value, na.rm = TRUE), .groups = "drop") |>
        tidyr::pivot_wider(names_from = "id", values_from = "value", values_fill = 0) |>
        dplyr::arrange(symbol, date) |>
        stats::na.omit() |>
        dplyr::mutate(nidiff = NI - CFOA.NI) |>
        dplyr::select(symbol, date, NI.IS = NI, CFOA.NI, nidiff)
      
      
      # Dividends (IS/BS/CFS)
      
      chgredp <- finstat() |>
        dplyr::filter(id %in% c("DP","RE","OCIL")) |>
        dplyr::select(symbol, date, amount) |>
        dplyr::group_by(symbol, date) |>
        dplyr::summarise(ending = base::sum(amount, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(symbol, date) |>
        dplyr::group_by(symbol) |>
        dplyr::mutate(
          beginning = dplyr::lag(ending),
          REDPVAR = ending - beginning
        ) |>
        stats::na.omit()
      
      divdiff <- totals() |>
        dplyr::filter(id %in% c("PAIDIV")) |>
        dplyr::group_by(symbol, date) |>
        dplyr::summarise(PAIDIV.CFS = base::sum(value), .groups = "drop") |>
        dplyr::left_join(dplyr::select(nidiff, symbol, date, NI.IS), by = c("symbol", "date")) |>
        dplyr::left_join(dplyr::select(chgredp, symbol, date, REDPVAR), by = c("symbol", "date")) |>
        dplyr::mutate(
          PAIDIV.ISBS = NI.IS - REDPVAR,
          divdiff = PAIDIV.CFS - PAIDIV.ISBS
        ) |>
        dplyr::select(symbol, date, REDPVAR, PAIDIV.ISBS, PAIDIV.CFS, divdiff)
      
      base::rm(chgredp)
      
      
      # Years
      
      timediff <- totals() |>
        dplyr::filter(id == "CASH") |>
        dplyr::select(symbol, date) |>
        base::unique() |>
        dplyr::arrange(symbol, date) |>
        dplyr::group_by(symbol) |>
        dplyr::mutate(
          previous = dplyr::lag(date),
          lag = date - previous,
          maxlag = base::round(base::max(lag, na.rm = TRUE)/365,0),
          timerange = base::round((base::max(date, na.rm = TRUE) - base::min(date, na.rm = TRUE))/365,0)
        ) |>
        stats::na.omit() |>
        dplyr::ungroup() |>
        dplyr::select(symbol, timerange, maxlag) |>
        base::unique()
      
      shinybusy::remove_modal_spinner()
      
      cashdiff |>
        dplyr::full_join(intdiff, by = c("symbol", "date")) |>
        dplyr::full_join(taxdiff, by = c("symbol", "date")) |>
        dplyr::full_join(nidiff, by = c("symbol", "date")) |>
        dplyr::full_join(divdiff, by = c("symbol", "date")) |>
        dplyr::full_join(timediff, by = c("symbol"))
    })
    
    additional_filters <- shiny::reactive({
      shiny::req(!base::is.null(checks()))
      
      shinybusy::update_modal_progress(value = 8/8, text = "Create additional filters")
      
      find_issue <- function(x){
        if (base::any(stats::na.omit(x) == "N")) "N" else "Y"
      }
      
      filters <- checks() |>
        dplyr::mutate(divdiff = dplyr::case_when(
          !base::is.finite(divdiff) ~ 0,
          !base::is.finite(PAIDIV.CFS) ~ 0,
          PAIDIV.CFS == 0 ~ 0,
          TRUE ~ base::abs(divdiff / PAIDIV.CFS)
        )) |>
        dplyr::select(symbol, date, cashdiff, intdiff, taxdiff, nidiff, divdiff, timerange, maxlag) |>
        dplyr::mutate(
          cashdiff = dplyr::case_when(base::is.na(cashdiff) ~ "Y", cashdiff == 0 ~ "Y", TRUE ~ "N"),
          nidiff   = dplyr::case_when(nidiff   == 0 ~ "Y", TRUE ~ "N"),
          divdiff = dplyr::case_when(divdiff <= 0.1 ~ "Y", TRUE ~ "N"),
          timerange = dplyr::case_when(timerange >= 4 ~ "Y", TRUE ~ "N"),
          maxlag = dplyr::case_when(maxlag == 1 ~ "Y", TRUE ~ "N")
        ) |>
        base::unique() |>
        dplyr::filter(date >= lubridate::ymd(input$afterdate)) |>
        dplyr::select(-date) |>
        dplyr::group_by(symbol) |>
        dplyr::mutate(
          cashdiff = find_issue(cashdiff),
          intdiff = find_issue(intdiff),
          taxdiff = find_issue(taxdiff),
          nidiff = find_issue(nidiff),
          divdiff = find_issue(divdiff),
          timerange = find_issue(timerange),
          maxlag = find_issue(maxlag)
        ) |>
        base::unique()
      
      shinybusy::remove_modal_progress()
      
      filters
    })
    
    
    preselection <- shiny::reactive({
      
      shiny::req(!base::is.null(additional_filters()))
      
      preselection <- fmpfiles() |>
        dplyr::filter(filingDate >= lubridate::ymd(input$afterdate))
      csvfiles <- csvfiles() |>
        dplyr::filter(reportDate >= lubridate::ymd(input$afterdate))
      additional_filters <- additional_filters()
      
      if (input$slctfocus == "Y"){
        preselection <- preselection |>
          dplyr::filter(symbol %in% focus()$symbol)
      }
      
      if (input$slctfocus == "N"){
        preselection <- preselection |>
          dplyr::filter(!(symbol %in% focus()$symbol))
      }
      
      if (input$slctjson == "Y"){
        preselection <- preselection |>
          dplyr::filter(symbol %in% jsonfiles()$symbol)
      }
      
      if (input$slctjson == "N"){
        preselection <- preselection |>
          dplyr::filter(!(symbol %in% jsonfiles()$symbol))
      }
      
      if (input$slcthtml == "Y"){
        preselection <- preselection |>
          dplyr::filter(symbol %in% htmlfiles()$symbol)
      }
      
      if (input$slcthtml == "N"){
        preselection <- preselection |>
          dplyr::filter(!(symbol %in% htmlfiles()$symbol))
      }
      
      if (input$slctcsv == "Y"){
        preselection <- preselection |>
          dplyr::filter(symbol %in% csvfiles$symbol)
      }
      
      if (input$slctcsv == "N"){
        preselection <- preselection |>
          dplyr::filter(!(symbol %in% csvfiles$symbol))
      }
      
      
      
      if (input$slctclass != "NA"){
        tmp <- csvfiles |>
          dplyr::filter(classified == input$slctclass)
        preselection <- preselection |>
          dplyr::filter(symbol %in% tmp$symbol)
      }
      
      if (input$slctbalan != "NA"){
        tmp <- csvfiles |>
          dplyr::filter(balanced == input$slctbalan)
        preselection <- preselection |>
          dplyr::filter(symbol %in% tmp$symbol)
      }
      
      if (input$slctcashdiff != "NA"){
        tmp <- additional_filters |>
          dplyr::filter(cashdiff == input$slctcashdiff)
        preselection <- preselection |>
          dplyr::filter(symbol %in% tmp$symbol)
      }
      
      if (input$slctnidiff != "NA"){
        tmp <- additional_filters |>
          dplyr::filter(nidiff == input$slctnidiff)
        preselection <- preselection |>
          dplyr::filter(symbol %in% tmp$symbol)
      }
      
      if (input$slctintdiff != "NA"){
        tmp <- additional_filters |>
          dplyr::filter(intdiff == input$slctintdiff)
        preselection <- preselection |>
          dplyr::filter(symbol %in% tmp$symbol)
      }
      
      if (input$slcttaxdiff != "NA"){
        tmp <- additional_filters |>
          dplyr::filter(taxdiff == input$slcttaxdiff)
        preselection <- preselection |>
          dplyr::filter(symbol %in% tmp$symbol)
      }
      
      if (input$slctdivdiff != "NA"){
        tmp <- additional_filters |>
          dplyr::filter(divdiff == input$slctdivdiff)
        preselection <- preselection |>
          dplyr::filter(symbol %in% tmp$symbol)
      }
      
      if (input$slcttimerange != "NA"){
        tmp <- additional_filters |>
          dplyr::filter(timerange == input$slcttimerange)
        preselection <- preselection |>
          dplyr::filter(symbol %in% tmp$symbol)
      }
      
      if (input$slctmaxlag != "NA"){
        tmp <- additional_filters |>
          dplyr::filter(maxlag == input$slctmaxlag)
        preselection <- preselection |>
          dplyr::filter(symbol %in% tmp$symbol)
      }
      
      preselection |>
        dplyr::arrange(symbol, filingDate)
    })
    
    
    output$filter_symbol <- shiny::renderUI({
      shiny::req(!base::is.null(preselection()))
      symbolChoices <- c(base::unique(base::unique(preselection()$symbol)))
      shiny::selectInput(
        "slctsymbol", "Symbol", choices = symbolChoices, selected = symbolChoices[[1]]
      )
    })
    
    afterSymbol <- shiny::reactive({
      shiny::req(!base::is.null(preselection()))
      shiny::req(!base::is.null(input$slctsymbol))
      if (input$slctsymbol != ""){
        dplyr::filter(preselection(), symbol == input$slctsymbol)
      } else preselection()
    })
    
    output$selected_filings <- DT::renderDataTable({
      shiny::req(!base::is.null(afterSymbol()))
      afterSymbol()
    })
    
    
    output$filter_date <- shiny::renderUI({
      shiny::req(!base::is.null(input$slctsymbol))
      
      shiny::req(!base::is.null(input$slcttype))
      if (input$slcttype == "JSON"){
        dates <- jsonfiles() |>
          dplyr::filter(symbol == input$slctsymbol) |>
          dplyr::select(reportDate)
      } else if (input$slcttype == "HTML"){
        dates <- htmlfiles() |>
          dplyr::filter(symbol == input$slctsymbol) |>
          dplyr::select(reportDate)
      } else {
        dates <- csvfiles() |>
          dplyr::filter(symbol == input$slctsymbol) |>
          dplyr::select(reportDate)
      }
      
      dates <- dates |>
        base::unique() |>
        dplyr::arrange(reportDate)
      
      dateChoices <- c(base::sort(base::as.character(dates$reportDate)))
      
      shiny::selectInput(
        "slctdate", "Date", choices = dateChoices, selected = dateChoices[[base::length(dateChoices)]]
      )
    })
    
    
    selectedfile <- shiny::reactive({
      shiny::req(!base::is.null(afterSymbol()))
      shiny::req(!base::is.null(input$slcttype))
      
      json <- jsonfiles() |>
        dplyr::filter(symbol == input$slctsymbol, reportDate == input$slctdate) |>
        dplyr::select(cik, form, reportDate, exthtmlpath = link)
      
      html <- htmlfiles() |>
        dplyr::filter(symbol == input$slctsymbol, reportDate == input$slctdate) |>
        dplyr::select(cik, form, reportDate, inthtmlpath = paths)
      
      csv <- csvfiles() |>
        dplyr::filter(symbol == input$slctsymbol, reportDate == input$slctdate) |>
        dplyr::filter(symbol == input$slctsymbol, reportDate == input$slctdate) |>
        dplyr::select(cik, form, reportDate, csvpath = paths)
      
      json |>
        dplyr::left_join(html, by = c("cik","form","reportDate")) |>
        dplyr::left_join(csv, by = c("cik","form","reportDate"))
    })
    
    file_name <- shiny::reactive({
      shiny::req(!base::is.null(selectedfile()))
      base::paste0(
        selectedfile()$cik[[1]],
        "_", selectedfile()$form[[1]], "_",
        base::as.character(selectedfile()$reportDate[[1]])
      )
    })
    
    shiny::observeEvent(input$openjson, {
      path <- base::paste0(preselection()$cik[[1]],".json")
      link <- base::paste0("https://data.sec.gov/submissions/", preselection()$cik[[1]],".json")
      clipr::write_clip(path)
      utils::browseURL(link)
    })
    
    shiny::observeEvent(input$openhtml, {
      clipr::write_clip(base::paste0(file_name(), ".html"))
      utils::browseURL(selectedfile()$exthtmlpath)
    })


    # Extraction ###############################################################

    tables <- shiny::reactive({
      file <- base::paste0("data/html/", file_name(), ".html")
      shiny::req(base::file.exists(file))
      collectR::get_tables_from_html(file)
    })

    
    patterns <- shiny::reactive({
      rights <- c(
        "asset", "cash","receivable|inventory|prepaid",
        "property|equipment|building|land|construction|vehicle|fixture|tangible|plant",
        "patent|copyright|franchise|license|goodwill",
        "accumulated","depreciation|depletion|amortization",
        "investment"
      )
      duties <- c(
        "liabilities","equity|share|stock",
        "payable|salaries|wage|compensation|benefit|tax|dividend|debt",
        "stockholder|shareholder",
        "retained|capital"
      )
      income <- c(
        "sale|revenue","cost|expense","profit|margin|earning|income","share",
        "interest|tax|gain|loss","general|selling|research|development|administrative"
      )
      cash <- c("operating","investing","financing","payment","proceeds","acqui","purchase","receivable")

      base::list(rights = rights, duties = duties, income = income, cash = cash)
    })

    output$extraction_prameters <- shiny::renderUI({
      shiny::wellPanel(
        shiny::selectizeInput(
          "right_patterns", "Keywords for rights",
          choices = patterns()$rights,
          selected = patterns()$rights,
          multiple = TRUE,
          options = base::list(create = TRUE),
          width = "100%"
        ),
        shiny::selectizeInput(
          "duties_patterns", "Keywords for duties",
          choices = patterns()$duties,
          selected = patterns()$duties,
          multiple = TRUE,
          options = base::list(create = TRUE),
          width = "100%"
        ),
        shiny::selectizeInput(
          "wealth_patterns", "Keywords for IS",
          choices = patterns()$income,
          selected = patterns()$income,
          multiple = TRUE,
          options = base::list(create = TRUE),
          width = "100%"
        ),
        shiny::selectizeInput(
          "cash_patterns", "Keywords for CFS",
          choices = patterns()$cash,
          selected = patterns()$cash,
          multiple = TRUE,
          options = base::list(create = TRUE),
          width = "100%"
        ),
        shiny::selectizeInput(
          "remove", "Tables to remove",
          choices = "0", selected = "0",
          multiple = TRUE,
          options = base::list(create = TRUE),
          width = "100%"
        ),
        shiny::numericInput("minscore", "Minimum match with keywords", value = 3, width = "100%"),
        shiny::numericInput("window", "How many words below the maximum match", value = 2, width = "100%"),
        shiny::textInput(
          "forceis", "For balance sheet",
          value = "",
          width = "100%"
        ),
        shiny::textInput(
          "forcebs", "For income statement",
          value = "",
          width = "100%"
        ),
        shiny::textInput(
          "forcecfs", "For cash flow statement",
          value = "",
          width = "100%"
        ),
        shiny::actionButton(
          "extract_statements", "Extract", icon = shiny::icon("download"),
          style = "color:#FFF;background-color:#306;width:100%;margin-top:25px;"
        )
      )
    })

    edited_patterns <- shiny::reactive({
      shiny::req(!base::is.null(input$right_patterns))
      shiny::req(!base::is.null(input$duties_patterns))
      shiny::req(!base::is.null(input$wealth_patterns))
      shiny::req(!base::is.null(input$cash_patterns))
      base::list(
        input$right_patterns,
        input$duties_patterns,
        input$wealth_patterns,
        input$cash_patterns
      )
    })

    positions <- shiny::reactive({
      shiny::req(!base::is.null(tables()))
      shiny::req(!base::is.null(edited_patterns()))
      shiny::req(!base::is.null(input$remove))
      shiny::req(!base::is.null(input$minscore))
      shiny::req(!base::is.null(input$window))
      collectR::identify_tables(tables(), edited_patterns(), input$remove, input$minscore, input$window)
    })

    output$tables_found <- shiny::renderText({
      shiny::req(!base::is.null(positions()))
      base::paste0(
        "Table for rights: ", positions()[[1]][1],
        "; table for duties: ", positions()[[2]][1],
        "; table for income: ", positions()[[3]][1],
        "; table for cash: ", positions()[[4]][1]
      )
    })

    statements <- shiny::reactive({
      shiny::req(!base::is.null(tables()))
      shiny::req(!base::is.null(positions()))
      shiny::req(positions()[[1]][1] != "")
      shiny::req(positions()[[2]][1] != "")
      shiny::req(positions()[[3]][1] != "")
      shiny::req(positions()[[4]][1] != "")
      collectR::retrieve_statements(
        tables(), positions(),
        input$forcebs, input$forceis, input$forcecfs
      )
    })

    output$display_statement <- DT::renderDataTable({
      shiny::req(!base::is.null(statements()))
      shiny::req(!base::is.null(input$slctcontent))
      if (input$slctcontent == "BS"){
        shiny::req(!base::is.na(statements()$BS))
        statements()$BS
      } else if (input$slctcontent == "IS"){
        shiny::req(!base::is.na(statements()$IS))
        statements()$IS
      } else {
        shiny::req(!base::is.na(statements()$CFS))
        statements()$CFS
      }
    }, options = base::list("pageLength" = 100))
    
    shiny::observeEvent(input$extract_statements, {
      shiny::req(!base::is.null(statements()))
      
      if (input$rightorder){
        statements <- dplyr::bind_rows(base::list(
          statements()$BS,
          statements()$IS,
          statements()$CFS
        ))
      } else {
        statements <- dplyr::bind_rows(base::list(
          statements()$BS |>
            dplyr::select(statement, label, year_1b = year_2, year_2b = year_1) |>
            dplyr::rename(year_1 = year_1b, year_2 = year_2b),
          statements()$IS |>
            dplyr::select(statement, label, year_1b = year_3, year_2, year_3b = year_1) |>
            dplyr::rename(year_1 = year_1b, year_3 = year_3b),
          statements()$CFS |>
            dplyr::select(statement, label, year_1b = year_3, year_2, year_3b = year_1) |>
            dplyr::rename(year_1 = year_1b, year_3 = year_3b)
        ))
      }
      
      dates <- jsonfiles() |>
        dplyr::filter(
          cik == selectedfile()$cik[[1]],
          reportDate <= selectedfile()$reportDate
        ) |>
        dplyr::arrange(dplyr::desc(reportDate)) |>
        dplyr::slice_head(n = 3)
      
      date1 <- base::as.character(lubridate::ymd(dates$reportDate[1]))
      date2 <- base::as.character(lubridate::ymd(dates$reportDate[2]))
      date3 <- base::as.character(lubridate::ymd(dates$reportDate[3]))
      
      if (base::is.na(date2)) date2 <-base::as.character(lubridate::ymd(date1) - lubridate::years(1))
      if (base::is.na(date3)) date3 <-base::as.character(lubridate::ymd(date2) - lubridate::years(1))
      
      statements <- statements |>
        dplyr::mutate(
          cik = selectedfile()$cik[[1]],
          reportDate = date1
        ) |>
        dplyr::select(cik, reportDate, statement, label, dplyr::everything()) |>
        tidyr::pivot_longer(cols = c("year_1","year_2","year_3"), names_to = "tmpyear", values_to = "amount") |>
        dplyr::filter(!base::is.na(amount), amount != "") |>
        dplyr::mutate(
          date = dplyr::case_when(
            tmpyear == "year_1" ~ date1,
            tmpyear == "year_2" ~ date2,
            TRUE ~ date3
          ),
          section = NA
        ) |>
        dplyr::select(cik, reportDate, statement, date, label, section, amount) |>
        dplyr::mutate(amount = amount * input$units)
      
      file <- base::paste0("data/csv/", file_name(), ".csv")
      utils::write.csv(statements, file, row.names = FALSE)
      shinyalert::shinyalert("Extracted!", "Your statements have been extracted.", "success")
    })
    
    
    
    # Classification ###########################################################

    output$classification <- rhandsontable::renderRHandsontable({
      shiny::req(!base::is.null(file_name()))
      csvfile <- base::paste0("data/csv/",file_name(),".csv")
      shiny::req(base::file.exists(csvfile))
      utils::read.csv(csvfile) |>
        dplyr::mutate(
          section = base::factor(section, levels = c("CA","NCA","CL","NCL","SE","REV","COS","OPEX","DEPR","LOSS","GAIN","INT","TAX","CFOA","CFIA","CFFA","OCF","ADDINFO"))
        ) |>
        rhandsontable::rhandsontable(
          height = 400, width = "100%", rowHeaders = NULL, stretchH = "all"
        ) |>
        #rhandsontable::hot_col(c(1:4), readOnly = TRUE) |>
        rhandsontable::hot_cols(
          colWidths = c("10%","5%","5%","5%","45%","10%","10%","10%")
        ) |>
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })
    
    
    shiny::observeEvent(input$savecsv, {
      shiny::req(!base::is.null(file_name()))
      csvfile <- base::paste0("data/csv/",file_name(),".csv")
      shiny::req(!base::is.null(input$classification))
      rhandsontable::hot_to_r(input$classification) |>
        dplyr::mutate_if(base::is.factor, base::as.character) |>
        utils::write.csv(csvfile, row.names = FALSE)
      shinyalert::shinyalert("Saved!", "Your classification has been saved.", "success")
    })
    
    
    section_totals <- shiny::reactive({
      shiny::req(!base::is.null(file_name()))
      csvfile <- base::paste0("data/csv/",file_name(),".csv")
      shiny::req(!base::is.null(input$classification))
      rhandsontable::hot_to_r(input$classification) |>
        dplyr::mutate(amount = dplyr::case_when(
          section == "REV" ~ amount,
          section == "COS" ~ - base::abs(amount),
          section == "OPEX" ~ - base::abs(amount),
          section == "DEPR" ~ - base::abs(amount),
          section == "GAIN" ~ base::abs(amount),
          section == "LOSS" ~ - base::abs(amount),
          section == "INT" ~ - base::abs(amount),
          section == "TAX" ~ - base::abs(amount),
          TRUE ~ amount
        )) |>
        dplyr::group_by(date, section) |>
        dplyr::summarise(amount = base::sum(amount), .groups = "drop")
    })
    
    
    output$check_section_totals <- DT::renderDataTable({
      shiny::req(!base::is.null(section_totals()))
      section_totals() |>
        dplyr::mutate(section = base::factor(
          section,
          levels = c("CA","NCA","CL","NCL","SE","REV","COS","OPEX","DEPR","LOSS","GAIN","INT","TAX","CFOA","CFIA","CFFA","OCF")
        )) |>
        dplyr::arrange(section, dplyr::desc(date))
    }, options = base::list("pageLength" = 6))
    
    
    aggregates <- shiny::reactive({
      shiny::req(!base::is.null(section_totals()))
      section_totals() |>
        dplyr::mutate(aggregate = dplyr::case_when(
          section %in% c("CA","NCA") ~ "Total assets",
          section %in% c("CL","NCL","SE") ~ "Total liabilities and equity",
          section %in% c("REV","COS","OPEX","GAIN","DEPR","LOSS","INT","TAX") ~ "Net Income",
          section %in% c("CFOA","CFIA","CFFA","OCF") ~ "Cash variation",
          TRUE ~ ""
        )) |>
        dplyr::group_by(date, aggregate) |>
        dplyr::summarise(amount = base::sum(amount), .groups = "drop")
    })
    
    
    output$check_aggregates <- DT::renderDataTable({
      shiny::req(!base::is.null(aggregates()))
      aggregates() |>
        dplyr::mutate(aggregate = base::factor(
          aggregate,
          levels = c("Total assets","Total liabilities and equity","Net Income","Cash variation")
        )) |>
        dplyr::arrange(aggregate, dplyr::desc(date))
    }, options = base::list("pageLength" = 6))

    
    
    ############################################################################
    # Verification
    
    output$displaytotals <- DT::renderDataTable({
      shiny::req(!base::is.null(totals()))
      totals() |>
        dplyr::filter(
          symbol %in% base::unique(afterSymbol()$symbol),
          date >= lubridate::ymd(input$afterdate)
        ) |>
        dplyr::arrange(dplyr::desc(date), id)
    })
    
    output$displaychecks <- DT::renderDataTable({
      shiny::req(!base::is.null(checks()))
      checks() |>
        dplyr::filter(
          symbol %in% base::unique(afterSymbol()$symbol),
          date >= lubridate::ymd(input$afterdate)
        ) |>
        dplyr::arrange(dplyr::desc(date))
    })
    
    
    ############################################################################
    # Exportation

    shiny::observeEvent(input$getstatement, {
      preselection() |>
        dplyr::select(symbol) |>
        base::unique() |>
        dplyr::left_join(finstat(), by = "symbol") |>
        dplyr::filter(date >= lubridate::ymd(input$afterdate)) |>
        utils::write.csv("data/out/statements.csv", row.names = FALSE)
      shinyalert::shinyalert("Exported!", "Your classified statements have been exported.", "success")
    })
    
    
    
    shiny::observeEvent(input$getmarket, {
      
      shinybusy::show_modal_progress_line(value = 0, text = "Collecting market data")
      
      market <- finstat() |>
        dplyr::filter(symbol %in% base::unique(preselection()$symbol)) |>
        dplyr::group_by(symbol) |>
        tidyr::nest()
      
      startdate <- base::as.character(base::min(lubridate::ymd(finstat()$date)) - lubridate::years(5) - lubridate::days(7))
      enddate <- base::as.character(base::max(lubridate::ymd(finstat()$date)) + lubridate::days(7))
      
      size <- base::nrow(market)
      
      for (i in base::seq_len(size)){
        
        shinybusy::update_modal_progress(value = i/size, text = market$symbol[[i]])
        
        market$data[[i]] <- collectR::get_market_data(
          symbol = market$symbol[[i]],
          fmpkey = "gZGlUkYsnnPTy7mrJ4zJnCMoAjHGC3x3",
          start = startdate,
          end = enddate
        )
        
      }
      
      market <- market |>
        dplyr::ungroup() |>
        dplyr::select(data) |>
        tidyr::unnest(data)
      
      utils::write.csv(market, "data/out/market.csv", row.names = FALSE)
      
      shinybusy::remove_modal_progress()
      
      shinyalert::shinyalert("Collected!", "Companies market data have been collected.", "success")
    })
    
    
    
    shiny::observeEvent(input$getindexes, {
      
      shinybusy::show_modal_spinner()
      
      indexes <- collectR::get_market_indexes(
        start = base::as.character(base::min(lubridate::ymd(finstat()$date)) - lubridate::years(5)),
        end = base::as.character(base::max(lubridate::ymd(finstat()$date)) + lubridate::days(7))
      )
      utils::write.csv(indexes, "data/out/indexes.csv", row.names = FALSE)
      
      shinybusy::remove_modal_spinner()
      
      shinyalert::shinyalert("Collected!", "US market indexes data have been collected.", "success")
    })
    
    
    
    shiny::observeEvent(input$getrates, {
      
      shinybusy::show_modal_spinner()
      
      rates <- collectR::get_treasury_yield(
        start = base::as.character(base::min(lubridate::ymd(finstat()$date)) - lubridate::years(5)),
        end = base::as.character(base::max(lubridate::ymd(finstat()$date)) + lubridate::days(7))
      )
      
      utils::write.csv(rates, "data/out/rates.csv", row.names = FALSE)
      
      shinybusy::remove_modal_spinner()
      
      shinyalert::shinyalert("Collected!", "US treasury yield data have been collected.", "success")
    })
    
    
    shiny::observeEvent(input$getcorp, {
      
      shinybusy::show_modal_spinner()
      
      statements <- utils::read.csv("data/out/statements.csv")
      markets <- utils::read.csv("data/out/market.csv")
      
      filings <- utils::read.csv("base/filings.csv") |>
        dplyr::filter(type == "10-K") |>
        dplyr::select(symbol, filingDate = fillingDate, form = type, link)
      
      corporations <- utils::read.csv("base/corporations.csv") |>
        stats::na.omit() |> dplyr::filter(base::nchar(cik) <= 10) |>
        dplyr::filter(symbol %in% base::intersect(statements$symbol, markets$symbol)) |>
        dplyr::mutate(cik = purrr::map_chr(cik, collectR::complete_cik)) |>
        dplyr::select(
          symbol, cik, isin, cusip, name, mkt,
          industry, sector, state, description, ipo
        )
      
      utils::write.csv(corporations, "data/out/corporations.csv", row.names = FALSE)
      
      filings <- corporations |>
        dplyr::select(symbol) |>
        dplyr::left_join(filings, by = "symbol")
      
      utils::write.csv(corporations, "data/out/filings.csv", row.names = FALSE)
      
      shinybusy::remove_modal_spinner()
      
      shinyalert::shinyalert("Collected!", "Corporate information have been exported", "success")
    })
    
    
    
    # Exit the application #####################################################

    shiny::observeEvent(input$exit, {
      shiny::stopApp()
    })
  }
)

