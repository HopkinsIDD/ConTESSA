function(input, output, session) {
  Sys.sleep(2)
  waiter_hide()

  ## Disconnect --------------------------------------------------------------

  sever(
    html = tagList(
      h2("it looks like you've disconnected."),
      br(),
      spin_loaders(id = 11, color = "#f1c400"),
      br(), br(), br(),
      reload_button("Click here to reload",
        class = "success"
      ),
      h3("Infectious Disease Dynamics | Johns Hopkins Bloomberg School of Public Health")
    ),
    bg_color = "#265CA4", opacity = 0.9
  )

  ## Bookmark inputs in URL ---------------------------------------------------

  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })

  ## Save / Load inputs --------------------------------------------------------

  saved <- reactiveValues(saved_inputs = NULL)

  output$file_msg <- renderUI({
    file <- input$load
    inputs <- c(
      "scenario_b",
      "name",
      "prepared_by",
      "n_detect",
      "n_detect_b",
      "n_infect_calc",
      "n_infect_calc_b",
      "n_deaths",
      "n_deaths_b",
      "ifr",
      "ifr_b",
      "ifr_other",
      "ifr_other_b",
      "prop_detect",
      "prop_detect_b",
      "n_isolate",
      "n_isolate_b",
      "n_detect_quar",
      "n_detect_quar_b",
      "pass_test",
      "pass_isol",
      "pass_isol_b",
      "pass_isol2",
      "pass_isol3",
      "contact_h",
      "contact_h_b",
      "omega_h",
      "omega_h_b",
      "house_quar",
      "house_quar_b",
      "contact_c",
      "contact_c_b",
      "omega_c",
      "omega_c_b",
      "comm_quar",
      "comm_quar_b",
      "mult",
      "mult_b",
      "alpha",
      "alpha_b",
      "kappa",
      "kappa_b",
      "R",
      "R_b",
      "nu",
      "nu_b",
      "generation",
      "generation_b",
      "t_pa",
      "t_pa_b"
    )
    if (is.null(file)) {
      return(NULL)
    }
    validate(
      need(
        tolower(file_ext(file$name)) == "yaml",
        "UPLOAD FAILED. Input file must be a .yaml \nfile previously downloaded from this application."
      )
    )
    saved_inputs <- yaml.load(read_yaml(input$load$datapath))
    validate(
      need(
        all(inputs %in% names(saved_inputs)),
        "UPLOAD FAILED. Input file must be a .yaml \nfile previously downloaded from this application."
      )
    )
    saved$saved_inputs <- saved_inputs
  })
  observeEvent(saved$saved_inputs, {
    saved_inputs <- saved$saved_inputs

    updateCheckboxInput(session, "scenario_b", value = saved_inputs[["scenario_b"]])
    updateTextInput(session, "name", value = saved_inputs[["name"]])
    updateTextInput(session, "prepared_by", value = saved_inputs[["prepared_by"]])
    updateNumericInput(session, "n_detect", value = saved_inputs[["n_detect"]])
    updateNumericInput(session, "n_detect_b", value = saved_inputs[["n_detect_b"]])
    updateRadioButtons(session, "n_infect_calc", selected = saved_inputs[["n_infect_calc"]])
    updateRadioButtons(session, "n_infect_calc_b", selected = saved_inputs[["n_infect_calc_b"]])
    updateNumericInput(session, "n_deaths", value = saved_inputs[["n_deaths"]])
    updateNumericInput(session, "n_deaths_b", value = saved_inputs[["n_deaths_b"]])
    updateRadioButtons(session, "ifr", selected = saved_inputs[["ifr"]])
    updateRadioButtons(session, "ifr_b", selected = saved_inputs[["ifr_b"]])
    updateNumericInput(session, "ifr_other", value = saved_inputs[["ifr_other"]])
    updateNumericInput(session, "ifr_other_b", value = saved_inputs[["ifr_other_b"]])
    updateSliderInput(session, "prop_detect", value = saved_inputs[["prop_detect"]])
    updateSliderInput(session, "prop_detect_b", value = saved_inputs[["prop_detect_b"]])
    updateNumericInput(session, "n_isolate", value = saved_inputs[["n_isolate"]])
    updateNumericInput(session, "n_isolate_b", value = saved_inputs[["n_isolate_b"]])
    updateNumericInput(session, "n_detect_quar", value = saved_inputs[["n_detect_quar"]])
    updateNumericInput(session, "n_detect_quar_b", value = saved_inputs[["n_detect_quar_b"]])
    updateSliderTextInput(session, "pass_test", selected = saved_inputs[["pass_test"]])
    updateSliderTextInput(session, "pass_isol", selected = saved_inputs[["pass_isol"]])
    updateSliderTextInput(session, "pass_isol_b", selected = saved_inputs[["pass_isol_b"]])
    updateSliderTextInput(session, "pass_isol2", selected = saved_inputs[["pass_isol2"]])
    updateSliderTextInput(session, "pass_isol3", selected = saved_inputs[["pass_isol3"]])
    updateNumericInput(session, "contact_h", value = saved_inputs[["contact_h"]])
    updateNumericInput(session, "contact_h_b", value = saved_inputs[["contact_h_b"]])
    updateSliderInput(session, "omega_h", value = saved_inputs[["omega_h"]])
    updateSliderInput(session, "omega_h_b", value = saved_inputs[["omega_h_b"]])
    updateSliderTextInput(session, "house_quar", selected = saved_inputs[["house_quar"]])
    updateSliderTextInput(session, "house_quar_b", selected = saved_inputs[["house_quar_b"]])
    updateNumericInput(session, inputId = "contact_c", value = saved_inputs[["contact_c"]])
    updateNumericInput(session, inputId = "contact_c_b", value = saved_inputs[["contact_c_b"]])
    updateNumericInput(session, "omega_c", value = saved_inputs[["omega_c"]])
    updateNumericInput(session, "omega_c_b", value = saved_inputs[["omega_c_b"]])
    updateSliderTextInput(session, "comm_quar", selected = saved_inputs[["comm_quar"]])
    updateSliderTextInput(session, "comm_quar_b", selected = saved_inputs[["comm_quar_b"]])
    updateSliderInput(session, "mult", value = saved_inputs[["mult"]])
    updateSliderInput(session, "mult_b", value = saved_inputs[["mult_b"]])
    updateSliderInput(session, "alpha", value = saved_inputs[["alpha"]])
    updateSliderInput(session, "alpha_b", value = saved_inputs[["alpha_b"]])
    updateSliderInput(session, "kappa", value = saved_inputs[["kappa"]])
    updateSliderInput(session, "kappa_b", value = saved_inputs[["kappa_b"]])
    updateNumericInput(session, "R", value = saved_inputs[["R"]])
    updateNumericInput(session, "R_b", value = saved_inputs[["R_b"]])
    updateSliderInput(session, "nu", value = saved_inputs[["nu"]])
    updateSliderInput(session, "nu_b", value = saved_inputs[["nu_b"]])
    updateRadioButtons(session, "generation", selected = saved_inputs[["generation"]])
    updateRadioButtons(session, "generation_b", selected = saved_inputs[["generation_b"]])
    updateSliderTextInput(session, "t_pa", selected = saved_inputs[["t_pa"]])
    updateSliderTextInput(session, "t_pa_b", selected = saved_inputs[["t_pa_b"]])
  })


  output$save <- downloadHandler(
    filename <- function() {
      glue("{input$file_name}.yaml")
      },
    content <- function(file) {
      inputs <- reactiveValuesToList(input)
      input_yaml <- as.yaml(inputs)
      write_yaml(input_yaml, file = file)
    }
  )

  ## Advanced Options ----

  alpha <- reactive({
    input$alpha / 100
  })

  alpha_b <- reactive({
    input$alpha_b / 100
  })

  omega_q <- reactive({
    omega_h() * eta() + omega_c() * (1 - eta())
  })

  omega_q_b <- reactive({
    omega_h_b() * eta_b() + omega_c_b() * (1 - eta_b())
  })

  t_ps <- reactive({
    text_to_time(input$pass_isol[2])
  })

  t_ps_b <- reactive({
    text_to_time(input$pass_isol_b)
  })

  t_pa <- reactive({
    text_to_time(input$t_pa)
  })

  t_pa_b <- reactive({
    text_to_time(input$t_pa_b)
  })

  t_qhs <- reactive({
    text_to_time(input$house_quar)
  })

  t_qhs_b <- reactive({
    text_to_time(input$house_quar_b)
  })

  t_qha <- reactive({
    t_pa() + (t_qhs() - t_ps())
  })

  t_qha_b <- reactive({
    t_pa_b() + (t_qhs_b() - t_ps_b())
  })

  t_qcs <- reactive({
    text_to_time(input$comm_quar)
  })

  t_qcs_b <- reactive({
    text_to_time(input$comm_quar_b)
  })

  t_qca <- reactive({
    t_pa() + (t_qcs() - t_ps())
  })

  t_qca_b <- reactive({
    t_pa_b() + (t_qcs_b() - t_ps_b())
  })

  t_q <- reactive({
    (t_qhs() * (1 - alpha()) + t_qha() * alpha() ) * eta() +
      (t_qcs() * (1 - alpha()) + t_qca() * alpha()) * (1 - eta())
  })

  t_q_b <- reactive({
    (t_qhs_b() * (1 - alpha_b()) + t_qha_b() * alpha_b()) * eta_b() +
      (t_qcs_b() * (1 - alpha_b()) + t_qca_b() * alpha_b()) * (1 - eta_b())
  })

  ## Home page -----------------------------------------------------------------


  output$welcome <- renderUI({
    req(input$name)
    span(glue("Welcome, {input$name}!"),
      style = "color: #002d72; font-size:30px; font-weight:400;"
    )
  })

  observeEvent(input$surv, {
    updateTabItems(session, "tabs", "passive")
  })

  ## Passive Detection -------------------------------------------------------

  observeEvent(input$next_house, {
    updateTabItems(session, "tabs", "contact-house")
  })

  ## 0. Input values -----

  n_infect <- reactive({
    if (input$n_infect_calc == "ifr") {
      req(input$n_deaths)
      return((input$n_deaths) / (ifr() / 100))
    }
    if (input$n_infect_calc == "direct") {
      req(input$n_detect)
      return(input$n_detect / (input$prop_detect / 100))
    }
  })

  n_infect_b <- reactive({
    if (input$n_infect_calc_b == "ifr") {
      req(input$n_deaths_b)
      return((input$n_deaths_b) / (ifr_b() / 100))
    }
    if (input$n_infect_calc_b == "direct") {
      req(input$n_detect_b)
      return(input$n_detect_b / (input$prop_detect_b / 100))
    }
  })

  n_detect <- reactive({
    input$n_detect
  })

  prop_detect_ifr <- reactive({
    min(n_detect() / n_infect(), 1)
  })

  prop_detect <- reactive({
    input$n_detect / n_infect()
  })

  ifr <- reactive({
    req(input$ifr_other)
    if (input$ifr == "other") {
      return(input$ifr_other)
    } else {
      as.numeric(input$ifr)
    }
  })

  ifr_b <- reactive({
    req(input$ifr_other_b)
    if (input$ifr_b == "other") {
      return(input$ifr_other_b)
    } else {
      as.numeric(input$ifr_b)
    }
  })

  output$ifr_out <- renderUI({
    glue("Your estimated IFR is {ifr()}%")
  })

  output$n_infections <- renderUI({
    div(
      class = "slider-subtitle",
      tagList(
        "We estimate that you had approximately ",
        strong(glue("{format(round(n_infect()), big.mark = ',')}")),
        " infections in your community in the past week.",
        icon("question-circle", class = "fa-xs")
      )
    )
  })

  output$ifr_warning <- renderUI({
    req(input$n_detect)
    req(input$n_deaths)

    if (n_infect() < n_detect()) {
      p(glue(
        "From the current inputs, you are detecting ",
        "{format(round(n_detect()), big.mark = ',')} cases. Per your selected IFR ({ifr()}%) ",
        "there are {format(round(n_infect()), big.mark = ',')} infected in your community. ",
        "These are not compatible, consider updating your estimated IFR or new case data."
      ),
      style = "background-color: #f1c400;"
      )
    } else {
      ""
    }
  })

  mult <- reactive({
    constraint <- (prop_detect_isol_passive() - (1 - alpha())) / alpha()
    if (constraint < input$mult) {
      return(input$mult)
    } else {
      return(constraint)
    }
  })

  mult_b <- reactive({
    constraint <- (prop_detect_isol_passive_b() - (1 - alpha_b())) / alpha_b()
    if (constraint < input$mult_b) {
      return(input$mult_b)
    } else {
      return(constraint)
    }
  })


  prop_detect_isol_passive <- reactive({
   (input$n_detect_quar) / (n_infect() - (input$n_isolate - input$n_detect_quar))
  })


  prop_detect_isol_passive_b <- reactive({
    input$n_detect_quar_b / (n_infect_b() - (input$n_isolate_b - input$n_detect_quar_b))
  })

  rho_a <- reactive({
    p_a_d <- alpha() / (alpha() + (1 / mult()) * (1 - alpha()))
    p_a_d * prop_detect_isol_passive() / alpha()
  })

  rho_s <- reactive({
    p_s_d <- (1 - alpha()) / ((1 - alpha()) + mult() * alpha())
    p_s_d * prop_detect_isol_passive() / (1 - alpha())
  })

  pass_test <- reactive({
    if (input$pass_test == "Symptom onset of case") {
      0
    } else if (input$pass_test == "Day 14+") {
      14
    } else {
      as.numeric(gsub("Day", "", input$pass_test))
    }
  })

  pass_test2 <- reactive({
    if (input$pass_isol[1] == "Symptom onset of case") {
      0
    } else if (input$pass_isol[1] == "Day 14+") {
      14
    } else {
      as.numeric(gsub("Day", "", input$pass_isol[1]))
    }
  })


  pass_isol <- reactive({
    isol <- input$pass_isol[2]
    if (isol == "Symptom onset of case") {
      0
    } else if (isol == "Day 14+") {
      14
    } else {
      as.numeric(gsub("Day", "", isol))
    }
  })

  output$pass_isol_out <- renderUI({
    p(glue(
      "On average, cases are told to isolate ",
      "{pass_isol() - pass_test()} days after ",
      "having a sample collected for testing."
    ), class = "slider-subtitle")
  })

  output$pass_isol2_out <- output$pass_isol3_out <- renderUI({
    p(glue(
      "On average, cases have a sample collected for testing
      {pass_test()} days after symptom onset and are told to isolate ",
      "{pass_isol() - pass_test()} days after ",
      "having a sample collected for testing. The average time from ",
      "symptom onset of an case to isolation is {pass_isol()}."
    ), class = "slider-subtitle")
  })

  output$pass_test_out <- renderUI({
    p(glue(
      "On average, cases have a sample collected for testing ",
      "{pass_test()} days after symptom onset."
    ), class = "slider-subtitle")
  })

  observeEvent(input$pass_test, {
    diff <- pass_isol() - pass_test2()
    if (pass_isol() == 0) {
      new_isol <- input$pass_isol[2]
    } else if (pass_test2() == 14) {
      new_isol <- "Day 14+"
    } else if (pass_test() + diff < 14) {
      new_isol <- glue("Day {pass_test() + diff}")
    } else {
      new_isol <- "Day 14+"
    }
    updateSliderTextInput(session, "pass_isol",
      selected = c(input$pass_test, new_isol)
    )
  })

  ## 1. Info boxes -------------------------------------------------------------

  n_detect_box <- reactive({
    infoBox(
      value = glue("{format(n_detect(), big.mark = ',')}"),
      title = "Detecting",
      subtitle = "infections on average per week",
      icon = icon("search-plus")
    )
  })

  output$n_detect_out <- renderInfoBox({
    n_detect_box()
  })


  prop_detect_box <- reactive({
    infoBox(
      value = glue("{round(prop_detect() * 100)}%"),
      title = "Detecting",
      subtitle = "of infections",
      icon = icon("search-plus")
    )
  })

  output$prop_detect_out <- renderInfoBox({
    prop_detect_box()
  })

  prop_isolate_box <- reactive({
    infoBox(
      value = glue("{round((input$n_isolate / n_infect()) * 100)}%"),
      title = "Isolating",
      subtitle = "of infections",
      icon = icon("user-circle-o")
    )
  })

  output$prop_isolate <- renderInfoBox({
    prop_isolate_box()
  })

  prop_quar_box <- reactive({
    infoBox(
      value = glue("{round(prop_detect_isol_passive() * 100)}%"),
      title = "Isolating",
      subtitle = glue("of infections who were not identified during contact tracing"),
      icon = icon("user-circle-o")
    )
  })

  output$prop_isol_not_quar <- renderInfoBox({
    prop_quar_box()
  })

  listen_tp <- reactive({
    list(input$pass_isol, input$t_pa)
  })

  changed_tp <- eventReactive(listen_tp(), {
    time <- t_ps() * (1 - alpha()) + t_pa() * (alpha())
    not_equal(time, pass_isol())
  })

  t_p <- reactive({
    time <- t_ps() * (1 - alpha()) + t_pa() * (alpha())
    disclaim <- ""
    if (changed_tp()) {
      disclaim <- "*"
    }
    infoBox(
      value = time,
      title = "Delay of",
      subtitle = glue("days, on average, from symptom onset to isolation of surveillance detected infections{disclaim}"),
      icon = icon("calendar-check-o")
    )
  })

  output$t_p <- renderInfoBox({
    t_p()
  })

  ## 2. Update assumptions based on inputs -------------------------------------

  output$n_isolate_warning <- renderUI({
    req(input$n_isolate)
    req(input$n_detect)

    if (input$n_isolate > input$n_detect) {
      js$bkg_col("n_isolate")
      p(glue(
        "From the current inputs, you are detecting ",
        "{format(round(n_detect()), big.mark = ',')} cases. Per your input above, ",
        "{format(input$n_isolate, big.mark = ',')} cases were contacted and isolated. ",
        "These are not compatible. The number of cases detected should be greater than ",
        "the number of cases contacted and isolated. Consider updating your ",
        "number of cases contacted and isolated or new case data."
      ),
      style = "background-color: #f1c400;"
      )
    } else {
      js$bkg_col("n_isolate", "#fff")
    }
  })

  observeEvent(input$n_isolate, {
    updateNumericInput(session, "n_detect_quar", value = input$n_isolate)
  })

  output$n_quar_warning <- renderUI({
    req(input$n_detect_quar)
    req(input$n_isolate)

    if (input$n_detect_quar > input$n_isolate) {
      js$bkg_col("n_detect_quar")
      p(glue(
        "From the current inputs, you are detecting ",
        "{format(input$n_detect_quar, big.mark = ',')} cases already in quarantine. Per your input above, ",
        "{format(input$n_isolate, big.mark = ',')} cases were contacted and isolated. ",
        "These are not compatible. The number of cases contacted and isolated should be greater than ",
        "the number of cases already in quarantine. Consider updating your ",
        "number of cases already in quarantine or your number contacted and isolated."
      ),
      style = "background-color: #f1c400;"
      )
    } else {
      js$bkg_col("n_detect_quar", "#fff")
    }
  })

  observeEvent(input$pass_isol, {
    updateSliderTextInput(session, "pass_isol2", selected = input$pass_isol)
    updateSliderTextInput(session, "pass_isol3", selected = input$pass_isol)
  })

  observeEvent(input$pass_isol, {
    updateSliderTextInput(session, "t_pa", selected = input$pass_isol[2])
  })

  observeEvent(input$pass_isol_b, {
    updateSliderTextInput(session, "t_pa_b", selected = input$pass_isol_b)
  })


  ## 3. Disclaimers ------------------------------------------------------------

  observeEvent(input$advanced_tab, {
    updateTabItems(session, "tabs", "advanced")
  })

  output$disclaimer <- renderUI({
    if (changed_tp()) {
      glue(
        "*It looks like you've updated the estimated times by symptomatic ",
        "status in the Advanced tab. This number reflects those changes."
      )
    } else {
      ""
    }
  })

  ## Household Contacts --------------------------------------------------------

  observeEvent(input$next_comm, {
    updateTabItems(session, "tabs", "contact-comm")
  })

  ## 0. Input values -----

  eta <- reactive({
    input$contact_h / (input$contact_h + input$contact_c)
  })

  eta_b <- reactive({
    input$contact_h_b / (input$contact_h_b + input$contact_c_b)
  })

  omega_h <- reactive({
    input$omega_h / 100
  })

  omega_h_b <- reactive({
    input$omega_h_b / 100
  })


  ## 1. Info boxes -------------------------------------------------------------


  contact_hbox <- reactive({
    infoBox(
      value = input$contact_h,
      title = "Average number",
      subtitle = glue("of household contacts per infected person"),
      icon = icon("address-book-o")
    )
  })

  omega_h_box <- reactive({
    infoBox(
      value = glue("{round(input$omega_h)}%"),
      title = "Quarantined",
      subtitle = glue("of household contacts"),
      icon = icon("home")
    )
  })

  house_quar <- reactive({
    quar <- input$house_quar
    if (quar == "Symptom onset of case") {
      0
    } else if (quar == "Day 14+") {
      14
    } else {
      as.numeric(gsub("Day", "", quar))
    }
  })

  listen_tqh <- reactive({
    list(input$t_qha, input$house_quar)
  })

  changed_tqh <- eventReactive(listen_tqh(), {
    time <- t_qhs() * (1 - alpha()) + t_qha() * (alpha())
    not_equal(time, house_quar())
  })

  t_hbox <- reactive({
    time <- t_qhs() * (1 - alpha()) + t_qha() * (alpha())
    disclaim <- ""
    if (changed_tqh()) {
      disclaim <- "*"
    }
    infoBox(
      value = time,
      title = "Delay of",
      subtitle = glue("days, on average, from symptom onset of a case to quarantine of household contacts{disclaim}"),
      icon = icon("calendar-check-o"),
      color = "black"
    )
  })

  output$omega_h_out <- renderInfoBox({
    omega_h_box()
  })

  output$t_hbox <- renderInfoBox({
    t_hbox()
  })
  output$contact_hbox <- renderInfoBox({
    contact_hbox()
  })

  output$house_quar_explain <- renderUI({
    p(glue(
      "Drag the slider above to indicate the average number of days ",
      "between symptom onset of a case and household contacts notified and ",
      "quarantined. On the 'Surveillance and Isolation' page you indicated that the average ",
      "number of days between symptom onset for a case and isolation ",
      "is {pass_isol()}, so value should be at least {pass_isol()}"
    ),
    style = "font-size: 16px;"
    )
  })

  ## 2. Update assumptions based on inputs ----

  listen_house_quar <- reactive({
    list(input$house_quar, input$pass_isol)
  })
  observeEvent(listen_house_quar(), {
    if (pass_isol() > house_quar()) {
      updateSliderTextInput(session, "house_quar", selected = input$pass_isol[2])
    }
  })

  ## 3. Disclaimers ----

  observeEvent(input$advanced_tab2, {
    updateTabItems(session, "tabs", "advanced")
  })

  output$disclaimer_house <- renderUI({
    if (changed_tqh()) {
      glue(
        "*It looks like you've updated assumptions in the Advanced tab. ",
        "This number reflects those changes."
      )
    } else {
      ""
    }
  })

  ## Community ---------------------------------------------------------------

  observeEvent(input$next_dash, {
    updateTabItems(session, "tabs", "dash")
  })

  ## 0. Input values ----

  omega_c <- reactive({
    input$omega_c / 100
  })

  omega_c_b <- reactive({
    input$omega_c_b / 100
  })

  ## 1. Info Boxes ----

  contact_cbox <- reactive({
    infoBox(
      value = input$contact_c,
      title = "Average number",
      subtitle = glue("of community contacts per infected person"),
      icon = icon("address-book-o")
    )
  })

  omega_c_box <- reactive({
    infoBox(
      value = glue("{round(input$omega_c)}%"),
      title = "Quarantined",
      subtitle = glue("of community contacts"),
      icon = icon("users")
    )
  })

  comm_quar <- reactive({
    quar <- input$comm_quar
    if (quar == "Symptom onset of case") {
      return(0)
    } else if (quar == "Day 14+") {
      return(14)
    } else {
      return(as.numeric(gsub("Day", "", quar)))
    }
  })

  listen_tqc <- reactive({
    list(input$t_qca, input$comm_quar)
  })

  changed_tqc <- eventReactive(listen_tqc(), {
    time <- t_qcs() * (1 - alpha()) + t_qca() * (alpha())
    not_equal(time, comm_quar())
  })

  t_cbox <- reactive({
    time <- t_qcs() * (1 - alpha()) + t_qca() * (alpha())
    disclaim <- ""
    if (changed_tqc()) {
      disclaim <- "*"
    }
    infoBox(
      value = time,
      title = "Delay of",
      subtitle = glue("days, on average, from symptom onset of a case to quarantine of community contacts{disclaim}"),
      icon = icon("calendar-check-o"),
      color = "black"
    )
  })

  output$omega_c_out <- renderInfoBox({
    omega_c_box()
  })
  output$t_cbox <- renderInfoBox({
    t_cbox()
  })
  output$contact_cbox <- renderInfoBox({
    contact_cbox()
  })


  output$comm_quar_explain <- renderUI({
    p(glue(
      "Drag the slider above to indicate the average number of days ",
      "between symptom onset of a case and community contacts notified and ",
      "quarantined. On the 'Surveillance and Isolation' page you indicated that the average ",
      "number of days between symptom onset for a case and isolation ",
      "is {pass_isol()}, so value should be at least {pass_isol()}"
    ),
    style = "font-size: 16px;"
    )
  })

  ## 2. Update assumptions based on inputs ----

  listen_comm_quar <- reactive({
    list(input$comm_quar, input$pass_isol)
  })

  observeEvent(listen_comm_quar(), {
    if (pass_isol() > comm_quar()) {
      updateSliderTextInput(session, "comm_quar", selected = input$pass_isol[2])
    }
  })


  ## 3. Disclaimers ----

  observeEvent(input$advanced_tab3, {
    updateTabItems(session, "tabs", "advanced")
  })

  output$disclaimer_comm <- renderUI({
    if (changed_tqc()) {
      glue(
        "*It looks like you've updated assumptions in the Advanced tab. ",
        "This number reflects those changes."
      )
    } else {
      ""
    }
  })

  ## Dashboard  --------------------------------------------------------------

  r <- reactiveValues(
    value = FALSE
  )

  observeEvent(input$tabs, {
    val()
    val_b()
    if (input$tabs == "dash" & !isTRUE(r$value)) {
      waiter_show(
        html = tagList(
          h2("crunching your numbers..."),
          br(),
          spin_loaders(id = 11, color = "#f1c400"),
          br(),
          h3("Infectious Disease Dynamics | Johns Hopkins Bloomberg School of Public Health")
        ),
        color = "rgba(38, 92, 164, 0.9)"
      )
    }
  })

  observe({
    if (isTRUE(r$value)) {
      waiter_hide()
    }
  })

  output$header <- renderUI({
    if (input$name == "") {
      p("DASHBOARD", class = "app-title")
    } else {
      p(glue("{input$name}"), class = "app-title")
    }
  })

  output$prepared_by_out <- renderUI({
    if (input$prepared_by == "") {
      ""
    } else {
      p(glue("Prepared by: {input$prepared_by}"), class = "subtitle")
    }
  })

  ## 1. Scenario B -------------------------------------------------------------
  a <- c(
    "n_detect_a", "n_infect_calc_a", "n_deaths_a", "ifr_a", "ifr_other_a",
    "prop_detect_a", "n_isolate_a", "n_detect_quar_a", "pass_test_a",
    "pass_isol_a", "contact_h_a", "omega_h_a", "house_quar_a", "contact_c_a",
    "omega_c_a", "comm_quar_a"
  )
  walk(a, disable)

  ## 1.1 Update inputs to initially match Scenario A ----

  observeEvent(input$n_detect, {
    updateNumericInput(session, "n_detect_a", value = input$n_detect)
    if (!input$scenario_b) {
    updateNumericInput(session, "n_detect_b", value = input$n_detect)
    }
  })

  observeEvent(input$n_infect_calc, {
    updateRadioButtons(session, "n_infect_calc_a", selected = input$n_infect_calc)
    if (!input$scenario_b) {
    updateRadioButtons(session, "n_infect_calc_b", selected = input$n_infect_calc)
    }
  })

  observeEvent(input$n_deaths, {
    updateNumericInput(session, "n_deaths_a", value = input$n_deaths)
    if (!input$scenario_b) {
    updateNumericInput(session, "n_deaths_b", value = input$n_deaths)
    }
  })

  observeEvent(input$ifr, {
    updateRadioButtons(session, "ifr_a", selected = input$ifr)
    if (!input$scenario_b) {
    updateRadioButtons(session, "ifr_b", selected = input$ifr)
    }
  })

  observeEvent(input$ifr_other, {
    updateNumericInput(session, "ifr_other_a", value = input$ifr_other)
    if (!input$scenario_b) {
    updateNumericInput(session, "ifr_other_b", value = input$ifr_other)
    }
  })

  observeEvent(input$prop_detect, {
    updateSliderInput(session, "prop_detect_a", value = input$prop_detect)
    if (!input$scenario_b) {
      updateSliderInput(session, "prop_detect_b", value = input$prop_detect)
    }
  })

  observeEvent(input$n_isolate, {
    updateNumericInput(session, "n_isolate_a", value = input$n_isolate)
    if (!input$scenario_b) {
      updateNumericInput(session, "n_isolate_b", value = input$n_isolate)
    }
  })

  observeEvent(input$n_isolate_b, {
    updateNumericInput(session, "n_detect_quar_b", value = input$n_isolate_b)
  })

  observeEvent(input$n_detect_quar, {
    updateNumericInput(session, "n_detect_quar_a", value = input$n_detect_quar)
    if (!input$scenario_b) {
    updateNumericInput(session, "n_detect_quar_b", value = input$n_detect_quar)
    }
  })

  output$ifr_warning_b <- renderUI({
    req(input$n_detect_b)
    req(input$n_deaths_b)

    if (n_infect_b() < input$n_detect_b) {
      p(glue(
        "From the current inputs, you are detecting ",
        "{format(round(input$n_detect_b), big.mark = ',')} cases. Per your selected IFR ({ifr_b()}%) ",
        "there are {format(round(n_infect_b()), big.mark = ',')} infected in your community. ",
        "These are not compatible, consider updating your estimated IFR or new case data."
      ),
      style = "background-color: #f1c400;"
      )
    } else {
      ""
    }
  })

  output$n_isolate_warning_b <- renderUI({
    req(input$n_isolate_b)
    req(input$n_detect_b)

    if (input$n_isolate_b > input$n_detect_b) {
      js$bkg_col("n_isolate_b")
      p(glue(
        "From the current inputs, you are detecting ",
        "{format(input$n_detect_b, big.mark = ',')} cases. Per your input above, ",
        "{format(input$n_isolate_b, big.mark = ',')} cases were contacted and isolated. ",
        "These are not compatible. The number of cases detected should be greater than ",
        "the number of cases contacted and isolated. Consider updating your ",
        "number of cases contacted and isolated or new case data."
      ),
      style = "background-color: #f1c400;"
      )
    } else {
      js$bkg_col("n_isolate_b", "#fff")
    }
  })

  output$n_quar_warning_b <- renderUI({
    req(input$n_detect_quar_b)
    req(input$n_isolate_b)

    if (input$n_detect_quar_b > input$n_isolate_b) {
      js$bkg_col("n_detect_quar_b")
      p(glue(
        "From the current inputs, you are detecting ",
        "{format(input$n_detect_quar_b, big.mark = ',')} cases already in quarantine. Per your input above, ",
        "{format(input$n_isolate_b, big.mark = ',')} cases were contacted and isolated. ",
        "These are not compatible. The number of cases contacted and isolated should be greater than ",
        "the number of cases already in quarantine. Consider updating your ",
        "number of cases already in quarantine or your number contacted and isolated."
      ),
      style = "background-color: #f1c400;"
      )
    } else {
      js$bkg_col("n_detect_quar_b", "#fff")
    }
  })

  observeEvent(input$contact_h, {
    updateSliderInput(session, "contact_h_a", value = input$contact_h)
    if (!input$scenario_b) {
      updateSliderInput(session, "contact_h_b", value = input$contact_h)
    }
  })

  observeEvent(input$contact_c, {
    updateSliderInput(session, "contact_c_a", value = input$contact_c)
    if (!input$scenario_b) {
      updateSliderInput(session, "contact_c_b", value = input$contact_c)
    }
  })


  observeEvent(input$omega_c, {
    updateSliderInput(session, "omega_c_a", value = input$omega_c)
    if (!input$scenario_b) {
    updateSliderInput(session, "omega_c_b", value = input$omega_c)
    }
  })

  observeEvent(input$omega_h, {
    updateSliderInput(session, "omega_h_a", value = input$omega_h)
    if (!input$scenario_b) {
      updateSliderInput(session, "omega_h_b", value = input$omega_h)
    }
  })


  observeEvent(listen_tp(), {
    updateSliderTextInput(session, "pass_isol_a", selected = input$pass_isol[2])
    if (!input$scenario_b) {
      updateSliderTextInput(session, "pass_isol_b", selected = input$pass_isol[2])
    }
  })

  observeEvent(listen_tqh(), {
    updateSliderTextInput(session, "house_quar_a", selected = input$house_quar)
    if (!input$scenario_b) {
      updateSliderTextInput(session, "house_quar_b", selected = input$house_quar)
    }
  })

  listen_house_quar_b <- reactive({
    list(input$house_quar_b, input$pass_isol_b)
  })

  observeEvent(listen_house_quar_b(), {
    isol <- input$pass_isol_b
    if (isol == "Symptom onset of case") {
      p <- 0
    } else if (isol == "Day 14+") {
      p <- 14
    } else {
      p <- as.numeric(gsub("Day", "", isol))
    }
    h <- input$house_quar_b
    if (h == "Symptom onset of case") {
      h_t <- 0
    } else if (h == "Day 14+") {
      h_t <- 14
    } else {
      h_t <- as.numeric(gsub("Day", "", h))
    }
    if (p > h_t) {
      updateSliderTextInput(session, "house_quar_b", selected = input$pass_isol_b)
    }
  })

  listen_comm_quar_b <- reactive({
    list(input$comm_quar_b, input$pass_isol_b)
  })

  observeEvent(listen_comm_quar_b(), {
    isol <- input$pass_isol_b
    if (isol == "Symptom onset of case") {
      p <- 0
    } else if (isol == "Day 14+") {
      p <- 14
    } else {
      p <- as.numeric(gsub("Day", "", isol))
    }
    c <- input$comm_quar_b
    if (c == "Symptom onset of case") {
      c_t <- 0
    } else if (c == "Day 14+") {
      c_t <- 14
    } else {
      c_t <- as.numeric(gsub("Day", "", c))
    }

    if (p > c_t) {
      updateSliderTextInput(session, "comm_quar_b", selected = input$pass_isol_b)
    }
  })

  observeEvent(listen_tqc(), {
    updateSliderTextInput(session, "comm_quar_a", selected = input$comm_quar)
    if (!input$scenario_b) {
      updateSliderTextInput(session, "comm_quar_b", selected = input$comm_quar)
    }
  })

  ## 1.2 Input values ----

  rho_s_b <- reactive({
    p_s_d <- (1 - alpha()) / ((1 - alpha()) + mult_b() * alpha())
    p_s_d * prop_detect_isol_passive_b() / (1 - alpha())
  })

  rho_a_b <- reactive({
    p_a_d <- alpha() / (alpha() + (1 / mult_b()) * (1 - alpha()))
    p_a_d * prop_detect_isol_passive_b() / alpha()
  })

  t_p_b <- reactive({
    if (input$pass_isol_b == "Symptom onset of case") {
      return(0)
    } else if (input$pass_isol_b == "Day 14+") {
      return(14)
    } else {
      return(as.numeric(gsub("Day", "", input$pass_isol_b)))
    }
  })

  t_h_b <- reactive({
    if (input$house_quar_b == "Symptom onset of case") {
      return(0)
    } else if (input$house_quar_b == "Day 14+") {
      return(14)
    } else {
    return(as.numeric(gsub("Day", "", input$house_quar_b)))
    }
  })

  t_c_b <- reactive({
    if (input$comm_quar_b == "Symptom onset of case") {
      return(0)
    } else if (input$comm_quar_b == "Day 14+") {
      return(14)
    } else {
     return(as.numeric(gsub("Day", "", input$comm_quar_b)))
    }
  })

  ## 2. Info boxes--------------------------------------------------------------

  output$n_detect2 <- renderInfoBox({
    n_detect_box()
  })

  output$prop_isolate2 <- renderInfoBox({
    prop_isolate_box()
  })

  output$prop_quar2 <- renderInfoBox({
    prop_quar_box()
  })

  output$t_p2 <- renderInfoBox({
    t_p()
  })
  output$omega_h2 <- renderInfoBox({
    omega_h_box()
  })
  output$omega_c2 <- renderInfoBox({
    omega_c_box()
  })
  output$t_hbox2 <- renderInfoBox({
    t_hbox()
  })
  output$t_cbox2 <- renderInfoBox({
    t_cbox()
  })

  ## 2.1 Scenario A

  output$n_detect2a <- renderInfoBox({
    n_detect_box()
  })

  output$prop_isolate2a <- renderInfoBox({
    prop_isolate_box()
  })

  output$prop_quar2a <- renderInfoBox({
    prop_quar_box()
  })

  output$t_p2a <- renderInfoBox({
    t_p()
  })
  output$omega_h2a <- renderInfoBox({
    omega_h_box()
  })
  output$omega_c2a <- renderInfoBox({
    omega_c_box()
  })
  output$t_hbox2a <- renderInfoBox({
    t_hbox()
  })
  output$t_cbox2a <- renderInfoBox({
    t_cbox()
  })


  ## 2.2 Info Boxes Scenario B ----


  output$prop_isol_not_quar_b <- renderInfoBox({
    infoBox(
      value = glue("{round(prop_detect_isol_passive_b() * 100)}%"),
      title = "Isolating",
      subtitle = glue("of infections who were not identified during contact tracing"),
      icon = icon("user-circle-o")
    )
  })

  output$t_p_b <- renderInfoBox({
    time <- t_ps_b() * (1 - alpha_b()) + t_pa_b() * (alpha_b())

    infoBox(
      value = time,
      title = "Delay of",
      subtitle = glue("days, on average, from symptom onset to isolation of surveillance detected infections"),
      icon = icon("calendar-check-o")
    )
  })

  output$omega_h_b_out <- renderInfoBox({
    infoBox(
      value = glue("{round(input$omega_h_b)}%"),
      title = "Quarantined",
      subtitle = glue("of household contacts"),
      icon = icon("home")
    )
  })
  output$omega_c_b_out <- renderInfoBox({
    infoBox(
      value = glue("{round(input$omega_c_b)}%"),
      title = "Quarantined",
      subtitle = glue("of community contacts"),
      icon = icon("users")
    )
  })
  output$t_hbox_b <- renderInfoBox({
    time <- t_qhs_b() * (1 - alpha_b()) + t_qha_b() * (alpha_b())

    infoBox(
      value = time,
      title = "Delay of",
      subtitle = glue("days, on average, from symptom onset of a case to quarantine of household contacts"),
      icon = icon("calendar-check-o")
    )
  })
  output$t_cbox_b <- renderInfoBox({
    time <- t_qcs_b() * (1 - alpha_b()) + t_qca_b() * (alpha_b())

    infoBox(
      value = time,
      title = "Delay of",
      subtitle = glue("days, on average, from symptom onset of a case to quarantine of community contacts"),
      icon = icon("calendar-check-o"),
      color = "black"
    )
  })

  ## 3. Validation -------------------------------------------------------------

  val <- reactive({
    validate(
      need(
        input$R,
        glue(
          "It looks like you removed the assumed R value. ",
          "Double check the Advanced Tab to fix this."
        )
      ),
      need(
        input$contact_h,
        glue(
          "It looks like you removed the estimated number of household contacts per index case ",
          "individuals. Double check the Household Contacts or Advanced Tab to fix this."
        )
      ),
      need(
        input$contact_c,
        glue(
          "It looks like you removed the estimated number of community contacts per index case ",
          "individuals. Double check the Household Contacts or Advanced Tab to fix this."
        )
      ),
      need(
        input$n_detect,
        glue(
          "It looks like you removed the number of cases detected. Check the ",
          "Surveillance and Isolation tab to fix this."
        )
      ),
      need(
        input$n_isolate,
        glue(
          "It looks like you removed the number of cases contacted and isolated. Check the ",
          "Surveillance and Isolation tab to fix this."
        )
      ),
      need(
        input$n_detect_quar,
        glue(
          "It looks like you removed the number of cases detected already in quarantine. Check the ",
          "Surveillance and Isolation tab to fix this."
        )
      ),
      need(
        n_infect() >= input$n_detect,
        glue(
          "The total number of infections must be greater than the number of ",
          "cases detected. Check the Surveillance and Isolation tab to ",
          "fix this."
        )
      ),
      need(
        input$n_detect >= input$n_isolate,
        glue(
          "The number of cases detected must be greater than the number of ",
          "cases contacted and isolated. Check the Surveillance and Isolation tab to ",
          "fix this."
        )
      ),
      need(
        input$n_isolate >= input$n_detect_quar,
        glue(
          "The number of cases contacted and isolated must be greater than the number of ",
          "cases detected already in quarantine. Check the Surveillance and Isolation tab to ",
          "fix this."
        )
      )
    )
  })

  val_b <- reactive({
    validate(
      need(
        input$n_detect_b,
        glue(
          "It looks like you removed the number of cases detected. Check the ",
          "Scenario B to fix this."
        )
      ),
      need(
        input$n_isolate_b,
        glue(
          "It looks like you removed the number of cases contacted and isolated. Check the ",
          "Scenario B to fix this."
        )
      ),
      need(
        input$n_detect_quar_b,
        glue(
          "It looks like you removed the number of cases detected already in quarantine. Check the ",
          "Scenario B to fix this."
        )
      ),
      need(
        n_infect_b() >= input$n_detect_b,
        glue(
          "The total number of infections must be greater than the number of ",
          "cases detected. Check the Scenario B to ",
          "fix this."
        )
      ),
      need(
        input$n_detect_b >= input$n_isolate_b,
        glue(
          "The number of cases detected must be greater than the number of ",
          "cases contacted and isolated. Check the Scenario B to ",
          "fix this."
        )
      ),
      need(
        input$n_isolate_b >= input$n_detect_quar_b,
        glue(
          "The number of cases contacted and isolated must be greater than the number of ",
          "cases detected already in quarantine. Check the Scenario B to ",
          "fix this."
        )
      )
    )
  })

  ## 4. Calculate R ------------------------------------------------------------

  ## 4.1 Calculate R A -----
  pqc <- reactive({
    val()

    get_pqc_equilibrium(
      alpha = alpha(),
      omega_c = omega_c(),
      omega_h = omega_h(),
      omega_q = omega_q(),
      rho_s = rho_s(),
      rho_a = rho_a(),
      R = input$R,
      kappa = input$kappa,
      eta = eta(),
      nu = input$nu,
      t_ps = t_ps(),
      t_pa = t_pa(),
      t_qcs = t_qcs(),
      t_qca = t_qca(),
      t_qhs = t_qhs(),
      t_qha = t_qha(),
      t_q = t_q(),
      shape = as.numeric(input$generation)
    )
  })

  perc_q <- reactive({
    round(get_prop_identified(pqc()) * 100, 1)
  })

  output$prop_q <- renderHighchart({
    hc_gauge(perc_q(),
      title = "",
      name = "percent of infections identified"
    )
  })

  output$r_ <- renderUI({
    p(glue("{round(input$R, 1)}"), class = "r-val")
  })

  r_eff <- reactive({
    get_r_effective(pqc(),
      alpha = alpha(),
      R = input$R,
      kappa = input$kappa,
      eta = eta(),
      nu = input$nu,
      t_ps = t_ps(),
      t_pa = t_pa(),
      t_qcs = t_qcs(),
      t_qca = t_qca(),
      t_qhs = t_qhs(),
      t_qha = t_qha(),
      t_q = t_q(),
      shape = as.numeric(input$generation)
    )
  })
  output$r_eff <- renderUI({
    p(glue("{round(r_eff(), 1)}"), class = "r-val")
  })

  ## 4.2 Calculate R B ----

  pqc_b <- reactive({
    val_b()

    get_pqc_equilibrium(
      alpha = alpha_b(),
      omega_c = omega_c_b(),
      omega_h = omega_h_b(),
      omega_q = omega_q_b(),
      rho_s = rho_s_b(),
      rho_a = rho_a_b(),
      R = input$R_b,
      kappa = input$kappa_b,
      eta = eta_b(),
      nu = input$nu_b,
      t_ps = t_ps_b(),
      t_pa = t_pa_b(),
      t_qcs = t_qcs_b(),
      t_qca = t_qca_b(),
      t_qhs = t_qhs_b(),
      t_qha = t_qha_b(),
      t_q = t_q_b(),
      shape = as.numeric(input$generation_b)
    )
  })

  perc_q_b <- reactive({
    round(get_prop_identified(pqc_b()) * 100, 1)
  })

  output$prop_q_b <- renderHighchart({
    hc_gauge(perc_q_b(),
      title = "",
      name = "percent of infections identified"
    )
  })

  output$prop_q_a <- renderHighchart({
    hc_gauge(perc_q(),
      title = "",
      name = "percent of infections identified"
    )
  })

  output$r_b <- renderUI({
    p(glue("{round(input$R, 1)}"), class = "r-val")
  })

  output$r_a <- renderUI({
    p(glue("{round(input$R, 1)}"), class = "r-val")
  })

  r_eff_b <- reactive({
    val_b()
    get_r_effective(pqc_b(),
      alpha = alpha_b(),
      R = input$R_b,
      kappa = input$kappa_b,
      eta = eta_b(),
      nu = input$nu_b,
      t_ps = t_ps_b(),
      t_pa = t_pa_b(),
      t_qcs = t_qcs_b(),
      t_qca = t_qca_b(),
      t_qhs = t_qhs_b(),
      t_qha = t_qha_b(),
      t_q = t_q_b(),
      shape = as.numeric(input$generation_b)
    )
  })

  output$r_eff_b <- renderUI({
    p(glue("{round(r_eff_b(), 1)}"), class = "r-val")
  })

  output$r_eff_a <- renderUI({
    p(glue("{round(r_eff(), 1)}"), class = "r-val")
  })

  ## 5. Surveillance Plot ------------------------------------------------------

  ## 5.1 Pass plot percent ----

  output$pass_plot <- renderHighchart({
    val()
    r$value <- FALSE

    if (input$scenario_b) {
      val_b()

      grid_b <- expand.grid(
        alpha = alpha_b(),
        R = input$R_b,
        kappa = input$kappa_b,
        eta = eta_b(),
        nu = input$nu_b,
        t_ps = t_ps_b(),
        t_pa = t_pa_b(),
        t_qcs = t_qcs_b(),
        t_qca = t_qca_b(),
        t_qhs = t_qhs_b(),
        t_qha = t_qha_b(),
        t_q = t_q_b(),
        omega_c = omega_c_b(),
        omega_h = omega_h_b(),
        omega_q = omega_q_b(),
        rho_s = seq(0, 1, 0.01),
        rho_a = 1,
        offset = - 2.31,
        shape = as.numeric(input$generation_b),
        rate = 0.69
      )

      grid_b <- grid_b %>%
        mutate(
          rho_a = pmin(rho_s * mult_b(), 1)
        )

      d_b <- pmap_df(grid_b, tti:::get_r_effective_df_one)

      d_b <- d_b %>%
        mutate(prop_detect = (rho_s * (1 - alpha) + rho_a * alpha) * 100)

      b_p <- tibble(
        x = prop_detect_isol_passive_b() * 100,
        y = r_eff_b(),
        name = "Scenario B"
      )
    }

    grid <- expand.grid(
      alpha = alpha(),
      R = input$R,
      kappa = input$kappa,
      eta = eta(),
      nu = input$nu,
      t_ps = t_ps(),
      t_pa = t_pa(),
      t_qcs = t_qcs(),
      t_qca = t_qca(),
      t_qhs = t_qhs(),
      t_qha = t_qha(),
      t_q = t_q(),
      omega_c = omega_c(),
      omega_h = omega_h(),
      omega_q = omega_q(),
      rho_s = seq(0, 1, 0.01),
      rho_a = 1,
      offset = - 2.31,
      shape = as.numeric(input$generation),
      rate = 0.69
    )

    grid <- grid %>%
      mutate(
        rho_a = pmin(rho_s * mult(), 1)
      )

    d <- pmap_df(grid, tti:::get_r_effective_df_one)

    d <- d %>%
      mutate(prop_detect = (rho_s * (1 - alpha) + rho_a * alpha) * 100)

    you_are_here <- tibble(
      x = (rho_s() * (1 - alpha()) + rho_a() * alpha()) * 100,
      y = r_eff(),
      name = "You are here"
    )
    p <- hchart(
      d, "line",
      hcaes(x = prop_detect, y = r_effective)
    ) %>%
      hc_title(text = "Impact of the percent of infections isolated who were not identified during contact tracing on the reproductive number") %>%
      hc_add_theme(idd_hc) %>%
      hc_add_series(you_are_here,
        type = "scatter",
        hcaes(x = x, y = y, group = name),
        color = "blue",
        showInLegend = TRUE
      ) %>%
      hc_yAxis(
        title = list(
          text = "R",
          rotation = "0"
        ),
        plotLines = list(
          list(
            value = 1,
            color = "#444",
            width = 2,
            zIndex = 4,
            dashStyle = "shortdash",
            label = list(
              text = "target",
              style = list(
                color = "#444",
                fontWeight = "bold"
              )
            )
          )
        ),
        min = min(1, min(d$r_effective)),
        max = max(max(d$r_effective), input$R)
      ) %>%
      hc_xAxis(title = list(text = "Percent of infections isolated who were not identified during contact tracing")) %>%
      hc_tooltip(
        valueDecimals = 2,
        pointFormat = "R: {point.y}<br>Percent Isolated: {point.x:.0f}%"
      )
    if (input$scenario_b) {
      p <- p %>%
        hc_add_series(b_p,
          type = "scatter",
          hcaes(x = x, y = y, group = name),
          color = "green",
          showInLegend = TRUE
        ) %>%
        hc_add_series(d_b,
          type = "line",
          hcaes(x = prop_detect, y = r_effective),
          color = "green"
        )
      r$value <- TRUE
      return(p)
    }
    r$value <- TRUE
    p
  })

  ## 5.2 Pass Plot Delay ----
  output$pass_plot_delay <- renderHighchart({
    val()
    r$value <- FALSE

    if (input$scenario_b) {
      val_b()

      diff <- t_pa_b() - t_ps_b()

      if (t_ps_b() <= t_pa_b()) {
        t_ps_b <- 0:14
      } else {
        t_ps_b <- (t_ps_b() - t_pa_b()):(14 + (t_ps_b() - t_pa_b()))
      }

      grid_b <- expand.grid(
        alpha = alpha_b(),
        R = input$R_b,
        kappa = input$kappa_b,
        eta = eta_b(),
        nu = input$nu_b,
        t_ps = t_ps_b,
        t_pa = 1,
        t_qcs = t_qcs_b(),
        t_qca = t_qca_b(),
        t_qhs = t_qhs_b(),
        t_qha = t_qha_b(),
        t_q = t_q_b(),
        omega_c = omega_c_b(),
        omega_h = omega_h_b(),
        omega_q = omega_q(),
        rho_s = rho_s_b(),
        rho_a = rho_a_b(),
        offset = - 2.31,
        shape = as.numeric(input$generation_b),
        rate = 0.69
      )

      grid_b <- grid_b %>%
        mutate(
          t_pa = t_ps + diff
        )

      d_b <- pmap_df(grid_b, tti:::get_r_effective_df_one)


      d_b <- d_b %>%
        mutate(t = t_ps * (1 - alpha) + t_pa * alpha)

      b_p <- tibble(
        x = t_ps_b() * (1 - alpha_b()) + t_pa_b() * alpha_b(),
        y = r_eff_b(),
        name = "Scenario B"
      )
    }

    diff <- t_pa() - t_ps()

    if (t_ps() <= t_pa()) {
      t_ps <- 0:14
    } else {
      t_ps <- (t_ps() - t_pa()):(14 + (t_ps() - t_pa()))
    }

    grid <- expand.grid(
      alpha = alpha(),
      R = input$R,
      kappa = input$kappa,
      eta = eta(),
      nu = input$nu,
      t_ps = t_ps,
      t_pa = 1,
      t_qcs = t_qcs(),
      t_qca = t_qca(),
      t_qhs = t_qhs(),
      t_qha = t_qha(),
      t_q = t_q(),
      omega_c = omega_c(),
      omega_h = omega_h(),
      omega_q = omega_q(),
      rho_s = rho_s(),
      rho_a = rho_a(),
      offset = - 2.31,
      shape = as.numeric(input$generation),
      rate = 0.69
    )

    grid <- grid %>%
      mutate(
        t_pa = t_ps + diff
      )

    d <- pmap_df(grid, tti:::get_r_effective_df_one)

    d <- d %>%
      mutate(t = t_ps * (1 - alpha) + t_pa * alpha)

    you_are_here <- tibble(
      x = t_ps() * (1 - alpha()) + t_pa() * alpha(),
      y = r_eff(),
      name = "You are here"
    )
    p <- hchart(
      d, "line",
      hcaes(x = t, y = r_effective)
    ) %>%
      hc_title(text = "Impact of the delay from symptom onset to isolation on the reproductive number") %>%
      hc_add_theme(idd_hc) %>%
      hc_add_series(you_are_here,
        type = "scatter",
        hcaes(x = x, y = y, group = name),
        color = "blue",
        showInLegend = TRUE
      ) %>%
      hc_yAxis(
        title = list(
          text = "R",
          rotation = "0"
        ),
        plotLines = list(
          list(
            value = 1,
            color = "#444",
            width = 2,
            zIndex = 4,
            dashStyle = "shortdash",
            label = list(
              text = "target",
              style = list(
                color = "#444",
                fontWeight = "bold"
              )
            )
          )
        ),
        min = min(1, min(d$r_effective)),
        max = max(max(d$r_effective), input$R)
      ) %>%
      hc_xAxis(title = list(text = "Days from symptom onset to isolation")) %>%
      hc_tooltip(
        valueDecimals = 2,
        pointFormat = "R: {point.y}<br>{point.x:.0f} days"
      )
    if (input$scenario_b) {
      p <- p %>%
        hc_add_series(b_p,
          type = "scatter",
          hcaes(x = x, y = y, group = name),
          color = "green",
          showInLegend = TRUE
        ) %>%
        hc_add_series(d_b,
          type = "line",
          hcaes(x = t, y = r_effective),
          color = "green"
        )
      r$value <- TRUE
      return(p)
    }
    r$value <- TRUE
    p
  })


  ## 6. House Plot -------------------------------------------------------------

  ## 6.1 House Plot proportion ----
  output$house_plot <- renderHighchart({
    val()
    r$value <- FALSE


    if (input$scenario_b) {
      val_b()

      grid_b <- expand.grid(
        alpha = alpha_b(),
        R = input$R_b,
        kappa = input$kappa_b,
        eta = eta_b(),
        nu = input$nu_b,
        t_ps = t_ps_b(),
        t_pa = t_pa_b(),
        t_qcs = t_qcs_b(),
        t_qca = t_qca_b(),
        t_qhs = t_qhs_b(),
        t_qha = t_qha_b(),
        t_q = t_q_b(),
        omega_c = omega_c_b(),
        omega_h = seq(0, 1, 0.1),
        omega_q = omega_q_b(),
        rho_s = rho_s_b(),
        rho_a = rho_a_b(),
        offset = - 2.31,
        shape = as.numeric(input$generation_b),
        rate = 0.69
      )

      d_b <- pmap_df(grid_b, tti:::get_r_effective_df_one)

      d_b <- d_b %>%
        mutate(omega_h = omega_h * 100)

      b_h <- tibble(
        x = input$omega_h_b,
        y = r_eff_b(),
        name = "Scenario B"
      )
    }

    grid <- expand.grid(
      alpha = alpha(),
      R = input$R,
      kappa = input$kappa,
      eta = eta(),
      nu = input$nu,
      t_ps = t_ps(),
      t_pa = t_pa(),
      t_qcs = t_qcs(),
      t_qca = t_qca(),
      t_qhs = t_qhs(),
      t_qha = t_qha(),
      t_q = t_q(),
      omega_c = omega_c(),
      omega_h = seq(0, 1, 0.1),
      omega_q = omega_q(),
      rho_s = rho_s(),
      rho_a = rho_a(),
      offset = - 2.31,
      shape = as.numeric(input$generation),
      rate = 0.69
    )
    d <- pmap_df(grid, tti:::get_r_effective_df_one)

    d <- d %>%
      mutate(omega_h = omega_h * 100)

    you_are_here <- tibble(
      x = input$omega_h,
      y = r_eff(),
      name = "You are here"
    )

    p <- hchart(
      d, "line",
      hcaes(x = omega_h, y = r_effective)
    ) %>%
      hc_title(text = "Impact of the percent of household contacts that are contacted and quarantined on the reproductive number") %>%
      hc_add_theme(idd_hc) %>%
      hc_add_series(you_are_here,
        type = "scatter",
        hcaes(x = x, y = y, group = name),
        color = "blue",
        showInLegend = TRUE
      ) %>%
      hc_yAxis(
        title = list(
          text = "R",
          rotation = "0"
        ),
        plotLines = list(
          list(
            value = 1,
            color = "#444",
            width = 2,
            zIndex = 4,
            dashStyle = "shortdash",
            label = list(
              text = "target",
              style = list(
                color = "#444",
                fontWeight = "bold"
              )
            )
          )
        ),
        min = min(1, min(d$r_effective)),
        max = max(max(d$r_effective), input$R)
      ) %>%
      hc_xAxis(title = list(text = "Percent of household contacts that are contacted and quarantined")) %>%
      hc_tooltip(
        valueDecimals = 2,
        pointFormat = "R: {point.y}<br>Percent of household contacts quarantined: {point.x}%"
      )
    if (input$scenario_b) {
      p <- p %>%
        hc_add_series(b_h,
          type = "scatter",
          hcaes(x = x, y = y, group = name),
          color = "green",
          showInLegend = TRUE
        ) %>%
        hc_add_series(d_b,
          type = "line",
          hcaes(x = omega_h, y = r_effective),
          color = "green"
        )
    }
    r$value <- TRUE
    p
  })

  ## 6.2 House Plot Delay ----

  output$house_plot_delay <- renderHighchart({
    val()
    r$value <- FALSE


    if (input$scenario_b) {
      val_b()

      diff <- t_qha_b() - t_qhs_b()

      if (t_qhs_b() <= t_qha_b()) {
        t_qhs_b <- 0:14
      } else {
        t_qhs_b <- (t_qhs_b() - t_qha_b()):(14 + (t_qhs_b() - t_qha_b()))
      }

      grid_b <- expand.grid(
        alpha = alpha_b(),
        R = input$R_b,
        kappa = input$kappa_b,
        eta = eta_b(),
        nu = input$nu_b,
        t_ps = t_ps_b(),
        t_pa = t_pa_b(),
        t_qcs = t_qcs_b(),
        t_qca = t_qca_b(),
        t_qhs = t_qhs_b,
        t_qha = 1,
        t_q = t_q(),
        omega_c = omega_c_b(),
        omega_h = omega_h_b(),
        omega_q = omega_q_b(),
        rho_s = rho_s_b(),
        rho_a = rho_a_b(),
        offset = - 2.31,
        shape = as.numeric(input$generation_b),
        rate = 0.69
      )

      grid_b <- grid_b %>%
        mutate(
          t_qha = t_qhs + diff
        )

      d_b <- pmap_df(grid_b, tti:::get_r_effective_df_one)

      d_b <- d_b %>%
        mutate(t = t_qhs * (1 - alpha) + t_qha * alpha)

      b_h <- tibble(
        x = t_qhs_b() * (1 - alpha_b()) + t_qha_b() * alpha_b(),
        y = r_eff_b(),
        name = "Scenario B"
      )
    }

    diff <- t_qha() - t_qhs()

    if (t_qhs() <= t_qha()) {
      t_qhs <- 0:14
    } else {
      t_qhs <- (t_qhs() - t_qha()):(14 + (t_qhs() - t_qha()))
    }

    grid <- expand.grid(
      alpha = alpha(),
      R = input$R,
      kappa = input$kappa,
      eta = eta(),
      nu = input$nu,
      t_ps = t_ps(),
      t_pa = t_pa(),
      t_qcs = t_qcs(),
      t_qca = t_qca(),
      t_qhs = t_qhs,
      t_qha = 1,
      t_q = t_q(),
      omega_c = omega_c(),
      omega_h = omega_h(),
      omega_q = omega_q(),
      rho_s = rho_s(),
      rho_a = rho_a(),
      offset = - 2.31,
      shape = as.numeric(input$generation),
      rate = 0.69
    )

    grid <- grid %>%
      mutate(
        t_qha = t_qhs + diff
      )

    d <- pmap_df(grid, tti:::get_r_effective_df_one)

    d <- d %>%
      mutate(t = t_qhs * (1 - alpha) + t_qha * alpha)

    you_are_here <- tibble(
      x = t_qhs() * (1 - alpha()) + t_qha() * alpha(),
      y = r_eff(),
      name = "You are here"
    )

    p <- hchart(
      d, "line",
      hcaes(x = t, y = r_effective)
    ) %>%
      hc_title(text = "Impact of the delay in quarantine of household contacts on the reproductive number") %>%
      hc_add_theme(idd_hc) %>%
      hc_add_series(you_are_here,
        type = "scatter",
        hcaes(x = x, y = y, group = name),
        color = "blue",
        showInLegend = TRUE
      ) %>%
      hc_yAxis(
        title = list(
          text = "R",
          rotation = "0"
        ),
        plotLines = list(
          list(
            value = 1,
            color = "#444",
            width = 2,
            zIndex = 4,
            dashStyle = "shortdash",
            label = list(
              text = "target",
              style = list(
                color = "#444",
                fontWeight = "bold"
              )
            )
          )
        ),
        min = min(1, min(d$r_effective)),
        max = max(max(d$r_effective), input$R)
      ) %>%
      hc_xAxis(title = list(text = "Delay in quarantine of household contacts")) %>%
      hc_tooltip(
        valueDecimals = 2,
        pointFormat = "R: {point.y}<br>{point.x} days"
      )
    if (input$scenario_b) {
      p <- p %>%
        hc_add_series(b_h,
          type = "scatter",
          hcaes(x = x, y = y, group = name),
          color = "green",
          showInLegend = TRUE
        ) %>%
        hc_add_series(d_b,
          type = "line",
          hcaes(x = t, y = r_effective),
          color = "green"
        )
    }
    r$value <- TRUE
    p
  })


  ## 7. Community Plot ---------------------------------------------------------

  ## 7.1 Community Plot Percent ----

  output$comm_plot <- renderHighchart({
    val()
    r$value <- FALSE

    if (input$scenario_b) {
      val_b()

      grid_b <- expand.grid(
        alpha = alpha_b(),
        R = input$R_b,
        kappa = input$kappa_b,
        eta = eta_b(),
        nu = input$nu_b,
        t_ps = t_ps_b(),
        t_pa = t_pa_b(),
        t_qcs = t_qcs_b(),
        t_qca = t_qca_b(),
        t_qhs = t_qhs_b(),
        t_qha = t_qha_b(),
        t_q = t_q_b(),
        omega_c = seq(0, 1, 0.1),
        omega_h = omega_h_b(),
        omega_q = omega_q_b(),
        rho_s = rho_s_b(),
        rho_a = rho_a_b(),
        offset = - 2.31,
        shape = as.numeric(input$generation_b),
        rate = 0.69
      )


      d_b <- pmap_df(grid_b, tti:::get_r_effective_df_one)

      d_b <- d_b %>%
        mutate(omega_c = omega_c * 100)

      b_c <- tibble(
        x = input$omega_c_b,
        y = r_eff_b(),
        name = "Scenario B"
      )
    }


    grid <- expand.grid(
      alpha = alpha(),
      R = input$R,
      kappa = input$kappa,
      eta = eta(),
      nu = input$nu,
      t_ps = t_ps(),
      t_pa = t_pa(),
      t_qcs = t_qcs(),
      t_qca = t_qca(),
      t_qhs = t_qhs(),
      t_qha = t_qha(),
      t_q = t_q(),
      omega_c = seq(0, 1, 0.1),
      omega_h = omega_h(),
      omega_q = omega_q(),
      rho_s = rho_s(),
      rho_a = rho_a(),
      offset = - 2.31,
      shape = as.numeric(input$generation),
      rate = 0.69
    )
    d <- pmap_df(grid, tti:::get_r_effective_df_one)
    d <- d %>%
      mutate(omega_c = omega_c * 100)

    you_are_here <- tibble(
      x = input$omega_c,
      y = r_eff(),
      name = "You are here"
    )

    p <- hchart(
      d, "line",
      hcaes(x = omega_c, y = r_effective)
    ) %>%
      hc_title(text = "Impact of the percent of community contacts that are contacted and quarantined on the reproductive number") %>%
      hc_add_theme(idd_hc) %>%
      hc_add_series(you_are_here,
        type = "scatter",
        hcaes(x = x, y = y, group = name),
        color = "blue",
        showInLegend = TRUE
      ) %>%
      hc_yAxis(
        title = list(
          text = "R",
          rotation = "0"
        ),
        plotLines = list(
          list(
            value = 1,
            color = "#444",
            width = 2,
            zIndex = 4,
            dashStyle = "shortdash",
            label = list(
              text = "target",
              style = list(
                color = "#444",
                fontWeight = "bold"
              )
            )
          )
        ),
        min = min(1, min(d$r_effective)),
        max = max(max(d$r_effective), input$R)
      ) %>%
      hc_xAxis(title = list(text = "Percent of community contacts that are contacted and quarantined")) %>%
      hc_tooltip(
        valueDecimals = 2,
        pointFormat = "R: {point.y}<br>Percent of community contacts quarantined: {point.x}%"
      )
    if (input$scenario_b) {
      p <- p %>%
        hc_add_series(b_c,
          type = "scatter",
          hcaes(x = x, y = y, group = name),
          color = "green",
          showInLegend = TRUE
        ) %>%
        hc_add_series(d_b,
          type = "line",
          hcaes(x = omega_c, y = r_effective),
          color = "green"
        )
    }
    r$value <- TRUE
    p
  })

  ## 7.2 Community Plot Delay ----

  output$comm_plot_delay <- renderHighchart({
    val()
    r$value <- FALSE

    if (input$scenario_b) {
      val_b()

      diff <- t_qca_b() - t_qcs_b()

      if (t_qcs_b() <= t_qca_b()) {
        t_qcs_b <- 0:14
      } else {
        t_qcs_b <- (t_qcs_b() - t_qca_b()):(14 + (t_qcs_b() - t_qca_b()))
      }

      grid_b <- expand.grid(
        alpha = alpha_b(),
        R = input$R_b,
        kappa = input$kappa_b,
        eta = eta_b(),
        nu = input$nu_b,
        t_ps = t_p_b(),
        t_pa = t_p_b(),
        t_qcs = t_qcs_b,
        t_qca = 1,
        t_qhs = t_qhs_b(),
        t_qha = t_qha_b(),
        t_q = t_q_b(),
        omega_c = omega_c_b(),
        omega_h = omega_h_b(),
        omega_q = omega_q_b(),
        rho_s = rho_s_b(),
        rho_a = rho_a_b(),
        offset = - 2.31,
        shape = as.numeric(input$generation_b),
        rate = 0.69
      )

      grid_b <- grid_b %>%
        mutate(t_qca = t_qcs + diff)

      d_b <- pmap_df(grid_b, tti:::get_r_effective_df_one)

      d_b <- d_b %>%
        mutate(t = t_qcs * (1 - alpha) + t_qca * alpha)

      b_c <- tibble(
        x = t_qcs_b() * (1 - alpha_b()) + t_qca_b() * alpha_b(),
        y = r_eff_b(),
        name = "Scenario B"
      )
    }

    diff <- t_qca() - t_qcs()

    if (t_qcs() <= t_qca()) {
      t_qcs <- 0:14
    } else {
      t_qcs <- (t_qcs() - t_qca()):(14 + (t_qcs() - t_qca()))
    }

    grid <- expand.grid(
      alpha = alpha(),
      R = input$R,
      kappa = input$kappa,
      eta = eta(),
      nu = input$nu,
      t_ps = t_ps(),
      t_pa = t_pa(),
      t_qcs = t_qcs,
      t_qca = 1,
      t_qhs = t_qhs(),
      t_qha = t_qha(),
      t_q = t_q(),
      omega_c = omega_c(),
      omega_h = omega_h(),
      omega_q = omega_q(),
      rho_s = rho_s(),
      rho_a = rho_a(),
      offset = - 2.31,
      shape = as.numeric(input$generation),
      rate = 0.69
    )
    grid <- grid %>%
      mutate(
        t_qca = t_qcs + diff
      )

    d <- pmap_df(grid, tti:::get_r_effective_df_one)

    d <- d %>%
      mutate(t = t_qcs * (1 - alpha) + t_qca * alpha)

    you_are_here <- tibble(
      x = t_qcs() * (1 - alpha()) + t_qca() * alpha(),
      y = r_eff(),
      name = "You are here"
    )

    p <- hchart(
      d, "line",
      hcaes(x = t, y = r_effective)
    ) %>%
      hc_title(text = "Impact of the delay in the quarantining of community contacts on the reproductive number") %>%
      hc_add_theme(idd_hc) %>%
      hc_add_series(you_are_here,
        type = "scatter",
        hcaes(x = x, y = y, group = name),
        color = "blue",
        showInLegend = TRUE
      ) %>%
      hc_yAxis(
        title = list(
          text = "R",
          rotation = "0"
        ),
        plotLines = list(
          list(
            value = 1,
            color = "#444",
            width = 2,
            zIndex = 4,
            dashStyle = "shortdash",
            label = list(
              text = "target",
              style = list(
                color = "#444",
                fontWeight = "bold"
              )
            )
          )
        ),
        min = min(1, min(d$r_effective)),
        max = max(max(d$r_effective), input$R)
      ) %>%
      hc_xAxis(title = list(text = "Delay in quarantine of community contacts")) %>%
      hc_tooltip(
        valueDecimals = 2,
        pointFormat = "R: {point.y}<br>{point.x} days"
      )
    if (input$scenario_b) {
      p <- p %>%
        hc_add_series(b_c,
          type = "scatter",
          hcaes(x = x, y = y, group = name),
          color = "green",
          showInLegend = TRUE
        ) %>%
        hc_add_series(d_b,
          type = "line",
          hcaes(x = t, y = r_effective),
          color = "green"
        )
    }
    r$value <- TRUE
    p
  })

  ## Assumptions -------------------------------------------------------------

  output$eta <- renderUI({
    glue("The percent of contacts that are household contacts is {round(eta() * 100)}.")
  })

  output$natural_text <- renderUI({
    HTML(glue(
      "<span style='font-weight:bold;'>The output presented in this application assumes: ",
      "</span><span class='assumption'>{input$alpha}%</span> of infections are asymptomatic (people infected who never develop symptoms)."
    ))
  })

  output$detect_text <- renderUI({
    HTML(glue(
      "<p><span style='font-weight:bold;'>You input the following information ",
      "on the 'Surveillance and Isolation' page: </span>The percent of infections isolated that ",
      "are not already in quarantine is <span class='assumption'>{round(prop_detect_isol_passive() * 100)}%</span>. ",
      "The output presented in this application has default assumptions about the disease dynamics of",
      " asymptomatic and symptomatic cases.</p>",
      "<p><span style='font-weight:bold;'>The output presented in this application assumes: </span>",
      "Asymptomatic cases (people infected who never develop symptoms) are ",
      "<span class='assumption'>{mult()}</span> times as likely as symptomatic cases to be",
      " detected via surveillance. The probability of being identified via ",
      "surveillance for symptomatic persons is ",
      "<span class='assumption'>{round(rho_s(), 3)}</span>",
      " and the probability ",
      "of being identified via surveillance for asymptomatic persons ",
      "is <span class='assumption'>{round(rho_a(), 3)}</span>. </p>",
      "<p><span style='font-weight:bold;'>You input the following information ",
      "in the 'Household Contact Tracing' and 'Community Contact Tracing' tabs:</span> ",
      "The percent of household contacts that are traced and quarantined ",
      "is <span class='assumption'>{input$omega_h}%</span> and of community contacts is ",
      "<span class='assumption'>{input$omega_c}%</span>.</p>"
    ))
  })

  output$transmit_text <- renderUI({
    HTML(glue(
      "<span style='font-weight:bold;'>The output presented in this ",
      "application assumes: </span>Each individual, on average, is assumed to infect ",
      "<span class='assumption'>{input$R}</span> ",
      "individuals in the absence of intervention. Each individual is ",
      "assumed to have <span class='assumption'>{round(eta() * 100, 1)}%</span> ",
      "of their contacts inside their ",
      "household. The relative risk of infection for a household contact",
      " compared to a community contact is ",
      "<span class='assumption'>{input$nu}</span>. The relative ",
      "transmissibility of asymptomatic individuals (people infected who ",
      "never develop symptoms) compared to ",
      "symptomatic individuals is assumed to be ",
      "<span class='assumption'>{input$kappa}</span>. The median generation time is assumed to be ",
      "<span class='assumption'>{if (input$generation == '2.12') '5.4 days' else if (input$generation == '3') '6.7 days' else '8.1 days'}",
      "</span>."
    ))
  })

  output$time_text <- renderUI({
    HTML(glue(
      "You input information about the tracing timing in the 'Surveillance and Isolation', ",
      "'Household Contact Tracing', and 'Community Contact Tracing' tabs. The output presented ",
      "in this application has default assumptions about the disease dynamics ",
      "of asymptomatic and symptomatic cases. ",
      "<p><span style='font-weight:bold;'>The output presented in this application assumes: </span>The time delay from symptom onset of the case to isolation of a symptomatic person ",
      "detected via surveillance is <span class='assumption'>{t_ps()}</span> ",
      "days. The time delay from",
      " symptom onset of the case to isolation of an asymptomatic person (person infected who ",
      "never develops symptoms) detected via ",
      "surveillance is <span class='assumption'>{t_pa()}</span> days. The time ",
      "delay from symptom onset of the case to quarantine",
      " of a person infected by a symptomatic community ",
      "contact is <span class='assumption'>{t_qcs()}</span>. The ",
      "time delay from symptom onset of the case ",
      "to quarantine of a person infected by a symptomatic ",
      "household contact is <span class='assumption'>{t_qhs()}</span>. ",
      "The time delay from symtom onset of the case ",
      "to quarantine of person infected by an asymptomatic ",
      "household contact is <span class='assumption'>{t_qha()}</span>. The time ",
      "delay from symptom onset of the case to ",
      "quarantine of person with infected by an asymptomatic ",
      "community contact is <span class='assumption'>{t_qca()}</span>."
    ))
  })


  ## Report ------------------------------------------------------------------

  report <- reactiveValues(
    type = "word_document",
    filename = "contessa-report.docx",
    temp = "report-word.Rmd"
  )

  listen_report <- reactive({
    list(input$word, input$report_name)
  })

  observeEvent(listen_report(), {
    if (input$word == "word") {
      report$type <- "word_document"
      report$filename <- glue("{input$report_name}.docx")
      report$temp <- "report-word.Rmd"
    }
    if (input$word == "pdf") {
      report$type <- "pdf_document"
      report$filename <- glue("{input$report_name}.pdf")
      report$temp <- "report-pdf.Rmd"
    }
  })



  output$report <- downloadHandler(
    filename <- function() {
      report$filename
    },
    content <- function(file) {
      withProgress(message = "Download in progress...", {
      tempReport <- file.path(tempdir(), report$temp)

      if (input$scenario_b) {

        file.copy("report_versionB.Rmd", tempReport, overwrite = TRUE)

        params <- list(
          name = input$name,
          prepared_by = input$prepared_by,
          mult = mult(),
          alpha = alpha(),
          omega_c = omega_c(),
          omega_h = omega_h(),
          omega_q = omega_q(),
          rho_s = rho_s(),
          rho_a = rho_a(),
          R = input$R,
          kappa = input$kappa,
          eta = eta(),
          nu = input$nu,
          generation = as.numeric(input$generation),
          t_ps = t_ps(),
          t_pa = t_pa(),
          t_qcs = t_qcs(),
          t_qca = t_qca(),
          t_qhs = t_qhs(),
          t_qha = t_qha(),
          t_q = t_q(),
          alpha_b = alpha_b(),
          mult_b = mult_b(),
          omega_c_b = omega_c_b(),
          omega_h_b = omega_h_b(),
          omega_q_b = omega_q_b(),
          rho_s_b = rho_s_b(),
          rho_a_b = rho_a_b(),
          R_b = input$R_b,
          kappa_b = input$kappa_b,
          eta_b = eta_b(),
          nu_b = input$nu_b,
          t_ps_b = t_ps_b(),
          t_pa_b = t_pa_b(),
          t_qcs_b = t_qcs_b(),
          t_qca_b = t_qca_b(),
          t_qhs_b = t_qhs_b(),
          t_qha_b = t_qha_b(),
          t_q_b = t_q_b(),
          generation_b = as.numeric(input$generation_b)
        )
      } else {
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

        params <- list(
          name = input$name,
          prepared_by = input$prepared_by,
          mult = mult(),
          alpha = alpha(),
          omega_c = omega_c(),
          omega_h = omega_h(),
          omega_q = omega_q(),
          rho_s = rho_s(),
          rho_a = rho_a(),
          R = input$R,
          kappa = input$kappa,
          eta = eta(),
          nu = input$nu,
          generation = as.numeric(input$generation),
          t_ps = t_ps(),
          t_pa = t_pa(),
          t_qcs = t_qcs(),
          t_qca = t_qca(),
          t_qhs = t_qhs(),
          t_qha = t_qha(),
          t_q = t_q()
        )
      }

      rmarkdown::render(
        tempReport,
        output_file = file,
        output_format = report$type,
        params = params,
        envir = new.env(parent = globalenv())
      )
      })
    }
  )
}
