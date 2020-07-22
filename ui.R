function(request) {
  dashboardPage(
    title = "ConTESSA",
    dashboardHeader(
      titleWidth = 320,
      disable = FALSE,
      tags$li(
        class = "dropdown",
        tags$a(
          href = "http://www.iddynamics.jhsph.edu", target = "_blank",
          tags$img(src = "logo.png", height = 50),
          style = "padding-top: 0; padding-bottom:0"
        )
      ),
      title = tags$img(src = "jhu-logo.jpg", height = 50)
    ),
    dashboardSidebar(
      width = 320,
      sidebarMenu(
        id = "tabs",
        menuItem("Home", tabName = "home"),
        menuItem(
          "Surveillance and Isolation",
          tabName = "passive",
          icon = icon("search")
        ),
        menuItem(
          "Household Contact Tracing",
          tabName = "contact-house",
          icon = icon("home")
        ),
        menuItem(
          "Community Contact Tracing",
          tabName = "contact-comm",
          icon = icon("user")
        ),
        menuItem(
          "Dashboard",
          tabName = "dash",
          icon = icon("bar-chart")
        ),
        menuItem(
          "Advanced Options",
          tabName = "advanced",
          icon = icon("cogs")
        ),
        menuItem(
          "About",
          tabName = "about",
          icon = icon("info-circle")
        ),
        conditionalPanel(
          "input.get_report == false",
          actionButton("get_report", "Generate Report")
        ),
        conditionalPanel(
          "input.get_report > 0",
          radioButtons("word", "Would you like an editable Word document or a PDF?",
                       choices = c("Word" = "word", "PDF" = "pdf"),
                       selected = "word"
          ),
          textInput("report_name", "File name:", value = "contessa-report"),
          customdownloadButton("report", "Generate report")
        ),
        div(tagList("Please use the 'Save Inputs' button to ",
                    br(),
                    "save a file with your data inputs locally ",
                    br(),
                    "to your device. You can use this file to ",
                    br(),
                    "come back to your scenarios at a later ",
                    br(),
                    "time by using the 'Load Inputs' button."),
            style = "font-size: 14px; margin-left: 15px; margin-right: 6px;"),
        actionButton("save_name", "Save inputs"),
        conditionalPanel("input.save_name > 0",
                         textInput("file_name", "File name:", value = "contessa-inputs"),
                         customdownloadButton("save", "Save")
        ),
        actionButton("return", "Load inputs"),
        conditionalPanel(
          "input.return > 0",
          fileInput("load", label = ""),
          uiOutput("file_msg")
        )
      )
    ),
    dashboardBody(
      tags$head(
        includeHTML("google-analytics/google-analytics.html"),
        tags$link(
          href = "https://fonts.googleapis.com/css2?family=Roboto&display=swap",
          rel = "stylesheet"
        )
      ),
      includeCSS("www/custom.css"),
      use_sever(),
      use_waiter(include_js = FALSE),
      use_hostess(),
      waiter_show_on_load(
        html = tagList(
          h2("getting everything set up..."),
          br(),
          spin_loaders(id = 11, color = "#f1c400"),
          br(), br(),
          h3("Infectious Disease Dynamics | Johns Hopkins Bloomberg School of Public Health")
        ),
        color = "#002d72"
      ),
      useShinyjs(),
      extendShinyjs(text = 'shinyjs.bkg_col = function(params) {
      var defaultParams = {
      id : null,
      col : "#f1c400"
      };
      params = shinyjs.getParams(params, defaultParams);
      var el = $("#" + params.id);
      el.css("background-color", params.col);
                    }'),
      tags$script(HTML("$('body').addClass('fixed');")),
      tabItems(
        ## Home --------------------------------------------------------------------
        tabItem(
          tabName = "home",
          p(class = "app-title", "ConTESSA", align = "center"),
          p(glue(
            "Welcome to the Johns Hopkins Bloomberg School of Public Health Contact Tracing ",
            "Evaluation and Strategic Support Application (ConTESSA).",
            " This application was designed for contact tracing ",
            "program managers looking to:"
          )),
          HTML(glue(
            "<ol><li> Quantify the current impacts ",
            "of their contact tracing programs <li> Identify what kinds of ",
            "program changes would yield the greatest reductions in COVID-19 ",
            "transmission <li> Share their results with colleagues</ol>"
          )),
          p(glue(
            "As you navigate through the Surveillance and Isolation, Household Contact ",
            "Tracing, and Community Contact Tracing sections in the menu ",
            "on the left, you will enter quantitative measures or targets ",
            "for your contact tracing program. Then you will be able to ",
            "view your results for one or two contact tracing program ",
            "scenarios in the Dashboard."
          )),
          p(glue(
            "To read more about the mathematical model at the core of ",
            "this application or meet the team that designed it, visit ",
            "the About tab."
          )),
          p("Now let's get started with some basic information."),
          br(),
          fluidRow(
            box(
              width = 6, status = "primary",
              textInput("name", text_q("What is your community's name?", "help/name.md")),
              textInput("prepared_by", text_q("Who is preparing this report?", "help/prepared_by.md"))
            )
          ),
          fluidRow(
            box(width = 12, status = "primary",
            checkboxInput("save_server",
                          "Please check this box to save your inputs temporarily on the server. Checking this option may enhance your user experience with this application. Data will be accessible only to maintainers of this application and will be deleted automatically once every 48 hours.")
            ),
            actionButton("surv", "GET STARTED",
                         style = "background-color: #f1c400;"
            )
          )
        ),
        ## Passive -----------------------------------------------------------------
        tabItem(
          tabName = "passive",
          p(class = "app-title", "SURVEILLANCE AND ISOLATION", align = "center"),
          fluidRow(
            box(
              status = "primary", width = 12,
              includeMarkdown("help/surveillance.md")
            )
          ),
          ## S1 ----
          fluidRow(
            column(
              width = 8,
              box(
                width = 12,
                numericInput(
                  "n_detect",
                  text_q("S1. Over the past 4 weeks, what was the average weekly number of cases detected?",
                         "help/s1_n_detect_month.md"),
                  7500,
                  0,
                  1e10
                )
              )
            ),
            column(
              width = 4,
              infoBoxOutput("n_detect_out", width = NULL)
            )
          ),
          ## S2 ----
          fluidRow(
            column(
              width = 8,
              box(
                width = 12,
                radioButtons("n_infect_calc", glue(
                  "S2. We want to estimate the percent of all infected people in ",
                  "your community who are detected by your surveillance program. ",
                  "This is difficult to measure directly, but we have a calculator ",
                  "that will help us estimate it for you. If you feel more confident ",
                  "estimating this yourself, you may chose the option to do that below."
                ),
                choices = c(
                  "I'd like help with the calculation" = "ifr",
                  "I'd like to estimate it directly" = "direct"
                ),
                selected = "ifr"
                ),
                conditionalPanel(
                  "input.n_infect_calc == 'ifr'",
                  numericInput(
                    "n_deaths",
                    text_q("S2.1. How many COVID-19 deaths were recorded in your community on average per week over the past two weeks?",
                           "help/s1_n_death.md"),
                    375, 0, 1e10
                  ),
                  radioButtons("ifr",
                               text_q("S2.2. Which infection fatality ratio (IFR) best represents your community?",
                                      "help/s1_ifr.md"),
                               choices = c(
                                 "0.25%" = "0.25",
                                 "0.5%" = "0.5",
                                 "0.75%" = "0.75",
                                 "1%" = "1",
                                 "Other" = "other"
                               ),
                               selected = "0.75"
                  ),
                  conditionalPanel(
                    "input.ifr == 'other'",
                    numericInput("ifr_other",
                                 label = "Estimated IFR (%)",
                                 value = 0.75, min = 0, max = 20
                    ),
                    uiOutput("ifr_out")
                  ),
                  uiOutput("ifr_warning")
                ),
                conditionalPanel(
                  "input.n_infect_calc == 'direct'",
                  sliderInput("prop_detect",
                              glue(
                                "S2.1. What percent of all infected ",
                                "people in your community are detected by your surveillance program?"
                              ),
                              min = 0, max = 100, value = 20
                  )
                )
              )
            ),
            column(
              width = 4,
              infoBoxOutput("prop_detect_out", width = NULL)
            )
          ),
          ## S3 ----
          fluidRow(
            column(
              width = 8,
              box(
                width = 12,
                numericInput(
                  "n_isolate",
                  text_q("S3. Over the past 4 weeks, what was the average weekly number of cases isolated?",
                         "help/s1_n_isolate.md"),
                  5000,
                  0,
                  1e10
                ),
                uiOutput("n_isolate_warning"),
                ## S4 ----
                numericInput(
                  "n_detect_quar",
                  text_q("S4. Over the past 4 weeks, what is the average weekly number of newly isolated cases (from S3) that were not already in quarantine or detected through follow-up with contacts?",
                         "help/s4.md"),
                  5000,
                  0,
                  1e10
                ),
                "If you do not have this number, leave it as the same as S3.",
                uiOutput("n_quar_warning")
              )
            ),
            column(
              width = 4,
              infoBoxOutput("prop_isol_not_quar", width = NULL)
            )
          ),
          ## S5 S6 ----
          fluidRow(
            column(
              width = 8,
              box(
                width = 12,
                div(
                  class = "delay-slider",
                  style = "margin: auto; width: 92%",
                  sliderTextInput("pass_test", text_q("S5. Among cases detected in the past four weeks, what is the average number of days between symptom onset of a case and having a sample collected for testing?",
                                                      "help/s5.md"),
                                  choices = c("Symptom onset of case", glue("Day {seq(0.1, 13.9, by = 0.1)}"), "Day 14+"),
                                  "Day 2", width = "100%"
                  ),

                  div(
                    class = "slider-subtitle",
                    p(
                      "Drag the slider above to indicate the average number of days ",
                      "between symptom onset of the case and ",
                      span("having a sample collected for testing.", style = "background-color: #f1c400;")
                    )
                  ),
                  uiOutput("pass_test_out"),
                  sliderTextInput("pass_isol", text_q("S6. Among cases detected in the past four weeks, what is the average number of days between symptom onset of a case and when they are told to isolate?",
                                                      "help/s6.md"),
                                  choices = c("Symptom onset of case", glue("Day {seq(0.1, 13.9, by = 0.1)}"), "Day 14+"), c("Day 2", "Day 5"),
                                  from_fixed = TRUE, width = "100%"
                  ),
                  div(
                    class = "slider-subtitle",
                    p(
                      "Drag the slider above on the right",
                      "to indicate the average number of days between ",
                      "having a sample collected for testing and being told to ",
                      span("isolate.", style = "background-color: #68ace5")
                    )
                  ),
                  uiOutput("pass_isol_out")
                )
              )
            ),
            column(
              width = 4,
              infoBoxOutput("t_p", width = NULL)
            )
          ),
          fluidRow(
            column(width = 8),
            column(
              width = 4,
              actionButton("next_house", "Next",
                           style = "background-color: #f1c400;"
              )
            )
          ),
          hr(),
          "These assume the same rates and isolation times for symptomatic ",
          "and asymptomatic individuals. To change this assumption, refer to ",
          "the ",
          actionLink("advanced_tab", "Advanced Options tab."),
          br(),
          uiOutput("disclaimer")
        ),
        ## Household ---------------------------------------------------------------
        tabItem(
          tabName = "contact-house",
          p(class = "app-title", "HOUSEHOLD CONTACT TRACING", align = "center"),
          ## H1 ----
          fluidRow(
            column(
              width = 8,
              box(
                width = 12,
                numericInput(
                  "contact_h",
                  text_q(
                    "H1. Among cases detected in the past four weeks, what is the average number of household contacts per case?",
                    "help/household.md"
                  ),
                  1.8,
                  0,
                  100
                )
              )
            ),
            column(
              width = 4,
              infoBoxOutput("contact_hbox", width = NULL),
            )
          ),
          ## H2 ----
          fluidRow(
            column(
              width = 8,
              box(
                width = 12,
                sliderInput(
                  "omega_h",
                  "H2. Among cases detected in the past four weeks, what percent of their household contacts were traced and quarantined?",
                  0,
                  100,
                  50
                )
              )
            ),
            column(
              width = 4,
              infoBoxOutput("omega_h_out", width = NULL),
            )
          ),
          ## H3 ----
          fluidRow(
            column(
              width = 8,
              box(
                width = 12,
                div(
                  style = "padding-left: 20px; padding-right: 20px;",
                  p("H3. Among household contacts that have been notified and quarantined in the past four weeks, what is the average number of days between symptom onset of the case and when the household contacts were quarantined?", style = "font-size: 18px; font-weight: bold;"),
                ),
                div(
                  style = "background-color: #eee; padding-left: 20px; padding-right: 20px;",
                  p("These are the values you entered on the 'Surveillance and Isolation' ",
                    "page about timing of sample collection and isolation, for ",
                    "your reference. Please return to that page if you need to make modifications.",
                    style = "font-size: 14px; font-weight: bold;"
                  ),
                  div(
                    style = "margin-top: -30px",
                    sliderTextInput("pass_isol2", "",
                                    choices = c("Symptom onset of case", glue("Day {seq(0.1, 13.9, by = 0.1)}"), "Day 14+"), c("Day 2", "Day 5"),
                                    from_fixed = TRUE, to_fixed = TRUE, width = "100%"
                    ),
                    uiOutput("pass_isol2_out"),
                  )
                ),
                div(
                  style = "padding-left: 20px; padding-right: 20px; padding-bottom: 10px;",
                  sliderTextInput("house_quar", "",
                                  choices = c("Symptom onset of case", glue("Day {seq(0.1, 13.9, by = 0.1)}"), "Day 14+"), "Day 6", width = "100%"
                  ),
                  uiOutput("house_quar_explain")
                )
              )
            ),
            column(
              width = 4,
              infoBoxOutput("t_hbox", width = NULL)
            )
          ),
          fluidRow(
            column(width = 8),
            column(
              width = 4,
              actionButton("next_comm", "Next",
                           style = "background-color: #f1c400;"
              )
            )
          ),
          hr(),
          "These assume the same rates and isolation times for symptomatic ",
          "and asymptomatic individuals. To change this assumption, refer to ",
          "the ",
          actionLink("advanced_tab2", "Advanced Options tab."),
          br(),
          uiOutput("disclaimer_house")
        ),

        ## Community ---------------------------------------------------------------
        tabItem(
          tabName = "contact-comm",
          p(class = "app-title", "COMMUNITY CONTACT TRACING", align = "center"),
          ## C1 ----
          fluidRow(
            column(
              width = 8,
              box(
                width = 12,
                numericInput(
                  "contact_c",
                  text_q(
                    "C1. Among cases detected in the past four weeks, what is the average number of community contacts per case?",
                    "help/community.md"
                  ),
                  1,
                  0,
                  100
                )
              ),
            ),
            column(
              width = 4,
              infoBoxOutput("contact_cbox", width = NULL),
            )
          ),
          ## C2 ----
          fluidRow(
            column(
              width = 8,
              box(
                width = 12,
                sliderInput(
                  "omega_c",
                  "C2. Among cases detected in the past four weeks, what percent of their community contacts were traced and quarantined?",
                  0,
                  100,
                  50
                )
              )
            ),
            column(
              width = 4,
              infoBoxOutput("omega_c_out", width = NULL),
            )
          ),
          ## C3 ----
          fluidRow(
            column(
              width = 8,
              box(
                width = 12,
                div(
                  style = "padding-left: 20px; padding-right: 20px;",
                  p("C3. Among community contacts that have been notified and quarantined in the past four weeks, what is the average number of days between symptom onset of the case and when the community contacts were quarantined?", style = "font-size: 18px; font-weight: bold;"),
                ),
                div(
                  style = "background-color: #eee; padding-left: 20px; padding-right: 20px;",
                  p("These are the values you entered on the 'Surveillance and Isolation' ",
                    "page about timing of sample collection and isolation, for ",
                    "your reference. Please return to that page if you need to make modifications.",
                    style = "font-size: 14px; font-weight: bold;"
                  ),
                  div(
                    style = "margin-top: -30px",
                    sliderTextInput("pass_isol3", "",
                                    choices = c("Symptom onset of case", glue("Day {seq(0.1, 13.9, by = 0.1)}"), "Day 14+"), c("Day 2", "Day 5"),
                                    from_fixed = TRUE, to_fixed = TRUE, width = "100%"
                    ),
                    uiOutput("pass_isol3_out"),
                  )
                ),
                div(
                  style = "padding-left: 20px; padding-right: 20px; padding-bottom: 10px;",
                  sliderTextInput("comm_quar", "",
                                  choices = c("Symptom onset of case", glue("Day {seq(0.1, 13.9, by = 0.1)}"), "Day 14+"), "Day 6", width = "100%"
                  ),
                  uiOutput("comm_quar_explain")
                )
              )
            ),
            column(
              width = 4,
              infoBoxOutput("t_cbox", width = NULL)
            )
          ),
          fluidRow(
            column(width = 8),
            column(
              width = 4,
              actionButton("next_dash", "Next",
                           style = "background-color: #f1c400;"
              )
            )
          ),
          hr(),
          "These assume the same rates and isolation times for symptomatic ",
          "and asymptomatic individuals. To change this assumption, refer to ",
          "the ",
          actionLink("advanced_tab3", "Advanced Options tab."),
          br(),
          uiOutput("disclaimer_comm")
        ),
        ## Dashboard ---------------------------------------------------------------
        tabItem(
          "dash",
          uiOutput("header"),
          uiOutput("prepared_by_out"),
          fluidRow(
            conditionalPanel(
              "input.scenario_b == false",
              box(
                width = 6, status = "primary",
                p("All else equal, your program will bring the reproductive number in your community from", class = "subtitle"),
                box(
                  width = 5, status = "primary",
                  uiOutput("r_")
                ),
                box(
                  width = 2, status = "primary",
                  p("to", class = "subtitle"), icon("arrow-right", class = "fa-2x")
                ),
                box(
                  width = 5, status = "primary",
                  uiOutput("r_eff")
                )
              ),
              box(
                width = 6, status = "primary",
                p("Your program is identifying",
                  class = "subtitle"
                ),
                highchartOutput("prop_q", height = 200) %>%
                  withSpinner(color = "#f1c400"),
                h4("of the infections in your community", style = "color: #444;"),
              )
            ),
            conditionalPanel(
              "input.scenario_b == true",
              box(
                width = 6, status = "primary",
                h3("Scenario A"),
                p("All else equal, your program will bring the reproductive number in your community from", class = "subtitle"),
                box(
                  width = 5, status = "primary",
                  uiOutput("r_a")
                ),
                box(
                  width = 2, status = "primary",
                  p("to", class = "subtitle"), icon("arrow-right", class = "fa-2x")
                ),
                box(
                  width = 5, status = "primary",
                  uiOutput("r_eff_a")
                ),
                box(
                  width = 12, status = "primary",
                  p("Your program is identifying",
                    class = "subtitle"
                  ),
                  highchartOutput("prop_q_a", height = 200) %>%
                    withSpinner(color = "#f1c400"),
                  h4("of the infections in your community", style = "color: #444;"),
                )
              ),
              box(
                width = 6, status = "primary",
                h3("Scenario B"),
                p("All else equal, your program will bring the reproductive number in your community from", class = "subtitle"),
                box(
                  width = 5, status = "primary",
                  uiOutput("r_b")
                ),
                box(
                  width = 2, status = "primary",
                  p("to", class = "subtitle"), icon("arrow-right", class = "fa-2x")
                ),
                box(
                  width = 5, status = "primary",
                  uiOutput("r_eff_b")
                ),
                box(
                  width = 12, status = "primary",
                  p("Your program is identifying",
                    class = "subtitle"
                  ),
                  highchartOutput("prop_q_b", height = 200) %>%
                    withSpinner(color = "#f1c400"),
                  h4("of the infections in your community", style = "color: #444;"),
                )
              )
            )
          ),
          fluidRow(
            box(
              width = 12,
              p(glue(
                "The tabs below demonstrate how your contact ",
                "tracing program can impact the reproductive number"
              ), class = "subtitle"),
              includeMarkdown("help/dashboard-figure.md"),
              ## Scenario B ----
              checkboxInput("scenario_b", "I'd like to create a 'Scenario B'"),
              conditionalPanel("input.scenario_b == true",
                               p("If you uncheck this box, you will lose your current Scenario B inputs."),
              br()),
              conditionalPanel(
                "input.scenario_b == true & (input.update_b >= input.plots)",
                ## Scenario A Inputs ----
                fluidRow(
                  box(
                    width = 6,
                    h3("Scenario A"),
                    p("These are your main inputs, for your reference. To change these, navigate the tabs on the left."),
                    numericInput(
                      "n_detect_a",
                      "S1. Over the past 4 weeks, what was the average weekly number of cases detected? (Scenario A)",
                      7500,
                      0,
                      1e10
                    ),
                    radioButtons("n_infect_calc_a", glue(
                      "S2. We want to estimate the percent of all infected people in ",
                      "your community who are detected by your surveillance program. ",
                      "This is difficult to measure directly, but we have a calculator ",
                      "that will help us estimate it for you. If you feel more confident ",
                      "estimating this yourself, you may chose the option to do that below. (Scenario A)"
                    ),
                    choices = c(
                      "I'd like help with the calculation" = "ifr",
                      "I'd like to estimate it directly" = "direct"
                    ),
                    selected = "ifr"
                    ),
                    conditionalPanel(
                      "input.n_infect_calc_a == 'ifr'",
                      numericInput(
                        "n_deaths_a",
                        "S2.1. How many COVID-19 deaths were recorded in your community on average per week over the past two weeks? (Scenario A)",
                        375, 0, 1e10
                      ),
                      radioButtons("ifr_a",
                                   "S2.2. Which infection fatality ratio (IFR) best represents your community? (Scenario A)",
                                   choices = c(
                                     "0.25%" = "0.25",
                                     "0.5%" = "0.5",
                                     "0.75%" = "0.75",
                                     "1%" = "1",
                                     "Other" = "other"
                                   ),
                                   selected = "0.75"
                      ),
                      conditionalPanel(
                        "input.ifr_a == 'other'",
                        numericInput("ifr_other_a",
                                     label = "Estimated IFR (%)",
                                     value = 0.75, min = 0, max = 20
                        )
                      )
                    ),
                    conditionalPanel(
                      "input.n_infect_calc_a == 'direct'",
                      sliderInput("prop_detect_a",
                                  glue(
                                    "S2.1. What percent of all infected ",
                                    "people in your community are detected by your surveillance program? (Scenario A)"
                                  ),
                                  min = 0, max = 100, value = 20
                      )
                    ),
                    numericInput(
                      "n_isolate_a",
                      "S3. Over the past 4 weeks, what was the average weekly number of cases isolated? (Scenario A)",
                      5000,
                      0,
                      1e10
                    ),
                    numericInput(
                      "n_detect_quar_a",
                      "S4. Over the past 4 weeks, what is the average weekly number of newly isolated cases (from S3) that were not already in quarantine or detected through follow-up with contacts? (Scenario A)",
                      0,
                      0,
                      1e10
                    ),
                    sliderTextInput("pass_isol_a", "S6. Among cases detected in the past four weeks, what is the average number of days between symptom onset of a case and when they are told to isolate? (Scenario A)",
                                    choices = c("Symptom onset of case", glue("Day {seq(0.1, 13.9, by = 0.1)}"), "Day 14+"), c("Day 5"),
                                    width = "100%"
                    ),
                    numericInput(
                      "contact_h_a",
                      "H1. Among cases detected in the past four weeks, what is the average number of household contacts per case? (Scenario A)",
                      3,
                      0,
                      100
                    ),
                    sliderInput(
                      "omega_h_a",
                      "H2. Among cases detected in the past four weeks, what percent of their household contacts were traced and quarantined? (Scenario A)",
                      0,
                      100,
                      50
                    ),
                    sliderTextInput("house_quar_a", "H3. Among household contacts that have been notified and quarantined in the past four weeks, what is the average number of days between symptom onset of the case and when the household contacts were quarantined? (Scenario A)",
                                    choices = c("Symptom onset of case", glue("Day {seq(0.1, 13.9, by = 0.1)}"), "Day 14+"), "Day 6", width = "100%"
                    ),
                    numericInput(
                      "contact_c_a",
                      "C1. Among cases detected in the past four weeks, what is the average number of community contacts per case? (Scenario A)",
                      2,
                      0,
                      100
                    ),
                    sliderInput(
                      "omega_c_a",
                      "C2. Among cases detected in the past four weeks, what percent of their community contacts were traced and quarantined? (Scenario A)",
                      0,
                      100,
                      50
                    ),
                    sliderTextInput("comm_quar_a", "C3. Among community contacts that have been notified and quarantined in the past four weeks, what is the average number of days between symptom onset of the case and when the community contacts were quarantined? (Scenario A)",
                                    choices = c("Symptom onset of case", glue("Day {seq(0.1, 13.9, by = 0.1)}"), "Day 14+"), "Day 6", width = "100%"

                    )
                  ),
                  ## Scenario B Inputs ----
                  box(
                    width = 6,
                    h3("Scenario B"),
                    p("Change the sliders below to create a second scenario."),
                    numericInput(
                      "n_detect_b",
                      "S1. Over the past 4 weeks, what was the average weekly number of cases detected? (Scenario B)",
                      7500,
                      0,
                      1e10
                    ),
                    radioButtons("n_infect_calc_b", glue(
                      "S2. We want to estimate the percent of all infected people in ",
                      "your community who are detected by your surveillance program. ",
                      "This is difficult to measure directly, but we have a calculator ",
                      "that will help us estimate it for you. If you feel more confident ",
                      "estimating this yourself, you may chose the option to do that below. (Scenario B)"
                    ),
                    choices = c(
                      "I'd like help with the calculation" = "ifr",
                      "I'd like to estimate it directly" = "direct"
                    ),
                    selected = "ifr"
                    ),
                    conditionalPanel(
                      "input.n_infect_calc_b == 'ifr'",
                      numericInput(
                        "n_deaths_b",
                        "S2.1. How many COVID-19 deaths were recorded in your community on average per week over the past two weeks? (Scenario B)",
                        375, 0, 1e10
                      ),
                      radioButtons("ifr_b",
                                   "S2.2. Which infection fatality ratio (IFR) best represents your community? (Scenario B)",
                                   choices = c(
                                     "0.25%" = "0.25",
                                     "0.5%" = "0.5",
                                     "0.75%" = "0.75",
                                     "1%" = "1",
                                     "Other" = "other"
                                   ),
                                   selected = "0.75"
                      ),
                      conditionalPanel(
                        "input.ifr_b == 'other'",
                        numericInput("ifr_other_b",
                                     label = "Estimated IFR (%)",
                                     value = 0.75, min = 0, max = 20
                        )
                      ),
                      uiOutput("ifr_warning_b")
                    ),
                    conditionalPanel(
                      "input.n_infect_calc_b == 'direct'",
                      sliderInput("prop_detect_b",
                                  glue(
                                    "S2.1. What percent of all infected ",
                                    "people in your community are detected by your surveillance program? (Scenario B)"
                                  ),
                                  min = 0, max = 100, value = 20
                      )
                    ),
                    numericInput(
                      "n_isolate_b",
                      "S3. Over the past 4 weeks, what was the average weekly number of cases isolated? (Scenario B)",
                      5000,
                      0,
                      1e10
                    ),
                    uiOutput("n_isolate_warning_b"),
                    numericInput(
                      "n_detect_quar_b",
                      "S4. Over the past 4 weeks, what is the average weekly number of newly isolated cases (from S3) that were not already in quarantine or detected through follow-up with contacts? (Scenario B)",
                      0,
                      0,
                      1e10
                    ),
                    uiOutput("n_quar_warning_b"),
                    sliderTextInput("pass_isol_b", "S6. Among cases detected in the past four weeks, what is the average number of days between symptom onset of a case and when they are told to isolate? (Scenario B)",
                                    choices = c("Symptom onset of case", glue("Day {seq(0.1, 13.9, by = 0.1)}"), "Day 14+"), c("Day 5"),
                                    width = "100%"
                    ),
                    numericInput(
                      "contact_h_b",
                      "H1. Among cases detected in the past four weeks, what is the average number of household contacts per case? (Scenario B)",
                      3,
                      0,
                      100
                    ),
                    sliderInput(
                      "omega_h_b",
                      "H2. Among cases detected in the past four weeks, what percent of their household contacts were traced and quarantined? (Scenario B)",
                      0,
                      100,
                      50
                    ),
                    sliderTextInput("house_quar_b", "H3. Among household contacts that have been notified and quarantined in the past four weeks, what is the average number of days between symptom onset of the case and when the household contacts were quarantined? (Scenario B)",
                                    choices = c("Symptom onset of case", glue("Day {seq(0.1, 13.9, by = 0.1)}"), "Day 14+"), "Day 6", width = "100%"

                    ),
                    numericInput(
                      "contact_c_b",
                      "C1. Among cases detected in the past four weeks, what is the average number of community contacts per case? (Scenario B)",
                      2,
                      0,
                      100
                    ),
                    sliderInput(
                      "omega_c_b",
                      "C2. Among cases detected in the past four weeks, what percent of their community contacts were traced and quarantined? (Scenario B)",
                      0,
                      100,
                      50
                    ),
                    sliderTextInput("comm_quar_b", "C3. Among community contacts that have been notified and quarantined in the past four weeks, what is the average number of days between symptom onset of the case and when the community contacts were quarantined? (Scenario B)",
                                    choices = c("Symptom onset of case", glue("Day {seq(0.1, 13.9, by = 0.1)}"), "Day 14+"), selected = "Day 6", width = "100%"
                    )
                  )
                )
              ),
              conditionalPanel(
                "input.scenario_b == true",
                conditionalPanel(
                  "input.plots <= input.update_b",
                  actionButton("plots", "Show plots")
                ),
                conditionalPanel(
                  "input.update_b < input.plots",
                  actionButton("update_b", "Update Scenario B")
                )
              ),
              conditionalPanel(
                "input.scenario_b == false | input.plots > input.update_b",
                tabBox(
                  width = 12,
                  tabPanel(
                    "Surveillance",
                    highchartOutput("pass_plot") %>%
                      withSpinner(color = "#f1c400"),
                    text_q("Learn more about the axis above", "help/x-axis.md"),
                    br(), br(),
                    highchartOutput("pass_plot_delay") %>%
                      withSpinner(color = "#f1c400")
                  ),
                  tabPanel(
                    "Household",
                    highchartOutput("house_plot") %>%
                      withSpinner(color = "#f1c400"),
                    highchartOutput("house_plot_delay") %>%
                      withSpinner(color = "#f1c400")
                  ),
                  tabPanel(
                    "Community",
                    highchartOutput("comm_plot") %>%
                      withSpinner(color = "#f1c400"),
                    highchartOutput("comm_plot_delay") %>%
                      withSpinner(color = "#f1c400")
                  ),
                  checkboxInput("log_scale", "I'd like to view the y-axis on the log scale")
                )
              )
            )
          ),
          p("Your Model Assumptions", class = "app-title", style = "font-size: 36px;"),
          conditionalPanel("input.scenario_b == false",
                           h2("SURVEILLANCE AND ISOLATION", align = "center"),
                           fluidRow(
                             infoBoxOutput("prop_quar2", width = 6),
                             infoBoxOutput("t_p2", width = 6),
                           ),
                           h2("CONTACT TRACING", align = "center"),
                           fluidRow(
                             infoBoxOutput("omega_h2", width = 3),
                             infoBoxOutput("t_hbox2", width = 3),
                             infoBoxOutput("omega_c2", width = 3),
                             infoBoxOutput("t_cbox2", width = 3)
                           )
          ),
          conditionalPanel("input.scenario_b == true",
                           fluidRow(
                             box(width = 6, status = "primary",
                                 h2("SCENARIO A"),
                                 h2("SURVEILLANCE AND ISOLATION", align = "center"),
                                 infoBoxOutput("prop_quar2a", width = NULL),
                                 infoBoxOutput("t_p2a", width = NULL),
                                 h2("CONTACT TRACING", align = "center"),
                                 infoBoxOutput("omega_h2a", width = NULL),
                                 infoBoxOutput("t_hbox2a", width = NULL),
                                 infoBoxOutput("omega_c2a", width = NULL),
                                 infoBoxOutput("t_cbox2a", width = NULL)
                             ),
                             box(width = 6, status = "primary",
                                 h2("SCENARIO B"),
                                 h2("SURVEILLANCE AND ISOLATION", align = "center"),
                                 infoBoxOutput("prop_isol_not_quar_b", width = NULL),
                                 infoBoxOutput("t_p_b", width = NULL),
                                 h2("CONTACT TRACING", align = "center"),
                                 infoBoxOutput("omega_h_b_out", width = NULL),
                                 infoBoxOutput("t_hbox_b", width = NULL),
                                 infoBoxOutput("omega_c_b_out", width = NULL),
                                 infoBoxOutput("t_cbox_b", width = NULL)
                             )
                           )
          )
        ),
        ## Assumptions -------------------------------------------------------------
        tabItem(
          tabName = "advanced",
          p(class = "app-title", "ASSUMPTIONS", align = "center"),
          p(glue(
            "This tool is set up to function for an average community ",
            "and therefore makes several underlying assumptions. However, each ",
            "community is unique, and as such, you may want to change these ",
            "foundational four assumptions about natural history, detection, ",
            "transmission, and tracing timing. These are updated based on your ",
            "inputs in the 'Surveillance and Isolation', 'Household Contact Tracing' and 'Community ",
            "Contact Tracing' tabs, however there are additional assumptions that about ",
            "the asymptomatic / symptomatic behavior as well as underlying ",
            "disease dynamics"
          )),
          h3("Natural history assumptions"),
          uiOutput("natural_text", inline = TRUE),
          tags$a(href = "#nh", "Click here to update these assumptions."),
          h3("Detection assumptions"),
          uiOutput("detect_text", inline = TRUE),
          tags$a(href = "#d", "Click here to update these assumptions."),
          h3("Transmission assumptions"),
          uiOutput("transmit_text", inline = TRUE),
          tags$a(href = "#t", "Click here to update these assumptions."),
          h3("Tracing Timing assumptions"),
          uiOutput("time_text", inline = TRUE),
          tags$a(href = "#tt", "Click here to update these assumptions."),
          p(class = "app-title", "UPDATE ASSUMPTIONS", align = "center"),
          tags$a(
            href = "#nh",
            "1. Update natural history assumptions.",
            color = "#fff"
          ),
          br(),
          tags$a(
            href = "#d",
            "2. Update detection assumptions.",
            color = "#fff"
          ),
          br(),
          tags$a(
            href = "#t",
            "3. Update transmission assumptions.",
            color = "#fff"
          ),
          br(),
          tags$a(
            href = "#tt",
            "4. Update time tracing assumptions.",
            color = "#fff"
          ),
          br(),
          h2(
            "UPDATE NATURAL HISTORY ASSUMPTIONS",
            align = "center",
            id = "nh",
            class = "calc-header"
          ),
          fluidRow(
            box(
              sliderInput("alpha", "Percent with asymptomatic infection (people infected who never develop symptoms)", 1, 100, 20),
              conditionalPanel("input.scenario_b == true",
                               h3("Scenario B:"),
                               sliderInput("alpha_b", "Percent with asymptomatic infection (people infected who never develop symptoms) (Scenario B)", 1, 100, 20)
              )
            )
          ),
          br(),
          hr(),
          h2(
            "UPDATE DETECTION ASSUMPTIONS",
            align = "center",
            id = "d",
            class = "calc-header"
          ),
          fluidRow(
            box(
              sliderInput("mult", text_q(
                "Asymptomatic cases are x times as likely to be detected and isolated compared to symptomatic cases",
                "help/mult.md"),
                          min = 0, max = 1, value = 0.5
              ),
              uiOutput("mult_warning"),
              conditionalPanel("input.scenario_b == true",
                               h3("Scenario B:"),
                               sliderInput("mult_b", "Asymptomatic cases are x times as likely to be detected and isolated compared to symptomatic cases (Scenario B)",
                                           min = 0, max = 1, value = 0.5
                               ),
                               uiOutput("mult_b_warning"),
              )
            )
          ),
          br(),
          hr(),
          h2(
            "UPDATE TRANSMISSION ASSUMPTIONS",
            align = "center",
            id = "t",
            class = "calc-header"
          ),
          fluidRow(
            box(
              numericInput("R", "Reproductive number", 2.5, 0, 10),
              conditionalPanel("input.scenario_b == true",
                               h3("Scenario B:"),
                               numericInput("R_b", "Reproductive number (Scenario B)", 2.5, 0, 10))
            ),
            box(
              sliderInput(
                "nu",
                "Relative risk of infection for a household contact vs community contact",
                1,
                10,
                4,
                step = 0.1
              ),
              conditionalPanel("input.scenario_b == true",
                               h3("Scenario B:"),
                               sliderInput(
                                 "nu_b",
                                 "Relative risk of infection for a household contact vs community contact (Scenario B)",
                                 1,
                                 10,
                                 4,
                                 step = 0.1
                               ))
            ),
            box(
              sliderInput(
                "kappa",
                "Relative transmissibility of an asymptomatic individual (person infected who never develops symptoms) vs symptomatic",
                0,
                1,
                0.5
              ),
              conditionalPanel("input.scenario_b == true",
                               h3("Scenario B:"),
                               sliderInput(
                                 "kappa_b",
                                 "Relative transmissibility of an asymptomatic individual (person infected who never develops symptoms) vs symptomatic (Scenario B)",
                                 0,
                                 1,
                                 0.5
                               ))
            ),
            box(
              radioButtons(
                "generation",
                "Disease generation time",
                choices = c(
                  "Short (mean of 5 days)" = "0.9",
                  "Medium (mean of 6.5 days)" = "1.65",
                  "Long (mean of 8 days)" = "2.4"
                ),
                selected = "1.65"
              ),
              conditionalPanel("input.scenario_b == true",
                               h3("Scenario B:"),
                               radioButtons(
                                 "generation_b",
                                 "Disease generation time (Scenario B)",
                                 choices = c(
                                   "Short (mean of 5 days)" = "0.9",
                                   "Medium (mean of 6.5 days)" = "1.65",
                                   "Long (mean of 8 days)" = "2.4"
                                 ),
                                 selected = "1.65"
                               ))
            )
          ),
          br(),
          hr(),
          h2(
            "UPDATE TRACING TIMING ASSUMPTIONS",
            align = "center",
            id = "tt",
            class = "calc-header"
          ),
          fluidRow(
            box(width = 6,
                h3("Surveillance and Isolation (Asymptomatic)"),

                sliderTextInput("t_pa", "Among asymptomatic cases detected in the past four weeks, what is the average number of days between when symptom onset of the asymptomatic case would have occurred (uses the same number of days between infection and symptom onset for symptomatic cases) and when they are told to isolate?",
                                choices = c("Symptom onset of case", glue("Day {seq(0.1, 13.9, by = 0.1)}"), "Day 14+"), c("Day 5"),
                                width = "100%"
                ),
                conditionalPanel("input.scenario_b == true",
                                 h3("Scenario B: Surveillance and Isolation (Asymptomatic)"),

                                 sliderTextInput("t_pa_b", "Among asymptomatic cases detected in the past four weeks, what is the average number of days between when symptom onset of the asymptomatic case would have occurred (uses the same number of days between infection and symptom onset for symptomatic cases) and when they are told to isolate? (Scenario B)",
                                                 choices = c("Symptom onset of case", glue("Day {seq(0.1, 13.9, by = 0.1)}"), "Day 14+"), c("Day 5"),
                                                 width = "100%"
                                 ))
            )
          )
        ),
        ## About ----
        tabItem(
          "about",
          p(class = "app-title", "ABOUT", align = "center"),
          includeMarkdown("about.md")
        )
      ),
      tags$footer(
        tagList(
          br(), br(),
          p("Created by",
            a(href = "https://lucymcgowan.com", "Lucy D'Agostino McGowan", style = "color: #f1c400"),
            "in collaboration with Kyra Grantz, Elizabeth Lee, Justin Lessler, and Emily Gurley",
            class = "footer"
          )
        ),
        align = "center"
      )
    )
  )
}
