library(shiny)
library(shinydashboard)
library(flexdashboard)
library(shinyjs)
library(waiter)
library(highcharter)
library(sever)
library(tidyverse)
library(glue)
library(tti)
library(shinyWidgets)
library(viridisLite)
library(tools)
library(tippy)
library(yaml)
library(shinycssloaders)
library(fontawesome)
library(markdown)

## Helper functions ----

my_tippy <- function(text, help_file, placement = "right") {
  tippy(
    text = text, tooltip = includeMarkdown(help_file),
    placement = placement, animateFill = FALSE
  )
}

text_q <- function(text, help) {
  tagList(
    text,
    fa("question-circle") %>% my_tippy(help)
  )
}

not_equal <- function(x, y, tolerence = 0.01, ...) {
  !isTRUE(all.equal(x, y, tolerence = tolerence, ...))
}

customdownloadButton <- function(outputId, label = "Download") {
  tags$a(
    id = outputId,
    class = "btn btn-default shiny-download-link",
    href = "",
    target = "_blank",
    download = NA,
    NULL,
    label
  )
}

text_to_time <- function(text) {
  if (text == "Symptom onset of case") {
    return(0)
  } else if (text == "Day 14+") {
    return(14)
  } else {
    return(as.numeric(gsub("Day", "", text)))
  }
}

## highcharter theme------------------------------------------------------------

idd_hc <- hc_theme_merge(
  hc_theme_smpl(),
  hc_theme(
    colors = substr(viridis(5), 0, 7),
    chart = list(
      backgroundColor = "#F8F7F6",
      style = list(
        color = "#444",
        fontFamily = "Roboto",
        fontSize = "14px",
        textTransform = "uppercase"
      )
    ),
    title = list(
      style = list(
        fontFamily = "Roboto",
        fontWeight = "bold",
        color = "#002d72"
      ),
      align = "left"
    ),
    subtitle = list(
      style = list(
        fontFamily = "Roboto",
        fontSize = "14px",
        fontWeight = "bold",
        color = "#002d72"
      ),
      align = "left"
    ),
    xAxis = list(
      labels = list(style = list(
        fontFamily = "Roboto",
        color = "#002d72",
        fontSize = "12px"
      )),
      title = list(
        style =
          list(
            color = "#002d72",
            fontSize = "14px"
          )
      ),
      gridLineColor = "#444",
      lineColor = "#002d72",
      minorGridLineColor = "#444",
      tickColor = "#002d72",
      tickWidth = .5
    ),
    yAxis = list(
      labels = list(
        style =
          list(
            fontFamily = "Roboto",
            color = "#002d72",
            fontSize = "12px"
          )
      ),
      title = list(
        style =
          list(
            color = "#002d72",
            fontSize = "14px"
          )
      ),
      gridLineWidth = .5,
      gridLineColor = "#444",
      lineColor = "#002d72",
      minorGridLineColor = "#444",
      tickColor = "#002d72",
      tickWidth = 1
    )
  )
)

idd_hc_gauge <- hc_theme_merge(
  hc_theme_smpl(),
  hc_theme(
    chart = list(
      backgroundColor = "#F8F7F6",
      style = list(
        color = "#444",
        fontFamily = "Roboto",
        fontSize = "28px",
        textTransform = "uppercase"
      )
    ),
    title = list(
      style = list(
        fontFamily = "Roboto",
        fontWeight = "bold",
        color = "#444"
      ),
      align = "center",
      margin = -10
    ),
    yAxis = list(
      lineWidth = 0,
      tickWidth = 0,
      minorTickWidth = 0,
      tickAmount = 2,
      min = 0,
      max = 100,
      labels = list(
        y = 16,
        style =
          list(
            fontFamily = "Roboto",
            color = "#444",
            fontSize = "14px"
          )
      ),
      title = list(
        y = -70,
        style =
          list(
            color = "#444",
            fontSize = "18px"
          )
      )
    ),
    plotOptions =
      list(solidGauge = list(
        dataLabels = list(
          y = 5,
          borderWidth = 0,
          useHTML = TRUE
        )
      ))
  )
)

hc_gauge <- function(x, stops = list(
                       list(.25, "#68ace5"),
                       list(.50, "#68ace5"),
                       list(.75, "#68ace5")
                     ), title, name) {
  highchart() %>%
    hc_add_theme(idd_hc_gauge) %>%
    hc_chart(
      type = "solidgauge",
      marginBottom = -100, height = 200
    ) %>%
    hc_pane(
      startAngle = -90,
      endAngle = 90,
      background = list(
        outerRadius = "100%",
        innerRadius = "60%",
        shape = "arc"
      )
    ) %>%
    hc_yAxis(stops = stops) %>%
    hc_title(text = title) %>%
    hc_add_series(
      name = name,
      data = c(x),
      dataLabels = list(
        format = '<div style="text-align:center"><span style="font-size:32px; color:#444;">{y}%</span></div>',
        borderWidth = 0
      ),
      tooltip = list(valueSuffix = "%")
    )
}

## ggplot2 theme ---------------------------------------------------------------

## With help from: https://www.r-bloggers.com/ggplot-with-a-highcharts-taste/
theme_hc <- function() {
  theme(
    title = element_text(hjust = 0, size = 9),
    axis.title.x = element_text(hjust = .5),
    axis.title.y = element_text(hjust = .5, angle = 90),
    panel.grid.major.y = element_line(color = "gray", size = .3),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray", size = .3),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )
}
## Storage ---------------------------------------------------------------------

enableBookmarking("url")
