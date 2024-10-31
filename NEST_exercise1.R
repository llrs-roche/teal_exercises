library("shiny")
library("teal")
# https://github.com/insightsengineering/coredev-tasks/issues/449

# Example 1: Marcin ####
# Adapted code from the section "Running as a teal app" on
# https://github.com/insightsengineering/coredev-tasks/issues/449#issuecomment-1608204708
kmeans_ui <- function(id) {
  tagList(
    selectInput(NS(id, "selected"), "Variable", choices = NULL, multiple = TRUE),
    numericInput(NS(id, "centers"), "bins", 10, min = 1),
    tableOutput(NS(id, "table"))
  )
}

kmeans_server <- function(id, data, dataname) {
  moduleServer(id, function(input, output, session) {
    dataset <- reactive({
      x <- data()[[dataname]]
      x[vapply(x, is.numeric, logical(1))]
    })

    # initialize selection when server is accessed
    isolate({
      updateSelectInput(session, "selected",
                        choices = names(dataset()),
                        selected = names(dataset()))
    })

    dataset_out <- reactive(dataset()[, input$selected, drop = FALSE])

    clustering_data <- reactive({
      cbind(
        dataset_out(),
        clusters = kmeans(dataset_out(), centers = input$centers)$cluster
      )
    })

    output$table <- renderTable({
      clustering_data()
    })
  })
}


tm_kmeans <- function(label = "Kmeans", dataname) {
  module(
    label = "Kmeans",
    ui = kmeans_ui,
    server = kmeans_server,
    server_args = list(dataname = dataname)
  )
}

app <- teal::init(
  data = teal_data(i = iris, m = mtcars, code = "i <- iris;m <- mtcars") |> verify(),
  modules = list(
    tm_kmeans(label = "Tab kmeans", dataname = "i")
  )
)

runApp(app)

# Example 2: Kartikeya ####
# From section "Teal conversion"
# https://github.com/insightsengineering/coredev-tasks/issues/449#issuecomment-1612521093
library("ggplot2")
explore_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("variable1"), "Choose variable 1:", choices = NULL),
    selectInput(ns("variable2"), "Choose variable 2:", choices = NULL),
    sliderInput(ns("size"), "Point size:", min = 1, max = 10, value = 5),
    actionButton(ns("plot"), "Plot"),
    plotOutput(ns("scatterPlot"))
  )
}

explore_server <- function(id, data, dataname) {
  moduleServer(id, function(input, output, session) {
    dataset <- reactive({
      x <- data()[[dataname]]
    })

    observeEvent(dataset(), {
      updateSelectInput(session, "variable1", choices = names(dataset()),
                        selected = names(dataset()))
      updateSelectInput(session, "variable2", choices = names(dataset()),
                        selected = names(dataset()))
    })

    observe({
      variable1 <- input$variable1
      variable2 <- input$variable2
      size <- input$size

      output$scatterPlot <- renderPlot({
        ggplot(dataset(), aes(x = .data[[variable1]],
                              y = .data[[variable2]])) +
          geom_point(size = size) +
          labs(x = variable1, y = variable2) +
          theme_minimal()
      })
    })
  })
}

tm_explore <- function(label = "explore", dataname) {
  module(
    label = label,
    ui = explore_ui,
    server = explore_server,
    server_args = list(dataname = dataname)
  )
}

app <- teal::init(
  data = teal_data(iris = iris),
  modules = list(
    tm_explore(label = "Tab explore", dataname = "iris")
  )
)

runApp(app)

# Example 3: Aleksander ####
# https://github.com/insightsengineering/coredev-tasks/issues/449#issuecomment-1649550702

acmodule_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("selections_summary")),
    verbatimTextOutput(ns("data_content")),
    uiOutput(ns("selections_plotting")),
    plotOutput(ns("plot")),
    NULL
  )
}

acmodule_srv <- function(id,
                         data,
                         sumfuns = c("str", "summary", "head"),
                         plotfuns = c("sort", "rev"),
                         ...) {
  checkmate::assert_string(id)
  checkmate::assert_character(sumfuns, min.len = 1L, any.missing = FALSE)
  checkmate::assert_character(plotfuns, min.len = 1L, any.missing = FALSE)


  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("dataname", shinyvalidate::sv_required("choose a data set"))
    iv$add_rule("summary_function", shinyvalidate::sv_required("choose a summary function"))
    iv$enable()

    output[["selections_summary"]] <- renderUI({
      tagList(

        selectInput(
          ns("dataname"),
          "choose data set",
          choices = c("choose a data set" = "", datanames(data())),
          multiple = TRUE
        ),

        selectInput(
          ns("summary_function"),
          "choose function",
          choices = c("choose one" = "", sumfuns),
          multiple = FALSE
        ),

        NULL
      )
    })

    dataname_selected <- reactive({
      req(input[["dataname"]])
    })

    output[["data_content"]] <- renderPrint({
      sumfun_selected <- req(input[["summary_function"]])
      sumfun_selected <- get0(sumfun_selected, mode = "function", ifnotfound = simpleError("summary function not found"))
      lapply(
        structure(dataname_selected(), names = dataname_selected()),
        function(x) sumfun_selected(data()[[x]])
      )
    })

    output[["selections_plotting"]] <- renderUI({
      req(length(dataname_selected()) == 1L)
      numerics <- Filter(is.numeric, data()[[dataname_selected()]])

      tagList(

        selectInput(
          ns("variable"),
          "choose numeric variable",
          choices = c("choose one" = "", names(numerics)),
          multiple = FALSE
        ),

        selectInput(
          ns("plotting_function"),
          "choose plotting function(s)",
          choices = c("choose 1+ to compose" = "", plotfuns),
          multiple = TRUE
        ),

        NULL

      )
    })

    plotting_function <- reactive({
      plotfun_selected <- if (isTruthy(input[["plotting_function"]])) {
        input[["plotting_function"]]
      } else {
        "identity"
      }
      plotfun_selected <- structure(plotfun_selected, names = plotfun_selected)
      plotfun_selected <- lapply(plotfun_selected, function(x) get0(x, mode = "function"))
      funs_missing <- Filter(is.null, plotfun_selected)
      if (length(funs_missing) != 0L) {
        showNotification(sprintf("plotting functions not found: %s", toString(names(funs_missing))))
      }
      plotfun_selected <- Filter(Negate(is.null), plotfun_selected)
      req(plotfun_selected)

      function(x) {
        if (is.numeric(x)) {
          eval(Reduce(f = call, x = rev(names(plotfun_selected)), init = x, right = TRUE))
        } else if (is.character(x)) {
          deparse1(Reduce(call, rev(names(plotfun_selected)), init = str2lang(x), right = TRUE)) |>
            gsub("(\\(|\\))", " \\1 ", x = _)
        } else {
          stop("plotting funciton: don't know how to handle type ", typeof(x))
        }
      }

    })

    output[["plot"]] <- renderPlot({
      req(length(dataname_selected()) == 1L)
      dataname <- dataname_selected()
      dataset <- data()[[dataname]]
      variable <- req(input[["variable"]])
      fun <- plotting_function()
      x <- dataset[[variable]]
      x <- x[!is.na(x) & is.finite(x)]
      plot(
        x = fun(dataset[[variable]]),
        ylab = fun(sprintf("%s$%s", dataname, variable)),
        las = 1
      )
    })

  })
}

acmodule <- function(label = "this little module") {
  teal::module(
    label = label,
    server = acmodule_srv,
    ui = acmodule_ui,
    datanames = "all",
    server_args = list(
      sumfuns = c("head", "tail", "dim"),
      plotfuns = c("sort", "cumsum", "sample", "rev")
    )
  )
}

library(shiny)
library(shinyvalidate)
library(teal.slice)
library(teal)

app <- init(
  data =  teal_data(
    iris = iris,
    mtcars = mtcars,
    faithful = faithful,
    warpbreaks = warpbreaks
  ),
  modules = acmodule(),
  filter = teal_slices()
)

runApp(app)

# Example 4: Andre ####
# Adapted from https://github.com/insightsengineering/coredev-tasks/issues/449#issuecomment-1655356609
library(teal)
library(shiny)
library(pheatmap)

srv_xxxx <- function(id, data) {
  # Trick to allow for validation of both tdata (teal) and list (vanilla shiny)
  checkmate::assert_string(id)

  shiny::moduleServer(id, function(input, output, session) {

    selected_dataset <- shiny::reactive(input$dataset)
    selected_scale <- shiny::reactive(input$scale)
    selected_vars <- shiny::reactive(input$vars)

    selected_data <- shiny::reactive({
      shiny::req(selected_dataset(), selected_vars())
      # Needs to check to prevent leaking selected vars from another dataset
      shiny::req(all(selected_vars() %in% colnames(data()[[selected_dataset()]])))
      data()[[selected_dataset()]][, selected_vars()]
    })

    shiny::observe({
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "dataset",
        choices = datanames(data()),
        selected = character(0)
      )
    })

    shiny::observe({
      shiny::req(selected_dataset())
      possible_vars <- colnames(data()[[selected_dataset()]])[
        vapply(data()[[selected_dataset()]][1,], is.numeric, logical(1))
      ]

      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "vars",
        choices = possible_vars,
        selected = possible_vars,
        disabled = FALSE
      )
    }) |>
      shiny::bindEvent(selected_dataset(), ignoreNULL = TRUE)

    module_code_q <- shiny::reactive({
      shiny::validate(
        shiny::need(
          !is.null(selected_dataset()),
          "\u2139\uFE0F Please select a dataset"
        ),
        shiny::need(
          !is.null(selected_vars()) && length(selected_vars()) >= 2,
          "\u2139\uFE0F Please select at least 2 variables"
        )
      )

      shiny::validate(shiny::need(
        NROW(selected_data()) > 0,
        "\u2139\uFE0F Please check your filters as at least 1 observation is needed"
      ))

      pheatmap::pheatmap(
        as.matrix(selected_data()),
        scale = selected_scale(),
        angle_col = 0
      )
    })

    # shiny component to view
    output$heatmap <- shiny::renderPlot({
      module_code_q()
    })
  })
}

ui_xxxx <- function(id, scale = "column") {
  ns <- shiny::NS(id)
  tags <- shiny::tags

  tags$div(
    tags$div(
      tags$h4(
        tags$blockquote(
          "Heatmap visualization of IRIS and MTCAR datasets from R."
        )
      ),
      tags$p(
        "\u2139\uFE0F Start by selecting one of the datasets and you will immediately see ",
        "the heatmap for the full dataset."
      )
    ),
    tags$hr(),
    tags$div(tags$em("Data options:"), style = "padding-bottom: 1em;"),
    tags$div(
      shinyWidgets::radioGroupButtons(
        inputId = ns("dataset"),
        choices = "Loading...",
        selected = character(0),
        label = "Dataset"
      ),
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("vars"),
        label = "Variables to show",
        choices = "Waiting for a dataset to be choosen...",
        selected = character(0),
        disabled = TRUE
      ),
      tags$hr(),
      tags$div(tags$em("Plot options:"), style = "padding-bottom: 1em;"),
      tags$div(
        style = "padding-left: 1em;",
        shinyWidgets::radioGroupButtons(
          inputId = ns("scale"),
          label = "Scaling",
          choices = c("column", "row", "none"),
          selected = scale
        )
      ),
      tags$hr(),
      shiny::plotOutput(ns("heatmap")),
    )
  )
}

tm_xxxx <- function(label) {
  checkmate::assert_string(label)

  teal::module(
    label = label,
    server = srv_xxxx,
    ui = ui_xxxx,
    ui_args = list(),
    server_args = list(),
    datanames = "all"
  )
}

td_mtcars <- tibble::rownames_to_column(mtcars, var = "Model")

app <- teal::init(
  data = teal_data(
    MTCARS = td_mtcars,
    IRIS = iris
  ),
  modules = tm_xxxx(
    label = "Sample xxxx Module"
  ),
  header = "Simple app with xxxx module"
)

runApp(app)

# Example 5: Vedha ####
library(teal)
library(survminer)
library(survival)
library(colourpicker)
library(shinyWidgets)
library(shinyBS)

code_label <- function(function_name, label) {
  HTML(paste0("<code>", function_name, "</code> ", label))
}

default_colors <- c("#E7B800", "#2E9FDF", "#59E372", "#ED632D")

survival_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarPanel(
      tags$h4("Select Dataset"),
      selectInput(ns("dataset"), NULL, NULL),
      tags$h4("Fit Survival Curves"),
      helpText("survfit(Surv(Time, Status) ~ Subgroup, Dataset)"),
      selectInput(ns("time_var"), HTML("Select <code>Time</code> column"), NULL, NULL),
      selectInput(ns("status_var"), HTML("Select <code>Status</code> column"), NULL, NULL),
      selectInput(ns("subgroup_var"), HTML("Select <code>Subgroup</code> column"), NULL, NULL),
      tags$h4(
        "Customize plot",
        shinyBS::popify(
          icon("circle-info"),
          title = NULL,
          content = paste0(
            'Please refer to the ',
            '<a href="https://rpkgs.datanovia.com/survminer/reference/ggsurvplot.html#arguments" target="_blank">',
            'survminer::ggsurvplot',
            '</a> for more info'
          ),
          trigger = "click"
        )
      ),
      checkboxInput(
        ns("risk.table"),
        label = code_label("risk.table", "Show risk table"),
        value = TRUE
      ),
      checkboxInput(
        ns("ncensor.plot"),
        label = code_label(
          "ncensor.plot",
          "Plot the number of censored subjects at time t"
        ),
        value = TRUE
      ),
      checkboxInput(
        ns("pval"),
        label = code_label("pval", "Show p-value of log-rank test"),
        value = TRUE
      ),
      checkboxInput(
        ns("conf.int"),
        label = code_label(
          "conf.int",
          "Show confidence intervals for point estimates of survival curves"
        ),
        value = TRUE
      ),
      selectInput(
        ns("conf.int.style"),
        label = code_label(
          "conf.int.style",
          "Customize style of confidence intervals"
        ),
        choices = c("ribbon", "step"),
        selected = "step"
      ),
      selectInput(
        ns("surv.median.line"),
        label = code_label(
          "surv.median.line",
          "Add the median survival pointer"
        ),
        choices = c("none", "hv", "h", "v"),
        selected = "hv"
      ),
      shinyWidgets::numericRangeInput(
        ns("xlim"),
        code_label(
          "xlim",
          "Present narrower X axis, but not affect survival estimates"
        ),
        c(0, 500)
      ),
      textInput(
        ns("xlab"),
        label = code_label("xlab", "Customize X axis label"),
        value = "Time in days"
      ),
      numericInput(
        ns("break.time.by"),
        label = code_label("break.time.by", "Break X axis in time"),
        value = 100
      ),
      tags$h4("Style Plot"),
      numericInput(ns("plot_height"), "Plot Height (px)", 700),
      uiOutput(ns("palettes"))
    ),
    mainPanel(
      uiOutput(ns("main"))
    )
  )
}

survival_server <- function(id, data) {
  checkmate::assert_string(id)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    updateSelectInput(inputId = "dataset", choices = datanames(data()))

    output$main <- renderUI({
      plotOutput(ns("plot"), height = input$plot_height)
    })

    observeEvent(input$dataset, {
      req(input$dataset)
      col_names <- names(data()[[input$dataset]])
      updateSelectInput(
        inputId = "time_var",
        choices = col_names,
        selected = "time"
      )
      updateSelectInput(
        inputId = "status_var",
        choices = col_names,
        selected = "status"
      )
      updateSelectInput(
        inputId = "subgroup_var",
        choices = col_names,
        selected = "sex"
      )
    })

    plot_data <- reactive({
      req(input$time_var)
      req(input$status_var)
      req(input$subgroup_var)
      plot_data <- data()[[input$dataset]]
      names(plot_data)[names(plot_data) == input$time_var] <- "time_var"
      names(plot_data)[names(plot_data) == input$status_var] <- "status_var"
      names(plot_data)[names(plot_data) == input$subgroup_var] <- "subgroup_var"
      plot_data
    })
    fit <- reactive({
      survival::survfit(
        Surv(time_var, status_var) ~ subgroup_var,
        data = plot_data()
      )
    })

    output$palettes <- renderUI({
      tagList(
        lapply(1:length(plot_data()[["subgroup_var"]] |> unique()), function(i) {
          colourInput(paste0(ns("picker"), i), NULL, default_colors[i])
        })
      )
    })

    output$plot <- renderPlot({
      palette <- c()
      for (i in 1:length(plot_data()[["subgroup_var"]] |> unique())) {
        picker_input_id <- paste0("picker", i)
        color <- input[[picker_input_id]]
        palette <- c(palette, color)
      }

      plot <- ggsurvplot(
        fit = fit(),
        data = plot_data(),
        risk.table = input$risk.table,
        ncensor.plot = input$ncensor.plot,
        ncensor.plot.height = 0.25,
        pval = input$pval,
        conf.int = input$conf.int,
        palette = palette,
        xlim = input$xlim,
        xlab = input$xlab,
        break.time.by = input$break.time.by,
        risk.table.y.text.col = TRUE,
        risk.table.height = 0.25,
        risk.table.y.text = FALSE,
        conf.int.style = input$conf.int.style,
        surv.median.line = input$surv.median.line,
        legend.labs = plot_data()[["subgroup_var"]] |> unique(),
        ggtheme = theme_light()
      )
      plot
    })
  })
}

survival_module <- function(label = "Survival Plot") {
  teal::module(
    label = label,
    ui = survival_ui,
    server = survival_server
  )
}

app <- teal::init(
  data =  teal_data(
    lung = survival::lung,
    kidney = survival::kidney,
    diabetic = survival::diabetic
  ),
  modules = survival_module()
)

runApp(app)

# Example 6: Dony ####
library(shiny)
library(teal)
library(teal.slice)

custom_ui2 <- function(id) {
  ns <- NS(id)

  div(
    h2("Exercise Module"),
    selectInput(ns("selectdataset"), "Select a dataset",
                choices = c("iris", "mtcars")),
    uiOutput(ns("columnUI")),
    plotOutput(ns("myplot"))
  )
}

custom_server2 <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    selected_dataset <- reactive(data()[[input$selectdataset]])

    ns <- session$ns

    observeEvent(selected_dataset(), {
      vars <- names(selected_dataset()[sapply(selected_dataset(), is.numeric)])

      output$columnUI <- renderUI({
        selectInput(
          ns("selectcolumn"),
          "Select a Column",
          choices = vars
        )
      })
    })

    output$myplot <- renderPlot({

      column <- req(input$selectcolumn)
      x <- req(selected_dataset()[[column]])

      hist(
        x,
        breaks = "FD", col = "blue", border = "black",
        main = paste("Histogram of", input$selectcolumn),
        xlab = input$selectcolumn
      )
    })

  })
}

example_module <- function(label = "my label") {
  module(
    label = label,
    server = custom_server2,
    ui = custom_ui2,
    datanames = "all"
  )
}

filters <- teal_slices(
  teal_slice(dataname = "iris", varname = "Species", selected = "virginica"),
  teal_slice(dataname = "mtcars", varname = "cyl", selected = c(6,8), fixed = TRUE)
)

app <- init(
  data = teal_data(iris = iris, mtcars = mtcars),
  modules = example_module(label = "histogram"),
  title = "my teal app",
  filter = filters
)

runApp(app)

# Example 7: Lluís ####
library("shiny")
library("teal")
library("pharmaversesdtm")
library("ggplot2")
library("dplyr")
ui_llrs <- function(id) {
  ns <- NS(id)
  div(
    h2("Multiple data"),
    selectInput(ns("dataset"), "Select dataset:", choices = NULL),
    selectInput(ns("var1"), "Select column for x:", choices = NULL),
    selectInput(ns("var2"), "Select column for y:", choices = NULL),
    selectInput(ns("var3"), "Select column for coloring:", choices = NULL),
    plotOutput(ns("myplot"))
  )
}

server_llrs <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    updateSelectInput(session, "dataset",
                      choices = datanames(data()),
                      selected = datanames(data())[1])

    shinyjs::hideElement("var1")
    shinyjs::hideElement("var2")
    shinyjs::hideElement("var3")

    # Adding all the possible variables
    observeEvent(input$dataset, {
      data_name <- req(input$dataset)
      dataset <- data()[[data_name]]
      vars <- colnames(dataset)
      numerical_vars <- vapply(dataset, is.numeric, logical(1L))
      non_empty_vars <- colSums(is.na(dataset)) < nrow(dataset)
      keep_vars <- numerical_vars & non_empty_vars

      # Keep only filter columns to those that can be colored easily
      cat_vars <- vapply(dataset, function(x){
        any(between(n_distinct(na.omit(x)), left = 1, right = 9))
        }, logical(1L))

      v1 <- vars[keep_vars]
      v2 <- vars[!numerical_vars & non_empty_vars & cat_vars]
      updateSelectInput(inputId = "var1", choices = v1, selected = v1[1])
      shinyjs::showElement("var1")
      updateSelectInput(inputId = "var2", choices = v1, selected = v1[2])
      shinyjs::showElement("var2")

      # If no columns are valid we hide asking for colors
      if (length(v2) > 0) {
        updateSelectInput(inputId = "var3", choices = v2, selected = v2[1])
        shinyjs::showElement("var3")
      } else {
        shinyjs::hideElement("var3")
      }
    })

    output$myplot <- renderPlot({
      data_name <- req(input$dataset)
      dataset <- data()[[data_name]]
      x <- req(input$var1)
      y <- req(input$var2)

      validate(need(x %in% colnames(dataset), "Column not in dataset"),
               need(y %in% colnames(dataset), "Column not in dataset"))
      p <- dataset |>
        ggplot(aes(.data[[x]], .data[[y]])) +
        theme_minimal()

      colors <- req(input$var3)
      need(!colors %in% colnames(dataset) || length(colors) < 1,
           "Selected column not in data for coloring")
      if (colors %in% colnames(dataset) && length(colors) == 1) {
        p + geom_count(aes(col = .data[[colors]]))
      } else {
        p + geom_count()
      }
    })
  })
}



llrs_module <- function(label = "Ex. 1") {
  module(
    label = label,
    server = server_llrs,
    ui = ui_llrs,
  )
}

llrs_app <- init(
  data = teal_data(ae = pharmaversesdtm::ae,
                   other = mtcars),
  modules = llrs_module(label = "Numeric variables"),
  title = "Teal DevCore exercise 1 ",
  filter =  teal_slices(
    teal_slice(dataname = "ae", varname = "AEENDY", keep_na = FALSE)
  )
)

runApp(llrs_app)

## v3 ####
library("dplyr")
ui_llrs <- function(id) {
  ns <- NS(id)
  div(
    h2("Multiple data"),
    selectInput(ns("dataset"), "Select dataset:", choices = NULL),
    shinyjs::hidden(selectInput(ns("var_x"), "Select column for x:", choices = NULL)),
    shinyjs::hidden(selectInput(ns("var_y"), "Select column for y:", choices = NULL)),
    shinyjs::hidden(selectInput(ns("var_color"), "Select column for coloring:", choices = NULL)),
    plotOutput(ns("myplot"))
  )
}

server_llrs <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    updateSelectInput(session, "dataset",
                      choices = datanames(data()),
                      selected = datanames(data())[1]
    )

    # Adding all the possible variables
    observeEvent(input$dataset, {
      data_name <- req(input$dataset)
      dataset <- data()[[data_name]]
      numerical <- dataset %>%
        select_if(is.numeric) %>%
        select(where(~ !(all(is.na(.))))) %>%
        names()

      categorical <- dataset %>%
        select_if(~ !is.numeric(.)) %>%
        select(where(~ between(n_distinct(na.omit(.)), 1, 9))) %>%
        names()
      if (!length(numerical)) {
        shinyjs::hide("var_x")
        shinyjs::hide("var_y")
      } else {
        updateSelectInput(inputId = "var_x", choices = numerical, selected = numerical[1])
        shinyjs::show("var_x")
        updateSelectInput(inputId = "var_y", choices = numerical, selected = numerical[2])
        shinyjs::show("var_y")
      }

      # If no columns are valid we hide asking for colors
      if (!length(categorical)) {
        updateSelectInput(inputId = "var_color", choices = categorical, selected = categorical[1])
        shinyjs::show("var_color")
      } else {
        updateSelectInput(inputId = "var_color", choices = NULL, selected = NULL)
        shinyjs::hide("var_color")
      }
    })

    output$myplot <- renderPlot({
      data_name <- req(input$dataset)
      dataset <- data()[[data_name]]
      x <- req(input$var_x)
      y <- req(input$var_y)

      validate(
        need(x %in% colnames(dataset), "Column not in dataset"),
        need(y %in% colnames(dataset), "Column not in dataset")
      )
      p <- ggplot(dataset, aes(.data[[x]], .data[[y]])) +
        theme_minimal()

      colors <- req(input$var_color)
      # need(
      #   !colors %in% colnames(dataset) || length(colors) < 1,
      #   "Selected column not in data for coloring"
      # )
      if (length(colors)) {
        p + geom_count(aes_string(col = colors))
      } else {
        p + geom_count()
      }
    })
  })
}

llrs_module <- function(label = "Ex. 1") {
  module(
    label = label,
    server = server_llrs,
    ui = ui_llrs,
  )
}

llrs_app <- init(
  data = teal_data(
    ae = pharmaversesdtm::ae,
    other = mtcars,
    iris = iris[, 5, drop = FALSE]
  ),
  modules = llrs_module(label = "Numeric variables"),
  title = "Teal DevCore exercise 1 ",
  filter = teal_slices(
    teal_slice(dataname = "ae", varname = "AEENDY", keep_na = FALSE)
  )
)

runApp(llrs_app)
