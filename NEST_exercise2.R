# Please develop a module that utilizes `qenv`.
#
# The module should display a `plot`, a `table`, and include combined reproducible code for both the `plot` and `table`.
#
# Please refrain from using any functions from `teal.transform`.
# https://github.com/insightsengineering/coredev-tasks/issues/451

# Example 1: Vedha ####
library(teal)
library(survminer)
library(survival)
library(reactable)

survival_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    reactableOutput(ns("table")),
    plotOutput(ns("plot"), height = 500),
    actionButton(ns("get_code"), "Get Code"),
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.11/clipboard.min.js"),
    tags$script(
      HTML(
        "
        new ClipboardJS('#copy-code', {
          text: function(trigger) {
            return document.getElementById('code-text').textContent;
          }
        });
      "
      )
    )
  )
}

survival_server <- function(id, data) {
  checkmate::assert_string(id)
  checkmate::assert_class(data, "tdata")
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    q <- reactive({
      teal.code::new_qenv(teal::tdata2env(data),
                          code = teal::get_code_tdata(data))
    })

    table_qenv <- reactive({
      call <- quote({
        table <- reactable::reactable(LUNG)
        table
      })
      teal.code::eval_code(q(), call)
    })
    output$table <- renderReactable({
      teal.code::get_var(table_qenv(), "table")
    })

    plot_qenv <- reactive({
      call <- quote({
        fit <- survival::survfit(
          formula = survival::Surv(time, status) ~ sex,
          data = LUNG
        )
        plot <- survminer::ggsurvplot(
          fit = fit,
          data = LUNG
        )
        plot
      })
      teal.code::eval_code(q(), call)
    })
    output$plot <- renderPlot({
      teal.code::get_var(plot_qenv(), "plot")
    })

    observeEvent(input$get_code, {
      showModal(
        modalDialog(
          tagList(
            tags$pre(
              id = "code-text",
              class = "shiny-text-output",
              paste(teal.code::get_code(teal.code::join(table_qenv(), plot_qenv())), collapse = "\n")
            ),
            actionButton("copy-code", "Copy", "data-clipboard-target" = ".shiny-text-output")
          )
        )
      )
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
  data = teal.data::teal_data(
    dataset(
      "LUNG",
      survival::lung,
      code = "LUNG <- survival::lung"
    ),
    check = TRUE
  ),
  modules = survival_module()
)

shinyApp(app$ui, app$server)

# Example 2: kartikeya #####
library(teal)
library(teal.widgets)
library(shiny)
library(magrittr)
library(ggplot2)
library(dplyr)
library(teal.code)

explore_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("variable1"), "Choose variable 1:", choices = NULL),
    selectInput(ns("variable2"), "Choose variable 2:", choices = NULL),
    sliderInput(ns("size"), "Point size:", min = 1, max = 10, value = 5),
    actionButton(ns("plot"), "Plot"),
    teal.widgets::verbatim_popup_ui(ns("code"), "Get code"),
    plotOutput(ns("scatterPlot")),
    tableOutput(ns("dataTable"))

  )
}

explore_server <- function(id, data, dataname) {
  moduleServer(id, function(input, output, session) {
    dataset <- reactive({
      teal.code::new_qenv(tdata2env(data), code = get_code_tdata(data)) %>%
        eval_code(substitute(
          expr = {selected_data <- dataname
          col_names <- colnames(selected_data)},
          env = list(
            dataname = as.name(dataname)
          )
        ))
    })

    output_data <- reactive({
      req(input$variable1)
      req(input$variable2)
      variable1 <- input$variable1
      variable2 <- input$variable2

      eval_code(
        dataset(),
        bquote({scatter_plot <- ggplot(selected_data, aes_string(x = .(input$variable1), y = .(input$variable2))) +
          geom_point(size = .(input$size)) +
          labs(x = .(variable1), y = .(variable2)) +
          theme_minimal()
        print(scatter_plot)
        suset_table <- selected_data %>%
          select(c(.(variable1), .(variable2)))
        suset_table})
      )
    })

    observeEvent(dataset(), {
      updateSelectInput(session, "variable1", choices = get_var(dataset(), "col_names"), selected = get_var(dataset(), "col_names")[[1]])
      updateSelectInput(session, "variable2", choices = get_var(dataset(), "col_names"), selected = get_var(dataset(), "col_names")[[2]])
    })

    output$scatterPlot <- renderPlot({
      req(output_data())
      get_var(output_data(), "scatter_plot")
    })

    output$dataTable <- renderTable({
      req(output_data())
      get_var(output_data(), "suset_table")
    })

    teal.widgets::verbatim_popup_srv(
      id = "code",
      verbatim_content = reactive(teal.code::get_code(output_data())),
      title = "Show R Code"
    )

  })
}

## Dataset creation ####
x1 <- dataset(
  "iris_dt",
  iris,
  code = "iris_dt <- iris"
)

x2 <- dataset(
  "mtcars_dt",
  mtcars,
  code = "mtcars_dt <- mtcars"
)

tm_explore <- function(label = "explore", dataname) {
  module(
    label = label,
    ui = explore_ui,
    server = explore_server,
    server_args = list(dataname = dataname)
  )
}

app <- teal::init(
  data = teal_data(x1, x2),
  modules = list(
    tm_explore(label = "Tab explore", dataname = "mtcars_dt")
  )
)

runApp(app)

# Example 3: Andre ####
srv_xxxx <- function(id, data) {
  checkmate::assert_string(id)
  checkmate::assert_class(data, "tdata")

  shiny::moduleServer(id, function(input, output, session) {

    selected_dataset <- shiny::reactive(input$dataset)
    selected_scale <- shiny::reactive(input$scale)
    selected_vars <- shiny::reactive(input$vars)
    selected_rownames <- shiny::reactive(input$rownames)

    #
    # Radio buttons choices update

    # Update the choices in the radio button with available datasets
    shiny::observe({
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "dataset",
        choices = names(data),
        selected = character(0)
      )
    })

    # Update the choices in the radio button with available variables from the
    #  selected dataset
    shiny::observe({
      shiny::req(selected_dataset())
      all_data <- data[[selected_dataset()]]()

      possible_vars <- colnames(all_data)[
        vapply(all_data[1,], is.numeric, logical(1))
      ]

      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "vars",
        choices = possible_vars,
        selected = possible_vars,
        disabled = FALSE
      )

      possible_rownames_index <- vapply(
        seq(NCOL(all_data)),
        function(.col) NROW(all_data[, .col]) == NROW(unique(all_data[, .col])),
        logical(1)
      )

      if (!any(possible_rownames_index)) {
        shinyWidgets::updateRadioGroupButtons(
          session = session,
          inputId = "rownames",
          choices = "None of the columns can be a rowname",
          selected = character(0),
          disabled = TRUE
        )
      } else {
        shinyWidgets::updateRadioGroupButtons(
          session = session,
          inputId = "rownames",
          choices = colnames(all_data)[possible_rownames_index],
          selected = character(0),
          disabled = FALSE
        )
      }
    }) |>
      shiny::bindEvent(selected_dataset(), ignoreNULL = TRUE)

    #
    # qenv blocks

    plot_code_q <- shiny::reactive({
      validate_output(selected_dataset, selected_vars, data, 2)

      data_code_q <- teal.code::new_qenv(
        teal::tdata2env(data),
        code = teal::get_code_tdata(data)
      )

      # Breaking into q1, q2 & q3 for learning purposes
      q1 <- teal.code::eval_code(
        data_code_q,
        substitute(
          expr = tmpMatrix <- as.matrix(dataname[, vars]),
          env = list(
            dataname = as.name(selected_dataset()),
            vars = selected_vars()
          )
        )
      )

      q2 <- q1
      if (!is.null(selected_rownames()) && NROW(selected_rownames()) != 0) {
        # Only sets rownames if a valid column is selected
        q2 <- teal.code::eval_code(
          q1,
          substitute(
            expr = rownames(tmpMatrix) <- dataname[, col_id_rownames],
            env = list(
              dataname = as.name(selected_dataset()),
              col_id_rownames = selected_rownames()
            )
          )
        )
      }

      q3 <- teal.code::eval_code(
        q2,
        substitute(
          expr = {
            hm <- pheatmap::pheatmap(tmpMatrix, scale = scale, angle_col = 0)
            hm
          },
          env = list(scale = selected_scale())
        )
      )
      q3
    })

    # Appending table code to plot due to exercise description
    #  It would be better to have a code block for each
    table_code_q <- shiny::reactive({
      shiny::validate(
        shiny::need(plot_code_q(), label = "Please, check the parameters")
      )

      validate_output(selected_dataset, selected_vars, data, 1)

      q1 <- teal.code::eval_code(
        plot_code_q(),
        substitute(
          expr = {
            lyc <- rtables::basic_table() |>
              rtables::analyze(selected_vars, function(x, ...) {
                if (is.numeric(x)) {
                  rtables::in_rows(
                    "Mean (sd)" = c(mean(x), sd(x)),
                    "Median" = median(x),
                    "Min - Max" = range(x),
                    .formats = c("xx.xx (xx.xx)", "xx.xx", "xx.xx - xx.xx")
                  )
                } else if (is.factor(x) || is.character(x)) {
                  rtables::in_rows(.list = rtables::list_wrap_x(table)(x))
                } else {
                  stop("type not supported")
                }
              })
            summary_table <- rtables::build_table(lyc, dataname[, selected_vars])
            summary_table
          },
          env = list(
            selected_vars = selected_vars(),
            selected_dataset = selected_dataset(),
            dataname = as.name(selected_dataset())
          )
        )
      )
    })

    # "Show code" event
    shiny::observe({
      # In case there are errors with visualizations, they'll show in the code
      code_r <- tryCatch(
        teal.code::get_code(table_code_q()),
        error = function(err) {
          c(
            "# \U26A0\uFE0F Problem with inputs:",
            paste("#", strsplit(err$message, "\n")[[1]])
          )
        }
      )
      shiny::showModal(
        shiny::modalDialog(
          easyClose = TRUE,
          title = "Source code",
          shiny::tagList(
            lapply(
              code_r,
              function(.code_line) {
                shiny::tagList(
                  shiny::HTML(prismjs::prism_highlight_text(
                    .code_line, language = 'r'
                  )),
                  shiny::tags$br()
                )
              }
            )
          )
        )
      )
    }) |>
      shiny::bindEvent(input$show_code)

    # Render plot & table
    output$heatmap <- shiny::renderPlot(
      plot_code_q()[["hm"]]
    )
    output$table <- shiny::renderUI({
      # Render as html
      rtables::as_html(table_code_q()[["summary_table"]])
    })

  })
}

#' Validate input parameters
#'
#' Auxiliary function that takes reactive variables to avoid repetitive code
#' between table and plot
validate_output <- function(selected_dataset_r,
                            selected_vars_r,
                            data_r,
                            min_length = 1) {
  shiny::validate(
    shiny::need(
      !is.null(selected_dataset_r()),
      "\u2139\uFE0F Please select a dataset"
    ),
    shiny::need(
      !is.null(selected_vars_r()) && length(selected_vars_r()) >= min_length,
      glue::glue("\u2139\uFE0F Please select at least {min_length} variables")
    )
  )

  shiny::validate(shiny::need(
    NROW(data_r[[selected_dataset_r()]]()) > 0,
    "\u2139\uFE0F Please check your filters as at least 1 observation is needed"
  ))

  shiny::req(
    all(selected_vars_r() %in% colnames(data_r[[selected_dataset_r()]]()))
  )
}

ui_xxxx <- function(id, scale = "column") {
  ns <- shiny::NS(id)
  tags <- shiny::tags

  tags$div(
    shiny::includeCSS("https://cdnjs.cloudflare.com/ajax/libs/prism/9000.0.1/themes/prism-coy.min.css"),
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
      shiny::actionButton(ns("show_code"), "Show Code"),
      tags$hr(),

      tags$div(
        tags$h3("Heatmap"),
        tags$div(tags$em("Heatmap options:"), style = "padding-bottom: 1em;"),
        tags$div(
          style = "padding-left: 1em;",
          shinyWidgets::radioGroupButtons(
            inputId = ns("scale"),
            label = "Color scale by:",
            choices = c("column", "row", "none"),
            selected = scale
          ),
          shinyWidgets::radioGroupButtons(
            inputId = ns("rownames"),
            label = "Show column as row id",
            choices = "Waiting for a dataset to be choosen...",
            selected = character(0),
            disabled = TRUE
          )
        ),
        shiny::plotOutput(ns("heatmap"))
      ),
      tags$div(
        tags$h3("Summary table"),
        shiny::uiOutput(ns("table"))
      )
    )
  )
}

#  module :: the function which creates the teal module for users
tm_xxxx <- function(label) {
  checkmate::assert_string(label)

  teal::module(
    label = label,
    server = srv_xxxx,
    ui = ui_xxxx,
    ui_args = list(),
    server_args = list(),
    filters = "all"
  )
}

##  shiny app ####

library(shiny)
library(teal)
library(teal.code)
library(rtables)
library(pheatmap)
library(teal)

td_mtcars <- tibble::rownames_to_column(mtcars, var = "Model")

app <- teal::init(
  data = list(
    MTCARS = td_mtcars,
    IRIS = iris
  ),
  modules = tm_xxxx(
    label = "Sample xxxx Module"
  ),
  header = "Simple app with xxxx module"
)

if (interactive()) {
  shiny::shinyApp(app$ui, app$server)
}

# Example 4: Dony ####

library(shiny)
library(teal)
library(teal.slice)
library(teal.code)

custom_ui2 <- function(id, data) {
  ns <- NS(id)
  div(
    h2("Exercise Module"),
    selectInput(ns("selectdataset"), "Select a dataset", choices = names(data)),
    uiOutput(ns("columnUI")),
    plotOutput(ns("myplot")),
    verbatimTextOutput(ns("show_code"))
  )
}

custom_server2 <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    selected_dataset <- reactive(data[[input$selectdataset]]())

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

    hist_qenv <- reactive({
      selected_column <- req(input$selectcolumn)
      selected_dataset <- req(input$selectdataset)
      req(selected_dataset()[[selected_column]])

      q1 <- new_qenv(
        env = tdata2env(data),
        code = get_code_tdata(data)
      )

      q2 <- eval_code(
        q1,
        substitute(
          expr = {
            myhist <- hist(
              dataname[, column],
              breaks = "FD",
              main = paste("Histogram of", column),
              xlab = column
            )
          },
          env = list(
            dataname = as.name(selected_dataset),
            column = selected_column
          )
        )
      )

      q2
    })

    output$myplot <- renderPlot({

      hist_qenv()[["myhist"]]

    })

    output$show_code <- renderText({
      paste(get_code(hist_qenv()), collapse = "\n")
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
  data = teal_data(
    dataset("iris", iris, code = "iris"),
    dataset("mtcars", mtcars, code = "mtcars"),
    check = TRUE
  ),
  modules = example_module(label = "histogram"),
  title = "my teal app",
  filter = filters
)

shinyApp(app$ui, app$server)

# Example 5: Marcin ####
library(teal)
library(teal.code)
library(ggplot2)

qenv <- new_qenv()

kmeans_ui <- function(id) {

  teal.widgets::standard_layout(
    output = tagList(
      selectInput(NS(id, "selected"), "Variable", choices = NULL, multiple = TRUE),
      selectInput(NS(id, "plotx"), "X plot variable", choices = NULL, multiple = FALSE),
      selectInput(NS(id, "ploty"), "Y plot variable", choices = NULL, multiple = FALSE),
      numericInput(NS(id, "centers"), "bins", 5, min = 1),
      plotOutput(NS(id, "plot")),
      #verbatimTextOutput(NS(id, "table_rcode")),
      tableOutput(NS(id, "table"))
    ),
    forms = teal.widgets::verbatim_popup_ui(NS(id, "rcode"), "Show R code")
  )
}

kmeans_server <- function(id, data, dataname) {
  moduleServer(id, function(input, output, session) {
    dataset <- reactive({
      x <- data[[dataname]]()
      x[vapply(x, is.numeric, logical(1))]
    })

    # initialize selection when server is accessed
    isolate({
      updateSelectInput(session, "selected", choices = names(dataset()), selected = names(dataset()))
      updateSelectInput(session, "plotx", choices = names(dataset()), selected = names(dataset())[1])
      updateSelectInput(session, "ploty", choices = names(dataset()), selected = names(dataset())[2])
    })

    selected <- reactive({input$selected})
    centers <- reactive({input$centers})
    dtset <- reactive({data[[dataname]]()})

    clustering_data_output <- reactive({
      req(input$centers)

      teal.code::new_qenv(
        tdata2env(data),
        code = get_code_tdata(data)
      ) |>
        teal.code::eval_code(
          substitute(
            expr = {
              dataset <- dtset[vapply(dtset, is.numeric, logical(1))]
              data_trim <- dataset[, selected, drop = FALSE]

              clustering_data <-
                cbind(
                  data_trim,
                  clusters = kmeans(data_trim, centers = centers)$cluster
                )
            },
            env = list(
              dataname = dataname,
              data = data,
              dtset = dtset(),
              centers = centers(),
              selected = selected()
            )
          )
        )
    })

    output$plot <- renderPlot({
      ggplot(
        clustering_data_output()[["clustering_data"]],
        aes_string(
          x = input$plotx,
          y = input$ploty,
          col = 'as.factor(clusters)'
        )
      ) +
        geom_point() +
        xlab(input$plotx) +
        ylab(input$ploty) +
        theme(legend.position = "top") +
        guides(col = guide_legend(title = "Clusters", nrow = 1))

    })

    output$table <- renderTable({
      clustering_data_output()[["clustering_data"]]
    })

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = reactive(teal.code::get_code(clustering_data_output())),
      title = "R Code"
    )


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
  data = list(iris = iris),
  modules = list(
    tm_kmeans(label = "Tab kmeans", dataname = "iris")
  )
)

runApp(app)

# Example 6 Aleksander ####
library(shiny)
library(shinyvalidate)
library(teal.slice)
library(teal)
library(teal.code)
library(teal.widgets)

acmodule_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("selections_summary")),
    div(
      DT::dataTableOutput(ns("table")),
    ),
    uiOutput(ns("selections_plotting")),
    plotOutput(ns("plot")),
    div(
      verbatim_popup_ui(ns("rcode"), "combined code", type = "button"),
      NULL),
    NULL
  )
}

acmodule_srv <- function(id,
                         data,
                         sumfuns = c("identity"),
                         plotfuns = c("sort", "rev"),
                         ...) {
  checkmate::assert_string(id)
  checkmate::assert_class(data, "tdata")
  checkmate::assert_character(sumfuns, min.len = 1L, any.missing = FALSE)
  checkmate::assert_character(plotfuns, min.len = 1L, any.missing = FALSE)


  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    iv_data <- shinyvalidate::InputValidator$new()
    iv_data$add_rule("dataname", shinyvalidate::sv_required("choose a data set"))
    iv_data$add_rule("dataname", function(value) if (length(value) > 1L) "select only one data set")
    iv_data$enable()
    iv_table <- shinyvalidate::InputValidator$new()
    iv_table$add_rule("summary_function", shinyvalidate::sv_required("choose a summary funciton"))
    iv_table$add_rule("summary_function", function(value) {
      if (is.null(get0(value, mode = "function"))) "summary function not found"
    })
    iv_table$enable()
    iv_plot <- shinyvalidate::InputValidator$new()
    iv_plot$add_rule("variable", shinyvalidate::sv_required("choose variable to plot"))
    iv_plot$add_rule("plotting_function", shinyvalidate::sv_optional())
    iv_plot$add_rule("plotting_function", function(value) {
      selection <- sapply(value, function(x) get0(x, mode = "function"), simplify = FALSE)
      funs_missing <- Filter(is.null, selection)
      if (length(funs_missing) != 0L) {
        sprintf("plotting functions not found: %s", toString(names(funs_missing)))
      }
    })
    iv_plot$enable()

    output[["selections_summary"]] <- renderUI({
      tagList(

        selectInput(
          ns("dataname"),
          "choose data set",
          choices = c("choose a data set" = "", names(data)),
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

    sumfun_selected <- reactive({
      req(input[["summary_function"]])
    })
    output[["selections_plotting"]] <- renderUI({
      req(length(dataname_selected()) == 1L)
      numerics <- Filter(is.numeric, data[[dataname_selected()]]())

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
    variable <- reactive({
      req(input[["variable"]])
    })
    plotting_function <- reactive({
      selection <-
        if (isTruthy(input[["plotting_function"]])) {
          input[["plotting_function"]]
        } else {
          "identity"
        }
      selection <- sapply(selection, get0, simplify = FALSE)

      function(x) {
        if (is.numeric(x)) {
          eval(Reduce(f = call, x = rev(names(selection)), init = x, right = TRUE))
        } else if (is.character(x)) {
          deparse1(Reduce(call, rev(names(selection)), init = str2lang(x), right = TRUE)) |>
            gsub("(\\(|\\))", " \\1 ", x = _)
        } else {
          stop("plotting funciton: don't know how to handle type ", typeof(x))
        }
      }
    })



    # handle data set selection
    q_data <- reactive({
      new_qenv(env = tdata2env(data), code = get_code_tdata(data))
    })

    # preparing table view
    q_dt <- reactive({
      validate_inputs(iv_data, iv_table)

      eval_code(q_data(), code = substitute(
        env = list(
          dataset = as.name(dataname_selected()),
          summary_function = as.name(sumfun_selected())
        ),
        expr = {
          dt <- summary_function(dataset)
        }
      ))
    })

    # preparing plot
    q_plot <- reactive({
      validate_inputs(iv_data, iv_plot)

      eval_code(q_data(), code = substitute(
        env = list(
          dataname = dataname_selected(),
          dataset = as.name(dataname_selected()),
          variable = variable(),
          plotting_function = plotting_function()
        ),
        expr = {
          x <- dataset[[variable]]
          x <- x[!is.na(x) & is.finite(x)]
          fun <- plotting_function
          pl <- plot(
            x = fun(dataset[[variable]]),
            ylab = fun(sprintf("%s$%s", dataname, variable)),
            las = 1
          )
        }
      ))
    })


    output[["table"]] <- DT::renderDataTable({
      q_dt()[["dt"]]
    })

    output[["plot"]] <- renderPlot({
      q_plot()[["pl"]]
    })


    combined_code <- reactive({
      validate_inputs(iv_data, iv_plot, iv_table)
      get_code(join(q_plot(), q_dt()))
    })

    verbatim_popup_srv("rcode", verbatim_content = combined_code(), title = "R Code For Table")

  })
}

acmodule <- function(label = "this little module") {
  teal::module(
    label = label,
    server = acmodule_srv,
    ui = acmodule_ui,
    datanames = "all",
    server_args = list(
      sumfuns = c("identity", "head", "tail"),
      plotfuns = c("sort", "cumsum", "sample", "rev")
    )
  )
}



app <- init(
  data =  list(
    iris = iris,
    mtcars = mtcars,
    faithful = faithful,
    warpbreaks = warpbreaks
  ),
  modules = acmodule(),
  filter = teal_slices()
)

shinyApp(app$ui, app$server, options = list("launch.browser" = TRUE))

# Example 7: Lluís ####
library("teal")
library("teal.code")
library("ggplot2")
library("palmerpenguins")
library("reactable")

llrs_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    reactableOutput(ns("table")),
    selectInput(ns("x"), "X plot variable", choices = NULL, multiple = FALSE),
    selectInput(ns("y"), "Y plot variable", choices = NULL, multiple = FALSE),
    plotOutput(ns("plot"), height = 500),
    actionButton(ns("get_code"), "Get Code")
    )

}

llrs_srv <- function(id, data, dataname) {
  checkmate::assert_string(id)
  moduleServer(id, function(input, output, session) {
    dataset <- reactive({data()[[dataname]]})
    isolate({
      names_dataset <- colnames(dataset())
      updateSelectInput(session, "x", choices = names_dataset,
                        selected = names_dataset[1])
      updateSelectInput(session, "y", choices = names_dataset,
                        selected = names_dataset[2])
    })
    # browser()
    # ns <- session$ns
    #
    # qenv_dataset <- within(qenv(), {data <- dataset()})
    #
    # ## Table ####
    # table_qenv <- reactive({
    #   call <- quote({
    #     table <- reactable::reactable(dataset())
    #     table
    #   })
    #   teal.code::eval_code(qenv_dataset(), call)
    # })
    #
    # output$table <- renderReactable({
    #   teal.code::get_var(table_qenv(), "table")
    # })
    # ## Plot ####
    plot_qenv <- reactive({
      call <- quote({
        ggplot(table) +
          geom_point(aes(x, y))
      })
      teal.code::eval_code(qenv_dataset(), call)
    })

    output$plot <- renderPlot({
      x <- req(input$x)
      y <- req(input$y)
      ggplot(dataset()) +
        geom_point(aes(.data[[x]], .data[[y]]))
    })

    # output$plot
    ## Show code ####
    observeEvent(input$get_code, {
      showModal(
        modalDialog(
          tagList(
            tags$pre(
              id = "code-text",
              class = "shiny-text-output",
              paste(get_code(table_qenv()),collapse = "\n")
            ),
            actionButton("copy-code", "Copy", "data-clipboard-target" = ".shiny-text-output")
          )
        )
      )
    })

  })
}

llrs_tm <- function(label = "explore", dataname) {
  module(
    label = label,
    ui = llrs_ui,
    server = llrs_srv,
    server_args = list(dataname = dataname)
  )
}

llrs_app <- teal::init(
  data = teal_data(penguins = palmerpenguins::penguins, mtcars = mtcars,
                   code = "penguins <- palmerpenguins::penguins
                   mtcars <- mtcars") |> verify(),
  modules = list(
    llrs_tm(label = "Explore penguins", dataname = "penguins"),
    llrs_tm(label = "Explore mtcars", dataname = "mtcars")
  )
)

runApp(llrs_app)
