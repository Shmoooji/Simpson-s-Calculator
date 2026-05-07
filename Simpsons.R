library(shiny)
library(bslib)
library(bsicons)
library(plotly)
library(reactable)

parse_function <- function(expr_text) {
  expr <- parse(text = expr_text)
  function(x) eval(expr, envir = list(x = x))
}

simpsons_one_third <- function(f, a, b, n) {
  if (n %% 2 != 0) n <- n + 1L
  h    <- (b - a) / n
  i    <- 0:n
  x    <- a + i * h
  fx   <- vapply(x, f, numeric(1))
  coef <- ifelse(i == 0 | i == n, 1L,
          ifelse(i %% 2 == 1L, 4L, 2L))
  weighted <- coef * fx
  list(
    n = n, h = h, i = i, x = x, fx = fx,
    coef = coef, weighted = weighted,
    approx = (h / 3) * sum(weighted)
  )
}

app_theme <- bs_theme(
  version       = 5,
  primary       = "#4F46E5",
  secondary     = "#64748B",
  success       = "#10B981",
  base_font     = font_google("Inter"),
  heading_font  = font_google("Inter"),
  code_font     = font_google("JetBrains Mono"),
  bg            = "#FFFFFF",
  fg            = "#0F172A"
) |>
  bs_add_rules("
    .math-block { text-align:center; font-size:1.05rem; margin: .75rem 0; }
    .accordion-button:not(.collapsed) { background:#EEF2FF; color:#3730A3; }
    .value-box .value-box-title { font-weight:600; letter-spacing:.02em; }
    .value-box .value-box-value { font-size: 1.6rem; word-break: break-all; }
    .syntax-table td, .syntax-table th { padding:.4rem .9rem; }
    .navbar { display: none !important; }
  ")

intro_panel <- nav_panel(
  title = "Introduction",
  value = "Introduction",

  div(class = "container py-4", style = "max-width: 940px;",

    div(class = "p-4 mb-4 rounded-3 d-flex flex-column flex-md-row
                 align-items-md-center justify-content-between gap-3",
        style = "background: linear-gradient(135deg,#EEF2FF 0%,#E0E7FF 100%);
                 border: 1px solid #C7D2FE;",
      div(
        h4(class = "fw-semibold mb-1", "Ready to integrate?"),
        p(class = "mb-0 text-muted",
          "Skip ahead and try the calculator now, or read on for the theory.")
      ),
      actionButton("go_calc",
        label = tagList(bs_icon("calculator-fill"), " Open Calculator"),
        class = "btn btn-primary btn-lg px-4")
    ),

    card(
      card_header(class = "fs-4 fw-semibold", "What is Simpson's 1/3 Rule?"),
      card_body(
        p(class = "lead",
          HTML("<strong>Simpson's 1/3 rule</strong> is a numerical integration
                technique that approximates the definite integral of a function
                by fitting <strong>parabolic arcs</strong> through successive
                triples of sample points instead of straight-line segments.")),
        withMathJax(p(class = "math-block",
          "$$\\int_{a}^{b} f(x)\\,dx \\;\\approx\\;
            \\frac{h}{3}\\Big[f(x_0) + 4f(x_1) + 2f(x_2) + 4f(x_3)
            + \\dots + 4f(x_{n-1}) + f(x_n)\\Big]$$")),
        p("Where:"),
        withMathJax(tags$ul(
          tags$li("\\(h = \\dfrac{b-a}{n}\\) is the step size"),
          tags$li("\\(x_i = a + i \\cdot h\\) for \\(i = 0, 1, \\dots, n\\)"),
          tags$li("\\(n\\) is the number of subintervals (must be even)"),
          tags$li("Weights follow the pattern 1, 4, 2, 4, 2, ..., 4, 1")
        ))
      )
    ),

    card(class = "mt-3",
      card_header(class = "fs-5 fw-semibold", "Why parabolas?"),
      card_body(
        p("The trapezoidal rule joins consecutive points with straight lines,
           which underestimates curvature. Simpson's 1/3 rule fits a quadratic
           polynomial through every three consecutive points, capturing the
           curvature of \\(f(x)\\) more faithfully and yielding a much smaller
           error for smooth functions:"),
        withMathJax(p(class = "math-block",
          "$$E = -\\frac{(b-a)}{180}\\, h^{4}\\, f^{(4)}(\\xi),
              \\quad \\xi \\in [a,b]$$")),
        p(class = "text-muted small",
          "The fourth-power dependence on h makes Simpson's rule converge
           rapidly: halving h cuts the error by roughly a factor of 16.")
      )
    ),

    card(class = "mt-3",
      card_header(class = "fs-5 fw-semibold", "Applications"),
      card_body(
        layout_columns(col_widths = c(6, 6, 6, 6),
          card(card_body(h6(class = "fw-semibold", "Engineering"),
            "Computing work, fluid-flow rates, and centroids of irregular
             cross-sections.")),
          card(card_body(h6(class = "fw-semibold", "Physics"),
            "Evaluating integrals appearing in mechanics, thermodynamics,
             and electromagnetism.")),
          card(card_body(h6(class = "fw-semibold", "Statistics"),
            "Approximating tail probabilities under non-elementary density
             curves (e.g. the normal CDF).")),
          card(card_body(h6(class = "fw-semibold", "Economics"),
            "Computing consumer / producer surplus and integrating cost
             functions over output ranges."))
        )
      )
    ),

    card(class = "mt-3",
      card_header(class = "fs-5 fw-semibold", "Function syntax (R)"),
      card_body(
        p("Type your function in valid R syntax. A few common rewrites:"),
        tags$div(class = "table-responsive",
          tags$table(class = "table table-sm syntax-table",
            tags$thead(tags$tr(tags$th("Mathematics"), tags$th("R syntax"))),
            tags$tbody(
              tags$tr(tags$td("\\(2x\\)"),            tags$td(tags$code("2 * x"))),
              tags$tr(tags$td("\\(x^{2}\\)"),         tags$td(tags$code("x^2"))),
              tags$tr(tags$td("\\(\\sin(x)\\)"),      tags$td(tags$code("sin(x)"))),
              tags$tr(tags$td("\\(e^{x}\\)"),         tags$td(tags$code("exp(x)"))),
              tags$tr(tags$td("\\(\\ln(x)\\)"),       tags$td(tags$code("log(x)"))),
              tags$tr(tags$td("\\(\\log_{10}(x)\\)"), tags$td(tags$code("log10(x)"))),
              tags$tr(tags$td("\\(|x|\\)"),           tags$td(tags$code("abs(x)"))),
              tags$tr(tags$td("\\(\\sqrt{x}\\)"),     tags$td(tags$code("sqrt(x)")))
            )
          )
        )
      )
    )
  )
)

calculator_panel <- nav_panel(
  title = "Calculator",
  value = "Calculator",

  div(class = "d-flex justify-content-between align-items-center px-3 pt-3",
    actionButton("go_home",
      label = tagList(bs_icon("arrow-left-circle"), " Back to Introduction"),
      class = "btn btn-outline-secondary btn-sm"),
    tags$span(class = "text-muted small",
              "Adjust inputs on the left to recompute live.")
  ),

  layout_sidebar(
    sidebar = sidebar(
      width = 320, open = "always",
      title = tags$span(class = "fs-5 fw-semibold", "Inputs"),
      textInput("func", "Function f(x)", value = "sin(x)"),
      numericInput("a", "Lower bound a", value = 0,  step = 0.1),
      numericInput("b", "Upper bound b", value = pi, step = 0.1),
      numericInput("n", "Subintervals n (even)",
                   value = 6, min = 2, step = 2),
      hr(),
      uiOutput("n_note"),
      tags$small(class = "text-muted",
        "Tip: increase n for better accuracy. An odd n is bumped up
         by 1 automatically so the 1-4-2-4-1 pattern stays valid.")
    ),

    layout_columns(
      col_widths = breakpoints(xs = 12, md = 12, lg = c(6, 6)),
      value_box(
        title           = "Approximated integral",
        value           = textOutput("approx_value"),
        showcase        = bs_icon("graph-up-arrow"),
        showcase_layout = showcase_top_right(),
        theme           = "primary",
        min_height      = "130px"
      ),
      value_box(
        title           = "Absolute error vs. integrate()",
        value           = textOutput("abs_error"),
        showcase        = bs_icon("rulers"),
        showcase_layout = showcase_top_right(),
        theme           = "secondary",
        min_height      = "130px"
      )
    ),

    navset_card_tab(id = "results_tabs",
      nav_panel(title = tagList(bs_icon("activity"), " Plot"),
        plotlyOutput("integral_plot", height = "520px"),
        p(class = "text-muted small mt-2",
          "The shaded region is the area being approximated. Red dotted
           parabolas show how Simpson's rule fits a quadratic through every
           three consecutive sample points.")
      ),
      nav_panel(title = tagList(bs_icon("list-ol"), " Steps"),
        uiOutput("steps_ui")
      ),
      nav_panel(title = tagList(bs_icon("table"), " Table"),
        reactableOutput("simpson_table"),
        p(class = "text-muted small mt-2",
          "Each row shows a sample point x_i, its function value f(x_i),
           the Simpson coefficient (1-4-2-4-1 pattern), and the weighted
           contribution c_i * f(x_i).")
      )
    )
  )
)

ui <- page_navbar(
  id       = "main_nav",
  title    = "Simpson's 1/3 Rule",
  theme    = app_theme,
  selected = "Introduction",
  header   = tags$head(withMathJax()),

  intro_panel,
  calculator_panel
)

server <- function(input, output, session) {

  observeEvent(input$go_calc, {
    nav_select(id = "main_nav", selected = "Calculator")
  })
  observeEvent(input$go_home, {
    nav_select(id = "main_nav", selected = "Introduction")
  })

  results <- reactive({
    req(input$func, input$a, input$b, input$n)

    validate(
      need(is.numeric(input$a) && is.numeric(input$b),
           "Bounds must be numeric."),
      need(input$b > input$a,
           "Upper bound b must be greater than lower bound a."),
      need(is.numeric(input$n) && input$n >= 2,
           "Number of subintervals n must be at least 2."),
      need(nzchar(input$func),
           "Please provide a function f(x).")
    )

    f <- tryCatch(parse_function(input$func), error = function(e) NULL)
    validate(need(!is.null(f),
      "Could not parse the function. Check syntax (e.g. use 2*x, not 2x)."))

    test <- tryCatch(f((input$a + input$b) / 2),
                     error   = function(e) NA,
                     warning = function(w) NA)
    validate(need(is.numeric(test) && is.finite(test),
      "Function did not evaluate to a finite number at the midpoint."))

    res <- simpsons_one_third(f, input$a, input$b, as.integer(input$n))

    true_val <- tryCatch(
      stats::integrate(Vectorize(f), input$a, input$b,
                       subdivisions = 1000L)$value,
      error = function(e) NA_real_
    )
    res$true_val  <- true_val
    res$abs_error <- if (is.na(true_val)) NA_real_ else abs(res$approx - true_val)
    res$f         <- f
    res
  })

  output$n_note <- renderUI({
    res <- results()
    if (!isTRUE(input$n == res$n)) {
      div(class = "alert alert-info py-2 px-3 small mb-2",
        bs_icon("info-circle"),
        sprintf(" n was odd. Using n = %d.", res$n))
    }
  })

  output$approx_value <- renderText({
    formatC(results()$approx, digits = 8, format = "g")
  })
  output$abs_error <- renderText({
    err <- results()$abs_error
    if (is.na(err)) "-" else formatC(err, digits = 4, format = "g")
  })

  output$integral_plot <- renderPlotly({
    res <- results(); f <- res$f
    a   <- input$a;   b <- input$b
    pad <- (b - a) * 0.20

    xs <- seq(a - pad, b + pad, length.out = 400)
    ys <- vapply(xs, f, numeric(1))

    xs_shade <- seq(a, b, length.out = 200)
    ys_shade <- vapply(xs_shade, f, numeric(1))

    p <- plot_ly() |>
      add_trace(
        x = c(a, xs_shade, b),
        y = c(0, ys_shade, 0),
        type = "scatter", mode = "lines",
        fill = "toself",
        fillcolor = "rgba(79, 70, 229, 0.18)",
        line = list(color = "rgba(0,0,0,0)"),
        name = "Area under f(x)",
        hoverinfo = "skip"
      ) |>
      add_trace(
        x = xs, y = ys,
        type = "scatter", mode = "lines",
        line = list(color = "#0F172A", width = 2.5),
        name = "f(x)"
      )

    for (k in seq(1, res$n, by = 2)) {
      x012 <- res$x[k:(k + 2)]
      y012 <- res$fx[k:(k + 2)]
      A <- cbind(x012^2, x012, 1)
      coefs <- tryCatch(solve(A, y012), error = function(e) NULL)
      if (!is.null(coefs)) {
        xx <- seq(x012[1], x012[3], length.out = 60)
        yy <- coefs[1] * xx^2 + coefs[2] * xx + coefs[3]
        p <- p |> add_trace(
          x = xx, y = yy, type = "scatter", mode = "lines",
          line = list(color = "#EF4444", width = 1.8, dash = "dot"),
          showlegend = (k == 1),
          name = "Parabolic fits",
          hoverinfo = "skip"
        )
      }
    }

    p |>
      add_trace(
        x = res$x, y = res$fx,
        type = "scatter", mode = "markers",
        marker = list(color = "#4F46E5", size = 9,
                      line = list(color = "white", width = 1.5)),
        name = "Sample points (x_i, f(x_i))"
      ) |>
      layout(
        title = list(
          text = sprintf("∫ f(x) dx from %g to %g", a, b),
          font = list(family = "Inter", size = 16, color = "#0F172A")
        ),
        xaxis = list(title = "x", zeroline = TRUE, gridcolor = "#E5E7EB"),
        yaxis = list(title = "f(x)", zeroline = TRUE, gridcolor = "#E5E7EB"),
        plot_bgcolor  = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        hovermode = "x unified",
        legend = list(orientation = "h", y = -0.18, x = 0),
        margin = list(t = 60, l = 60, r = 30, b = 60)
      ) |>
      config(displaylogo = FALSE)
  })

  output$steps_ui <- renderUI({
    res <- results()
    a   <- input$a; b <- input$b

    cap_terms <- function(vec, fmt, sep = ",\\;") {
      s <- sprintf(fmt, seq_along(vec) - 1L, vec)
      if (length(s) > 12L)
        s <- c(s[1:6], "\\dots", s[(length(s) - 4L):length(s)])
      paste(s, collapse = sep)
    }

    step1 <- tagList(
      p(HTML(sprintf(
        "Use \\(h = \\dfrac{b-a}{n}\\) with \\(a = %g\\), \\(b = %g\\), \\(n = %d\\):",
        a, b, res$n))),
      p(class = "math-block",
        HTML(sprintf("$$h = \\dfrac{%g - (%g)}{%d} = %.6g$$",
                     b, a, res$n, res$h)))
    )

    step2 <- tagList(
      p("Compute each \\(x_i = a + i \\cdot h\\):"),
      p(class = "math-block",
        HTML(paste0("$$",
                    cap_terms(res$x, "x_{%d} = %.4f"),
                    "$$")))
    )

    step3 <- tagList(
      p("Evaluate the function at every sample point:"),
      p(class = "math-block",
        HTML(paste0("$$\\begin{aligned}",
                    cap_terms(res$fx, "f(x_{%d}) &= %.6f", sep = " \\\\ "),
                    "\\end{aligned}$$")))
    )

    step4 <- tagList(
      p("Apply the Simpson 1/3 coefficient pattern (1, 4, 2, 4, 2, ..., 4, 1):"),
      p(class = "math-block",
        HTML(paste0("$$c = [", paste(res$coef, collapse = ",\\;"), "]$$")))
    )

    weighted_sum <- sum(res$weighted)
    sum_terms_str <- {
      s <- sprintf("%d \\cdot %.4f", res$coef, res$fx)
      if (length(s) > 12L)
        s <- c(s[1:6], "\\dots", s[(length(s) - 4L):length(s)])
      paste(s, collapse = " + ")
    }
    step5 <- tagList(
      p("Plug everything into Simpson's 1/3 rule:"),
      p(class = "math-block",
        HTML(sprintf(
          "$$\\int_{%g}^{%g} f(x)\\,dx \\approx \\dfrac{h}{3}\\sum c_i f(x_i)
            = \\dfrac{%.4g}{3}\\big( %s \\big)$$",
          a, b, res$h, sum_terms_str))),
      p(class = "math-block",
        HTML(sprintf(
          "$$\\approx \\dfrac{%.4g}{3} \\cdot %.4g \\;\\approx\\; \\boxed{%.6g}$$",
          res$h, weighted_sum, res$approx)))
    )

    withMathJax(
      accordion(
        open = "Step 1. Compute step size h",
        accordion_panel("Step 1. Compute step size h",
                        icon = bs_icon("1-circle-fill"), step1),
        accordion_panel("Step 2. List sample points x_i",
                        icon = bs_icon("2-circle-fill"), step2),
        accordion_panel("Step 3. Evaluate f(x_i)",
                        icon = bs_icon("3-circle-fill"), step3),
        accordion_panel("Step 4. Apply 1-4-2-4-1 coefficients",
                        icon = bs_icon("4-circle-fill"), step4),
        accordion_panel("Step 5. Final sum and result",
                        icon = bs_icon("5-circle-fill"), step5)
      )
    )
  })

  output$simpson_table <- renderReactable({
    res <- results()
    df  <- data.frame(
      i        = res$i,
      x_i      = res$x,
      f_xi     = res$fx,
      coef     = res$coef,
      weighted = res$weighted
    )
    reactable(df,
      defaultPageSize = 12, compact = TRUE,
      striped = TRUE, highlight = TRUE,
      theme = reactableTheme(
        headerStyle = list(background   = "#F1F5F9",
                           fontWeight   = 600,
                           borderBottom = "2px solid #E2E8F0"),
        cellStyle   = list(fontFamily   = "Inter")
      ),
      columns = list(
        i        = colDef(name = "i", align = "center", width = 70),
        x_i      = colDef(name = "x_i",    format = colFormat(digits = 6)),
        f_xi     = colDef(name = "f(x_i)", format = colFormat(digits = 6)),
        coef     = colDef(name = "Coefficient c_i", align = "center",
          cell = function(value) {
            color <- switch(as.character(value),
                            "1" = "#94A3B8",
                            "2" = "#4F46E5",
                            "4" = "#10B981",
                            "#94A3B8")
            div(style = sprintf(
                  "background:%s;color:white;border-radius:9999px;
                   padding:2px 12px;display:inline-block;font-weight:600;",
                  color),
                value)
          }
        ),
        weighted = colDef(name = "c_i * f(x_i)", format = colFormat(digits = 6))
      )
    )
  })
}

shinyApp(ui, server)
