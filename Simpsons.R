library(shiny)
library(bslib)
library(bsicons)
library(plotly)
library(reactable)

# ── helpers ───────────────────────────────────────────────────────────────────

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

# ── shared CSS (mode-independent) ────────────────────────────────────────────

shared_css <- "
  .math-block { text-align:center; font-size:1.05rem; margin:.75rem 0; }
  .value-box .value-box-title { font-weight:600; letter-spacing:.02em; }
  .value-box .value-box-value { font-size:1.6rem; word-break:break-all; }
  .syntax-table td, .syntax-table th { padding:.4rem .9rem; }
  .navbar { display:none !important; }
  
  /* dark-mode toggle bar sits above all content */
  #mode-bar {
    position: sticky; top: 0; z-index: 1030;
    display: flex; justify-content: flex-end; align-items: center;
    padding: 8px 18px;
    border-bottom: 1px solid rgba(128,128,128,.15);
  }
  
  /* Custom Sun/Moon Button */
  .mode-toggle-btn {
    border-radius: 8px; padding: 6px 14px; font-size: 1.25rem;
    display: inline-flex; align-items: center; justify-content: center;
    transition: all 0.2s ease-in-out; border: none; outline: none;
  }
  
  /* callout boxes */
  .callout {
    border-left: 4px solid; border-radius: 0 8px 8px 0;
    padding: .85rem 1.1rem; margin: 1rem 0;
  }
  /* takeaway rows in conclusion */
  .takeaway-row {
    display: flex; gap: .85rem; align-items: flex-start;
    padding: .7rem 0; border-bottom: 1px solid rgba(128,128,128,.12);
  }
  .takeaway-row:last-child { border-bottom: none; }
"

# ── light theme ───────────────────────────────────────────────────────────────

light_theme <- bs_theme(
  version      = 5,
  primary      = "#4F46E5",
  secondary    = "#64748B",
  success      = "#10B981",
  danger       = "#EF4444",
  warning      = "#F59E0B",
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter"),
  code_font    = font_google("JetBrains Mono"),
  bg           = "#FFFFFF",
  fg           = "#0F172A"
) |>
  bs_add_rules(paste0(shared_css, "
    .accordion-button:not(.collapsed) { background:#EEF2FF; color:#3730A3; }
    #mode-bar { background:#F8FAFC; }
    
    /* Light mode button style (Moon) */
    .mode-toggle-btn { background: #F1F5F9; color: #0F172A; }
    .mode-toggle-btn:hover { background: #E2E8F0; }
    
    .callout.info    { background:#EEF2FF; border-color:#4F46E5; color:#1e1b52; }
    .callout.success { background:#ECFDF5; border-color:#10B981; color:#065f46; }
    .callout.warning { background:#FFFBEB; border-color:#F59E0B; color:#78350f; }
    .callout.danger  { background:#FEF2F2; border-color:#EF4444; color:#7f1d1d; }
    .hero-banner     { background: linear-gradient(135deg,#EEF2FF 0%,#E0E7FF 100%);
                       border: 1px solid #C7D2FE; }
  "))

# ── dark theme ────────────────────────────────────────────────────────────────

dark_theme <- bs_theme(
  version      = 5,
  primary      = "#818CF8",
  secondary    = "#94A3B8",
  success      = "#34D399",
  danger       = "#F87171",
  warning      = "#FCD34D",
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter"),
  code_font    = font_google("JetBrains Mono"),
  bg           = "#111111",
  fg           = "#E2E8F0"
) |>
  bs_add_rules(paste0(shared_css, "
    .accordion-button:not(.collapsed) { background:#1a1a2e; color:#A5B4FC; }
    #mode-bar { background:#0D0D0D; }
    
    /* Dark mode button style (Green Sun) */
    .mode-toggle-btn { background: #1E293B; color: #34D399; }
    .mode-toggle-btn:hover { background: #334155; }
    
    .callout.info    { background:#1a1a2e; border-color:#818CF8; color:#c7d2fe; }
    .callout.success { background:#022c22; border-color:#34D399; color:#6ee7b7; }
    .callout.warning { background:#1c1502; border-color:#FCD34D; color:#fde68a; }
    .callout.danger  { background:#1f0808; border-color:#F87171; color:#fca5a5; }
    .hero-banner     { background: linear-gradient(135deg,#1a1a2e 0%,#16213e 100%);
                       border: 1px solid #2d2d5e; }
  "))

# ── Introduction panel ────────────────────────────────────────────────────────

intro_panel <- nav_panel(
  title = "Introduction",
  value = "Introduction",

  div(class = "container py-4", style = "max-width:940px;",

    # LANDING HERO
    div(class = "text-center py-5 mb-2",
      h1(class = "display-5 fw-bold mb-3",
        "Simpson's 1/3 Rule"),
      p(class  = "lead text-muted mx-auto mb-0",
        style  = "max-width:600px;",
        HTML("A numerical integration method that approximates definite integrals
              by fitting <strong>parabolic arcs</strong> through sample points —
              achieving fourth-order accuracy with minimal computation."))
    ),

    # hero banner
    div(class = "hero-banner p-4 mb-4 rounded-3 d-flex flex-column flex-md-row
                 align-items-md-center justify-content-between gap-3",
      div(
        h4(class = "fw-semibold mb-1", "Ready to integrate?"),
        p(class = "mb-0 text-muted",
          "Skip ahead and try the calculator, or read on for the theory.")
      ),
      div(class = "d-flex gap-2",
        actionButton("go_calc",
          label = tagList(bs_icon("calculator-fill"), " Open Calculator"),
          class = "btn btn-primary btn-lg px-4"),
        actionButton("go_conclusion_intro",
          label = tagList(bs_icon("flag-fill"), " Conclusion"),
          class = "btn btn-outline-secondary btn-lg px-3")
      )
    ),

    # What is it?
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
          tags$li("\\(h = \\dfrac{b-a}{n}\\) — step size (width of each subinterval)"),
          tags$li("\\(x_i = a + i \\cdot h\\) for \\(i = 0, 1, \\dots, n\\)"),
          tags$li("\\(n\\) — number of subintervals; must be a positive even integer"),
          tags$li("Weights follow the pattern \\(1,\\,4,\\,2,\\,4,\\,2,\\,\\dots,\\,4,\\,1\\)")
        ))
      )
    ),

    # ── Why parabolas? ── CONCEPTUAL card (geometry / intuition only) ─────────
    card(class = "mt-3",
      card_header(class = "fs-5 fw-semibold",
        tagList(bs_icon("bezier2"), " Why Parabolas?")),
      card_body(
        p(HTML("Every numerical integration rule replaces the curve with a simpler
               shape over small intervals and sums the resulting areas:")),
        tags$ul(
          tags$li(HTML("<strong>Rectangles</strong> — constant height per strip;
                        crude for any curved function.")),
          tags$li(HTML("<strong>Trapezoids</strong> (Trapezoidal Rule) — straight-line
                        segments between consecutive points; better, but the linear
                        approximation still <em>misses</em> the curvature of
                        \\(f(x)\\).")),
          tags$li(HTML("<strong>Parabolas</strong> (Simpson's 1/3 Rule) — a
                        quadratic polynomial is uniquely determined by any
                        <em>three</em> points. The rule groups sample points into
                        overlapping triples
                        \\((x_{2k},\\,x_{2k+1},\\,x_{2k+2})\\), fits one parabola
                        per triple, and sums the exact area under each parabola.
                        This matches the curvature of \\(f(x)\\) far more
                        faithfully."))
        ),
        div(class = "callout info",
          HTML("<strong>Why must n be even?</strong> Each parabolic strip spans
                <em>two</em> sub-intervals (three points). For the strips to tile
                \\([a,b]\\) perfectly without gaps or overlaps, the total number
                of sub-intervals must be even. If you enter an odd \\(n\\), this
                calculator bumps it up by 1 automatically.")),
        withMathJax()
      )
    ),

    # ── Error Analysis ── SEPARATE card (formula / convergence) ──────────────
    card(class = "mt-3",
      card_header(class = "fs-5 fw-semibold",
        tagList(bs_icon("graph-down-arrow"), " Error Analysis")),
      card_body(
        p(HTML("Because we fit <strong>degree-2</strong> polynomials, the errors
               from the constant, linear, and quadratic terms all cancel exactly.
               The first surviving error term involves the
               <strong>fourth derivative</strong> of \\(f\\):")),
        withMathJax(p(class = "math-block",
          "$$E = -\\frac{(b-a)}{180}\\, h^{4}\\, f^{(4)}(\\xi),
              \\quad \\xi \\in [a,b]$$")),
        tags$ul(
          tags$li(withMathJax(HTML("\\(h = (b-a)/n\\) — the step size"))),
          tags$li(withMathJax(HTML("\\(f^{(4)}(\\xi)\\) — the fourth derivative
                                    evaluated at some (unknown) point inside
                                    \\([a,b]\\)"))),
          tags$li(withMathJax(HTML("Error \\(\\propto h^4\\): <strong>doubling
                                    \\(n\\) (halving \\(h\\)) shrinks the error
                                    by a factor of \\(2^4 = 16\\)</strong>")))
        ),
        div(class = "callout warning",
          HTML("<strong>When does it struggle?</strong> Simpson's Rule is
                <em>exact</em> for any polynomial of degree ≤ 3 (the \\(h^4\\)
                term vanishes). It performs poorly when \\(f^{(4)}\\) is large
                — sharply oscillating functions, kinks, or singularities inside
                \\([a,b]\\). In those cases, increase \\(n\\) significantly or
                use adaptive integration (e.g. R's own
                <code>integrate()</code>)."))
      )
    ),

    # Applications
    card(class = "mt-3",
      card_header(class = "fs-5 fw-semibold", "Applications"),
      card_body(
        layout_columns(col_widths = c(6, 6, 6, 6),
          card(card_body(h6(class = "fw-semibold", "Engineering"),
            "Computing work, fluid-flow rates, and centroids of irregular
             cross-sections.")),
          card(card_body(h6(class = "fw-semibold", "Physics"),
            "Evaluating integrals in mechanics, thermodynamics, and
             electromagnetism.")),
          card(card_body(h6(class = "fw-semibold", "Statistics"),
            "Approximating tail probabilities under non-elementary density
             curves (e.g. the normal CDF).")),
          card(card_body(h6(class = "fw-semibold", "Economics"),
            "Computing consumer / producer surplus and integrating cost
             functions over output ranges."))
        )
      )
    ),

    # Function syntax
    card(class = "mt-3",
      card_header(class = "fs-5 fw-semibold", "Function Syntax (R)"),
      card_body(
        p("Type your function in valid R syntax. Common rewrites:"),
        tags$div(class = "table-responsive",
          tags$table(class = "table table-sm syntax-table",
            tags$thead(tags$tr(tags$th("Mathematics"), tags$th("R syntax"))),
            tags$tbody(
              tags$tr(tags$td(withMathJax("\\(2x\\)")),           tags$td(tags$code("2 * x"))),
              tags$tr(tags$td(withMathJax("\\(x^{2}\\)")),         tags$td(tags$code("x^2"))),
              tags$tr(tags$td(withMathJax("\\(\\sin(x)\\)")),      tags$td(tags$code("sin(x)"))),
              tags$tr(tags$td(withMathJax("\\(e^{x}\\)")),         tags$td(tags$code("exp(x)"))),
              tags$tr(tags$td(withMathJax("\\(\\ln(x)\\)")),       tags$td(tags$code("log(x)"))),
              tags$tr(tags$td(withMathJax("\\(\\log_{10}(x)\\)")), tags$td(tags$code("log10(x)"))),
              tags$tr(tags$td(withMathJax("\\(|x|\\)")),           tags$td(tags$code("abs(x)"))),
              tags$tr(tags$td(withMathJax("\\(\\sqrt{x}\\)")),     tags$td(tags$code("sqrt(x)")))
            )
          )
        ),
        div(class = "callout danger",
          HTML("<strong>Common mistakes:</strong><br>
                • Write <code>2 * x</code>, not <code>2x</code> — R requires
                  explicit multiplication.<br>
                • Write <code>sin(x)^2</code>, not <code>sin^2(x)</code>.<br>
                • Functions with singularities inside [a, b] (e.g.
                  <code>1/x</code> over [−1, 1]) will produce an error."))
      )
    ),

    # Footer for Introduction Page
    div(class = "text-center text-muted small mt-4 pt-2 pb-4",
      p("Cabantoc · Florentino · Juma-ang · Orpilla · Rosillosa — MAT Final Project")
    )
  ),
)

# ── Calculator panel ───────────────────────────────────────────────────────────

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

# ── Conclusion panel ───────────────────────────────────────────────────────────

conclusion_panel <- nav_panel(
  title = "Conclusion",
  value = "Conclusion",

  div(class = "container py-4", style = "max-width:940px;",

    div(class = "d-flex gap-2 mb-4",
      actionButton("go_home2",
        label = tagList(bs_icon("arrow-left-circle"), " Back to Introduction"),
        class = "btn btn-outline-secondary btn-sm"),
      actionButton("go_calc2",
        label = tagList(bs_icon("calculator-fill"), " Open Calculator"),
        class = "btn btn-outline-primary btn-sm")
    ),

    # Summary
    card(
      card_header(class = "fs-4 fw-semibold",
        tagList(bs_icon("flag-fill"), " Summary")),
      card_body(
        p(class = "lead",
          HTML("Simpson's 1/3 Rule is one of the most <strong>elegant and
               widely used</strong> methods in numerical integration. By fitting
               parabolic arcs through every three consecutive sample points,
               it achieves <strong>fourth-order accuracy</strong> — far superior
               to the Trapezoidal Rule — while remaining computationally
               straightforward to implement.")),
        withMathJax(p(class = "math-block",
          "$$\\int_{a}^{b} f(x)\\,dx \\approx
            \\frac{h}{3}\\Big[f(x_0) + 4f(x_1) + 2f(x_2)
            + \\cdots + 4f(x_{n-1}) + f(x_n)\\Big]$$"))
      )
    ),

    # Key Takeaways
    card(class = "mt-3",
      card_header(class = "fs-5 fw-semibold",
        tagList(bs_icon("lightbulb-fill"), " Key Takeaways")),
      card_body(

        div(class = "takeaway-row",
          tags$span(class = "badge rounded-pill bg-primary fs-6 flex-shrink-0", "1"),
          div(
            tags$strong("Parabolas capture curvature that straight lines miss."),
            p(class = "mb-0 text-muted small",
              "The Trapezoidal Rule connects points with straight lines, systematically
               under- or over-estimating curved integrands. Fitting a quadratic through
               every triple of points matches the curvature of f(x) far more faithfully,
               often with far fewer sample points needed.")
          )
        ),

        div(class = "takeaway-row",
          tags$span(class = "badge rounded-pill bg-primary fs-6 flex-shrink-0", "2"),
          div(
            tags$strong("Fourth-order convergence is rapid."),
            withMathJax(p(class = "mb-0 text-muted small",
              "The error scales as \\(h^4\\). Doubling \\(n\\) — which halves
               \\(h\\) — reduces the error by \\(2^4 = 16\\times\\). In practice,
               even a modest \\(n\\) (e.g. 10–20) yields excellent accuracy for
               smooth, well-behaved functions."))
          )
        ),

        div(class = "takeaway-row",
          tags$span(class = "badge rounded-pill bg-primary fs-6 flex-shrink-0", "3"),
          div(
            tags$strong("It is exact for polynomials of degree 3 or lower."),
            p(class = "mb-0 text-muted small",
              "Even though we fit parabolas (degree 2), the rule integrates cubic
               polynomials exactly — the error term's fourth-derivative factor
               vanishes for cubics. This is a pleasant bonus that makes the method
               stronger than its degree-2 fitting suggests.")
          )
        ),

        div(class = "takeaway-row",
          tags$span(class = "badge rounded-pill bg-primary fs-6 flex-shrink-0", "4"),
          div(
            tags$strong("n must always be even."),
            p(class = "mb-0 text-muted small",
              "Each parabolic strip spans two sub-intervals (three points). The
               strips must tile [a, b] perfectly, which requires an even number
               of sub-intervals. This is a hard constraint, not a guideline —
               the calculator enforces it automatically.")
          )
        ),

        div(class = "takeaway-row",
          tags$span(class = "badge rounded-pill bg-warning fs-6 flex-shrink-0",
                    style = "color:#1c1c1c;", "!"),
          div(
            tags$strong("Know the limitations."),
            p(class = "mb-0 text-muted small",
              "The rule struggles when f has a large fourth derivative inside
               [a, b] — sharp oscillations, kinks, near-singularities. In those
               cases, either increase n substantially or switch to an adaptive
               integrator (such as R's built-in integrate()).")
          )
        )
      )
    ),

    # Method comparison table
    card(class = "mt-3",
      card_header(class = "fs-5 fw-semibold",
        tagList(bs_icon("bar-chart-line-fill"), " Method Comparison")),
      card_body(
        tags$div(class = "table-responsive",
          tags$table(class = "table table-sm table-bordered syntax-table",
            tags$thead(class = "table-light",
              tags$tr(
                tags$th("Method"),
                tags$th("Shape fitted"),
                tags$th("Error order"),
                tags$th("n constraint")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("Midpoint Rule"),
                tags$td("Rectangles"),
                tags$td(withMathJax("\\(O(h^2)\\)")),
                tags$td("Any")
              ),
              tags$tr(
                tags$td("Trapezoidal Rule"),
                tags$td("Trapezoids (linear)"),
                tags$td(withMathJax("\\(O(h^2)\\)")),
                tags$td("Any")
              ),
              tags$tr(class = "table-primary fw-semibold",
                tags$td("Simpson's 1/3 Rule ★"),
                tags$td("Parabolas (quadratic)"),
                tags$td(withMathJax("\\(O(h^4)\\)")),
                tags$td("Even")
              ),
              tags$tr(
                tags$td("Simpson's 3/8 Rule"),
                tags$td("Cubics"),
                tags$td(withMathJax("\\(O(h^4)\\)")),
                tags$td("Multiples of 3")
              )
            )
          )
        )
      )
    ),

    # Reflection
    card(class = "mt-3",
      card_header(class = "fs-5 fw-semibold",
        tagList(bs_icon("journal-text"), " Reflection")),
      card_body(
        p(HTML("Building this calculator deepened our understanding of how a
               seemingly simple formula — a weighted sum of function values —
               rests on a rich geometric and algebraic foundation. The connection
               between the 1-4-2-4-1 coefficient pattern and the fitting of
               parabolas is not obvious at first glance; it becomes clear only
               once you see that integrating a quadratic exactly over two
               sub-intervals gives precisely
               \\(\\tfrac{h}{3}(f_0 + 4f_1 + f_2)\\).")),
        p(HTML("We also found that <strong>numerical error is not abstract</strong>.
               The calculator computes it concretely against R's own high-accuracy
               integrator, making it easy to see how rapidly accuracy improves as
               \\(n\\) increases — and to appreciate when the rule struggles.")),
        div(class = "callout success",
          HTML("<strong>Bottom line:</strong> Simpson's 1/3 Rule is a practical,
                accurate, and widely applicable tool. When the integrand is smooth
                and the interval is finite, it should be one of the first methods
                you reach for — and now you know exactly why."))
      )
    ),

    # Footer
    div(class = "text-center text-muted small mt-4 pt-2 pb-4",
      p("Cabantoc · Florentino · Juma-ang · Orpilla · Rosillosa — MAT Final Project")
    )
  )
)

# ── UI ─────────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  id       = "main_nav",
  title    = "Simpson's 1/3 Rule",
  theme    = light_theme,
  selected = "Introduction",

  # Mode bar: sits at the top of the content area on every page
  header = tagList(
    tags$head(withMathJax()),
    div(id = "mode-bar",
      actionButton("dark_toggle", label = bs_icon("moon"), class = "mode-toggle-btn")
    )
  ),

  intro_panel,
  calculator_panel,
  conclusion_panel
)

# ── Server ─────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── dark mode ────────────────────────────────────────────────────────────────
  is_dark <- reactiveVal(FALSE)

  observeEvent(input$dark_toggle, {
    # Flip the boolean state
    new_state <- !is_dark()
    is_dark(new_state)
    
    # Apply the correct theme
    session$setCurrentTheme(
      if (new_state) dark_theme else light_theme
    )
    
    # Swap the button icon automatically
    updateActionButton(
      session, 
      "dark_toggle", 
      label = if (new_state) bs_icon("sun") else bs_icon("moon")
    )
  }, ignoreInit = TRUE)

  # ── navigation ───────────────────────────────────────────────────────────────
  observeEvent(input$go_calc,            nav_select("main_nav", "Calculator"))
  observeEvent(input$go_calc2,           nav_select("main_nav", "Calculator"))
  observeEvent(input$go_home,            nav_select("main_nav", "Introduction"))
  observeEvent(input$go_home2,           nav_select("main_nav", "Introduction"))
  observeEvent(input$go_conclusion_intro,nav_select("main_nav", "Conclusion"))

  # ── core reactive ─────────────────────────────────────────────────────────────
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

  # ── plot (reacts to dark mode) ────────────────────────────────────────────────
  output$integral_plot <- renderPlotly({
    res  <- results()
    f    <- res$f
    a    <- input$a
    b    <- input$b
    dark <- is_dark()
    pad  <- (b - a) * 0.20

    # colour palette switches with theme
    bg_col   <- if (dark) "#1E293B" else "#FFFFFF"
    fg_col   <- if (dark) "#E2E8F0" else "#0F172A"
    grid_col <- if (dark) "#334155" else "#E5E7EB"
    line_col <- if (dark) "#A5B4FC" else "#0F172A"
    dot_col  <- if (dark) "#F87171" else "#EF4444"
    pt_col   <- if (dark) "#818CF8" else "#4F46E5"

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
        line = list(color = line_col, width = 2.5),
        name = "f(x)"
      )

    for (k in seq(1, res$n, by = 2)) {
      x012  <- res$x[k:(k + 2)]
      y012  <- res$fx[k:(k + 2)]
      A     <- cbind(x012^2, x012, 1)
      coefs <- tryCatch(solve(A, y012), error = function(e) NULL)
      if (!is.null(coefs)) {
        xx <- seq(x012[1], x012[3], length.out = 60)
        yy <- coefs[1] * xx^2 + coefs[2] * xx + coefs[3]
        p  <- p |> add_trace(
          x = xx, y = yy, type = "scatter", mode = "lines",
          line = list(color = dot_col, width = 1.8, dash = "dot"),
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
        marker = list(color = pt_col, size = 9,
                      line = list(color = if (dark) "#0F172A" else "white",
                                  width = 1.5)),
        name = "Sample points (x_i, f(x_i))"
      ) |>
      layout(
        title = list(
          text = sprintf("\u222b f(x) dx from %g to %g", a, b),
          font = list(family = "Inter", size = 16, color = fg_col)
        ),
        xaxis = list(title = "x",    zeroline = TRUE,
                     gridcolor = grid_col, color = fg_col),
        yaxis = list(title = "f(x)", zeroline = TRUE,
                     gridcolor = grid_col, color = fg_col),
        plot_bgcolor  = bg_col,
        paper_bgcolor = bg_col,
        hovermode = "x unified",
        legend = list(orientation = "h", y = -0.18, x = 0,
                      font = list(color = fg_col)),
        margin = list(t = 60, l = 60, r = 30, b = 60)
      ) |>
      config(displaylogo = FALSE)
  })

  # ── steps ─────────────────────────────────────────────────────────────────────
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
        "Use \\(h = \\dfrac{b-a}{n}\\) with \\(a = %g\\), \\(b = %g\\),
         \\(n = %d\\):", a, b, res$n))),
      p(class = "math-block",
        HTML(sprintf("$$h = \\dfrac{%g - (%g)}{%d} = %.6g$$",
                     b, a, res$n, res$h)))
    )

    step2 <- tagList(
      p("Compute each \\(x_i = a + i \\cdot h\\):"),
      p(class = "math-block",
        HTML(paste0("$$", cap_terms(res$x, "x_{%d} = %.4f"), "$$")))
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

  # ── table ─────────────────────────────────────────────────────────────────────
  output$simpson_table <- renderReactable({
    res  <- results()
    dark <- is_dark() 
    
    df <- data.frame(
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
        headerStyle = list(
          background   = if (dark) "#1e293b" else "#F1F5F9",
          fontWeight   = 600,
          borderBottom = if (dark) "2px solid #334155" else "2px solid #E2E8F0",
          color        = if (dark) "#94a3b8" else "inherit"
        ),
        cellStyle = list(fontFamily = "Inter"),
        style = list(
          background = if (dark) "#0f172a" else "white",
          color      = if (dark) "#e2e8f0" else "inherit"
        ),
        stripedColor    = if (dark) "#1e293b" else "#f8fafc",
        highlightColor  = if (dark) "#334155" else "#f1f5f9"
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
                  color), value)
          }
        ),
        weighted = colDef(name = "c_i * f(x_i)", format = colFormat(digits = 6))
      )
    )
  })
}

shinyApp(ui, server)