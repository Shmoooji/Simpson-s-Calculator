# Simpson's 1/3 Rule Integrator

A small R Shiny application that approximates the definite integral

    integral from a to b of f(x) dx

using Simpson's 1/3 rule. Type any single-variable function in valid R syntax,
choose the bounds and the number of subintervals, and the app shows the
approximation, the error against R's own integrator, an interactive plot, a
step-by-step derivation, and a sample-point table.

---

## Quick start

If you already have R installed, this is the entire setup:

```r
install.packages(c("shiny", "bslib", "bsicons", "plotly", "reactable"))
shiny::runApp("Simpsons.R")
```

A browser window opens at `http://127.0.0.1:XXXX`. Press `Ctrl + C` in the R
console (or click the red stop sign in RStudio) to shut the app down.

---

## What you need before you start

| Requirement | Why |
|-------------|-----|
| R 4.1 or newer | The script uses the native pipe `\|>`, which only exists from R 4.1. |
| RStudio (recommended) | Easiest way to open `Simpsons.R` and click *Run App*. |
| Internet on first launch | Google Fonts and MathJax are pulled from a CDN. After the first run they are cached. |

To check your R version, open R and type `R.version.string`.

---

## Step-by-step setup for first-time users

1. **Install R**
   Download from <https://cran.r-project.org/> and run the installer with default settings.

2. **Install RStudio Desktop** (optional but easier)
   Download the free version from <https://posit.co/download/rstudio-desktop/>.

3. **Open the project**
   In RStudio: `File` -> `Open File...` -> select `Simpsons.R` from this folder.

4. **Install the five packages**
   In the R console, paste this once and press Enter:

   ```r
   install.packages(c("shiny", "bslib", "bsicons", "plotly", "reactable"))
   ```

   Wait for each package to download and compile. The first time can take a
   few minutes; subsequent runs reuse the cached binaries.

5. **Run the app**
   Either click the green *Run App* button at the top right of the editor, or
   type in the console:

   ```r
   shiny::runApp("Simpsons.R")
   ```

---

## How to use the app

When the app launches you land on the **Introduction** page. Read through the
formula and theory, or click **Open Calculator** to skip ahead.

In the Calculator:

1. **Function f(x)** -- type any expression in R syntax. Examples:
   `sin(x)`, `x^2`, `exp(-x^2)`, `1 / (1 + x^2)`, `2 * x * cos(2 * x)`.
2. **Lower bound a** and **Upper bound b** -- numeric. `b` must be greater than `a`.
3. **Subintervals n** -- positive even integer. If you type an odd number it
   is bumped up by 1 so the 1-4-2-4-1 coefficient pattern stays valid.

The right side recomputes live and shows:

- **Approximated integral** -- the value Simpson's rule produced.
- **Absolute error vs. integrate()** -- how far off Simpson's rule is from
  R's high-accuracy reference integrator.
- **Plot tab** -- the function curve, the shaded area being approximated, and
  the parabolic arcs Simpson's rule fits through every triple of sample points.
- **Steps tab** -- a five-stage derivation: compute h, list x_i, evaluate
  f(x_i), apply coefficients, sum it all up.
- **Table tab** -- every sample point, its function value, coefficient,
  and weighted contribution.

Click **Back to Introduction** at the top of the Calculator to return.

---

## Function syntax cheat sheet

| Mathematics       | R syntax    |
|-------------------|-------------|
| `2x`              | `2 * x`     |
| `x^2`             | `x^2`       |
| `sin x`           | `sin(x)`    |
| `e^x`             | `exp(x)`    |
| `ln x`            | `log(x)`    |
| `log_10 x`        | `log10(x)`  |
| `\|x\|`           | `abs(x)`    |
| `sqrt(x)`         | `sqrt(x)`   |

Two common pitfalls:

- `2x` is **not** valid R. Always write `2 * x`.
- `sin^2(x)` should be `sin(x)^2`.

---

## How Simpson's 1/3 rule works

For an interval `[a, b]` divided into `n` even subintervals of width
`h = (b - a) / n`, the approximation is

    integral approx (h / 3) *
        ( f(x_0) + 4 f(x_1) + 2 f(x_2) + 4 f(x_3) + ... + 4 f(x_{n-1}) + f(x_n) )

with sample points `x_i = a + i * h` for `i = 0, 1, ..., n`. The coefficient
on each `f(x_i)` follows the pattern `1, 4, 2, 4, 2, ..., 4, 1`. Geometrically
the rule fits a parabola through every three consecutive sample points and
sums the area under each parabola.

The error term is

    E = -(b - a) / 180 * h^4 * f^{(4)}(xi)

for some `xi` in `[a, b]`. Because the error is proportional to `h^4`, halving
`h` cuts the error by roughly a factor of 16, so the rule converges quickly
for smooth functions.

---

## Project structure

```
MAT final project/
├── Simpsons.R                              The Shiny app (UI + server + entry)
├── README.md                               This file
├── Rubrics for Final Activity.xlsx         Grading rubric used as reference
└── FinalActivity_BorromeoGarciaSamson.R    Earlier Central-Difference reference
```

`Simpsons.R` is self-contained. There are no other R source files to load and
nothing has to be sourced before running the app.

---

## Troubleshooting

- **`could not find function "bs_icon"`**
  The `bsicons` package is not installed. Run
  `install.packages("bsicons")`.

- **`This Bootstrap icon "x" does not exist`**
  Replace the icon name with a valid one from
  <https://icons.getbootstrap.com>.

- **`Function did not evaluate to a finite number at the midpoint`**
  Either the function has a singularity inside `[a, b]` (e.g. `1/x` over
  `[-1, 1]`), or the syntax is wrong. Make sure to write `2 * x`, not `2x`,
  and use parentheses around function arguments (`sin(x)`, not `sin x`).

- **`Permission denied copying htmltools.dll`** during `install.packages`
  Another R session has the DLL locked. Close every open R or RStudio
  window, open a single fresh session, and retry the install. If a
  `00LOCK-htmltools` folder is left behind in your library, delete it.

- **`unexpected '|' in "f |>"`**
  Your R is older than 4.1. Update R from
  <https://cran.r-project.org/>.

- **The app launches but the math equations show as raw `$$...$$`**
  MathJax could not load. Check your internet connection and reload the
  browser tab.

---

## Authors

Borromeo, Garcia, Samson -- MAT Final Project.
