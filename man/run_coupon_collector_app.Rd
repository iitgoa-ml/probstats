% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coupon_collector_problem.R
\name{run_coupon_collector_app}
\alias{run_coupon_collector_app}
\title{Run the Coupon Collector Problem Visualization Shiny App}
\usage{
run_coupon_collector_app()
}
\value{
A Shiny app instance that runs locally in your browser.
}
\description{
This function launches a Shiny app that provides interactive visualizations and simulations for the Coupon Collector Problem.
Users can explore theoretical expectations, run Monte Carlo simulations, and view distributions (PMF, CDF) related to the problem.
}
\details{
Features:
- Simulate the Coupon Collector Problem with customizable parameters.
- Visualize expected trials, simulation results, probability mass function (PMF), and cumulative distribution function (CDF).
- Dynamic plot themes (Light, Minimal, Dark).
- Includes a detailed explanation of the Coupon Collector Problem and its key formula.
}
\examples{
\dontrun{
run_coupon_collector_app() # Run the Shiny app
}

}
