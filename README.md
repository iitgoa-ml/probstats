
# ProbabilityConceptVisualizer

An R package containing interactive Shiny applications to help students
visualize and understand key probability concepts.

------------------------------------------------------------------------

## Installation

You can install the **ProbabilityConceptVisualizer** package directly
from CRAN (once published):

``` r
install.packages("ProbabilityConceptVisualizer")
```

Alternatively, to install the development version from GitHub:

``` r
# Install devtools if not already installed
install.packages("devtools")

# Install the package from GitHub
devtools::install_github("iitgoa-ml/probstats")
```

### Dependencies

The package requires the following R packages, which will be
automatically installed if not already available: - `DT` - `dplyr` -
`ggplot2` - `gridExtra` - `palmerpenguins` - `plotly` - `readxl` -
`shiny` - `zoo`

If necessary, you can install these dependencies manually:

``` r
install.packages(c("DT", "dplyr", "ggplot2", "gridExtra", "palmerpenguins", "plotly", "readxl", "shiny", "zoo"))
```

------------------------------------------------------------------------

## Usage

Once the package is installed, you can load it in R:

``` r
library(ProbabilityConceptVisualizer)
```

To run a specific Shiny app, use one of the following functions:

- `run_birthday_paradox_app()`
- `run_clt_lln_app()`
- `run_descriptive_analysis_app()`
- `run_distribution_visualizer_app()`
- `run_probability_tree_app()`
- `run_three_experiments_app()`
- `run_two_experiments_app()`

**Note:** An active internet connection is required to display equations
correctly, as the package uses MathJax for rendering LaTeX in the
browser.

------------------------------------------------------------------------

## Apps Overview

Below is detailed documentation for each app. Follow this format for
consistency.

------------------------------------------------------------------------

### Descriptive Analysis App

This Shiny app allows users to explore datasets by generating
descriptive statistics and visualizations. It supports built-in datasets
(`iris` and `palmerpenguins`) and user-uploaded Excel files.

#### Key Features

- Upload Excel files to analyze custom datasets.
- Use built-in datasets like `iris` or `palmerpenguins`.
- Generate histograms, boxplots, and scatter plots.
- Calculate and display correlation coefficients for scatter plots.
- Filter plots by variables or categories.

#### How to Use

1.  Launch the app:

    ``` r
    library(ProbabilityConceptVisualizer)
    run_descriptive_analysis_app()
    ```

2.  Upload an Excel file (optional) or choose a built-in dataset.

3.  Select the type of plot from the sidebar: Histogram, Boxplot, or
    Scatter Plot.

4.  Adjust settings like variable selection, number of bins (for
    histograms), or fill options.

#### User Interface

- **Dataset Selection**:
  - Upload Excel files or select built-in datasets.
  - Add or remove sheets dynamically.
- **Plot Options**:
  - Choose plot types and customize with inputs specific to each plot.
  - Configure fill variables to group data in plots.

#### Example Visualizations

**Histogram Example**:  
*Histogram of a numeric variable from the selected dataset.*

<figure>
<img src="man/figures/descriptive_histogram.png"
alt="Histogram Example" />
<figcaption aria-hidden="true">Histogram Example</figcaption>
</figure>

**Scatter Plot Example**:  
*Scatter plot showing relationships between two numeric variables,
including correlation.*

<figure>
<img src="man/figures/descriptive_scatter.png"
alt="Scatter Plot Example" />
<figcaption aria-hidden="true">Scatter Plot Example</figcaption>
</figure>

------------------------------------------------------------------------

------------------------------------------------------------------------

### Central Limit Theorem and Law of Large Numbers App

This Shiny app allows users to explore two important statistical
concepts: the **Central Limit Theorem (CLT)** and the **Law of Large
Numbers (LLN)**.

#### Key Features

- Visualize the Central Limit Theorem by generating sample means from
  various distributions.
- Explore the Law of Large Numbers by observing convergence of sample
  averages.
- Interactive controls for choosing distribution types, adjusting
  parameters, and toggling visualization options.

#### How to Use

1.  Launch the app:

    ``` r
    library(ProbabilityConceptVisualizer)
    run_clt_lln_app()
    ```

2.  Choose a mode (CLT or LLN) from the dropdown menu.

3.  Adjust the settings in the sidebar to customize the visualizations.

#### User Interface

- **CLT Mode**:
  - Select distribution type: Uniform, Exponential, or Binomial.
  - Adjust parameters like `min` and `max` for Uniform, `rate` for
    Exponential, or `size` and `prob` for Binomial.
  - Modify sliders for sample size, number of samples, and histogram
    bins.
  - Toggle theoretical normal curve display.
- **LLN Mode**:
  - Choose an experiment type: Coin Flip, Dice Roll, or Custom.
  - Adjust number of trials and rolling average window.
  - Optionally, overlay the theoretical mean on the plot.

#### Example Visualizations

**Central Limit Theorem Mode**:  
*Visualization of sample means forming a normal distribution.*

<figure>
<img src="man/figures/clt.png" alt="CLT Histogram Sample Means" />
<figcaption aria-hidden="true">CLT Histogram Sample Means</figcaption>
</figure>

**Law of Large Numbers Mode**:  
*Observation of sample averages converging to the expected value.*

<figure>
<img src="man/figures/lln.png" alt="LLN Convergence" />
<figcaption aria-hidden="true">LLN Convergence</figcaption>
</figure>

------------------------------------------------------------------------

------------------------------------------------------------------------

### Two Experiments Visualization App

This Shiny app allows users to explore the outcomes of two experiments
(such as a coin toss or dice roll) and visualize the probabilities and
outcomes interactively.

#### Key Features

- Select between “Coin Toss” or “Dice Roll” for the two experiments.
- Choose specific outcomes for each event or explore using interactive
  mode.
- Visualize the results and probabilities on a 2D grid.

#### How to Use

1.  Launch the app:

    ``` r
    library(ProbabilityConceptVisualizer)
    run_two_experiments_app()
    ```

2.  Select the type of each experiment (either Coin Toss or Dice Roll).

3.  Choose the outcomes for each experiment.

4.  Visualize the probability and outcomes on a 2D plot.

5.  Optionally, use the interactive mode to click on the grid and select
    outcomes.

#### User Interface

- **Experiment Selection**: Choose between “Coin Toss” or “Dice Roll”
  for each experiment.
- **Outcome Selection**: Select outcomes for each event (e.g., “Head” or
  “Tail” for Coin Toss, or values from 1 to 6 for Dice Roll).
- **Interactive Mode**: In this mode, users can click on the 2D plot to
  select outcomes, which are highlighted.

#### Example Visualizations

**Outcome Visualization**:  
*Interactive 2D plot showing the outcomes of two experiments (coin toss
or dice roll).*

<figure>
<img src="man/figures/two_experiments.png"
alt="Two Experiments Visualization" />
<figcaption aria-hidden="true">Two Experiments
Visualization</figcaption>
</figure>

------------------------------------------------------------------------

------------------------------------------------------------------------

### Markov Chain Weather State Visualizer

This Shiny app simulates weather transitions using a Markov Chain model.
It allows users to define a transition matrix, set initial
probabilities, and observe the evolution of state probabilities over
time through interactive visualizations.

#### Key Features

- Validate the transition matrix and initial state probabilities.
- Simulate and visualize the evolution of state probabilities over a
  defined number of steps.
- Analyze steady-state probabilities using eigenvalues or linear
  equations.
- View dynamic representations such as state transition diagrams,
  probability plots, and pie charts.

#### How to Use

1.  Launch the app:
    ```` ```{=html} <div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div> ``` ````
2.  Define the transition matrix. Each row represents the probabilities
    of transitioning from one state to others.
3.  Set the initial probabilities for states: Sunny, Cloudy, and Rainy.
4.  Specify the number of simulation steps and click “Run Simulation.”
5.  Observe results via visualizations such as:
    - **State Transition Diagram**: Shows transitions between states
      with probabilities.
    - **Probability Evolution Plot**: Displays state probabilities over
      time.
    - **Initial and Final State Pie Charts**: Illustrate the
      distribution of probabilities at the first and final steps.

#### User Interface

- **Input Parameters**:
  - Transition matrix: Define probabilities for transitioning between
    states.
  - Initial state probabilities: Specify the likelihood of starting in
    each state.
  - Number of steps: Set the number of iterations for the simulation.
- **Visualizations**:
  - Probability evolution plot: Line graph tracking probabilities over
    time.
  - State transition diagram: Graphical representation of transitions
    and weights.
  - Pie charts: Compare initial and final state distributions.

#### Example Visualizations

**State Transition Diagram**:  
*Visual representation of transitions between weather states.*

![State Transition Diagram](man/figures/markov_transitions.png)
*(Placeholder for image: Replace with a diagram showing transitions like
Sunny → Cloudy with weighted edges.)*

**Probability Evolution Plot**:  
*Line graph showing the probability of Sunny, Cloudy, and Rainy states
over time.*

<figure>
<img src="man/figures/markov_evolution.png"
alt="Probability Evolution Plot" />
<figcaption aria-hidden="true">Probability Evolution Plot</figcaption>
</figure>

**Initial and Final State Pie Charts**:  
*Compare the probability distributions at the initial and final steps.*

<figure>
<img src="man/figures/markov_piecharts.png"
alt="Probability Pie Charts" />
<figcaption aria-hidden="true">Probability Pie Charts</figcaption>
</figure>

#### Additional Analysis

- **Steady-State Probabilities**: The app computes steady-state
  probabilities using:
  - Eigenvalue decomposition.
  - Linear equation solving.
- **Dynamic Relationships**:
  - Users can observe how the initial probabilities interact with the
    transition matrix to yield probabilities at later steps.
  - Detailed mathematical relationships are rendered using MathJax.

------------------------------------------------------------------------

------------------------------------------------------------------------

### Probability of Repeated Outcomes in Random Events Shiny App

This Shiny app allows users to explore the probabilities of repeated
outcomes in various random events such as the Birthday Paradox, Dice
Rolls, Card Draws, and Coin Flips. It also includes a simulation feature
to visualize the outcomes and highlight repeated events.

#### Key Features

- Visualize probabilities of repeated outcomes in different random
  events.
- Simulate random events and visualize the distribution of repeated
  outcomes.
- Adjust parameters like the number of samples and iterations for
  simulations.

#### How to Use

1.  Launch the app:

    ``` r
    library(ProbabilityConceptVisualizer)
    run_birthday_paradox_app()  # Launch the app
    ```

2.  Select an event type (Birthday Paradox, Dice Rolls, Card Draws, or
    Coin Flips).

3.  Adjust the number of samples (`n`) and simulate repeated outcomes.

4.  View the probability of a repeated outcome occurring at least once.

5.  Run multiple simulations and analyze the distribution of repeated
    outcomes.

#### User Interface

- **Input Parameters**:
  - Event type: Choose from “Birthday Paradox”, “Dice Rolls”, “Card
    Draws”, or “Coin Flips”.
  - Number of samples (`n`): Set the number of samples for the selected
    event.
  - Number of repeated runs: Set how many times to repeat the simulation
    for analysis.
- **Visualizations**:
  - **Probability Curve**: Line graph showing the probability of
    repeated outcomes as the number of samples increases.
  - **Simulation Plot**: Histogram displaying the frequency of repeated
    outcomes across multiple simulation runs.
  - **Probability Meter**: Displays the current probability of a
    repeated outcome occurring in the selected event.

#### Example Visualizations

**Probability Curve**:  
*Visual representation of how the probability of a repeated outcome
increases with the number of samples.*

<figure>
<img src="man/figures/birthday_curve.png" alt="Probability Curve" />
<figcaption aria-hidden="true">Probability Curve</figcaption>
</figure>

**Simulation Plot**:  
*Histogram showing the frequency of repeated outcomes across multiple
simulation runs.*

<figure>
<img src="man/figures/birthday_simulation.png" alt="Simulation Plot" />
<figcaption aria-hidden="true">Simulation Plot</figcaption>
</figure>

#### Mathematical Formulas

For each event type, the app dynamically displays the probability
equation:

1.  **Birthday Paradox**:  
    $$P(A') = 1 \times \frac{364}{365} \times \frac{363}{365} \times \cdots \times \frac{365 - n + 1}{365}$$

2.  **Dice Rolls**:  
    $$P(A') = 1 \times \frac{5}{6} \times \frac{4}{6} \times \cdots \times \frac{6 - n + 1}{6}$$

3.  **Card Draws**:  
    $$P(A') = 1 \times \frac{51}{52} \times \frac{50}{52} \times \cdots \times \frac{52 - n + 1}{52}$$

4.  **Coin Flips**:  
    $$P(A') = 1 \times \frac{1}{2} \times \cdots \times \frac{2 - n + 1}{2}$$

#### Additional Analysis

- **Simulations**: Users can run multiple simulations to observe how
  repeated outcomes vary with different numbers of samples and
  iterations.
- **Dynamic Visualizations**: As the user adjusts parameters, the app
  updates the visualizations and equations to reflect the changes in
  real-time.

------------------------------------------------------------------------

### Probability Tree Shiny App for Disease and Test Result

This Shiny app visualizes a probability tree to help understand disease
prevalence, test sensitivity, and test specificity. The app calculates
and displays probabilities related to the likelihood of disease and test
results, using both a “Disease First” and “Test First” approach.

#### Key Features

- Visualize the probability tree for disease prevalence and test
  outcomes.
- Adjust the prevalence, sensitivity, and specificity to see how they
  affect the probabilities.
- Recalculate probabilities and view the updated probability tree
  diagrams.

#### How to Use

1.  Launch the app:

    ``` r
    library(ProbabilityConceptVisualizer)
    run_probability_tree_app()  # Launch the app
    ```

2.  Adjust the values for prevalence (`P(D)`), sensitivity (`P(T | D)`),
    and specificity (`P(~T | ~D)`).

3.  Press the “Recalculate” button to update the probability tree
    diagrams.

4.  View the visualizations for the disease-first and test-first
    probability trees.

#### User Interface

- **Input Parameters**:
  - Prevalence (`P(D)`): The probability of having the disease.
  - Sensitivity (`P(T | D)`): The probability of a positive test result
    given that the person has the disease.
  - Specificity (`P(~T | ~D)`): The probability of a negative test
    result given that the person does not have the disease.
- **Visualizations**:
  - **Disease First Probability Tree**: Shows the probability of test
    results starting with the disease state.
  - **Test First Probability Tree**: Shows the probability of disease
    given the test result.

#### Example Visualizations

**Disease First Probability Tree**:  
*Visual representation of the probability tree for disease prevalence
first, showing outcomes based on test results.*

<figure>
<img src="man/figures/conditional_disease_first.png"
alt="Disease First Probability Tree" />
<figcaption aria-hidden="true">Disease First Probability
Tree</figcaption>
</figure>

**Test First Probability Tree**:  
*Visual representation of the probability tree for test results first,
showing the likelihood of disease given the test outcome.*

<figure>
<img src="man/figures/conditional_test_first.png"
alt="Test First Probability Tree" />
<figcaption aria-hidden="true">Test First Probability Tree</figcaption>
</figure>

#### Mathematical Formulas

The app uses the following formulas for probability calculations:

1.  **Disease First Probability Tree**:
    - Total Probability for Disease and Test:
      $$ P(D \cap T) = P(T|D) \times P(D) $$
    - Total Probability for No Disease and Test:
      $$ P(\neg D \cap T) = P(T|\neg D) \times P(\neg D) $$
2.  **Test First Probability Tree**:
    - Total Probability of a Positive Test:
      $$ P(T) = P(T|D) \times P(D) + P(T|\neg D) \times P(\neg D) $$

    - Probability of Disease Given a Positive Test (Bayes’ Theorem):
      $$ P(D|T) = \frac{P(T|D) \times P(D)}{P(T)} $$

------------------------------------------------------------------------

### Three Events Visualization Shiny App

This Shiny app visualizes the outcomes of three experiments: Coin Toss
or Dice Roll. Users can select different experiment types and outcomes,
and visualize the probability of selected outcomes in a 3D scatter plot.

#### Key Features

- Select between “Coin Toss” or “Dice Roll” for three different
  experiments.
- Choose specific outcomes for each event.
- Visualize the probability of selected outcomes in a 3D scatter plot.
- Display statistics for favorable outcomes, total outcomes, and event
  probability.

#### How to Use

1.  Launch the app:

    ``` r
    library(ProbabilityConceptVisualizer)
    run_three_experiments_app()  # Run the Shiny app
    ```

2.  Select the type of experiment for each of the three events (Coin
    Toss or Dice Roll).

3.  Choose the specific outcomes you want to observe for each
    experiment.

4.  Press the “Refresh Visualization” button to update the plot and
    statistics.

5.  View the 3D scatter plot of possible outcomes and the calculated
    statistics.

#### User Interface

- **Input Parameters**:
  - Select event types for the three experiments (Coin Toss or Dice
    Roll).
  - Choose specific outcomes for each event (e.g., Heads or Tails for
    Coin Toss, or numbers 1 to 6 for Dice Roll).
- **Visualizations**:
  - **3D Scatter Plot**: Displays the possible outcomes for all three
    events, with selected outcomes highlighted.
- **Event Statistics**:
  - **Favorable Outcomes**: Number of outcomes that match your criteria.
  - **Total Outcomes**: Total possible outcomes based on selected event
    types.
  - **Probability of Event**: Probability of the event (favorable
    outcomes / total outcomes).

#### Example Visualizations

**3D Scatter Plot**:  
*A 3D plot showing the possible outcomes of the three events, with
selected outcomes highlighted.*

<figure>
<img src="man/figures/three_experiments.png" alt="3D Scatter Plot" />
<figcaption aria-hidden="true">3D Scatter Plot</figcaption>
</figure>

#### Mathematical Formulas

The probability is calculated based on the selected outcomes for each
event:

- **Favorable Outcomes**: The number of outcomes that match the user’s
  selected outcomes.
- **Total Outcomes**: The total number of possible outcomes for the
  three experiments, determined by the product of the number of choices
  for each experiment.
- **Event Probability**:
  $$ P(\text{Event}) = \frac{\text{Number of Favorable Outcomes}}{\text{Total Number of Outcomes}} $$

------------------------------------------------------------------------

------------------------------------------------------------------------

### Distribution Probability Visualizer

This Shiny app provides an interactive platform to explore probabilities
under various statistical distributions, including Uniform, Normal,
Exponential, Binomial, and Geometric. Users can input parameters,
calculate probabilities, and visualize results dynamically.

#### Key Features

- Interactive visualizations of probabilities for various distributions.
- Supports multiple variables and highlights user-defined intervals as
  an approach to understanding distributions.
- Simulations to validate and compare theoretical probabilities with
  empirical results.
- Dynamic updates to visualizations based on user-defined inputs.

#### How to Use

1.  Launch the app:

    ``` r
    library(ProbabilityConceptVisualizer)
    run_distribution_visualizer_app()
    ```

2.  Choose a probability distribution:

    - Uniform
    - Normal
    - Exponential
    - Binomial
    - Geometric

3.  Configure distribution-specific parameters, such as:

    - Mean and standard deviation (Normal distribution).
    - Rate parameter (Exponential distribution).
    - Probability of success and trials (Binomial distribution).
    - Range for Uniform distribution.

4.  Specify intervals for analysis as an approach to understanding
    probabilities.

5.  Explore dynamic visualizations:

    - Probability density or mass functions.
    - Cumulative distribution functions (CDF).
    - Highlighted probabilities based on user-defined intervals.

#### User Interface

- **Input Parameters**:
  - **Distribution type**: Choose one of the supported distributions.
  - **Parameters**: Adjust relevant parameters based on the selected
    distribution.
  - **Number of simulations**: Specify the number of runs for empirical
    analysis.
- **Visualizations**:
  - **Probability Density/Mass Function (PDF/PMF)**: Visualize the
    theoretical distribution with highlighted intervals.
  - **Cumulative Distribution Function (CDF)**: Compare theoretical and
    simulated cumulative probabilities.
  - **Simulation Histograms**: Visualize empirical distributions and
    validate against theoretical probabilities.

------------------------------------------------------------------------

### Inputs and Outputs for Each Distribution

#### 1. **Uniform Distribution**

- **Inputs**:
  - **Min**: Minimum value of the uniform distribution.
  - **Max**: Maximum value of the uniform distribution.
  - **Number of simulations**: How many samples to generate for
    empirical results.
  - **Interval**: The range for calculating probabilities, specified by
    the user (e.g., to calculate the probability of being within a
    certain range).
- **Outputs**:
  - **Probability Density Function (PDF)**: Displays the constant
    density across the uniform range.
  - **Cumulative Distribution Function (CDF)**: Displays the cumulative
    probability up to a given value.
  - **Highlighted Interval**: The portion of the distribution specified
    by the user for analysis.
  - **Simulation Histogram**: Compares the theoretical uniform
    distribution with a histogram from simulated data.

**Visualization**:  
*Probability density function of a uniform distribution with highlighted
intervals.*  
![Uniform Distribution](man/figures/distribution_uniform.png)

#### 2. **Normal Distribution**

- **Inputs**:
  - **Mean ($\mu$)**: The mean (center) of the normal distribution.
  - **Standard Deviation ($\sigma$)**: The spread or dispersion of the
    distribution.
  - **Number of simulations**: How many samples to generate for
    empirical results.
  - **Interval**: The range of values for which to compute the
    probability.
- **Outputs**:
  - **Probability Density Function (PDF)**: The bell-shaped curve of the
    normal distribution.
  - **Cumulative Distribution Function (CDF)**: The area under the curve
    up to a given value.
  - **Highlighted Interval**: The portion of the normal curve for which
    the probability is calculated.
  - **Simulation Histogram**: Comparison between theoretical and
    empirical results.

**Visualization**:  
*Normal distribution density curve and CDF with highlighted regions.*
![Normal Distribution](man/figures/distribution_normal.png)

#### 3. **Exponential Distribution**

- **Inputs**:
  - **Rate ($\lambda$)**: The rate parameter of the exponential
    distribution (inverse of the mean).
  - **Number of simulations**: How many samples to generate for
    empirical results.
  - **Interval**: The range of values for which to compute the
    probability.
- **Outputs**:
  - **Probability Density Function (PDF)**: The exponential decay curve.
  - **Cumulative Distribution Function (CDF)**: The cumulative
    probability up to a given value.
  - **Highlighted Interval**: The portion of the exponential curve for
    which the probability is calculated.
  - **Simulation Histogram**: Comparison between theoretical and
    empirical results.

**Visualization**:  
*Exponential distribution with density and cumulative functions, showing
highlighted intervals.* ![Exponential
Distribution](man/figures/distribution_exponential.png)

#### 4. **Binomial Distribution**

- **Inputs**:
  - **Number of Trials (n)**: The number of independent trials (e.g.,
    number of coin flips).
  - **Probability of Success (p)**: The probability of success on each
    trial (e.g., the probability of getting heads in a coin flip).
  - **Number of simulations**: How many samples to generate for
    empirical results.
  - **Interval**: The number of successes for which the probability is
    calculated.
- **Outputs**:
  - **Probability Mass Function (PMF)**: The distribution of the number
    of successes in $n$ trials.
  - **Cumulative Distribution Function (CDF)**: The cumulative
    probability of getting up to a certain number of successes.
  - **Highlighted Interval**: The portion of the binomial distribution
    for which the probability is calculated.
  - **Simulation Histogram**: Comparison between theoretical and
    empirical results.

**Visualization**:  
*Binomial distribution PMF and CDF with highlighted intervals, overlaid
with simulation results.*  
![Binomial Distribution](man/figures/distribution_binomial.png)

#### 5. **Geometric Distribution**

- **Inputs**:
  - **Probability of Success (p)**: The probability of success on each
    trial.
  - **Number of simulations**: How many samples to generate for
    empirical results.
  - **Interval**: The number of trials for which the probability is
    calculated (e.g., the number of trials before the first success).
- **Outputs**:
  - **Probability Mass Function (PMF)**: The distribution of the number
    of trials until the first success.
  - **Cumulative Distribution Function (CDF)**: The cumulative
    probability of getting a success by a certain trial.
  - **Highlighted Interval**: The portion of the geometric distribution
    for which the probability is calculated.
  - **Simulation Histogram**: Comparison between theoretical and
    empirical results.

**Visualization**:  
*Geometric distribution with probability mass and cumulative functions,
showing highlighted intervals.*  
![Geometric Distribution](man/figures/distribution_geometric.png)

------------------------------------------------------------------------

### Additional Features

- **Simulation Validation**:
  - Compare theoretical probabilities with simulation results for
    empirical validation.
- **Dynamic Visualizations**:
  - Adjust visualizations interactively based on parameter changes.

#### Mathematical Equations

For each distribution, the app dynamically displays relevant formulas.
For example:

1.  **Normal Distribution**:
    - Probability density function (PDF):
      $$f(x) = \frac{1}{\sqrt{2 \pi \sigma^2}} e^{-\frac{(x - \mu)^2}{2 \sigma^2}}$$
    - Cumulative distribution function (CDF):
      $$F(x) = P(X \leq x) = \int_{-\infty}^x f(t) dt$$
2.  **Exponential Distribution**:
    - PDF:  
      $$f(x; \lambda) = \lambda e^{-\lambda x}, \, x \geq 0$$
    - CDF:  
      $$F(x; \lambda) = 1 - e^{-\lambda x}$$
3.  **Binomial Distribution**:
    - PMF:  
      $$P(X = k) = \binom{n}{k} p^k (1-p)^{n-k}$$
    - CDF:  
      $$P(X \leq k) = \sum_{i=0}^k P(X = i)$$
4.  **Geometric Distribution**:
    - PMF:  
      $$P(X = k) = (1-p)^{k-1} p$$
    - CDF:  
      $$P(X \leq k) = 1 - (1-p)^k$$

------------------------------------------------------------------------

This app provides a comprehensive exploration of distributions,
empowering users to understand and visualize probabilities
interactively.

------------------------------------------------------------------------

## License

This project is licensed under the MIT License - see the LICENSE file
for details.

## Contact

For any questions, feel free to contact me at
<sarthak.choudhary.21031@iitgoa.ac.in>
