
# ProbabilityConceptVisualizer

An R package that contains interactive Shiny applications to help students visualize and understand key probability concepts.

## Installation

You can install the package directly from GitHub using the devtools package in R. Make sure you have Git installed on your system:

### Step 1: Install Git
If you do not already have Git installed, follow the installation instructions for your platform:
- Windows: Download and install from [git-scm.com](https://git-scm.com).
- macOS: Install via Homebrew using the command ```brew install git```.
- Linux: Use your package manager, e.g., ```sudo apt install git``` (Ubuntu/Debian) or sudo yum install git (Fedora/RedHat).

### Step 2: Install R and RStudio (for Windows users)
If you're using Windows, it is recommended to use RStudio, an integrated development environment (IDE) for R. Download and install RStudio from [posit.co](https://posit.co/download/rstudio-desktop/).

### Step 3: Install devtools
Open RStudio (or any R console), and install the devtools package, which is needed to install packages from GitHub. In your R session, run the following command:

```r
install.packages("devtools")
```

### Step 4: Install the package from GitHub
Once devtools is installed, you can load the devtools package and install the ProbabilityConceptVisualizer package directly from GitHub. In the R session, run:

```r
library(devtools)
devtools::install_github("iitgoa-ml/probstats")
```

### Dependencies
The following R packages will be automatically installed if they are not already present in your environment:
- DT
- dplyr
- ggplot2
- gridExtra
- palmerpenguins
- plotly
- readxl
- shiny
- zoo

For manual installation run the following in R console:
```r
install.packages(c("DT", "dplyr", "ggplot2", "gridExtra", "palmerpenguins", "plotly", "readxl", "shiny", "zoo"))
```

## Usage

Once the package is installed, you can load it in R:
```r
library(ProbabilityConceptVisualizer)
```

To run a specific Shiny app, use one of the following functions:

- run_birthday_paradox_app()
- run_clt_lln_app()
- run_descriptive_analysis_app()
- run_distribution_visualizer_app()
- run_probability_tree_app()
- run_three_experiments_app()
- run_two_experiments_app()

**Note:** An active internet connection is required to display equations correctly, as the package uses MathJax for rendering LaTeX in the browser.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Contact

For any questions, feel free to contact me at sarthak.choudhary.21031@iitgoa.ac.in