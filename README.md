![alt text](https://i.imgur.com/dFbt0Qr.png)
# Functional diversity in human song repository 🎷🎸🎶

## Overview
This repository contains data and code for the analysis presented in the paper **"Functional diversity in human song"** by Colares et al., published in *PLOS One* in 2024. The study explores how functional diversity—a concept originating in ecology—can be applied to music, analyzing 12,944 songs from the top 100 artists of the 2010s across four streaming platforms. The study examines the relationship between musical diversity and song popularity, finding that high functional richness correlates with increased listens and revenue.

## Repository Structure
This repository is organized as an **R project** and includes the following main directories:

- **`datasets/`**: Contains the raw data used in the analysis. Files include:
  - `Genres.csv`: Genre classification of songs.
  - `Streams.csv`: Streaming counts across platforms.
  - `Traits.csv`: Song-level characteristics.
  - `careerTime.txt`: Career duration of artists.
  - `final genius tags.csv`: Additional metadata from Genius.
  - `Excluded_Genres.txt`: List of excluded genres.

- **`results/`**: Stores processed data and outputs from the analysis, including:
  - Correlation matrices (e.g., `correlation matrix - albums traits.csv`).
  - Functional diversity calculations (`functional diversity.csv`).
  - Principal Component Analysis results (`loadings pca.csv`, `pca variance.csv`).
  - Regression model coefficients (`model coefs streams ~ traits of songs.csv`, etc.).
  - Stepwise regression outputs (`stepwise streams ~ artists.csv`, etc.).

- **`scripts/`**: Contains R scripts used for statistical analysis and data processing. Notable scripts include:
  - `00. setup.R`: Loads necessary libraries and functions.
  - `01. calculate functional diversity.R`: Computes functional diversity metrics (richness, evenness, divergence).
  - `02. statistical analysis.R`: Performs statistical analyses using generalized linear models (GLMs) and plot results.
  
- **Project Files**:
  - `DataPop.Rproj`: The R project file.
  - `README.md`: This document.

## Getting Started  

1. Clone the repository:  
   ```bash  
   git clone https://github.com/lucas-colares/data-pop.git  
   ```  
2. Open `DataPop.Rproj` in RStudio.  
3. Run `00. setup.R` to set up the analysis environment.  
4. Use the provided scripts to explore and analyze the data.  

## Citation
If you use this repository, please cite:

> Colares, L., Lopes-Neto, R. B., Siqueira, A. S. D., Leão, C. F., Castro, A. F. D., & Dunck, B. (2024). *Functional diversity in human song*. *PLOS One, 19*(7), e0307032.  
[https://doi.org/10.1371/journal.pone.0307032](https://doi.org/10.1371/journal.pone.0307032)

## License
This project is licensed under the **MIT License**.  
You are free to use, modify, and distribute the code with proper attribution.
