#### App Functions

1. Clean invalid air-quality observations.
2. Remove countries with unreliable PM2.5 data.
3. Calculate country-level average pollutant values.
4. Display pollutant-by-pollutant correlations.
5. Click a non-diagonal cell to generate a regression plot.

---

#### Cleaning Rules

1. Remove missing country, pollutant, and value.
2. Remove negative pollutant values.
3. Remove countries with fewer than 3 PM2.5 observations.
4. Remove countries whose PM2.5 values are all zero.

---

#### How to Use This Page

This heatmap shows the correlation between different pollutants.
Red cells indicate positive correlation.
Blue cells indicate negative correlation.
Diagonal cells are disabled because self-correlation is always 1.
Click a non-diagonal cell to view the regression relationship between the selected two pollutants.