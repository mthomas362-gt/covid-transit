# covid-transit-analysis

This analysis aims to evaluate the association between COVID-19 and mass transportation adoption in the US.

## To  Run Analysis

1. Open .Rproj file with RStudio.
2. Run `analysis.R` for model results.
3. Run `figures.R` for figures.

`summarizeNHTS::` library will be needed to run this analysis. Please see the documentation for installing that package here [install summarizeNHTS](https://github.com/Westat-Transportation/summarizeNHTS/tree/master/inst/install)

You will also need to add a Data\.ENV file containing only your key from the US Census Bureau.

More information on ACS API [here](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwir-pLp9670AhXwQzABHX7xD1MQFnoECAQQAQ&url=https%3A%2F%2Fwww.census.gov%2Fcontent%2Fdam%2FCensus%2Flibrary%2Fpublications%2F2020%2Facs%2Facs_api_handbook_2020_ch02.pdf&usg=AOvVaw3kN0aD8ZJZSBAtHhHqpBiM)
Link for key request [here](https://api.census.gov/data/key_signup.html)
