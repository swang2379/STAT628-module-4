# STAT628-module-4-Group-13
This repository contains data, code, and documentation for recreating Group 13's metrics and analysis of Spotify podcast for Module 4 of STAT 628 Fall 2024 at UW-Madison.

## To use this repository:
1. Pull the repository
2. Move the data files in the data directory into the same directory as the code if you don't want to use the code to update the data.
3. To recreate the metrics and analyses, run the code in the following order:
    1. get_data.R (or directly use the all_episodes.csv in the data file)
    2. data_process.Rmd
    3. app.R

### Code Directory Contents:
- get_data.R: fetch podcast data from Spotify API. Note that your own API client id, secret, and redirect uri are needed in the script.
- data_process.Rmd: do data cleaning for the Description column(get rid of the website link and punctuations, make letters to lower case.);build our metrics.
- shinyapp file: contain app.R, which build the server and UI. and if you don't want to use the code, you can visit the website, here is the link: https://cjiang232.shinyapps.io/shinyapp/.

### Data Directory Contents: 
- all_episodes.csv: contain 7 categories of podcast's episodes.
- clean version.csv: add the Description_clean column
- data use in app.rds: add the metric scores to the data

