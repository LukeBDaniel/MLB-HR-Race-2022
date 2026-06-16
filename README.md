# MLB Home Run Race Visualization (2022)

A short data visualization project written in R that tracks Aaron Judge's historic 2022 home run chase. The project scrapes game-by-game data and compares Judge's season pace to Roger Maris's 1961 season and Barry Bonds's 2001 season.

I originally built this project back in 2022 as a fun way to practice and learn R, specifically focusing on data scraping, cleaning, and visualization techniques.

## Features
- **Web Scraping:** Automatically fetches game-log data directly from Baseball-Reference using `rvest`.
- **Data Cleaning:** Cleans and normalizes the data to account for missed games, double-headers, and varying date formats using `dplyr`.
- **Animated Visualization:** Uses `ggplot2` and `gganimate` to generate an animated line chart showing the home run race game-by-game across the three historic seasons.

## Files
- `Judge_HR_Race.R`: The core R script that fetches the data, processes it, and generates the animated GIF.
- `Judge_HR_Race_Markdown.Rmd`: An R Markdown notebook explaining the step-by-step process of building the script and visualizing the data.
- `hr_race.gif`: The final generated animation of the home run race.

## Note on 2022+ Baseball-Reference Data
This project was originally written in 2022. It has been updated to account for a change in Baseball-Reference's date formatting (from `"Apr 08"` to `"YYYY-MM-DD"`).
