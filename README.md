# MLB Statcast Analysis App

## Overview

This Shiny app provides statistical analysis and visualizations for MLB players based on Statcast data from the 2023 season. It is designed for high school, college, and professional players who want to compare their metrics to the average major leaguer. The app focuses on objective stats such as exit velocity and pitcher velocity for a more accurate comparison.

## Features

- **Pitcher Stats Visualization:** Explore the average MLB pitcher velocity and spin rates based on pitch type.
- **Hitter Stats Comparison:** Compare various statistics for qualified hitters, including exit velocity, launch angle, and sprint speed.

## Data Source

The data used in this app was obtained from [Baseball Savant](https://baseballsavant.mlb.com/leaderboard/custom?year=2023&type=batter&filter=&sort=4&sortDir=desc&min=q&selections=xba,xslg,xwoba,xobp,xiso,exit_velocity_avg,launch_angle_avg,barrel_batted_rate,&chart=false&x=xba&y=xba&r=no&chartType=beeswarm). Please make sure to select the necessary stats and player type filters to get the exact data.

## How to Use

1. Choose the "Pitcher" tab to explore pitcher statistics visualizations.
2. Navigate to the "Hitters" tab to compare stats for qualified hitters.
3. Select specific tables, such as the default table or the top 30 hitters by exit velocity or sprint speed.

## Installation

To run the app locally, follow these steps:

1. Clone this repository to your local machine.
2. Install the required R packages using `install.packages(c("shiny", "shinythemes", "tidyverse", "RColorBrewer", "DT"))`.
3. Run the app using `shiny::runApp()` in your R console.

## Contact

Feel free to reach out if you have any comments, recommendations, or concerns about the app.

- LinkedIn: [Hunter Mott](https://www.linkedin.com/in/hunter-mott/)
- Email: [h-mott@outlook.com](mailto:h-mott@outlook.com)

## License

This project is licensed under the [MIT License](LICENSE).


