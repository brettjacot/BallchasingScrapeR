### Ballchasing Scraper (R)
This project was a way for me to learn R by doing something I actually enjoy, so I created a basic package that allows for ranked replays on Ballchasing.com to be parsed for stats (bear with me as this is the first time I've uploaded any sort of code to github, this may not be perfect).


### Usage
This package can be downloaded to the user's rstudio by installing the `devtools` package and running the command `devtools::install_github('brettjacot/BallchasingScrapeR')` (i havent tested this but i think its right?). The general workflow of this package is as follows:
1. get_replays_by_criteria() function to acquire a list of replay IDs based on the user-input criteria 
2. RunReplays() function with the results of get_replays_by_criteria() 
3. RankedStats() function with the results of RunReplays()

For this package to work, you will also need a Ballchasing API key, which can be generated by logging into ballchasing.com and going to https://ballchasing.com/upload.


### Credits
Thanks to CantFly for creating ballchasing.com, Colin Doyle for giving me an environment to explore this idea within Ontario Campus Carball, and Aditya Sapre for always answering my dumb questions while I figured this out.
