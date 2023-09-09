# IPL Data Anaysis
This repository is a personal project of mine where I want to develop an end to end data science project from colecting ball by ball data of IPL to predicting winners of each match and creating a web application for the same.

## Phase 1
Web scraping ball by ball data from espncricinfo.com for IPL 2008-2022

The web scrapper has to be optimized for the Impact Player rule of IPL 2023

## Phase 2 (Ongoing)
Data analysis and visualization using R

## Setup
- Install MYSQL to use as the database for the project
- Run `pip install -r requirements.txt`
- Go to `SqlAlchemy Connector` section in `Scrapper.ipynb` and change the `engine` variable according to your local vaiable
- Run all the cells above the `For Debugging` section

## Exception Handling
You might face some exceptions that might have to be dealt with manually or my restarting the web scrapping process.

- **Human Intervention Error** - It is raised when there is a network issue or when a new name of a cricketer that is the same as the name of another player and there is no nickname for that player. If the referred name is a common first name/surname, you might add the players name in the `nickname` dictionary in the following format - 
x_y : z , where x = first name/surname, y = team's name and z = Full name

- **Assertion Error** - It is raised when the total score/wickets doesn't equal to the sum of the runs/wickets of all the players in an innings. One of the reason might that Byes were given on a No Ball.

- **StaleElementReferenceException, ElementClickInterceptedException, WebDriverException** - These are caused due to network issues. You just have to run the `Main Code` section again.

## SQL queries to run
You need to run some sql queries for exceptional cases.

- update all_matches set all_matches.Non_Striker = 'Kumar Sangakkara' where all_matches.Match_Id = 336003 and all_matches.Non_Striker = 'Check Required'

- update all_matches set all_matches.Non_Striker = 'Adam Gilchrist' where all_matches.Match_Id = 548328 and all_matches.Non_Striker = 'Check Required'

- update all_matches set all_matches.Non_Striker = 'Brad Hogg' where all_matches.Match_Id = 980951 and all_matches.Non_Striker = null

You also need to check for fielders that have `sub ` in their name because they were substitute fielders and you need to change their name manually.
