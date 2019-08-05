# Machine learning models

## Intro

Machine learning is really good at taking a vector of inputs (x1, x2, ... , xn) and, using a “model,” or an algorithm trained on similar data, turning that vector into some singular output, like a probability of falling into class 0 or class 1. That’s what we set out to do here with March Madness games — to fully define the problem, we have vectors of statistics for each team, and we want to output a win probability in (0,1) for each matchup between two given teams. This way, we can simulate every possible matchup in the 2019 tournament, choose the most probable winners, and let our models fill out our brackets for us.

## Approach

Kaggle provided us with a great data set listing the game-level statistics for the winning and losing teams of every NCAA men’s basketball game since 1985, as well as aggregate ranking system ranks at the end of each season for each team. The trickiest part of this exercise for me was deciding how to take that data and wrangle it into a form suitable for training models and then predicting 2019 tournament games. Game-level data was hard to work with, because Kaggle will only provide season-level data for each tournament team going in to their first-round games. The obvious thing to do here was to aggregate the game-level regular season data into season-level data for each tournament team, which we could then use to train a model to predict tournament games (we treat the tournament as “postseason” here, so when we say “season-level data” we mean regular season statistics including conference championships but not including tournament results). From above, we notice the next tricky part of this problem: we have two vectors of team-level statistics, but our models take a single vector of inputs. We have a two choices here, as I see it: we could simply concatenate the vectors — but that might get ugly with the number of inputs the model would have to accept, and we might run into issues with variable importance etc — or we could subtract the losing team’s stats from the winning teams, creating a vector of winning team/losing team “deltas” that should paint a complete picture of the statistical parity of each matchup. That seems elegant. Let’s do that.

So we’ve defined the problem and our inputs: we’ll aggregate the game-level data into seasons for each tournament team in each year going back 10 years (due to the limitations of the dataset), then we’ll use the season-level “deltas” for each tournament matchup over the past 10 years, along with the correct results, to train the models. For tasks like these, my machine learning class taught me that gradient boosting is the best tool to use out of the box, so we’ll try a classic cross-validated xgboost model, but to compare performance on this specific dataset, we’ll train a random forest model and a lasso logistic regression as well. We’ll evaluate the models on the average misclassification rate on the training set using 10-fold cross-validation, pick the best one, and go from there. 

One caveat, though. This is March Madness. Empirically, upsets happen a lot — the best team doesn’t always win. So we’ll compensate for this by having the computer flip a weighted coin for each matchup based on the calculated win probability. Say our model tells us (1) Duke has a 95% chance of beating (16) ND State in the round of 64. We assign the set (0,0.95) to Duke and (0.95,1) to ND State and have the computer randomly pick a number between 0 and 1. If it’s in Duke’s set, they win — else ND State wins. This introduces an element of randomness into the chalk that our bracket would otherwise be — which can only be a good thing during March Madness.

The last thing to decide is the individual statistics we want to input into our model — we have 20-something “classic” statistics and an additional set of aggregate rankings like Pomeroy, Sagarin etc. The cool thing about machine learning is that the models give weights to the most important inputs on the fly, so we don’t need to pick and choose — let’s use all of the game-level stats and 5 solid ranking systems to stay reasonable. Admittedly there was not much method to this and I went with what I knew (+ some exploratory analysis/googling to see which ranking systems have been most predictive over time). We also include RPI, NCAA’s strength-of-schedule metric, non-negotiably.

## Results

Well. We didn’t win the Kaggle competition. We were in 30th out of 800+ for a few rounds there. That was cool. But we finished around 300something. Hey, still top 50%, right? Our models performed admirably: The random forest model scored in the 88.6th percentile on ESPN’s Tournament Challenge leaderboard, with the other two performing similarly to slightly lower. Overall, this was a super fun exercise (and definitely something I’ll be coming back to in future years) — and it certainly demonstrates the versatility and accuracy of machine learning, given appropriate data, in predicting basically anything for which enough data exists to train reasonable models. Obviously, the possibilities here are endless -- for now, I’ll be out here looking for niche stats and ranking systems to throw into my models, and maybe expanding to other sports if I ever have any free time.

# Visualization Dashboard (MM_vis)

*To run the code, download the folder and start a local http server by cd'ing into the folder and using this terminal command `python3 -m http.server 8888 &`. You can then navigate using a web browser to `localhost:8888/vis_final.html`, which should show the dashboard :)*

The yearly NCAA Division I Men’s Basketball Tournament, also known as March Madness, is (in)famously one of the most unpredictable and exciting events in sports. Though the single-elimination tournament is consistently filled with upsets and subject to the randomness inherent in having a season decided by only one game, millions of people every year attempt to predict the outcome of each of the 68 tournament games, filling out tournament brackets and entering “bracket pools” in which the bracket with the most correctly predicted games (usually scored by weighting later tournament rounds with more points per game) wins the pool. Most pools involve a monetary buy-in, with the winner collecting the proceeds – indeed, billions of dollars each year are exchanged on March Madness sports betting, within more informal bracket pools as well as at Las Vegas sportsbooks. Luckily, basketball is a relatively data-rich sport, and readily available statistics detailing games, teams, and players can lend insight into prediction of tournament game outcomes. 

Broadly, our domain situation for this visualization is understanding the predictive value of individual statistics at the game level and over time, with the goal of lending insight into prediction of future NCAA Tournament games. More concretely, we’re trying to answer the following questions through our faceted visualizations: 

•	Which game-level statistics have the most predictive value for determining the winner of a given game?

•	How have the predictive values of these statistics changed over time? 

•	Do correlations exist between seemingly unrelated statistics that could lend further insight into their predictive values?


To answer our questions, we work with a dataset aggregated by and available from the data science website Kaggle – the dataset lists the total box-score statistics for the winning and losing teams of every NCAA Tournament game for the past 16 Tournaments (the 2003-2018 tournaments -- 2019 tournament statistics had not been aggregated in time to include). For time relevance and reduction of visual clutter, we decided to filter the dataset to only include the past 10 years of data (tournaments from 2009-2018). Since we’re interested in the statistical differentials between winning and losing teams, rather than the absolute values of the statistics themselves, we needed to create derived attributes for each of the statistics by subtracting the losing team’s values from the winning team’s for each of the 12 statistical categories visualized. This single transformation created a dataset sufficient to answer questions 1) and 3) above, but to examine the time variation of each of these differentials, we also had to aggregate each game-level attribute to a season-level average, which we could then visualize as time series. To ensure that the data played nicely with D3, we also “melted” the data from matrix form (with each column representing an attribute, and each row a game) to key-value form (where each entry represents a specific statistic-differential pair for each game, where the statistic is the key and the differential in that game is the value). All data manipulation was done in R, using the dplyr and reshape2 packages. The R script used to transform the original dataset is attached to the submission.

At the encoding level, we faceted the dataset into views meant to specifically answer one of the three questions in the domain situation:

•	We use the heatmap idiom to visualize the overall predictive value of each statistic at the individual game level: in the heatmap, one categorical key and one ordered key are encoded via the spatial position channel within a matrix, and one quantitative value encoded via the color channel in each cell. Individual games make up the ordered key, which is encoded by the y-position of each cell, and the statistical differentials are the categorical key, represented by the x-position of each cell. The quantitative value – the magnitude of each statistical differential – is encoded via the color of each cell based on a standard diverging colormap from colorbrewer2.org. The hue channel encodes the sign of the differential, while the luminance channel encodes its absolute value. 

•	To determine whether the predictive value of each statistic has changed over time, we use a series of juxtaposed area charts to visualize the change in season-level average values of game-level differentials from 2009 to 2018. Superimposing 12 line charts on the same graph was also considered, but given that this visualization is meant for the global task of comparing the slopes and values of each differential across the entire time period – and keeping in mind that there are 12 separate time series to visualize – we follow the guidelines of Javed et al. (Munzner Chapter 12) and use the juxtaposed area charts idiom instead. In this idiom, the y-position of the area encodes the average value of each statistic, and the x-position encodes time. Color and text channels are used to encode the identity of the time series displayed by each chart.

•	To look at correlations between statistical differentials, we use a scatterplot matrix to plot the differential values of the selected statistic in each game against corresponding game-level values for all 12 statistics on faceted scatterplots (note that this visualization is interaction-dependent, as it requires the user to select a statistic before the plots are generated). The value of each differential is encoded by the x- and y- position of the points in each scatterplot, and the season of each game is encoded via the luminance of the point (using a single-hue colormap again from colorbrewer2.org).

We use a slider that allows the user to select and highlight individual statistics as the interaction idiom in this visualization, combined with an overview-detail navigation idiom between the faceted views. Highlighting is linked between the first and second views, and the luminance channel is used for the linked highlighting: decreasing the luminance of all items not matching the selected statistic creates significant visual popout for the statistic selected by the user for further investigation. The scatterplot matrix then generates a detailed view of the overview provided by the first two idioms after the user interaction, taking the user’s selection and generating scatterplots between the selection and every other statistic for the detailed investigation of correlations and trends between statistical categories at the game level.

