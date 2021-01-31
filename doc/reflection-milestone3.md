## Milestone 3 Reflection

In this milestone, the group developed a working version of an interactive dashboard in R and successfully deployed the app on Heroku. As a group, we made the decision that we are going to develop the final version of our dashboard in Python. In this case, we plan to implement the same dashboard as milestone 2 for this milestone.

### Features Implemented

Same as the last milestone, the initial proposal included a crossplot feature where the user could select any two variables to see if and how they related to each other. After prototyping this, however, the majority of plots were not useful to the end-user. Instead, the main plot of the dashboard now contains time-series data where the user can choose a y-axis variable from a dropdown selector. There are currently eight y-variables to choose from that have been selected due to their importance to the end-user.  

Two other interactive plotting features have been included in this release. Both allow the user to filter data in ways that could be useful to the hotel management business. This includes a button selection that allows the user to choose to see data from city, resort or both types of hotels. There is also a slider bar that allows the user to see data for particular seasons (e.g. spring).

In addition to the main plot, two histogram plots containing useful information about guests have been included. The data in these plots update with the two interactive plotting features (i.e. filters) mentioned in the previous paragraph.

We have also implement the TA feedback for milestone 1 in this milestone. we have grouped our data into weekly summaries that are averaged over the years in the dataset. The user can choose the range of weeks over which to view. Furthermore, we would like to give the option of choosing which year to view data from in the next milestones.

### Strengths and Limitations

The strengths of the dashboard at the time of this release are:

* Clean layout
* Easy to interact with
* Relevant information to the end-user

The limitation for this milestone was caused by the difference of implememting dashboard in R versus implementing in Python. For last milestone, we were able to extract all the functions and data wrangling part to a seperate file. But when implementing in R, we had to convert all the data wrangling script from Python to R in order to make it deploy on Heroku. The experience with development and deployment in R is less enjoyable than in Python. As a result, we are planning to continue working in Python next week.

### Future Improvements

Future improvements to the dashboard include adding more end-user flexibility and implementing several more widgets that will make the dash board more informative. This includes features such as:

* Replacing the main plot with two new plots, one showing detailed information over the selected year, the other showing information for the selected month
* Tiles to show the average values for data shown on the plots and number of data observations selected
* Potential for more plotting interactivity
* Layout and aesthetic improvements
