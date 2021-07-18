install.packages("lattice")
install.packages("car")


# Predictive Model for Los Angeles Dodgers Promotion and Attendance (R)

## The car package is titled "Companion to Applied Regression" and this line of code is used to load the package so that regression analysis can be performed later
library(car)  # special functions for linear regression
## The lattice package is titled "Trellis Graphics for R" and this line of code is used to load the package. This package will be particularly useful for creating visualizations and graphs for multivariate data.
library(lattice)  # graphics package

## This code chunk is reading in the dodgers data set into R and creating a data frame for it
## The data frame is saved and stored the values for the columns and rows of the dataset for convenient use
## Then, the dataframe structure is printed, or displayed in the console, so we can make sure everything looks right from a summary level of the data structure.
## The output we see in the console from printing the structure of the dataframe is all of the variables in the dataset and their characteristics
## We can also see how many observations we have in total
# read in data and create a data frame called dodgers
dodgers <- read.csv("dodgers.csv")
print(str(dodgers))  # check the structure of the data frame

## This chunk of code is being used to create a day of week variable to be used for plots and summaries during the analysis of the data. For example, instead of "Tuesday", the new variable will say "Tue"
## The code assigns a level to each day of the week, 1-7, starting with Monday.
## The with function is used to evaluate the expression without having to re-type the dataframe every time
## The ifelse function is an if statement that checks if the day of the week equals a value(string). If so, it returns a particular value, otherwise it moves to the next if statement.
## Ex: If a record's day of week is Monday, then return the value of 1, otherwise continue checking with the other if statements
## After the with function is run, we will have an additional variable called ordered_day_of_week in the dodgers dataset that takes on the values of 1-7 depending on the day_of_week variable
## The factor function is used to encode the vector we just created for the day of week variable as a factor
## The values for the ordered day of the week serve as the levels of the factor, and the function is changed based on the labels of the variable
## 1 is "Mon" ... 7 is "Sun"
## After the factor function is run, the values of the ordered_day_of_week variable will be changed from a numeric value to a shorted text string for the day of week
# define an ordered day-of-week variable 
# for plots and data summaries
dodgers$ordered_day_of_week <- with(data=dodgers,
                                    ifelse ((day_of_week == "Monday"),1,
                                            ifelse ((day_of_week == "Tuesday"),2,
                                                    ifelse ((day_of_week == "Wednesday"),3,
                                                            ifelse ((day_of_week == "Thursday"),4,
                                                                    ifelse ((day_of_week == "Friday"),5,
                                                                            ifelse ((day_of_week == "Saturday"),6,7)))))))
dodgers$ordered_day_of_week <- factor(dodgers$ordered_day_of_week, levels=1:7,
                                      labels=c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun"))

## This chunk of code is creating a graph (box plots) that shows the attendance numbers by the ordered day of week variable we created
## The with function is used to evaluate the expression without having to re-type the dataframe each time
## The plot command is the one that creates the graphic. The first variable referenced is the x variable, the second variable referenced is the y variable.
## For the y variable, we are dividing total attendance by 1000 to get attendance numbers in thousands of people
## xlab is labeling the x axis and ylab is labeling the y axis with the appropriate variables
## col picks a color for the graph
## las=1 makes sure the labels are horizontally oriented in comparison to the axis it is labeling
# exploratory data analysis with standard graphics: attendance by day of week
with(data=dodgers,plot(ordered_day_of_week, attend/1000, 
                       xlab = "Day of Week", ylab = "Attendance (thousands)", 
                       col = "violet", las = 1))

## Here we are trying to determine what day of the week the dodgers use bobblehead promotions
## with command used in same fashion as previously
## The table function produces a cross tabulation of the specified factors (variables), those being the ordered_day_of_week variable we created and the bobblehead yes/no variable 
## The output of the crosstabulation shows that bobblehead promotions are used most often on Tuesdays (6 instances), but also occurred on Thursday twice, Saturday twice, and Sunday once.
# when do the Dodgers use bobblehead promotions
with(dodgers, table(bobblehead,ordered_day_of_week)) # bobbleheads on Tuesday

## This chunk of code serves the same purpose as the one above relating to the ordered_day_of_week variable, but instead we are focused on the month
## The same process as used as before in which we take the values that currently exist for the "month" variable and assign it a numeric value, or level
## We then use the assigned levels and then change the variable values to long versions of months
## For example, APR is the original value for the month variable. It first gets assigned 4 if the value for month is "APR", and then it gets changed to "April" with the factor function based on the label specifications made
## The end result of this chunk of code is a new variable named ordered_month that has values of text string for a month that is not in all caps like the "month" variable
# define an ordered month variable 
# for plots and data summaries
dodgers$ordered_month <- with(data=dodgers,
                              ifelse ((month == "APR"),4,
                                      ifelse ((month == "MAY"),5,
                                              ifelse ((month == "JUN"),6,
                                                      ifelse ((month == "JUL"),7,
                                                              ifelse ((month == "AUG"),8,
                                                                      ifelse ((month == "SEP"),9,10)))))))
dodgers$ordered_month <- factor(dodgers$ordered_month, levels=4:10,
                                labels = c("April", "May", "June", "July", "Aug", "Sept", "Oct"))

## This chunk of code serves the same purpose as a previous chunk that created a graphic showing the attendance by month
## This chunk of code is creating a graph (box plots) that shows the attendance numbers by the ordered month variable we created
## The with function is used to evaluate the expression without having to re-type the dataframe each time
## The plot command is the one that creates the graphic. The first variable referenced is the x variable, the second variable referenced is the y variable.
## For the y variable, we are dividing total attendance by 1000 to get attendance numbers in thousands of people
## xlab is labeling the x axis and ylab is labeling the y axis with the appropriate variables
## col picks a color for the graph
## las=1 makes sure the labels are horizontally oriented in comparison to the axis it is labeling
# exploratory data analysis with standard graphics: attendance by day of week
# exploratory data analysis with standard R graphics: attendance by month 
with(data=dodgers,plot(ordered_month,attend/1000, xlab = "Month", 
                       ylab = "Attendance (thousands)", col = "light blue", las = 1))

# exploratory data analysis displaying many variables
# looking at attendance and conditioning on day/night
# the skies and whether or not fireworks are displayed
## The library command is used to load the lattice package into this r instance. The lattice package is helpful for creating graphics
library(lattice) # used for plotting 
# let us prepare a graphical summary of the dodgers data
## The following chunk of code is using the c function to create vectors that will be used for visualizations in later graphs
## The c function combines values into a vector or list, and we are creating vectors for labels, symbols, colors, and shape fill to be used in later visualizations
group.labels <- c("No Fireworks","Fireworks")
group.symbols <- c(21,24)
group.colors <- c("black","black") 
group.fill <- c("black","red")

## The purpose of the code chunk below is to create scatterplots showing the relationship between temperature and attendance, but splitting the panes by condition of the sky as well as time of game
## xyplot is used to create bivariate scatterplots. The two variables being used are attendance divided by 1000, and temperature
## attend and temp are the primary variables, but skies and day_night are being used as conditioning variables to divide the scatterplots by the specified conditions
## We are specifying the data to be used for the plot to be the dodgers dataframe we created
## The groups argument is distinguishing the scatterplot between fireworks and no fireworks games
## The pch argument is specifying the style of symbols that are to be plotted in the scatterplot. This references the group.symbols vector that we previously created
## The aspect argument controls the aspect ratio of the plot
## The cex argument defines the scale and 1.5 indicates that is is 50% larger than the default
## The col argument specifies the color of the tick marks and labels. This references the group.colors vector we created previously indicating black for both
## The fill argument defines the color of fill for the shapes plotted. This references the group.fill vector we created previously. The fireworks are red shapes and the no fireworks are black shapes
## The layout argument is specifying how the panels of the plot are to be arranged. This indicates that we will have 2 columns and 2 rows, so we will have 4 panes (2x2) for the plot
## The type argument specifies that p, or points, are being used in the plot and the g adds a reference grid to the panel
## The strip argument specifies for strips to be drawn using the custom format
## xlab labels the x axis for the plot, ylab labels the y axis for the plot
## The key argument defines how the key is displayed on the plot. We specified for the key to be at the top, and displays how the points are shown in the plot using the same specifications as before
xyplot(attend/1000 ~ temp | skies + day_night, 
       data = dodgers, groups = fireworks, pch = group.symbols, 
       aspect = 1, cex = 1.5, col = group.colors, fill = group.fill,
       layout = c(2, 2), type = c("p","g"),
       strip=strip.custom(strip.levels=TRUE,strip.names=FALSE, style=1),
       xlab = "Temperature (Degrees Fahrenheit)", 
       ylab = "Attendance (thousands)",
       key = list(space = "top", 
                  text = list(rev(group.labels),col = rev(group.colors)),
                  points = list(pch = rev(group.symbols), col = rev(group.colors),
                                fill = rev(group.fill))))
## The code below creates another visualization. The y axis shows all the opponents that the Dodgers played at home games, and the x axis measures attendance in thousands for those games
## Many of the same arguments are used from the previous plot, but this plot differs in that it has 1 panel that plots the attendance by opponent and the different colored points specify if the game was a day or night game
## bwplot is the function used for box-and-whisker plots, but the parameters we give for the plot will only show dots for each opponent rather than a conventional box-and-whisker plot 
## Similar to before, we are using the c function to combine values into a vector or list, and we are creating vectors for labels, symbols, and symbol size
## The panel arguments are used to assign details to the panel output
## The length function in combination of the levels function is used to assign an h value to the panel argument. We use these to specify the number of horizontal and vertical reference lines to be added to the plot.
## The panel.stripplot function creates a one dimensional scatterplot of x for each level of the y variable
## The end result of all of this code is a plot that shows a one dimensional scatterplot for attendance in thousands by opponent, grouped by whether the game was a day or night game.
# attendance by opponent and day/night game
group.labels <- c("Day","Night")
## Symbol 1 is an unfilled circle and symbol 20 is a filled circle
group.symbols <- c(1,20)
group.symbols.size <- c(2,2.75)
bwplot(opponent ~ attend/1000, data = dodgers, groups = day_night, 
       xlab = "Attendance (thousands)",
       panel = function(x, y, groups, subscripts, ...) 
       {panel.grid(h = (length(levels(dodgers$opponent)) - 1), v = -1)
               panel.stripplot(x, y, groups = groups, subscripts = subscripts, 
                               cex = group.symbols.size, pch = group.symbols, col = "darkblue")
       },
       key = list(space = "top", 
                  text = list(group.labels,col = "black"),
                  points = list(pch = group.symbols, cex = group.symbols.size, 
                                col = "darkblue")))

## We set the seed to ensure we can reproduce the results in the future. The sampling is still random, but this is a measure taken to ensure we can get the same training/test split that we perform
# employ training-and-test regimen for model validation
set.seed(1234) # set seed for repeatability of training-and-test split
## We are creating a training_test vector in order to split the dataset into training data and testing data
## Rep is used to replicate a particular value if it meets a certain condition. 2/3 of the number of rows in the dodgers data is being assigned a value of 1, and the difference in the total number of rows and the number of rows assigned with 1 are being assigned the value of 2
## The trunc function is used to round the expression to the nearest whole number in the direction of 0. Since our value is positive, if we get a decimal when computing 2/3 * number of rows, it will round down to the nearest whole number
training_test <- c(rep(1,length=trunc((2/3)*nrow(dodgers))),
                   rep(2,length=(nrow(dodgers) - trunc((2/3)*nrow(dodgers)))))
## We are adding a column to the original dodgers dataset to indicate if a particular row is assigned to the training set or the testing set
## The sample function takes a random sample of the training_test vector we created and assigns the value to the record
## The factor function is then used to encode the vector as a factor and assign the labels TRAIN and TEST based on if the record has a value or 1 or 2
dodgers$training_test <- sample(training_test) # random permutation 
dodgers$training_test <- factor(dodgers$training_test, 
                                levels=c(1,2), labels=c("TRAIN","TEST"))
## Now we are creating 2 new dataframes by splitting the original dataset into 2 new datasets
## The first is the training data which takes a subset of the original dataset for all records that have "TRAIN" as the value in the training_test field we created
## The second is the test data which takes a subset of the original dataset for all records that have "TEST" as the value in the training_test field we created
## We are then printing the structures of the new datasets to make sure the summary level information looks correct, such as the number of observations in each
dodgers.train <- subset(dodgers, training_test == "TRAIN")
print(str(dodgers.train)) # check training data frame
dodgers.test <- subset(dodgers, training_test == "TEST")
print(str(dodgers.test)) # check test data frame

## Here we are creating a model to be fit using the training data later
## attend is the response variable (attendance of a game)
## ordered_month, ordered_day_of_week, and bobblehead are the predictor variables
## This code is specifying the model structure, but we will fit the model on data later 
# specify a simple model with bobblehead entered last
my.model <- {attend ~ ordered_month + ordered_day_of_week + bobblehead}

## This chunk of code is used to fit the model we previously specified to the training dataset
## The lm function is used to fit a linear model
## my.model is being used as the formula for the linear model (as specified previously) and we are indicating that the model is only being fit using the training dataset we created
# fit the model to the training set
train.model.fit <- lm(my.model, data = dodgers.train)
## The print function is used to display a summary of the model. We are given the coefficients of the variables as well as their errors, test statisitcs, and p values
## The summary will also show the r squared, f statistic, and p value of the entire model
# summary of model fit to the training set
print(summary(train.model.fit))
## Using the model we created, we are creating a new variable in the training dataset that predicts the attendance for a game using the model output we generated
## Each record in the dodgers.train dataset will have a predicted attendance that uses the model output we generated
## The predict function is the function used to generate the predictions, and we specify that the object used for prediction is the train.model.fit
# training set predictions from the model fit to the training set
dodgers.train$predict_attend <- predict(train.model.fit) 

## Now we want to see how the fitted model performs on new data. We will use the testing data to evaluate model performance
## The code below is adding a variable to the dodgers.test dataset
## The variable will be predicted attendance and uses the model we created using the training data to predict an attendance figure for each record in the testing data
## The newdata argument is telling the line of code where to look for the explanatory variables for the predictions. This is saying to use the model on the test data
# test set predictions from the model fit to the training set
dodgers.test$predict_attend <- predict(train.model.fit, 
                                       newdata = dodgers.test)

## Here we are computing a user defined function on the testing data to see the proportion of variance in the response variable that is accounted for when evaluating against the out of sample predictions we made
## We are using the cat function to concatenate the title for our expression with the output of the expression
## We are naming the output "Proportion of Test Set Variance Accounted for:"
## We are rounding the output of the expression, and we are specifying that the data to be used when evaluating the expression is the dodgers.test dataset
## The function to be evaluated is the correlation between attendance and predicted attendance squared
## digits specifies the number of significant digits to be used, and the sep argument indicates that we are not adding any string after each element 
# compute the proportion of response variance
# accounted for when predicting out-of-sample
cat("\n","Proportion of Test Set Variance Accounted for: ",
    round((with(dodgers.test,cor(attend,predict_attend)^2)),
          digits=3),"\n",sep="")

## The rbind command is used to join the training and testing dataframes together into a new dataframe
## We are creating a new dataframe (dataset) called dodgers.plotting.frame that is the combination of the training data and the testing data
## We can use the rbind command because both datasets have the same variables, and the command joins the two dataframes vertically. This means the training data is added first, and then the testing data is appended after
# merge the training and test sets for plotting
dodgers.plotting.frame <- rbind(dodgers.train,dodgers.test)

## The purpose the the chunk of code below is to create a visualization that shows the error in predictions.
## The goal is to plot the predicted values for attendance versus the actual attendance value for a particular observation (game).
## Similar to the visualizations created previously, we are creating vectors to be used in the plot command
## group.labels will be used to assign labels as either No Bobbleheads or Bobbleheads
## group.symbols indicates that we will be using symbols 21 and 24 in the plot - 21 is a filled circle and 24 is a filled triangle
## group.colors will be used to assign colors to the tick marks and labels
## group.fill will be used to fill the shapes of the plot, either black or red
# generate predictive modeling visual for management
group.labels <- c("No Bobbleheads","Bobbleheads")
group.symbols <- c(21,24)
group.colors <- c("black","black") 
group.fill <- c("black","red")

## xyplot is used to create bivariate scatterplots. The two variables being used are predicted attendance divided by 1000 and attendance divided by 1000
## Conditioning variable is training_test, and this is used to divide the plot into observations based on whether they come from the training or testing data
## We are specifying the data for the plot to be the dodgers.plotting.frame dataframe we created. This is a combination of the training and testing dataframes
## The groups argument is distinguishing the scatterplot between bobbleheads and no bobbleheads games. What this does is create different points for if the game is a bobblehead game or if the game is not a bobblehead game (circle vs triangle)
## cex sets the scale for the plot axis labels at 2 
## The pch argument is specifying the style of symbols that are to be plotted in the plot. This references the group.symbols vector that we previously created. group.symbols indicates that we will be using symbols 21 and 24 in the plot - 21 is a filled circle and 24 is a filled triangle
## The col argument specifies the color of the tick marks and labels. This references the group.colors vector we created previously indicating black for both
## The fill argument defines the color of fill for the shapes plotted. This references the group.fill vector we created previously. The bobblehead games are red shapes and the no bobblehead games are black shapes
## The layout argument is specifying how the panels of the plot are to be arranged. This indicates that we will have 2 columns and 1 row, so we will have 2 panes for the plot
## The xlim and ylim arguments are used to define the range of the axes used for the plots. We are limiting both the x and y axes to attendance values between 20 and 65 (attendance/1000) 
## The aspect argument controls the aspect ratio of the plot
## The type argument specifies that p, or points, are being used in the plot and the g adds a reference grid to the panel
## The panel argument specifies the function to be used in plotting and also indicates that 2 line segments will be added to the visualization between the points (25,25) and (60,60)
## panel.xyplot creates the scatterplot between the two variables we defined for the plot
## xlab labels the x axis for the plot, ylab labels the y axis for the plot
## The key argument defines how the key is displayed on the plot. We specified for the key to be at the top, and displays how the points are shown in the plot using the same specifications as before
xyplot(predict_attend/1000 ~ attend/1000 | training_test, 
       data = dodgers.plotting.frame, groups = bobblehead, cex = 2,
       pch = group.symbols, col = group.colors, fill = group.fill, 
       layout = c(2, 1), xlim = c(20,65), ylim = c(20,65), 
       aspect=1, type = c("p","g"),
       panel=function(x,y, ...)
       {panel.xyplot(x,y,...)
               panel.segments(25,25,60,60,col="black",cex=2)
       },
       strip=function(...) strip.default(..., style=1),
       xlab = "Actual Attendance (thousands)", 
       ylab = "Predicted Attendance (thousands)",
       key = list(space = "top", 
                  text = list(rev(group.labels),col = rev(group.colors)),
                  points = list(pch = rev(group.symbols), 
                                col = rev(group.colors),
                                fill = rev(group.fill))))

## We are creating a new model fit on the entire dodgers data in order to see how bobblehead promotions increase attendance
# use the full data set to obtain an estimate of the increase in
# attendance due to bobbleheads, controlling for other factors 
## We are creating a linear model fit using the lm function
## The formula used for the linear model follow the same model inputs as before so we are using my.model as the formula
## Since we want to see the estimate on the entire data, we are specifying the data that is used to fit the model as the dodgers dataframe
my.model.fit <- lm(my.model, data = dodgers)  # use all available data
## The print command is used to display an output of something, and the summary function is used to produce the results of a fitted model
## The summary output of the model we fit on the entire dodgers dataset is then displayed in the console including the fit, residuals, coefficients, and model summary such as r squared, p value, etc 
print(summary(my.model.fit))

# tests statistical significance of the bobblehead promotion
# type I anova computes sums of squares for sequential tests
## The print command is used to display the output of an object in the console, and we want to show the output of the anova command on the model that we fitted to the entire dodgers dataframe
## The anova command computes the analysis of variance on a fitted model, and the object we enter for analysis is our fitted model on the entire dataset
## The end product of this line is the analysis of variance table
print(anova(my.model.fit))  


## The cat function is used to concatenate and print and object
## We are concatenating the title of our expression with an expression that we enter as an argument in the command
## The expression that is being concatenated is the rounded value to the nearest whole number of the bobbleheadYES coefficient.
## The length function is used to determine the length of the model. We use this in order to pick the correct coefficient to pull for interpretation. 
## The length of the model is 14, so the code is looking for the 14th coefficient out of all of them to get the bobbleheadYES coefficient since that is the one of interest and the 14th one that gets outputted in the results
cat("\n","Estimated Effect of Bobblehead Promotion on Attendance: ",
    round(my.model.fit$coefficients[length(my.model.fit$coefficients)],
          digits = 0),"\n",sep="")

# standard graphics provide diagnostic plots
## The plot command is used to give the common plots associated with a linear model.
## Using the plot command on out fitted linear model returns 4 plots that we can cycle through to evaluate the model
## We get the residuals vs fitted values plot, the normal Q-Q plot, the scale-location plot, and the residuals vs leverage plot
plot(my.model.fit)

# additional model diagnostics drawn from the car package
## We use the library function to load the car package
library(car)
## The residualplots function plots the residuals versus each term in a mean function and versus fitted values
## Since we have a linear model, it also is Turkey's test for nonadditivity when plotting against fitted values
## The result of the residualplots function is three plots appearing that show the residuals for each term, ordered_month and ordered_day_of_week, and also the results of the Turkey test in the console
residualPlots(my.model.fit)
## The marginalmodelplots function draws marginal model plots versus each of the terms in the model that we fit to the entire dataset and versus the fitted values
marginalModelPlots(my.model.fit)
## The print function is displaying the output of another function in the console command
## We want to display the results of the Bonferroni Outlier Test using the outlierTest function of the car package
## The Bonferroni p-values for testing each observation in turn to be a mean-shift outlier is displayed in the console
print(outlierTest(my.model.fit))

## These are just comments made for suggestions to further look into the modeling
# Suggestions for the student:
# Examine regression diagnostics for the fitted model.
# Examine other linear predictors and other explanatory variables.
# See if you can improve upon the model with variable transformations.