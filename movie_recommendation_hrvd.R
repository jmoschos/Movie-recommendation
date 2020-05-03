################################
# Movie Recommendation system
# May 2020
################################



# Library installation

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")

## Library loading
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(randomForest)
library(rpart)
library(xgboost)
library(ggplot2)
library(stringr)
library(ggpubr)
library(tinytex)
library(kableExtra)



#Creating the dataset

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)



## Creating train and test sets for the models from edx dataset. VALIDATION IS NOT TO BE USED.
## 90-10split for data
set.seed(1)    ##for reproducability
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

#remove temp columns
rm(test_index, temp, removed)











# Exploratory data analysis
###################################################################################################################################

# In this section, we aim to uncoverer the main properties of the data and see which of the fields may be useful for predicting the rating for "new" movies and users. 

# Given the dataset we got, we have 5 main variables to explore: Users, Movies, Time(stamp), Genre, and Title. Based on the available resources online, we expect the users and the movies to have a strong effect on the rating, but we plan to explore all the variables and see if we can further improve our models. However the title doesnt seem to be very beneficial in predicting the rating of the movie, and thus it will not be examined.

#Furthermore, we will also explore the variable we aim to predict (rating), and examine its properties. 

###################################################################################################################################



## 1) Timestamp analysis: Checking if timestamp affects the rating.

##Ploting the average rating by day,week,month and year to see if time plays any role. Using 01/01/1970 as origin


## Plotting the avg rating per day
t_day<-train_set%>%
  mutate(day=round_date(as.POSIXct(timestamp,origin="1970-01-01",tz="GMT"),unit="day"))%>%
  group_by(day)%>%
  summarize(rating=mean(rating))%>%
  ggplot(aes(day,rating))+
  geom_line()+
  geom_smooth(method="loess")+
  ylim(0,5)

## Plotting the avg rating per week
t_week<-train_set%>%
  mutate(week=round_date(as.POSIXct(timestamp,origin="1970-01-01",tz="GMT"),unit="week"))%>%
  group_by(week)%>%
  summarize(rating=mean(rating))%>%
  ggplot(aes(week,rating))+
  geom_line()+
  geom_smooth(method="loess")+
  ylim(0,5)

## Plotting the avg rating per month
t_month<-train_set%>%
  mutate(month=round_date(as.POSIXct(timestamp,origin="1970-01-01",tz="GMT"),unit="month"))%>%
  group_by(month)%>%
  summarize(rating=mean(rating))%>%
  ggplot(aes(month,rating))+
  geom_line()+
  geom_smooth(method="loess")+
  ylim(0,5)

## Plotting the avg ratin per year
t_year<-train_set%>%
  mutate(year=round_date(as.POSIXct(timestamp,origin="1970-01-01",tz="GMT"),unit="year"))%>%
  group_by(year)%>%
  summarize(rating=mean(rating))%>%
  ggplot(aes(year,rating))+
  geom_line()+
  geom_smooth(method="loess")+
  ylim(0,5)

ggarrange(t_day,t_week,t_month,t_year,nrow = 2,ncol=2)
rm(t_day,t_week,t_month,t_year)


## plotting the avg rating per year + Overall average
train_set%>%
  mutate(year=round_date(as.POSIXct(timestamp,origin="1970-01-01",tz="GMT"),unit="year"))%>%
  group_by(year)%>%
  summarize(rating=mean(rating))%>%
  ggplot(aes(year,rating))+
  geom_line(col="red")+
  geom_smooth(method="loess")+
  geom_hline(yintercept = mean(train_set$rating), col = "black", size = 1.5)+
  ylim(3,4)


#################################################################################################################################

# We can see that there is a small effect on how time(stamp) affects the average rating given to a movie.


#However, when the grouping is done at a day/week or even month level the time-series object is far too volatile to be used effectively in our model. For that reason, we want to smoothen the effect, while still approximating the general trend of the rating change over time. Therefore, we will use the year transformation and fit a loess curve to capture the smoothened yearly average rating. See 

#--> 2nd model [with time bias] in the Data analysis section

#################################################################################################################################

## 2) User analysis

## Plotting how many times do users rate movies (1-off or multiple times)

train_set%>%
  group_by(userId)%>%
  summarize(count=n())%>%
  ggplot()+
  geom_histogram(aes(count))+
  labs(x = "Number of ratings",
       y = "Number of users")

## Most times a user has rated a movie

train_set%>%
  group_by(userId)%>%
  summarize(count=n())%>%
  arrange(desc(count))%>%
  top_n(5)


## Least times a user has rated a movie

train_set%>%
  group_by(userId)%>%
  summarize(count=n())%>%
  arrange(count)%>%
  head(5)


## Pareto-chart to see the cumulative number of ratings for the users

z<-train_set%>%
  group_by(userId)%>%
  summarize(n=n())%>%
  arrange(desc(n))%>%
  mutate(percentage=n/sum(n))%>%
  mutate(cumSum=cumsum(percentage))     ##cumulative percentage

z$id<-1:length(z$userId)            ##index for users 

z%>%
  ggplot(aes(id,100*cumSum))+
  geom_line()+
  labs(title = "Pareto chart between users and cumulative percentage of movie ratings",
       x = "Cumulative number of users",
       y = "Cumulative percentage of retings")


rm(z)

#################################################################################################################################

# We see that the majority of users have fewer than 2000 reviews but some extreme outliers with more than 5000 reviews exist. At the same time every user has at least 10 movie ratings, which means that if the rating is dependent on the user (which is expected), then we do not need to exclude any users from the analysis. 

# Next we examine how the rating is changing for different users

#################################################################################################################################


## Plotting average rating distribution by user, and the average rating for all movies (red line)
train_set%>%
  group_by(userId)%>%
  summarize(rating=mean(rating))%>%
  ggplot(aes(rating))+
  geom_histogram(col="yellow")+
  geom_vline(xintercept = mean(train_set$rating),col="red",size=2)+
  labs(title = "Histogram of user rating behaviour",
       x = "Rating",
       y= "Number of users")


## Lets examine a few individual users and see how their data is distributed

## We select a heavy user with >2000 ratings, a medium user with >100 but less than 500 ratings, and a light user with <20 ratings.

#id of heavy user
heavy_user<-train_set%>%
  group_by(userId)%>%
  summarize(n=n())%>%
  filter(n>2000)%>%
  filter(row_number()==1)%>%        ## Select first line. We do not aim for anyone specifically -->Selecting the first
  .$userId


## Saving the plot to arrange it later
t1<-train_set%>%
  filter(userId==heavy_user)%>%
  ggplot(aes(rating))+
  geom_histogram(col="black")+
  labs(title = "Heavy user rating distribution",
       x = "Rating",
       y = "Number of times")

#Id of medium user
medium_user<-train_set%>%
  group_by(userId)%>%
  summarize(n=n())%>%
  filter(n>100&n<500)%>%
  filter(row_number()==1)%>%      ## Select first line. We do not aim for anyone specifically -->Selecting the first
  .$userId

#saving the plot to arrange it later
t2<-train_set%>%
  filter(userId==medium_user)%>%
  ggplot(aes(rating))+
  geom_histogram(col="black")+
  labs(title = "Medium user rating distribution",
       x = "Rating",
       y = "Number of times")

# Id of light user
light_user<-train_set%>%
  group_by(userId)%>%
  summarize(n=n())%>%
  filter(n<20)%>%
  filter(row_number()==1)%>%    ## Select first line. We do not aim for anyone specifically -->Selecting the first
  .$userId

#Saving the plot to arrange it later
t3<-train_set%>%
  filter(userId==light_user)%>%
  ggplot(aes(rating))+
  geom_histogram(col="black")+
  labs(title = "Light user rating distribution",
       x = "Rating",
       y = "Number of times")


rm(heavy_user,medium_user,light_user)   ## Removing the ids; no longer required.

ggarrange(t1,t2,t3,ncol=1,nrow = 3)


#################################################################################################################################

#Based on the plots, we observe that these users behave differently (this is based on the seed; we have set the seed to 1 for the dataset creation, for reproducability); the light user appears to be neutral (or slightly positive), with his ratings being mainly 3, while also giving some perfect scores and some low scores. The medium user seems to have a more diverse rating behaviour (although on average he seems more neutral than positive or negative). Finally, the heavy user is positive, with very few ratings below 3.

#Therefore, we conclude that there is definately a relationship between the user and the ratings, and thus it will be tested in our models.

#################################################################################################################################


## 3) Movie analysis

## Plotting Average rating per movie.

train_set %>%
  group_by(movieId)%>%
  summarize(rating=mean(rating))%>%
  ggplot(aes(rating))+
  geom_histogram(col="blue")+
  labs( title = "Rating distribution across movies",
        x = "Rating",
        y = "Number of times")

## Plotting Average number of ratings per movie.

train_set%>%
  group_by(movieId)%>%
  summarize(count=n())%>%
  ggplot(aes(count))+
  geom_histogram(col="blue")+
  labs( title = "Number of ratings distribution across movies",
        x = "Number of ratings",
        y = "Number of movies")

## Average number of ratings per movie outliers (low count)
train_set%>%
  group_by(movieId)%>%
  summarize(count=n())%>%
  arrange(count)


## Comparing the average rating per movie with the number of ratings the movie received.
train_set%>%
  group_by(movieId)%>%
  summarize(count=n(),avg_rating=mean(rating))%>%
  ggplot(aes(count,avg_rating))+
  geom_point()+
  geom_hline(yintercept = mean(train_set$rating),col="red")+
  labs(title = "Average rating based on number of ratings for different movies",
       x = "Number of ratings",
       y = "Average rating")


###################################################################################################################################

# We can see that in general different movies receive different ratings: Some movies are more popular and some are not. When we plot the average rating vs the number of ratings the movie received, we observe two very interesting things:

# Firstly, movies with more ratings (more people have seen them and rated them) appear to have an overall higher rating than the rest. Secondly, there are some movies with very few ratings that have either very good or very bad average ratings. This is intuitive as when the number of ratings increases, the average rating is more representative of the movie's appeal to the public (larger sample). Additionally, the spread of ratings is much larger in movies with a low number of ratings than it is in those with a higher number.

# Next we will explore seperatelly the characteristics of ratings in movies in the high, medium and low count categories, respectively.

###################################################################################################################################


## For all the following graphs, first we group by movie and calculate the mean rating and number of ratings each movie recieved. We then filter based on the number of ratings (count) for each category and make a histogram of the ratings per category. In each histogram, we also plot the average rating of the category. We arrange all these 4 plots vertically to observe the differences in the distribution, and the mean value. We then remove the sub-plots we created:



## Avg rating in low count movies (low<10)

p_low<-train_set%>%
  group_by(movieId)%>%
  summarize(count=n(),rating=mean(rating))%>%
  filter(count<10)%>%
  ggplot(aes(rating))+
  geom_histogram()+
  xlim(c(0.5, 5))+
  geom_vline(aes(xintercept = mean(rating)),col='red',size=1.5)+
  geom_text(aes(x=mean(rating)+0.35, label=paste0("Mean = ",  round(mean(rating),2)), y=30), colour="red", angle=0, size=2)+
  labs(y = "Low")+
  theme(axis.title.x=element_blank())

## Avg rating in medium count movies (10<medium<100)
p_med<-train_set%>%
  group_by(movieId)%>%
  summarize(count=n(),rating=mean(rating))%>%
  filter(count<100&count>10)%>%
  ggplot(aes(rating))+
  geom_histogram()+
  xlim(c(0.5, 5))+
  geom_vline(aes(xintercept = mean(rating)),col='yellow',size=1.5)+
  geom_text(aes(x=mean(rating)+0.35, label=paste0("Mean = ",  round(mean(rating),2)), y=200), colour="yellow", angle=0, size=2)+
  labs(y = "Medium")+
  theme(axis.title.x=element_blank())

## Avg rating in high count movies (high>100)

p_high<-train_set%>%
  group_by(movieId)%>%
  summarize(count=n(),rating=mean(rating))%>%
  filter(count>100)%>%
  ggplot(aes(rating))+
  geom_histogram()  +
  xlim(c(0.5, 5))+
  geom_vline(aes(xintercept = mean(rating)),col='green',size=1.5)+
  geom_text(aes(x=mean(rating)+0.35, label=paste0("Mean = ",  round(mean(rating),2)), y=300), colour="green", angle=0, size=2)+
  labs(y = "High")+
  theme(axis.title.x=element_blank())


## Avg rating in extr. high count movies (extr. high>5000)
p_vhigh<-train_set%>%
  group_by(movieId)%>%
  summarize(count=n(),rating=mean(rating))%>%
  filter(count>5000)%>%
  ggplot(aes(rating))+
  geom_histogram()+ 
  xlim(c(0.5, 5))+
  geom_vline(aes(xintercept = mean(rating)),col='white',size=1.5)+
  geom_text(aes(x=mean(rating)+0.3, label=paste0("Mean = ",  round(mean(rating),2)), y=20), colour="white", angle=0, size=2)+
  labs(y = "Very high")+
  theme(axis.title.x=element_blank())


fig<-ggarrange(p_low,p_med,p_high,p_vhigh,nrow = 4,ncol=1,common.legend = TRUE)
rm(p_low,p_med,p_high,p_vhigh)

annotate_figure(fig, 
                left = text_grob("Number of ratings",rot=90),
                bottom= text_grob("Average Rating"))


###################################################################################################################################

# We can clearly see that when the number of ratings increases the mean value also increases. Simultaneously, the higher the number of ratings, the more close the average ratings are. In the graph, we can see that the distribution is more wide in the upper charts compared to the bottom ones. 

# Based on this analysis, the movie, as expected, appears to have a strong effect in the rating prediction, and thus it will be included in the model (data analysis sections) to test if that is the case. 

###################################################################################################################################


## 4) Genre analysis

## Genres can be either broken down to their individual components or each combination can be treated as a unique category.

## Number of Unique genre combinations
length(unique(train_set$genres))

## How many observations do we have per genre combination? This assumes that we use each genre name as a new category, despite the fact that it can be broken to a combination of single genres.We will explore the latter in the second part of the analysis

train_set%>%
  group_by(genres)%>%
  summarize(n=n())%>%
  arrange(desc(n))        ## Genres with more observations on top.

train_set%>%
  group_by(genres)%>%
  summarize(n=n())%>%
  arrange(n)    ## Genres with least observations on top.


## Filtering OUT genres with less than 10 observations and counting number of remaining genres
train_set%>%
  group_by(genres)%>%
  summarize(rating_g=mean(rating),number_of_observations=n())%>%
  filter(number_of_observations>=10)%>% 
  count(genres)%>%
  summarize(sum(n))%>%
  as.numeric()


## Does genre play a role on average rating?

train_set%>%
  group_by(genres)%>%
  summarize(rating_g=mean(rating),number_of_observations=n())%>%
  filter(number_of_observations>=10)%>%       #We select only genres with more than 10 observations to have a meaningful sample
  ggplot(aes(genres,rating_g))+
  geom_point()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())         # Removing axis elements in the graph, as there are 793 genre combinations.

#################################################################################################################################

# Based on the graph, genres seem to have an impact on the rating given to them. The ones included have a range between 2 and 4.5, for the average rating within the genre, and a very wide distribution amongst different genres.

#However, we also want to investigate the actual unique genres and how they affect the rating to a movie. We will attempt to extract the different (individual) components and investigate if we observe similar results in that case too.

#################################################################################################################################


## Creating a new dataframe with only the rating and the genre for faster calculations.
genre<-train_set[,c("rating","genres")]

##Splitting the genres by the character "|" and saving them in a list, since some genres contain more than 1 genre element. 
list<-str_split(genre$genres,"\\|")

## Saving the content of the list into a char vector for easier manipulation.
vec<-unlist(list)
rm(list)

## Unique genres and number of unique genres
x_u<-unique(vec)
x_u
length(x_u)

rm(vec)

## Creating a variable for each genre seperately, and setting it its value to 0. 
## We start by creating an lapply function to set the values to 0, and utilizing the setnames we create one variable for each x_u (unique genre). Finally, we bind the columns with the existing genre dataframe. The final result is a dataframe with the existing 2 columns (rating, genre) and one dummy column per genre (+20) for a total of 22 columns. The column cells in each genre are at the moment set to 0, but we will manipulate them in a later step.
genre<-cbind(genre, setNames( lapply(x_u, function(x) x=0), x_u) )


## We are going to now indicate if a certain genre is present as a binary variable (x=1 if genres contain X genre, 0 otherwise) for all 20 genres (and columns). We are therefore creating a loop, and using the grep (get regular expression) to find pattern x_u[i] in the column "genres". If the result is TRUE we set the respective column to 1, otherwise we leave it as 0. Its important to underline that while i is looping between 1 and length(x_u) (which is 20), the column we are changing is the i+2 (since we already have the first 2 columns, i.e. rating and the genres).

for (i in 1:length(x_u)){
  genre[grep(x_u[i],genre$genres),i+2]<-1
}

## We now have a dummy variable matrix where each movie gets a 1 if it belongs to a certain unique genre, or 0 otherwise. 

## We now want to calculate the scores per unique genre and see how they behave and if they are similar to what we experienced with the grouped data before.

## Variable i for loop to calculate scores for all 20 variables
i<-1:length(x_u)

## Looping over i, and we calculate score as Rating*Dummy variable. IF dummy variable=1, then the score for that genre is set to the movie's rating, and 0 otherwise.
k<-sapply(i,function(i){
  score<-genre[,i+2]*genre[,"rating"]
})

## Since we have 0s in some cells, if we simply use a mean function, it will not correctly calculate the average rating per genre. Instead we only use the cells that do not have a 0 value (and we loop over the i index.)
score_per_genre<-as.data.frame(sapply(i,function(i){mean(k[k[,i]!=0,i])}))

## Changing the column name for the average rating
colnames(score_per_genre)<-"Averate rating"

## Creating a 2nd column for the genre category
score_per_genre$Category<-x_u

## Plotting the average score per genre and the overall average
score_per_genre%>%
  ggplot(aes(Category,`Averate rating`))+
  geom_point()+
  ylim(c(0.5, 5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(yintercept = mean(train_set$rating),col="red")

## Removing the objects created.
rm(score_per_genre,k,i,x_u)

#################################################################################################################################

# We observe that different genres have different average ratings but the range of those ratings is not as extreme as it was when the combined genre was treated as a unique category, and thus it might not be as effective as the unique genre combinations. Therefore, we will test the genre effect on the movie rating prediction, but we will treat each combination of genres as a unique category (method 1).

#################################################################################################################################


## 5) Rating analysis


#################################################################################################################################

# The final variable we are going to examine is the variable we aim to predict: the rating. 

#################################################################################################################################

## Plotting a histogram of the ratings.

train_set%>%
  ggplot()+
  geom_histogram(aes(rating))


#################################################################################################################################

# Based on this histogram alone, we can observe two interesting features of the rating: Firstly, rating given by users is in multiples of 0.5 and not continuous. Secondly, the minimum score is 0.5 (and not 0 as expected), while the maximum is 5. This means that a very easy way to improve predictions in the data analysis chapter is that all observations, which are less than 0.5, to be set to 0.5 and all predictions above 5 to be set to 5.

# y_hat<0.5 --> y_hat=0.5
# y_hat>5   --> y_hat=5

#################################################################################################################################



















# Data analysis - Modelling 

## Removing the title from the dataset, as it will not be used.
train_set<-train_set%>%
  select(-title)




## Mean value for rating
mu <- mean(train_set$rating) 




## Model 1
## Naive: Assume all movies get an average rating
rmse_naive<-RMSE(test_set$rating,mu)    
rmse_naive                              ##1.059





## Model 2
## Adding the time effect

## Adding a year variable and calculating the UNSMOOTHENED time effect bt as bt= Y-mu
time_bias<-train_set%>%
  mutate(year=round_date(as.POSIXct(timestamp,origin="1970-01-01",tz="GMT"),unit="year"))%>%
  group_by(year)%>%
  summarize(rating=mean(rating-mu))       ##Time based bias

##fitting a loess (smoothened) line for the time object and using it to estimate the time bias 

l<-loess(time_bias$rating~as.numeric(time_bias$year))         
time_bias$rating<-l$fitted
names(time_bias)<-c("year","Timed_avg")
rm(l)

## adding the year variable to the train set
train_set<-train_set%>%
  mutate(year=round_date(as.POSIXct(timestamp,origin="1970-01-01",tz="GMT"),unit="year"))

##Joining data frames by the year variable to get the loess smoothened time-bias
train_set<-train_set%>%
  left_join(time_bias,by="year")

##Joining also for the test set to estimate accuracy
test_set<-test_set%>%
  mutate(year=round_date(as.POSIXct(timestamp,origin="1970-01-01",tz="GMT"),unit="year"))%>%
  left_join(time_bias,by="year")

##In case a new year is provided in the test set, where we do not have observations in the train set, we set the bias to 0, as we cannot measure it.

test_set$Timed_avg[is.na(test_set$Timed_avg)]<-0

##Prediction  y^=mu+bt
y_hat<-test_set$Timed_avg+mu

## Accuracy model 2
rmse_time_naive<-RMSE(test_set$rating,y_hat)
rmse_time_naive                                      ## 1.058529


## We observe a very small improvement in our model:  improv 0.5%





## Model 3 
## Adding the movie effect

## Calculating the movie bias bi as bi=Y-mu-bt          grouped per movie
movie_bias<-train_set%>%
  group_by(movieId)%>%
  summarize(bi=mean(rating-mu-Timed_avg))


##Joining the data frame with the train_set
train_set<-train_set%>%
  left_join(movie_bias,by = "movieId")


## Joining the data_frame with the test_set to make the prediction

test_set<-test_set%>%
  left_join(movie_bias,by="movieId")


test_set$bi[is.na(test_set$bi)]<-0          #we set the movie bias to 0 if its a new movie, as we cannot estimate the bias then.

y_hat<-mu+test_set$Timed_avg+test_set$bi
rmse_time_movie<-RMSE(y_hat,test_set$rating)      ## 0.9428478
rmse_time_movie

## We further improved our accuracy by 10.9%






## Model 4 
## Including the user bias in the model: We calculate the user bias as bu=Y-mu-bi-bt grouped per user

user_bias <- train_set %>% 
  group_by(userId) %>%
  summarize(bu = mean(rating - mu - bi-Timed_avg))


##Joining the data frame with the train_set
train_set<-train_set%>%
  left_join(user_bias,by="userId")


## We combine the dataframe with our test_set to make our prediction
test_set<-test_set%>%
  left_join(user_bias,by="userId")


test_set$bu[is.na(test_set$bu)]<-0                     ## We set the bu for new users, as the bias of that user cannot be measured

y_hat<-mu+test_set$bu+test_set$bi+test_set$Timed_avg
rmse_time_movie_user<-RMSE(y_hat,test_set$rating)             ## 0.8650181
rmse_time_movie_user


## We further improved our accuracy by 8.2%






## Model 5 
## Adding a genre effect: bg=Y-mu-bt-bi-bu


genre_bias<-train_set%>%
  group_by(userId,genres)%>%
  filter(n()>10)%>%    # To have a meaningful gender bias, we only select user that have seen that type of genre more than 10 times.
  summarize(bg=mean(rating-mu-bi-bu))

train_set<-train_set%>%
  left_join(genre_bias,by=c("userId"="userId", "genres"="genres"))


test_set<-test_set%>%
  left_join(genre_bias,by=c("userId"="userId", "genres"="genres"))

test_set$bg[is.na(test_set$bg)]<-0

y_hat<-mu+test_set$bi+test_set$bu+test_set$bg+test_set$Timed_avg
rmse_genre_time_user_movie<-RMSE(y_hat,test_set$rating)
rmse_genre_time_user_movie                            ##0.8631007

## We improved our accuracy by 0.2%


## Table with all results

Results_train<-data.frame(method=c("Naive","Time bias","Time bias+Movie bias","Time bias + Movie bias + User bias","Time bias + Movie bias + User bias + Genre bias"), RMSE=c(rmse_naive,rmse_time_naive,rmse_time_movie,rmse_time_movie_user,rmse_genre_time_user_movie))


Results_train

## Best method with min RMSE
Results_train$method[which.min(Results_train$RMSE)]


#################################################################################################################################

#  From the above table, we see that the best performing model is using all parameters: Time, movie, user and genre effects. We therefore select this as the model we will use to estimate the ratings for the movies in the validation set. 

#################################################################################################################################







#Validation check and final RMSE calculation

##Joining validation set with bias tables generated in test set
validation<-validation%>%
  left_join(genre_bias,c("userId"="userId", "genres"="genres"))%>%
  left_join(movie_bias,by="movieId")%>%
  left_join(user_bias,by="userId")%>%
  mutate(year=round_date(as.POSIXct(timestamp,origin="1970-01-01",tz="GMT"),unit="year"))%>%
  left_join(time_bias,by="year")


## For any new years, movies, users or genres (not in the train set, i.e. cold start) we set the bias to 0.

validation$Timed_avg[is.na(validation$Timed_avg)]<-0
validation$bg[is.na(validation$bg)]<-0
validation$bu[is.na(validation$bu)]<-0
validation$bi[is.na(validation$bi)]<-0


##Y estimation: y=mu+bt+bi+bu+bg

y_h<-mu+validation$Timed_avg+validation$bu+validation$bg+validation$bi

## Fixing values below 0.5 and above 5, as described in data exploration 5) Rating analysis
y_h<-ifelse(y_h<0.5,0.5,y_h)
y_h<-ifelse(y_h>5,5,y_h)

r<-RMSE(y_h,validation$rating)
r                              ## Final RMSE: 0.8647722 
