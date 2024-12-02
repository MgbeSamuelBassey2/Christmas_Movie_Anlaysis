library(tidyverse)

# Hi everyone. so  in the sptirt of christmas, we are going to be analyzing the cristmas movie dataset

data <- read.csv('C:/Users/lenovo/Documents/Christmas_movie_analysis/christmas_movies.csv')

View(data)

# lets get a breif summary of the data
summary(data)

#lets get the average rating of different ratings of movies
movie_rating <- data %>%
  aggregate(imdb_rating ~ rating, mean) %>%
  arrange(desc(imdb_rating)) %>%
  head(10)
movie_rating
# TV-MA tends to have a higher rating than others
movie_rating %>%
  ggplot(aes(imdb_rating, rating)) +
  geom_bar(stat = 'identity', fill = 'lightblue')


# let us group the rating of movies into catergotiess(Excellent, Verygood, good, average, poor)
data <- data %>%
  mutate(category = case_when(
    is.na(imdb_rating) ~ 'Unknown',
    imdb_rating >= 8.0 ~ 'Excellent',
    imdb_rating >= 7.0 ~ 'Very Good',
    imdb_rating >= 6.0 ~ 'Good',
    imdb_rating >= 5.0 ~ 'Average',
    TRUE ~ 'Poor'
  ))

# Now lets count each category
data %>% 
  count(category) %>%
  arrange(desc(n)) %>%
  ggplot(aes(category, n)) +
  geom_bar(stat = 'identity', fill = 'red') +
  labs(y = 'Number of Movies',
       title = 'Number of Movies in each category') +
  theme_minimal()
# Looks like there are more good movies than other categories



#Lest take a look at the highest rated movies and they year they were releases
highest_rated_movie <- data %>%
  select(c(title, imdb_rating, release_year)) %>%
  arrange(desc(imdb_rating)) %>%
  head(10)
# looks like Anne of green gables made it into the top 5

highest_rated_movie
highest_rated_movie %>%
  ggplot(aes(x = imdb_rating, y = title, fill = release_year)) +
  geom_bar(stat = 'identity')
View(data)  


# lets group the year into classic era(1940-1959), family favourites(1960 - 1979)
# Modern  hits(1980 - 1999) and New traditions(2000 - 2020)

data <- data %>% 
  mutate(era = case_when(
    is.na(release_year) ~ 'Unknown',
    release_year >= 2000 ~ 'New traditions',
    release_year >= 1980 ~ 'Modern Hits',
    release_year >= 1960 ~ 'family Favourites',
    TRUE ~ 'Classic era'
  ))

# now lets get a count of each era and plot it in a graph
data %>%
  count(era) %>%
  arrange(desc(n)) %>%
  ggplot(aes(era, n)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  labs(title = 'Number of Movies by Eras',
       y = 'Number of Movies') +
  theme_minimal()
# New traditions has 729 movies making it the era with the most christmas movies

# Lets get the average rating for each of these eras
data %>%
  aggregate(imdb_rating ~ era, mean) %>%
  ggplot(aes(era, imdb_rating)) 
# fred savage is number 1. lets look at the movies he is responisble for
data %>%
  filter(director == 'Fred Savage')
  geom_bar(stat = 'identity', fill = 'green', color = 'black') +
  labs(title = 'Average Rating of different eras') +
  theme_minimal()
# looks like family favourites(1960 - 1979) had the highest rated movies

#Now lets get average imdb rating of directors
data %>%
  aggregate(imdb_rating ~ director, mean) %>%
  arrange(desc(imdb_rating))
# lets look at the movies that the top 5 highest ratings

highest_directors <- data %>%
  filter(director == 'Fred Savage'|
         director == 'Frank Perry'|
         director == 'Frank Capra'|
         director == 'Paul Dugdale'|
         director == 'Mark Sujay Samuel') %>%
  select(c(director, title, imdb_rating, release_year)) %>%
  arrange(desc(imdb_rating))

highest_directors

#lets get the votes of movies
data %>%
  select(c(title, votes, release_year)) %>%
  arrange(desc(votes)) %>%
  head(10)
# Looks like the santa summit has the highest votes



# lets get the correlation between release year and rating
data %>%
  ggplot(aes(release_year, imdb_rating)) +
  geom_point(color = 'blue')+
  labs(title = 'Correlation between release_year and imdb_rating')


# Lastly, why dont we look at movies with the longest runtime
data %>%
  select(c(title, runtime, release_year)) %>%
  arrange(desc(runtime)) %>%
  head(10) %>%
  ggplot(aes(runtime, title, fill = release_year)) +
  geom_bar(stat = 'identity')
# Looks like anne of green gables is the longest movie