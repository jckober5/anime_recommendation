{r echo=FALSE, warning=FALSE, message=FALSE}
### Read in the Necessary Packages
library(RMySQL)
library(ggplot2)
library(plotly)
library(devtools)
# devtools::install_github('jckober5/R_Package_Remove_Outliers/jkoutliers')
library(jkoutliers)
options(scipen=999)

### Bring in the Data Successfully
mysqlconnection <- dbConnect(RMySQL::MySQL(),
                             dbname='external_data',
                             host='23.239.4.168',
                             port=3306,
                             user='jkober',
                             password='4N4CrFHevPzpdt!')

df <- dbReadTable(mysqlconnection, 'divide_music')

### Perform Exploratory Analysis on potential Relationships on Views
# How many views can you expect on average per day based on the life of the channel at the time
model.data <- data.frame(views_to_life_ratio = df$views_to_life_ratio
                         , video_created_month = df$video_created_month
                         , video_created_day = df$video_created_day
                         , video_duration_minutes = as.character(round(df$video_duration_minutes, 0))
                         , days_since_last_video = df$days_since_last_video
                         , life_of_channel = round(difftime(df$video_created_date, df$channel_created_date, units = 'days'),0)
)

ggplotly(ggplot(model.data, aes(x = life_of_channel, y = views_to_life_ratio)) + geom_point(color = 'purple', size = 2) + 
           theme(panel.background = element_rect(fill = "#202123"),
                 plot.background = element_rect(fill = "#202123"),
                 title = element_text(colour = '#ffffff')) +
           ggtitle("Views per Day based on the Life of the Channel")
)

### Remove outliers affecting data model linearity
model.data.clean <- quantileRemove(model.data, 'views_to_life_ratio')
print(paste0(round(100 - ((nrow(model.data.clean) / nrow(model.data))* 100) ,0), '%'))

ggplotly(ggplot(model.data.clean, aes(x = life_of_channel, y = views_to_life_ratio)) + geom_point(color = 'purple', size = 2) +             
           theme(panel.background = element_rect(fill = "#202123"),
                 plot.background = element_rect(fill = "#202123"),                  title = element_text(colour = '#ffffff')) +            
           ggtitle("Views per Day based on the Life of the Channel") )

### Run Linear Regression model to discover significance and impact of the Channel life on expected views per day
model <- lm(views_to_life_ratio ~ life_of_channel, data = model.data.clean)
print(summary(model))

### What is the expected number of views to count on per day for their next video that will be published in 14 days
new_data <- data.frame(life_of_channel = max(model.data$life_of_channel) + 14)
print(round(predict(model, new_data),1))

### Perform Exploratory Analysis on potential Relationships on Likes
# How many likes can you expect on average per day based on the life of the channel at the time
model.data <- data.frame(likes_to_life_ration = df$likes_to_life_ration
                         , video_created_month = df$video_created_month
                         , video_created_day = df$video_created_day
                         , video_duration_minutes = as.character(round(df$video_duration_minutes, 0))
                         , days_since_last_video = df$days_since_last_video
                         , life_of_channel = round(difftime(df$video_created_date, df$channel_created_date, units = 'days'),0)
)

ggplotly(ggplot(model.data, aes(x = life_of_channel, y = likes_to_life_ration)) + geom_point(color = 'purple', size = 2) + 
           theme(panel.background = element_rect(fill = "#202123"),
                 plot.background = element_rect(fill = "#202123"),
                 title = element_text(colour = '#ffffff')) +
           ggtitle("Likes per Day based on the Life of the Channel")
)

### Remove outliers affecting data model linearity
model.data.clean <- quantileRemove(model.data, 'likes_to_life_ration')
print(paste0(round(100 - ((nrow(model.data.clean) / nrow(model.data))* 100) ,0), '%'))

ggplotly(ggplot(model.data.clean, aes(x = life_of_channel, y = likes_to_life_ration)) + geom_point(color = 'purple', size = 2) + 
           theme(panel.background = element_rect(fill = "#202123"),
                 plot.background = element_rect(fill = "#202123"),
                 title = element_text(colour = '#ffffff')) +
           ggtitle("Likes per Day based on the Life of the Channel")
)

### Run Linear Regression model to discover significance and impact of the Channel life on expected likes per day
model <- lm(likes_to_life_ration ~ life_of_channel, data = model.data.clean)
print(summary(model))

### What is the expected number of likes to count on per day for their next video that will be published in 14 days
new_data <- data.frame(life_of_channel = max(model.data$life_of_channel) + 14)
print(round(predict(model, new_data),1))