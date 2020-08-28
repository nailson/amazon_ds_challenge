library(dplyr)
library(lubridate)
library(readr)

# Load the Data
video_count <- read_csv("~/Pessoal/jobs/research_scientist_problems/data/challenge_2/video_count.csv", col_types = cols(date = col_character()))
video_features <- read_csv("~/Pessoal/jobs/research_scientist_problems/data/challenge_2/video_features.csv", col_types = cols(video_upload_date = col_character()))


# Defining popular videos

# count plays
video_count_pop = video_count%>%
                  group_by(video_id)%>%
                  summarise(
                    overall_count=sum(count)
                  )

# check the distribution
hist(video_count_pop$overall_count)              
boxplot(video_count_pop$overall_count)              

# Using 3rd quartile as popular videos threshould
pop_q3rd = quantile(video_count_pop$overall_count)[4]

video_count_pop = video_count_pop%>%
                  mutate(
                    is_popular=ifelse(overall_count>pop_q3rd,TRUE, FALSE)                               
                  )

video_popular = video_count%>%
                inner_join(
                  video_count_pop,
                  by = "video_id"
                )

# Defining stable or trend

# normalize the count to get the right coeficient
normalize_range<-function(column, min_value, max_value){
  ((column - min(column)) / (max(column) - min(column)))*(max_value-min_value)+min_value
}

# get angular coeficient and angle of the regression
get_angular_coeficient<-function(video_count_agg){
  video_count_agg$index = c(1:nrow(video_count_agg))
  video_count_agg$count_norm = normalize_range(video_count_agg$count, 1,nrow(video_count_agg))
  
  model = lm(count_norm ~ day, data=video_count_agg)
  
  video_count_agg$angular_coef = coef(model)[2]
  video_count_agg$angle = atan( video_count_agg$angular_coef) * 180 / pi
  video_count_agg
}

video_popular = video_popular%>%
                mutate(
                  date = ymd(date),
                  day = day(date)
                )

video_count_coef = video_popular%>%
                   group_by(video_id)%>%
                   do(get_angular_coeficient(.))%>%
                   ungroup()

x = video_count_coef%>%filter(video_count_coef$video_id==786)
plot(x$day,x$count)
abline(lm(count~day,x))

# Check the distribution
hist(video_count_coef$angular_coef, breaks=20, main="Angular Coeficient Histogram")
hist(video_count_coef$angle, breaks=20, main="Angular Coeficient Histogram")
boxplot(video_count_coef$angle, main="Angular Coeficient Histogram")
summary(video_count_coef)

# Define trending videos using 3rd quartile as threshould
angle_q3rd = quantile(video_count_coef$angle)[4]

# Define Stable as between 3rd and 1st quartiles
angle_q1rd = quantile(video_count_coef$angle)[2]

# Defining HOT, Stable and Popular and Everything else
video_classified = video_count_coef%>%
                   mutate(
                      classification=ifelse(angle>=angle_q3rd,"Hot", "Everything else"),
                      classification=ifelse(angle<angle_q3rd & angle>angle_q1rd & is_popular,"Stable and Popular", classification ) 
                   )

video_classified%>%count(classification)

# Awesome, now put that in a file
write.csv(video_classified, "data/video_classified.csv")
