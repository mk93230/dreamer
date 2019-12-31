# The idea behind this model is to suggest top 3 activities recommended for the user
# based on the recent dreams, sleep patterns and regular check-ins.
# The concept is to tap in to conscious (regular check-ins), subconscious triggers (sleeping patterns)
# and unconscious behavior of one's mind.

# The goal is to share this anonymous data to a another user and 
# obtain the next best activities that this other user will provide 
# to the experiencing user.

# We will call the data obtained from the user as "wellness seeker" 
#and the user who is providing the next best actions as the "contributor"

# The contributors could be wellness seekers themselves and or providers. For building this
# model we will keep providers out of scope.
# The contributors will be provided data on a anoymous user. The contributor will be able to 
# read the user's dream, their sleep patterns and their checkins. Based on this the contributor
# provides the rating for the user -5 to + 5 in steps of 0.5.

# This rating is suppose to mean the overall emotional health which then will be added to the overall
# score for that user. 

# The format of the data
#DeviceId DreamId DreamDescription DreamEmotions CheckInAssessment SleepQuality Rating NextBestAction

# The next best actions are; Get more sleep (find more time)
#                            Get advice (drs, friends&family)
#                            Find ways to relax - meditate, yoga, take some time, enjoy something you like, do what you love
#                            Maintain - Keep doing what you are doing
#                            Get support (Prayers, Psychiatrist, family&friends, dream interpretation)

# So the net result of this assessment is that the user of the device would get ratings from 
# many contributors on a specific instance of their state (conscious, subconscious and unconscious states)
# For the ratings the average rating can be obtained. For the next best action, the top most popular
# votes can be picked up. 

# When the user is recommended of next best action and when the user acknowledges that there is
# action in progress, we must not provide another one that either contradicts and or overwhelms
# the user more.

# The ratings provided by the contributor for a certian type of dreams emotions, with sleep type and checkin
# must be taken in to consideration and provide similar ratings for others using the model. 
# This is similar to movie ratings and recommendations.

# Lets pick 10,000 rows for quick calculation
dreams_df <- movielens[sample(nrow(movielens),10000),]

colnames(dreams_df) <- c("dreamId","title","year","category","deviceId","rating","timestamp")
head(dreams_df)

# Dream description will be added to the data frame. Ratings on sleep and ratings on
# check in will be added as well. Both these ratings will be self rated by the user.
# At some point the ratings for sleep can be attained from the monitoring device
# We can ask the users in the checkin if we show a scenario on a person
# how would they perceive, more pessimistic or optimistic. This will help 
# us to determine the personality type.The users rating others dream and their patterns of rating could be used to 
# determine their own check-in -  Little bit of Psychologist advise may be required here or we can 
# until the data shows some pattern. Interestingly we can predict their self ratings
# from how they rate others.


# We should have a determine period, lets says weekly report on dreams, checkin
# and sleep quality. We can come up with a factor for each one.

# Weekly dreams should have a value anywhere between -0.33 to + 0.33 - di
# Weekly checkin should have a value anywhere between -0.33 to + 0.33 - ci
# Weekly sleep quality should have a value anywhere between -0.33 to +0.33 - si

# the multiplication factor mf = di + ci + si. The maximum mf is ~1 (0.99) 
# and minimum is ~-1 (0.99)

######## Calculating the factor score for dreams ###################
# Weekly dreams can be processed to identify certain defined events
# Each dreams will be run through Natural Language Processing ...continue tomorrow
