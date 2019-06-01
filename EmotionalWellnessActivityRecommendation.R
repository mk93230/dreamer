# The idea behind this model is to suggest top 3 activities recommended for the user
# based on the recent dreams, sleep patterns and regular check-ins.
# The concept is to tap in to conscious (regular check-ins), subconscious triggers (sleeping patterns)
# and unconscious behavior of one's mind.

# The goal is to share this anonymous data to a another user and obtain the next best activities
# that this other user will provide to the experiencing user.

# We will call the data obtained from the user as "wellness seeker" and the user who is 
# providing the next best actions as the "contributor"

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
# many users on a specific instance of their state (conscious, subconscious and unconscious states)
# For the ratings the average rating can be obtained. For the next best action, the top most popular
# votes can be picked up. 

# When the user is recommended of next best action and when the user acknowledges that there is
# action in progress, we must not provide another one that either contradicts and or overwhelms
# the user more.

# The ratings provided by the contributor for a certian type of dreams emotions, with sleep type and checkin
# must be taken in to consideration and provide similar ratings for others using the model. 
# This is similar to movie ratings and recommendations.