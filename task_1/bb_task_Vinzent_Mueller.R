library(data.table)
library(ggplot2)


# 1 data prep

wd = "/home/vinzent/Documents/repos/dataenv/task_1"
setwd(wd)

events = fread("challenge_data/learner_item_data.csv", sep = ',')

# test case
#events <- events[uuid == 2990]

events$uuid <- as.factor(events$uuid)
events[, created_at := as.POSIXct(created_at)]
events$trainer_item_id <- as.factor(events$trainer_item_id)

# distinct users
#uniqueN(events$uuid)

setkey(events, uuid, created_at) # to order the data chronologically by user and timestamp

# enumerate a user's actions chronologically
events[ , counter := 1:.N, by = .(uuid)]

summary(events)

# to get the subsequent timestamp, we essentially do a self join, duplicating the table
times <- events[, -c("trainer_item_id")]
times$counter <- times$counter + 1

events$preceeding_timestamp <- setDT(times)[events, created_at, on = .(uuid, counter)]

rm(times) # not need any longer

# find new session starts
events[, new_session_start := as.integer(difftime(created_at, preceeding_timestamp, units = "mins")) > 60]
# fix first events
events[is.na(new_session_start), new_session_start := FALSE]

# session id, ,increments at every session start
events[, session_id := cumsum(as.integer(ifelse(new_session_start == TRUE, 1, 0))), by = .(uuid)]

# rounding up to full minutes
session_lengths <- events[, .(session_start = as.Date(min(created_at)), duration_min = as.integer(ceiling(difftime(max(created_at), min(created_at), units = "secs")/60))), by = .(session_id, uuid)]
# fix single item sesions
session_lengths[duration_min == 0, duration_min := 1]

# add A/B groups
test_groups = fread("challenge_data/test_groups.csv", sep = ',', colClasses = c("chr", "factor"))

session_lengths$group <- setDT(test_groups)[session_lengths, test_group, on = "uuid"]

# 2 a)

# duration histogram
ggplot(session_lengths, aes(session_lengths$duration_min)) + geom_histogram(, binwidth = 1, fill = "lightblue") + theme_bw() + scale_x_continuous(limits = c(0, 200)) + xlab("Session duration in minutes") + ylab("Sessions")

# duration statistics
summary(session_lengths$duration_min)


# 2 b)

# per user aggregate: day of session start & highest number of daily sessions
daily_sessions <- session_lengths[, .(daily_sessions = uniqueN(session_id)), by = .(uuid, group, session_start)][, .(max_sessions = max(daily_sessions)), by = .(uuid, group)]
#table(daily_sessions$group)

# histogram of daily sessions
ggplot(daily_sessions, aes(daily_sessions$max_sessions)) + geom_histogram(, binwidth = 1, fill = "lightblue") + theme_bw() + xlab("Session per day") + ylab("Users")


table(daily_sessions$max_sessions > 1)
table(daily_sessions$max_sessions)

round(prop.table(table(daily_sessions$max_sessions))* 100, 2)

round(uniqueN(daily_sessions[max_sessions > 1, uuid]) / uniqueN(daily_sessions$uuid) * 100, 2)


# 3 a)

# test metrics calculated per group
stats_table <- session_lengths[, .(median_duration = median(duration_min)), by = group]

stats_table <- cbind(stats_table, daily_sessions[, .(single_session = sum(ifelse(max_sessions == 1, 1, 0)), users = .N), by = group][ , -1])

stats_table[, prop_single_session := single_session / users]
# proportions aren't promising

stats_table

prop.test(cbind(stats_table[ , single_session], stats_table[, users-single_session]), alternative='greater')


# 3 b)

ggplot(session_lengths, aes(session_lengths$duration_min, color = group)) + geom_density() + theme_bw() + scale_x_continuous(limits = c(0, 60)) + xlab("Session duration in minutes")
# low duration sessions seem less common for test group...

# ... are they?
round(prop.table(table(session_lengths[, group], session_lengths[, duration_min]), margin = 1)[, 1:10]*100, 2)

# not a test for medians, but general population differences
#wilcox.test(duration_min ~ as.factor(group), data = session_lengths, alternative = "less") # not sig