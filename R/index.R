
# includes
suppressWarnings(suppressMessages(suppressPackageStartupMessages({
	source("R/functions.R")
	library("ggplot2")
	library("dplyr")
	require(data.table)
})));

# where is the data located?
data_location = "/media/shadash/321cfbdb-669b-484c-bf65-5e5e3ffbb541/"

# load sequences
t <- read.table(paste(data_location, "track_features/tf_000000000000.csv", sep=""), TRUE, ",") %>% select(track_id, tempo, valence, us_popularity_estimate, acousticness, beat_strength, bounciness, danceability, energy, flatness, instrumentalness, liveness, loudness, mechanism, organism, speechiness)
t <- rbind(t, read.table(paste(data_location, "track_features/tf_000000000001.csv", sep=""), TRUE, ",") %>% select(track_id, tempo, valence, us_popularity_estimate, acousticness, beat_strength, bounciness, danceability, energy, flatness, instrumentalness, liveness, loudness, mechanism, organism, speechiness))
d <- read.table(paste(data_location, "training_set/log_mini.csv", sep=""), TRUE, ",")

# pre-processing
d = d %>% inner_join(t, by=c("track_id_clean"="track_id")) %>% arrange(session_id, session_position)
df = data.table(d)
df[ , valence_diff := valence - shift(valence), by = session_id]
df[ , valence_diff_abs := abs(valence - shift(valence)), by = session_id]
df[ , tempo_diff := tempo - shift(tempo), by = session_id]
df[ , tempo_diff_abs := abs(tempo - shift(tempo)), by = session_id]
df[ , popularity_diff := us_popularity_estimate - shift(us_popularity_estimate), by = session_id]
df[ , popularity_diff_abs := abs(us_popularity_estimate - shift(us_popularity_estimate)), by = session_id]
df[ , acousticness_diff := acousticness - shift(acousticness), by = session_id]
df[ , acousticness_diff_abs := abs(acousticness - shift(acousticness)), by = session_id]
df[ , beat_strength_diff := beat_strength - shift(beat_strength), by = session_id]
df[ , beat_strength_diff_abs := abs(beat_strength - shift(beat_strength)), by = session_id]
df[ , bounciness_diff := bounciness - shift(bounciness), by = session_id]
df[ , bounciness_diff_abs := abs(bounciness - shift(bounciness)), by = session_id]
df[ , danceability_diff := danceability - shift(danceability), by = session_id]
df[ , danceability_diff_abs := abs(danceability - shift(danceability)), by = session_id]
df[ , energy_diff := energy - shift(energy), by = session_id]
df[ , energy_diff_abs := abs(energy - shift(energy)), by = session_id]
df[ , flatness_diff := flatness - shift(flatness), by = session_id]
df[ , flatness_diff_abs := abs(flatness - shift(flatness)), by = session_id]
df[ , instrumentalness_diff := instrumentalness - shift(instrumentalness), by = session_id]
df[ , instrumentalness_diff_abs := abs(instrumentalness - shift(instrumentalness)), by = session_id]
df[ , liveness_diff := liveness - shift(liveness), by = session_id]
df[ , liveness_diff_abs := abs(liveness - shift(liveness)), by = session_id]
df[ , loudness_diff := loudness - shift(loudness), by = session_id]
df[ , loudness_diff_abs := abs(loudness - shift(loudness)), by = session_id]
df[ , mechanism_diff := mechanism - shift(mechanism), by = session_id]
df[ , mechanism_diff_abs := abs(mechanism - shift(mechanism)), by = session_id]
df[ , organism_diff := organism - shift(organism), by = session_id]
df[ , organism_diff_abs := abs(organism - shift(organism)), by = session_id]
df[ , speechiness_diff := speechiness - shift(speechiness), by = session_id]
df[ , speechiness_diff_abs := abs(speechiness - shift(speechiness)), by = session_id]
df[ , valid:=session_position-shift(session_position)==1, by = session_id]
d = setDF(df)
rm(df)
d = d %>% filter(valid==TRUE) %>% mutate(skipped=ifelse(not_skipped=="true", 0, 1))

# visualization
#p=ggplot(d, aes(x=vdiff, color=factor(skipped, levels=c(1, 0), labels=c("true", "false")))) +
#	geom_histogram(fill="white", alpha=0.5, position="identity")+
#	scale_color_manual("skipped",values=c("red","blue"))
#ggsave("dist/valence.png", p, scale=1, dpi=300)

#p=ggplot(d, aes(x=tdiff, color=factor(skipped, levels=c(1, 0), labels=c("true", "false")))) +
#	geom_histogram(fill="white", alpha=0.5, position="identity")+
#	scale_color_manual("skipped",values=c("red","blue"))
#ggsave("dist/tempo.png", p, scale=1, dpi=300)

#p=ggplot(d, aes(x=vdiff_abs, color=factor(skipped, levels=c(1, 0), labels=c("true", "false")))) +
#	geom_histogram(fill="white", alpha=0.5, position="identity")+
#	scale_color_manual("skipped",values=c("red","blue"))
#ggsave("dist/valence_abs.png", p, scale=1, dpi=300)

#p=ggplot(d, aes(x=tdiff_abs, color=factor(skipped, levels=c(1, 0), labels=c("true", "false")))) +
#	geom_histogram(fill="white", alpha=0.5, position="identity")+
#	scale_color_manual("skipped",values=c("red","blue"))
#ggsave("dist/tempo_abs.png", p, scale=1, dpi=300)

# statistics
sink("dist/glm.m");
m=glm(skipped ~ valence_diff_abs + tempo_diff_abs + popularity_diff_abs + acousticness_diff_abs + beat_strength_diff_abs + bounciness_diff_abs + danceability_diff_abs + energy_diff_abs + flatness_diff_abs + instrumentalness_diff_abs + liveness_diff_abs + loudness_diff_abs + mechanism_diff_abs + organism_diff_abs + speechiness_diff_abs, d, family = "binomial")
summary(m)
sink()
