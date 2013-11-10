library(ggplot2)
# ----------------------------------------------------------------
#                                1


outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
names(outcome)[11] = "Deaths1"
outcome[, 11] = as.numeric(outcome[, 11])

g1 = ggplot(outcome, aes(x=Deaths1, y=..count..)) + geom_histogram()
g1 = g1 + labs(title = "Heart attack", x="30-Day Rate", y="Frequency")
g1 = g1 + geom_vline(xintercept = median(outcome[, 11], na.rm=TRUE))
g1 = g1 + geom_density(colour="darkgreen", size=1)
g1 

# ----------------------------------------------------------------
#                                2

names(outcome)[17] = "Deaths2"
outcome[, 17] = as.numeric(outcome[, 17])
names(outcome)[23] = "Deaths3"
outcome[, 23] = as.numeric(outcome[, 23])

g2 = ggplot(outcome, aes(x=Deaths2, y=..count..)) + geom_histogram()
g2 = g2 + labs(title = "Heart failure", x="30-Day Rate", y="Frequency")
g2 = g2 + geom_vline(xintercept = median(outcome[, 17], na.rm=TRUE))
g2 = g2 + geom_density(colour="darkgreen", size=1)

g3 = ggplot(outcome, aes(x=Deaths3, y=..count..)) + geom_histogram()
g3 = g3 + labs(title = "Pneunomia", x="30-Day Rate", y="Frequency")
g3 = g3 + geom_vline(xintercept = median(outcome[, 23], na.rm=TRUE))
g3 = g3 + geom_density(colour="darkgreen", size=1)
g3

multiplot(g1, g2, g3, cols=1)

# ----------------------------------------------------------------
#                                3


outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

states_valid = names(table(outcome$State))[table(outcome$State)>20]

outcome2 = outcome[outcome$State %in% states_valid,]
names(outcome2)[11] = "Deaths"
outcome2[, 11] = as.numeric(outcome2[, 11])

g = ggplot(outcome2, aes(factor(State), Deaths))
g + geom_boxplot()

# Order the boxes by median

medians = sort( tapply(outcome2$Death, outcome2$State, median, na.rm=TRUE) )
order = names(medians)
state_factor = factor(outcome2$State, levels=c(order))

g = ggplot(outcome2, aes(state_factor, Deaths))
g + geom_boxplot()

# ----------------------------------------------------------------
#                                4


outcome = read.csv("outcome-of-care-measures.csv", colClasses = "character")
hospital = read.csv("hospital-data.csv", colClasses = "character")
outcome.hospital <- merge(outcome, hospital, by = "Provider.Number")
names(outcome.hospital)[11] = "Deaths"
outcome.hospital[, 11] = as.numeric(outcome.hospital[, 11])
names(outcome.hospital)[15] = "NumPat"
outcome.hospital[, 15] = as.numeric(outcome.hospital[, 15])

g1 = ggplot(outcome.hospital, aes(x=NumPat, y=Deaths))
g1 = g1 + geom_point(shape=19, alpha=1/4) + geom_smooth(method=lm, se=FALSE)
g1 = g1 + labs(x="Number of Patients Seen", y="30-Day Rate")
g1 = g1 + facet_wrap(~Hospital.Ownership, ncol=3, nrow=3)
g1




