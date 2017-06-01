# Hotspots of Violent Political Conflict in the African Continent (1997-2010)

Statistical analysis, based on a random sample of violent political conflict events on the African continent. Data sourced from the Armed Conflict Location & Event Data Project [(ACLED)](http://www.acleddata.com/data/). The random sample used for this study can be downloaded [here](https://raw.githubusercontent.com/luisra/hotspots/master/conflictdata.csv).

## Synopsis

This project aims to evaluate the use of statistics to identify the hotspots of political activity in the African continent (1997-2010). The study is focused on how Friedman's test for a randomized block design and Fisher's exact test of independence allow us to identify with confidence the hotspots of violent political conflict (per event type).

## Code Example

The following questions guided our analysis:
* Is there a difference in the number of conflicts per type of event?
* Is there a relationship between type of event and country?

Summary of events during this timeframe:
```
summary(conflict$EventType)
```

Frequency of events:
```
hist(conflict$Year, main = "Number of Political Conflicts", xlab = "Year")
```

Number of events per year and event type:
```
boxplot(events_y ~ years_e, xlab = "Year", ylab = "Number of Conflicts",
        main = "Violent Political Conflicts from 1997-2010")
        
boxplot(events_y ~ types_e, ylab ="Number of Conflicts", xlab = "Type of Event",
        main = "Violent Political Conflicts from 1997-2010")
```

Interaction effect between type of event, year, and number of conflicts:
```
interaction.plot(years_e, type, events_y, xlab = "Year", ylab = "Number of Conflicts",
                 main = "Violent Political Conflicts from 1997-2010", legend = TRUE) 
```

Finally, we set up the two-way ANOVA for a randomized block design.
```
aov.Events_Year <- aov( events_y ~ types_e + years_e )
```

## Motivation

Moving forward, this analysis would allow us to quickly identify in what country of the African continent a violent political conflict is taking place. Depending on the type of event, instead of considering every country from the get-go, we would focus on the hotspots of political activity first.

## Installation

The hotspots.R script performs all aspects of this implementation.

## Tests

We chose to verify the ANOVA assumptions before proceeding.

Levene test for homogeneity of variance:
```
leveneTest(events_y ~ types_e)
```

Switch to nonparametric approach.
```
friedman.test(events_y ~ types_e|years_e)
```

Post-hoc analysis:
```
friedman.test.with.post.hoc(events_y ~ types_e|years_e, data = Master)
```

Fisher's exact test of independence:
```
fisher.test(counts, simulate.p.value = TRUE)
```

## References

[1] Galili, A. T. (2010, February 22). Post hoc analysis for Friedman's Test (R code). https://www.r-statistics.com/2010/02/post-hoc-analysis-for-friedmans-test-r-code/

[2] Post-Hoc tests for Friedman Test? (n.d.). http://r.789695.n4.nabble.com/Post-Hoc-tests-for-Friedman-Test-td892886.htmliver

## License

MIT License
