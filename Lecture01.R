library(tidyverse)
library(gapminder)
library(plotly)
library(ggplot2)
detach(package:plyr)

gapminder

gapminder %>% 
  filter(year == 1957)

gapminder %>% 
  filter(country == "Austria")

gapminder %>% 
  filter(country == "China")

gapminder %>% 
  arrange(desc(pop))

gapminder %>% 
  filter(year == 1957) %>% 
  arrange(desc(pop))


gapminder_2007 <- gapminder %>% 
  filter(year == 2007)


#Arrange
# Filter the gapminder dataset for the year 1957
gapminder %>% 
  arrange(gdpPercap) # ascending order 
gapminder %>% 
  arrange(desc(gdpPercap)) # descending order 
# more than one pipe
gapminder %>% 
  filter(year == 2007) %>% 
  arrange(desc(gdpPercap))

# Arranging observations by life expectancy
# Filter for the year 1957, then arrange 
# in descending order of population
gapminder %>% 
  filter(year == 1957) %>% 
  arrange(desc(pop))


plot <- ggplot(gapminder_2007, aes(x= gdpPercap, y = lifeExp, color = continent, size=pop)) +
  geom_point()+ scale_x_log10()

ggplotly(plot)


## Ejercicio:
# Scatter plot comparing gdpPercap and lifeExp, with color representing continent
# and size representing population, faceted by year
gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point() + facet_wrap(~ year)


gapminder %>%
  filter(year == 1957) %>%
  summarize(medianLifeExp = median(lifeExp), maxGDPperCapita = max(as.numeric(gdpPercap)))

# Ejercicio: 
# Create gapminder_1952
gapminder_1952 <- gapminder %>% 
  filter(year == 1952)
# put pop on the x-axis and gdpPercap on the y-axis
ggplot(gapminder_1952, aes(x = pop, y = gdpPercap, color=continent)) +
  geom_point()+scale_x_log10()

# summarizing by year
by_year <- gapminder %>%
  dplyr::group_by(year) %>%
  summarize(meanLifeExp = mean(lifeExp), totalPop = sum(as.numeric(pop)))
ggplot(by_year, aes(x = year, y = totalPop)) + geom_point()  # + expand_limits(y=0)

# by year and continent
by_year_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarize(meanLifeExp = mean(lifeExp), totalPop = sum(as.numeric(pop)))

ggplot(by_year_continent, aes(x = year, y = totalPop, 
                              color = continent)) + geom_point()  + expand_limits(y=0)


#Ejercicios
# Find median life expectancy and maximum GDP per capita in each year
by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianLifeExp = median(lifeExp), maxGDP = max(as.numeric(gdpPercap)))
ggplot(by_year, aes(x=year, y=medianLifeExp))+ geom_point() + expand_limits(y=0)

# Find median life expectancy and maximum GDP per capita in each continent in 1957
gapminder %>%
  filter(year == 1957) %>%
  group_by(continent) %>%
  summarize(medianLifeExp = median(lifeExp), maxGDP = max(as.numeric(gdpPercap)))

# Find median life expectancy and maximum GDP per
# capita in each continent/year combination
gapminder %>%
  group_by(continent, year) %>%
  summarize(medianLifeExp = median(lifeExp), maxGDP = max(as.numeric(gdpPercap)))

##histograma
gapminder_2007 <- gapminder %>% 
  filter(year == 2007)
ggplot(gapminder_2007, aes(x = lifeExp)) +
  geom_histogram(binwidth = 2) # + scale_x_log10()

#Ejercicios
# Create a histogram of population (pop_by_mil)
gapminder_1952 <- gapminder %>%
  filter(year == 1952) %>%
  mutate(pop_by_mil = pop / 1000000)
ggplot(gapminder_1952, aes(x = pop_by_mil)) +
  geom_histogram(bins = 50) 

# Create a histogram of population (pop), with x on a log scale
ggplot(gapminder_1952, aes(x = pop)) +
  geom_histogram(bins = 50)  + scale_x_log10()


#Line plot
by_year_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarize(meanLifeExp = mean(lifeExp), totalPop = sum(as.numeric(pop)))
ggplot(by_year_continent, aes(x = year, y = meanLifeExp, 
                              color = continent)) + geom_line()+expand_limits(y=0)

# Ejercicios: 
# Summarize the median gdpPercap by year, then save it as by_year
by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianGdpPercap = median(gdpPercap))
# Create a line plot showing the change in medianGdpPercap over time
ggplot(by_year, aes(x = year, y = medianGdpPercap)) + geom_line() + 
  expand_limits(y=0)
# Summarize the median gdpPercap by year & continent, save as by_year_continent
by_year_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarize(medianGdpPercap = median(gdpPercap))
# Create a line plot showing the change in medianGdpPercap by continent over time
ggplot(by_year_continent, aes(x = year, y = medianGdpPercap, color=continent)) + geom_line() + 
  expand_limits(y=0)



#Barplot
#by continent
by_continent <- gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) 
by_continent <-   aggregate( lifeExp ~continent, by_continent,mean )
ggplot(by_continent, aes(x = continent, y = lifeExp)) +
  geom_col()

#Ejercicios
# Summarize the median gdpPercap by continent in 1952
by_continent <- gapminder %>%
  filter(year == 1952) %>%
  group_by(continent) 
by_continent <- aggregate( gdpPercap ~continent, by_continent,median )

# Create a bar plot showing medianGdp by continent
ggplot(by_continent, aes(x = continent, y = gdpPercap)) +
  geom_col()

# Filter for observations in the Oceania continent in 1952
oceania_1952 <- gapminder %>%
  filter(year == 1952, continent == "Oceania")
oceania_1952
# Create a bar plot of gdpPercap by country
ggplot(oceania_1952, aes(x = country, y = gdpPercap)) +
  geom_col()


#Boxplots
gapminder_2007 <- gapminder %>% 
  filter(year == 2007)
ggplot(gapminder_2007, aes(x= continent, y = lifeExp)) +
  geom_boxplot()

# Ejercicios: 
# Create a boxplot comparing gdpPercap among continents
gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Add a title to this graph: "Comparing GDP per capita across continents"
ggplot(gapminder_1952, aes(x = continent, y = gdpPercap)) +
  geom_boxplot() +
  scale_y_log10() + labs(title = "Comparing GDP per capita across continents")


#Facet Plot
library(gapminder)
library(dplyr)
library(ggplot2)

gapminder_1952 <- gapminder %>%
  filter(year == 1952)
ggplot(gapminder_1952, aes(x= pop, y = lifeExp)) +
  geom_point() + scale_x_log10() + facet_wrap(~ continent)

# Ejercicio: 
# Scatter plot comparing gdpPercap and lifeExp, with color representing continent
# and size representing population, faceted by year
ggplot(gapminder, aes(x= gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point() + scale_x_log10() + facet_wrap(~ year)


