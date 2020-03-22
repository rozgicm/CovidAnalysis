# Read in necessary packages

# a collection of great packages, check it out by just googling the tidyverse
library(tidyverse)

# package to handle date data types, which are allways hell
library(lubridate)

# this package has some nice themes for graphics which I find appealing
library(see)

# allows for great annotaions using our data making the code transferable
library(glue) 

# nice package to arrange multiple plots
library(patchwork)

# helps us to make interactive plots 
library(plotly) 

# make even nicer plots
library(ggforce)

# a package containing an ODE solver for the SIR equation
library(deSolve)

# epidemiology package
library(EpiEstim)

################
source(paste0(getwd(),"/humanNumbers.R"))
################
# location of JHU data on github (here only confirmed cases, 
# see github page for the other files)
jhu_url = paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                 "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                 "time_series_19-covid-Confirmed.csv", sep = "")


# load data from csv and rename the province and country columns 
# ("%>%" ist the pipe operator, it works like the unix pipe "|", i.e., 
# it applies the next function to the argument on the left, 
# think of it like a left to right version of the function
# concatenation from mathematics [which is right to left]

dat =  read_csv(jhu_url) %>% rename(province = "Province/State", 
                                    country = "Country/Region")

head(dat)

# As we can see the dataframe we just loaded holds all the data from all the countries
# also the data is in 'wide' format, every date is one column. If we want to look into
# a single country this is rather inconvenient, we need the data in 'long' format,
# i.e., every row holds one datapoint information.

#############################
# Let's try to get some inofromation about europe
############################

datWorld =  dat %>% pivot_longer(-c(province,country, Lat, Long),
                          names_to = "Date", 
                          values_to = "cumulative_cases")
europe = read_csv2(file =paste0(getwd(), "/countriesOfEurope.csv"))

# consider "Mainland" only for the time beeing
datEurope = filter(datWorld, country %in% europe$Countries,
                   (province %in% europe$Countries | is.na(province)))

tPop = readRDS(file = paste0(getwd(), "/tPop.RDS"))
cCodes = readRDS(file = paste0(getwd(), "/countryCodes.RDS"))

datEurope = datEurope %>% 
  select (-province) %>% 
  left_join(., cCodes %>%  select(name, charcode), 
            by = c("country"="name")) %>% 
  left_join(., tPop %>% 
              filter(Year =="2020") %>%
              select(-Year),
            by ="charcode") %>% 
  mutate(Date = mdy(Date), value = value * 1000)


datEurope = datEurope %>% filter(cumulative_cases > 100) %>% 
  group_by(country) %>%
  group_modify(~{
    .x$daySince100 = 1:nrow(.x)
    return(.x)
  })%>% ungroup()


(datEurope %>% 
  ggplot(aes(x= daySince100, y = (cumulative_cases/value)*100, colour = country, group = country) )+
           geom_line() +
           labs(x = "Number of days after reaching 100 cases",
                y =  "Percentage of population per country",
                title = "Development of cases in european countries after reaching 100 infections \nOnly mainland provinces are cnsidered (e.g. only France not St.Martin)"
                ) +
           theme_lucid() +
  scale_colour_flat_d() )%>% ggplotly()

#######################
datLong = dat %>% 
  filter(country == "Germany") %>%  
  pivot_longer(-c(province,country, Lat, Long),
               names_to = "Date", 
               values_to = "cumulative_cases")
head(datLong)

# We will now get rid of some columns we don't need 
# and transform the data in Date into a fitting date format
# (mdy from package(lubridate) will do the trick)  
datLong = datLong %>% 
  select(-c(province, Lat, Long)) %>% 
  mutate(Date = mdy(Date)) 

# next we need to do some transformations:
# 1. We will omit 0's, since we will log transform our data, 0's will be..disturbing
# 2. We will compute the incident cases, just for the LoLz
# 3. We will append week numbers starting with 1 and count the days starting with 1
datLong = datLong %>%  
  filter(cumulative_cases != 0) %>% 
  mutate(incident_cases = c(0, diff(cumulative_cases)),
         myDay = 1:nrow(.),
         myWeek = week(Date)-3 ) 
head(datLong)

# It's picture time :-) 
# [when using ggplot + acts like the aforementioned %>%, weird right? ]


p1 = datLong %>% ggplot(aes(x=Date, y = cumulative_cases)) +
  geom_line() +
  geom_point(shape = "x") +
  labs(title = glue("Number of COVID-19 infections in {datLong$country[1]}"),
       subtitle = glue("data ranging from {min(datLong$Date)} to {max(datLong$Date)}")
       ) +
  theme_lucid()+
  scale_fill_material_c()

p2 = datLong %>% ggplot(aes(x=Date, y = log(cumulative_cases))) +
  geom_line() +
  geom_point(shape = "x") +
  labs(title = glue("Number of COVID-19 infections in {datLong$country[1]}"),
       subtitle = glue("data ranging from {min(datLong$Date)} to {max(datLong$Date)}"),
       caption = "log scale plot"
       ) +
  theme_lucid()+
  scale_fill_material_c()

# here is a funny way to arrange plots using the patchwork package
# it allows for an easy arrangement of ggplots using a nice synatx, see, e.g., 
# https://www.r-bloggers.com/patchwork-r-package-goes-nerd-viral/

p1 /p2
  

# Do you want the plots interactive? - let's do this!

p1 %>% ggplotly()

p2 %>% ggplotly()


# plots help us explore the data, for example we find,
# that a significant increase in cases happens around 25th of Feb.
# Let us exploit that and fit a linear model to the logarithmic data

myLinearModel = lm(log(cumulative_cases) ~ myDay,
                   datLong %>% 
                   filter(Date >= as.Date("2020-02-24")) 
                   )

# that was easy, wasn't it? Let's see how good we did

summary(myLinearModel)

# here goes nothing

linModelDf = broom::tidy(myLinearModel)

datLong %>% filter(Date >= as.Date("2020-02-24") ) %>% 
  ggplot(aes(x=myDay, y= log(cumulative_cases))) +
  geom_smooth(method = lm) +
  geom_point() +
  labs(title = glue("fitted linear model with  intercept {round(linModelDf$estimate[1],2)} and slope {round(linModelDf$estimate[2],2)}"))+
  theme_lucid()

# when there is a model, why not predict some stuff? 
# and as we are good and lawfull we will,
# of course, take prediction uncertainties into account



# okay, pictures please!

startDay =  datLong %>% 
  filter(Date >= as.Date("2020-02-24")) %>% pull(myDay) %>% min()
endDay = datLong %>% 
  filter(Date >= as.Date("2020-02-24")) %>% pull(myDay) %>% max()

startDate = datLong %>% 
  filter(Date >= as.Date("2020-02-24")) %>% pull(Date) %>% min()

endDate = datLong %>% 
  filter(Date >= as.Date("2020-02-24")) %>% pull(Date) %>% max()

predDF = broom::tidy(predict(myLinearModel,
                             newdata = data.frame(myDay = startDay:(endDay+7)),
                             interval = "prediction"))

predDF$Date = seq(startDate, 
                  max(datLong$Date)+days(7), 
                  by = "day") 

p3 = p2 + geom_ribbon(data = predDF,
                 inherit.aes = FALSE, 
                 aes(ymin=lwr, ymax = upr, x = Date), 
                 fill = "grey2", alpha =0.25) +
  geom_line(data = predDF,  inherit.aes = FALSE,
            aes(x= Date, y= fit, colour = "fit"))+
  labs(title = glue("Number of COVID-19 infections in {datLong$country[1]} with a 7 day fit") )+
  theme(legend.title = element_blank()) 

p4 = p1 + geom_ribbon(data = predDF,
                 inherit.aes = FALSE, 
                 aes(ymin=exp(lwr), ymax = exp(upr), x = Date), 
                 fill = "grey2", alpha =0.25) +
  geom_line(data = predDF,  inherit.aes = FALSE,
            aes(x= Date, y= exp(fit), colour = "fit")) +
  scale_y_continuous(labels = humanNumbers)+
  annotate("text",
           y = c(max(exp(predDF$fit)), max(exp(predDF$lwr)),max(exp(predDF$upr))),
           x = max(predDF$Date)+days(2),
           label = c(glue({round(max(exp(predDF$fit)))}),
                     glue({round(max(exp(predDF$lwr)))}),
                     glue({round(max(exp(predDF$upr)))})
                     )
           )+
  labs(title = glue("Number of COVID-19 infections in {datLong$country[1]} with a 7 day fit") )+
  theme(legend.title = element_blank()) 

p4/p3

p4 + facet_zoom(xlim= c(endDate, max(predDF$Date)))


# we can now compute the mean time it take untill the number of cases doubles

log(2)/(linModelDf %>%
          pivot_longer(-term) %>% 
          filter(term == "myDay", name =="estimate") %>% 
          pull(value)
        )

#########
# Allright, next we try to follow the analysis present here:
# https://timchurches.github.io/blog/posts/2020-02-18-analysing-covid-19-2019-ncov-outbreak-data-with-r-part-1/#estimating-changes-in-the-effective-reproduction-number


datLong %>% filter(Date >= startDate) %>% 
  ggplot() + 
  geom_bar(aes(y=incident_cases, x= Date, fill = myDay), stat = "identity") +
  theme_lucid()+
  scale_fill_material_c()+ 
  theme(legend.position = "none")

datLong %>% filter(Date >= startDate) %>% 
  ggplot() + 
  geom_bar(aes(y=cumulative_cases, x= Date, fill = myDay), stat = "identity") +
  theme_lucid()+
  scale_fill_material_c()+ 
  theme(legend.position = "none")




# The SIR  Model





# number of people living in Germany
#accoring to https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/zensus-geschlecht-staatsangehoerigkeit-2019.html

numPeople = 1000*83149.3
sirStartDate = min(datLong$Date)


init = c(S= numPeople - datLong$cumulative_cases[1],
         I = datLong$cumulative_cases[1],
         R = 0)

# first we need a function which translates the ODE's stated in the SIR model

SIR = function(time, state, parameters) {
  par = as.list(c(state, parameters))
  with(par, {
    dS = -beta * I * S/numPeople
    dI = beta * I * S/numPeople - gamma * I
    dR = gamma * I
    return(list(c(dS, dI, dR)))
  })
}

RSS = function(parameters) {
  names(parameters) = c("beta", "gamma")
  out = ode(y = init, times = datLong$myDay, func = SIR, parms = parameters)
  fit = out[, 3]
  return(sum((datLong$cumulative_cases - fit)^2))
}


Opt = optim(c(0.5, 0.5), 
             RSS, 
             method = "L-BFGS-B", 
             lower = c(0,0), 
             upper = c(1, 1)
             )
t = 1:100

optParams = set_names(Opt$par, "beta", "gamma")

fittedCumulativeIncidenceDf = data.frame(ode(y = init, 
                                             times = t, 
                                              func = SIR, 
                                             parms = optParams)) %>% 
  mutate(Date = min(datLong$Date) + days(time-1))


dat = left_join(fittedCumulativeIncidenceDf, datLong ,by=c("Date"))


dat %>% select(Date, cumulative_cases, I) %>% 
  rename(cCases =cumulative_cases, fittedCCases = I) %>% 
  pivot_longer(-Date) %>% 
  ggplot(aes(x=Date, y=value, colour = name)) +
  geom_line() +
  geom_point(shape = "x") +
  labs(title = glue("SIR-Modell based projection of COVID-19 infections in {datLong$country[1]}"),
       subtitle = glue("data ranging from {min(dat$Date)} to {max(dat$Date)}"),
       caption = glue("fitted data from SIR model,\n
                      computed R0 ={round(optParams[1]/optParams[2],2)},\n
                      fitted point of reduce: At {dat$Date[which(dat$I==max(dat$I))]} with {humanNumbers(round(dat$I[which(dat$I==max(dat$I))]))} infections"),
       y = "Cummulative Cases") +
  
  scale_y_continuous(labels = humanNumbers)+
  theme_lucid()+
  scale_color_material_d() +
  facet_zoom( y = name =="cCases", 
              xlim = c(min(datLong$Date), max(datLong$Date)), 
              horizontal = FALSE)

gerIncidentDf = datLong %>% 
  select(Date,incident_cases) %>%
  filter(Date >= as.Date("2020-01-28")) %>% 
  rename(local = incident_cases, 
         dates = Date)

gerIncidentDf$imported = 0
gerIncidentDf$imported[1] = gerIncidentDf$local[1]
gerIncidentDf$local[1] = 0

epiEstimObj = estimate_R(gerIncidentDf, 
                         method = "parametric_si", 
                         config = make_config(list(mean_si = 4.4,
                                                   std_si = 3.0))
           )
# okay the plots are not what we want them to be, so we need to fight a bit

pInc = gerIncidentDf %>% pivot_longer(-dates) %>% 
  ggplot() + 
  geom_bar(aes(y=value, x= dates, fill = name), stat = "identity") +
  labs(title = "Incidents in Germany")+
  theme_lucid()+
  scale_fill_flat_d()


pSI = plot(epiEstimObj, "SI") +theme_lucid()
pRi = plot(epiEstimObj, "R") + theme_lucid() 
(pInc|pSI)/pRi

pRi


