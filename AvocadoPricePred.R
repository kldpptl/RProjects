#  Import Packages
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, skimr, GGally, plotly, viridis, caret, randomForest, e1071, rpart, 
                xgboost, h2o, corrplot, rpart.plot, corrgram, ggplot2, highcharter, 
                ggthemes, psych, scales, treemap, treemapify, repr, cowplot, magrittr, ggpubr,
                RColorBrewer, plotrix, ggrepel, tidyverse, gridExtra, lubridate)

#install.packages("knitr")
#library(knitr)
#install.packages("tinytex")
#tinytex::install_tinytex()


library(tidyverse)
library(skimr)
library(GGally)
library(viridis)
library(caret)
library(e1071)
library(rpart)
library(xgboost)
library(corrplot)
library(corrgram)
library(ggplot2)
library(ggthemes)
library(psych)
library(scales)
library(treemap)
library(repr)
library(cowplot)
library(magrittr)
library(ggpubr)
library(RColorBrewer)
library(plotrix)
library(ggrepel)
library(gridExtra)
library(lubridate)
library(tibbletime)
library(reshape2)
library(tidyr)
library(ggpubr)
library(grid)
library(smooth)
library(forecast)
library(fpp2)



list.files(path = "../AvacadoPricePattern")

df <- read.csv("../AvacadoPricePattern/data/avocado.csv")

original_df <- df

#Types of Avacados
levels(factor(df$type))


#Avacado Price by Type chart
options(repr.plot.width=8, repr.plot.height=4)

ggplot(df, aes(x=AveragePrice, fill=type)) + geom_density() + facet_wrap(~type) + theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5), legend.position="bottom") + labs(title="Avocado Price by Type") + scale_fill_brewer(palette="Set2")

vol_type <- df %>% group_by(type) %>% summarise(avg.vol=mean(Total.Volume))  %>% mutate(pct=prop.table(avg.vol) * 100) 

vol_type

# Change the date column from factor to date
df$Date <- as.Date(df$Date, "%Y-%m-%d")
class(df$Date)

# Sort the dates
df <- df[order(as.Date(df$Date, format="%Y-%m-%d")),]


price_trend <- df %>% select(Date, AveragePrice, type) %>%
  ggplot(aes(x=Date, y=AveragePrice)) + geom_area(aes(color=type, fill=type), alpha = 0.3, position = position_dodge(0.8)) + 
  theme_minimal() +  scale_color_manual(values = c("#ED7921", "#62BE51")) + scale_fill_manual(values = c("#FD833E", "#B8FC5F"))

price_trend

# Create a Facet Wrap for each product
ggplot(data = df, aes(x = Date, y = AveragePrice, col=type)) +
  geom_line() +
  facet_wrap(~ type) + theme_minimal() + theme(legend.position="bottom")

# Filter by type
organic <- df %>% select(Date, AveragePrice, type, Total.Volume) %>% filter(type == "organic")
conventional <- df %>% select(Date, AveragePrice, type, Total.Volume) %>% filter(type == "conventional")

organic <- as_tbl_time(organic, index=Date)
organic <- as_period(organic, '1 month')


# Conventional Avocadoes
conventional <- as_tbl_time(conventional, index=Date)
conventional <- as_period(conventional, '1 month')

head(conventional)

# Now let's show monthly avocadoes price


options(repr.plot.width=8, repr.plot.height=6)
conventional_monthly <- conventional %>%
  ggplot(aes(x=Date, y=AveragePrice)) + geom_line(color="#7FB3D5") + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#D5D8DC")) + 
  labs(title="Conventional Avocados") + geom_hline(yintercept=max(conventional$AveragePrice), linetype="dashed", color = "red") + 
  geom_hline(yintercept=min(conventional$AveragePrice), linetype="dashed", color = "blue")

# Let's create a volume chart
conventional_volume <- conventional %>%
  ggplot(aes(x=Date, y=Total.Volume)) + geom_bar(stat='identity', fill="#7FB3D5", color="black") + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#D5D8DC")) + 
  geom_smooth(method="loess", color="red")

organic_monthly <- organic %>% 
  ggplot(aes(x=Date, y=AveragePrice)) + geom_line(color="#58D68D") + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#D5D8DC")) + 
  labs(title="Organic Avocados") + geom_hline(yintercept=max(organic$AveragePrice), linetype="dashed", color = "red") + 
  geom_hline(yintercept=min(organic$AveragePrice), linetype="dashed", color = "blue")

organic_volume <- organic %>%
  ggplot(aes(x=Date, y=Total.Volume)) + geom_bar(stat='identity', fill="#58D68D",color="black") + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#D5D8DC")) + geom_smooth(method="loess", color="red")

plot_grid(conventional_monthly, organic_monthly,conventional_volume, organic_volume, nrow=2, ncol=2)

# Seasonality patterns (let's start with this one)
head(df)
head(organic)

seasonal_df <- original_df

seasonal_df$month_year <- format(as.Date(original_df$Date), "%Y-%m")
seasonal_df$month <- format(as.Date(original_df$Date), "%m")
seasonal_df$year <- format(as.Date(original_df$Date), "%Y")


seasonal_df$monthabb <- sapply(seasonal_df$month, function(x) month.abb[as.numeric(x)])
seasonal_df$monthabb = factor(seasonal_df$monthabb, levels = month.abb)


# # Let's see if there are seasonal patterns with conventional avocadoes
ggplot(seasonal_df, aes(x = AveragePrice, fill = as.factor(year))) + 
  geom_density(alpha = .5) + 
  theme_economist() +
  facet_wrap(~ year) + theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F9E79F")) + 
  guides(fill = FALSE) + labs(title="Distribution of Prices by year", x = 'Average Price', y = 'Density') + 
  scale_fill_manual(values=c("#2E64FE", "#40FF00", "#FE642E", "#FE2E2E"))

# Detecting seasonality patterns
conv_patterns <- seasonal_df %>% select(monthabb, AveragePrice, type) %>% filter(type == "conventional") %>%
  group_by(monthabb) %>% summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=avg)) + geom_point(color="#F35D5D", aes(size=avg)) + geom_line(group=1, color="#7FB3D5") + 
  theme_economist() + theme(legend.position="none", plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F9E79F")) + 
  labs(title="Conventional Avocados", x="Month", y="Average Price")


org_patterns <- seasonal_df %>% select(monthabb, AveragePrice, type) %>% filter(type == "organic") %>%
  group_by(monthabb) %>% summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=avg)) + geom_point(color="#F35D5D", aes(size=avg)) + geom_line(group=1, color="#58D68D") + 
  theme_economist() + theme(legend.position="none", plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F9E79F")) + 
  labs(title="Organic Avocados", x="Month", y="Average Price")

plot_grid(conv_patterns, org_patterns, nrow=2)

# Hmm let's see if the Seasonality pattern is maintained each year.
options(repr.plot.width=8, repr.plot.height=6) 
conv_pat_yearly <- seasonal_df %>% select(year, monthabb, AveragePrice, type) %>% filter(type == "conventional", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>% summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=avg)) + geom_point(color="#5D6D7E") + geom_line(group=1, color="#F7DC6F") + facet_wrap(~as.factor(year)) + 
  theme_minimal() + theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7"), axis.text.x = element_text(angle = 90)) + 
  labs(title="Seasonal Fluctuations \n Convenctional Avocados", x="Month", y="Average Price")

org_pat_yearly <- seasonal_df %>% select(year, monthabb, AveragePrice, type) %>% filter(type == "organic", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>% summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=avg)) + geom_point(color="#5D6D7E") + geom_line(group=1, color="#E74C3C") + facet_wrap(~as.factor(year)) + 
  theme_minimal() + theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7"), axis.text.x = element_text(angle = 90)) + 
  labs(title="Seasonal Fluctuations \n Organic Avocados", x="Month", y="Average Price")

plot_grid(conv_pat_yearly, org_pat_yearly, nrow=2)

# Measuring standard deviation per month through each year by type of avocado.
std_conv <- seasonal_df %>% select(year, monthabb, AveragePrice, type) %>% filter(type == "conventional", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>% summarize(std=sd(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=std)) + 
  geom_point(aes(size=std), col="#5A96C6") +   # Draw points
  geom_segment(aes(x=monthabb, 
                   xend=monthabb, 
                   y=min(std), 
                   yend=max(std)), 
               linetype="dashed", 
               size=0.1) + 
  coord_flip() + 
  facet_wrap(~year) + 
  theme_tufte() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7"), legend.position="none") + 
  labs(title="Conventional Avocados \n Price Volatility",x="Months", y="Standard Deviation")


std_org <- seasonal_df %>% select(year, monthabb, AveragePrice, type) %>% filter(type == "organic", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>% summarize(std=sd(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=std)) + 
  geom_point(aes(size=std), col="#5AC67C") +   # Draw points
  geom_segment(aes(x=monthabb, 
                   xend=monthabb, 
                   y=min(std), 
                   yend=max(std)), 
               linetype="dashed", 
               size=0.1) + 
  coord_flip() + 
  facet_wrap(~year) + 
  theme_tufte() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7"), legend.position="none") + 
  labs(title="Organic Avocados \n Price Volatility",x="Months", y="Standard Deviation")

plot_grid(std_conv, std_org, nrow=2)

# Let's have a closer look how the price changes per month.
# filter by type and year

options(repr.plot.width=10, repr.plot.height=8) 

se <- function(x) sqrt(var(x)/length(x)) 

conv <- seasonal_df %>% select(year, monthabb, AveragePrice, type) %>% filter(type == "conventional", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>% 
  ggplot(aes(x=monthabb, y=AveragePrice, fill=monthabb), color="white") + geom_bar(width=1, stat='identity') + 
  geom_errorbar(aes(ymin = AveragePrice - se(AveragePrice), 
                    ymax = AveragePrice + se(AveragePrice), 
                    color = monthabb), 
                width = .2) + scale_y_continuous(breaks = 0:nlevels(seasonal_df$monthabb)) +
  facet_wrap(~year) + theme_minimal() + 
  theme(axis.ticks = element_blank(),
        axis.text.y=element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(), 
        plot.background=element_rect(fill="#FFF1E0"),
        legend.position="none", plot.title=element_text(hjust=0.5)) + 
  coord_polar() + labs(title="Seasonal cycle \n Conventional Avocados") + 
  scale_fill_manual(values=c('#57FCE0', '#57A6FC', '#3C546E', '#4AFA76', '#95CFA4', '#C0E436', '#F2A42D', '#F25F2D', '#F2442D',
                             '#AB4949', '#4950AB', '#4974AB'))


org <- seasonal_df %>% select(year, monthabb, AveragePrice, type) %>% filter(type == "organic", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>% 
  ggplot(aes(x=monthabb, y=AveragePrice, fill=monthabb), color="white") + geom_bar(width=1, stat='identity') + 
  geom_errorbar(aes(ymin = AveragePrice - se(AveragePrice), 
                    ymax = AveragePrice + se(AveragePrice), 
                    color = monthabb), 
                width = .2) + scale_y_continuous(breaks = 0:nlevels(seasonal_df$monthabb)) +
  facet_wrap(~year) + theme_minimal() + 
  theme(axis.ticks = element_blank(),
        axis.text.y=element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(), 
        plot.background=element_rect(fill="#FFF1E0"),
        legend.position="none", plot.title=element_text(hjust=0.5)) + 
  coord_polar() + labs(title="Seasonal cycle \n Conventional Avocados") + 
  scale_fill_manual(values=c('#57FCE0', '#57A6FC', '#3C546E', '#4AFA76', '#95CFA4', '#C0E436', '#F2A42D', '#F25F2D', '#F2442D',
                             '#AB4949', '#4950AB', '#4974AB'))



grid.arrange(conv, org, nrow = 2)

options(repr.plot.width=10, repr.plot.height=7) 
r_avg <- seasonal_df %>% group_by(year, monthabb) %>%  select(type, year, monthabb, AveragePrice) %>% 
  filter(type == "conventional", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>%
  summarize(avg=mean(AveragePrice))



structured_data <- spread_(r_avg, key="year", value="avg")


colnames(structured_data) <- c("Months", "First_year", "Second_year", "Third_year")


structured_data$first_pct <- NA
structured_data$second_pct <- NA

structured_data$first_pct <- (structured_data$Second_year - structured_data$First_year)/structured_data$First_year
structured_data$second_pct <- (structured_data$Third_year - structured_data$Second_year)/structured_data$Second_year


structured_data<- structured_data %>% 
  mutate(first_cond=ifelse(first_pct > 0, "Positive", "Negative"),
         second_cond=ifelse(second_pct > 0, "Positive", "Negative"))


firstp_change <- ggplot(structured_data) +
  geom_segment( aes(x=Months, xend=Months, y=First_year, yend=Second_year), color="#6E6A6A") +
  geom_point( aes(x=Months, y=First_year), color="#F74B4B", size=3 ) +
  geom_point( aes(x=Months, y=Second_year),color="#36ACD7", size=3 ) +
  coord_flip()+ 
  theme_economist() +
  theme(
    legend.position = "top",
    plot.title=element_text(hjust=0.5),
    plot.background=element_rect(fill="#F4F6F7")
  ) +
  labs(title="Conventional Avocado Price changes \n (2015 - 2016)", x="Months", y="Price",
       caption="Red: Year of 2015, Blue: Year of 2016")


secondp_change <- ggplot(structured_data) +
  geom_segment( aes(x=Months, xend=Months, y=Second_year, yend=Third_year), color="#6E6A6A") +
  geom_point( aes(x=Months, y=Second_year), color="#36ACD7", size=3 ) +
  geom_point( aes(x=Months, y=Third_year), color="#58FA58", size=3 ) +
  coord_flip()+ 
  theme_economist() +
  theme(
    legend.position = "top",
    plot.title=element_text(hjust=0.5),
    plot.background=element_rect(fill="#F4F6F7")
  ) +
  labs(title="Conventional Avocado Price changes \n (2016 - 2017)", x="Months", y="Price",
       caption="Blue: Year of 2016, Green: Year of 2017" )

# plot_grid(firstp_change, secondp_change, ncol=2)

first_pct_dif <- structured_data %>% select(Months, first_pct, first_cond) %>%
  ggplot(aes(fill=first_cond)) + geom_bar(stat='identity', aes(x=Months, y=round(first_pct,2) * 100), color="black") + 
  theme_economist() + theme(axis.text.x=element_text(angle=90), plot.background=element_rect(fill="#F4F6F7"), legend.position="bottom") + 
  labs(x="Month", y="% Difference") + 
  guides(fill=guide_legend(title="Diff Status")) + scale_fill_manual(values=c("#FB4D42", "#ADE175"))

second_pct_dif <- structured_data %>% select(Months, second_pct, second_cond) %>%
  ggplot(aes(fill=second_cond)) + geom_bar(stat='identity', aes(x=Months, y=round(second_pct,2) * 100), color="black") + 
  theme_economist() + 
  theme(axis.text.x=element_text(angle=90), plot.background=element_rect(fill="#F4F6F7"), legend.position="bottom") + labs(x="Month", y="% Difference") + 
  guides(fill=guide_legend(title="Diff Status")) + scale_fill_manual(values=c("#FB4D42", "#ADE175"))



plot_grid(firstp_change, secondp_change, first_pct_dif, second_pct_dif,  nrow=2, ncol=2)

# Organic avvocados

r_avg_org <- seasonal_df %>% group_by(year, monthabb) %>%  select(type, year, monthabb, AveragePrice) %>% 
  filter(type == "organic", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>%
  summarize(avg=mean(AveragePrice))



structured_data_org <- spread_(r_avg_org, key="year", value="avg")


colnames(structured_data_org) <- c("Months", "First_year", "Second_year", "Third_year")




structured_data_org$first_pct <- NA
structured_data_org$second_pct <- NA

structured_data_org$first_pct <- (structured_data_org$Second_year - structured_data_org$First_year)/structured_data$First_year
structured_data_org$second_pct <- (structured_data_org$Third_year - structured_data_org$Second_year)/structured_data$Second_year


structured_data_org<- structured_data_org %>% 
  mutate(first_cond=ifelse(first_pct > 0, "Positive", "Negative"),
         second_cond=ifelse(second_pct > 0, "Positive", "Negative"))


firstp_change_org <- ggplot(structured_data_org) +
  geom_segment( aes(x=Months, xend=Months, y=First_year, yend=Second_year), color="#6E6A6A") +
  geom_point( aes(x=Months, y=First_year), color="#F74B4B", size=3 ) +
  geom_point( aes(x=Months, y=Second_year),color="#36ACD7", size=3 ) +
  coord_flip()+ 
  theme_economist() +
  theme(
    legend.position = "top",
    plot.title=element_text(hjust=0.5),
    plot.background=element_rect(fill="#DCFCE6")
  ) +
  labs(title="Organic Avocado Price changes \n (2015 - 2016)", x="Months", y="Price",
       caption="Red: Year of 2015, Blue: Year of 2016")


secondp_change_org <- ggplot(structured_data_org) +
  geom_segment( aes(x=Months, xend=Months, y=Second_year, yend=Third_year), color="#6E6A6A") +
  geom_point( aes(x=Months, y=Second_year), color="#36ACD7", size=3 ) +
  geom_point( aes(x=Months, y=Third_year), color="#58FA58", size=3 ) +
  coord_flip()+ 
  theme_economist() +
  theme(
    legend.position = "top",
    plot.title=element_text(hjust=0.5),
    plot.background=element_rect(fill="#DCFCE6")
  ) +
  labs(title="Organic Avocado Price changes \n (2016 - 2017)", x="Months", y="Price",
       caption="Blue: Year of 2016, Green: Year of 2017" )

# plot_grid(firstp_change, secondp_change, ncol=2)

first_pct_dif_org <- structured_data_org %>% select(Months, first_pct, first_cond) %>%
  ggplot(aes(fill=first_cond)) + geom_bar(stat='identity', aes(x=Months, y=round(first_pct,2) * 100), color="black") + 
  theme_economist() + theme(axis.text.x=element_text(angle=90), plot.background=element_rect(fill="#DCFCE6"), legend.position="bottom") + 
  labs(x="Month", y="% Difference") + 
  guides(fill=guide_legend(title="Diff Status")) + scale_fill_manual(values=c("#FB4D42", "#ADE175"))

second_pct_dif_org <- structured_data_org %>% select(Months, second_pct, second_cond) %>%
  ggplot(aes(fill=second_cond)) + geom_bar(stat='identity', aes(x=Months, y=round(second_pct,2) * 100), color="black") + 
  theme_economist() + 
  theme(axis.text.x=element_text(angle=90), plot.background=element_rect(fill="#DCFCE6"), legend.position="bottom") + labs(x="Month", y="% Difference") + 
  guides(fill=guide_legend(title="Diff Status")) + scale_fill_manual(values=c("#FB4D42", "#ADE175"))



plot_grid(firstp_change_org, secondp_change_org, first_pct_dif_org, second_pct_dif_org,  nrow=2, ncol=2)

options(repr.plot.width=8, repr.plot.height=6) 

# Let's create a seasonal column and plot a point line chart by each year.
seasonal_df$season <- ifelse(seasonal_df$month %in% c("03", "04","05"), "Spring",
                             ifelse(seasonal_df$month %in% c("06","07" ,"08"), "Summer",
                                    ifelse(seasonal_df$month %in% c("09","10","11"), "Fall", "Winter")))


seasonality.plot.conventional <- seasonal_df %>% select(season, year, AveragePrice, type) %>% 
  filter(type == "conventional", year == c("2015", "2016", "2017")) %>%
  group_by(season, year) %>%
  summarize(avg=mean(AveragePrice)) %>% ggplot(aes(x=season, y=avg, color=season)) + geom_point(size=3) + 
  geom_segment(aes(x=season, 
                   xend=season, 
                   y=0, 
                   yend=avg)) + 
  coord_flip() + facet_wrap(~as.factor(year)) + theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7")) + 
  scale_color_manual(values=c("#a06a31", "#9bd16b", "#d1706b", "#3bbf9e")) + 
  labs(title="Conventional Avocados by Season", x="Season", y="Average Price") + 
  geom_text(aes(x=season, y=0.01, label= paste0("$ ", round(avg,2))),
            hjust=-0.5, vjust=-0.5, size=4, 
            colour="black", fontface="italic",
            angle=360)

seasonality.plot.organic <- seasonal_df %>% select(season, year, AveragePrice, type) %>% 
  filter(type == "organic", year == c("2015", "2016", "2017")) %>%
  group_by(season, year) %>%
  summarize(avg=mean(AveragePrice)) %>% ggplot(aes(x=season, y=avg, color=season)) + geom_point(size=3) + 
  geom_segment(aes(x=season, 
                   xend=season, 
                   y=0, 
                   yend=avg)) + 
  coord_flip() + facet_wrap(~as.factor(year)) + theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7")) + 
  scale_color_manual(values=c("#a06a31", "#9bd16b", "#d1706b", "#3bbf9e")) + 
  labs(title="Organic Avocados by Season", x="Season", y="Average Price") + 
  geom_text(aes(x=season, y=0.01, label= paste0("$ ", round(avg,2))),
            hjust=-0.5, vjust=-0.5, size=4, 
            colour="black", fontface="italic",
            angle=360)


plot_grid(seasonality.plot.conventional, seasonality.plot.organic, nrow=2)

head(seasonal_df)

options(repr.plot.width=8, repr.plot.height=7) 


regions_monthabb <- seasonal_df %>% select(monthabb, Total.Volume) %>% 
  group_by(monthabb) %>% summarize(avg.volume=mean(Total.Volume)) %>%
  ggplot(aes(x=monthabb, y=avg.volume)) + geom_bar(stat="identity",  fill="#81FF5F") + 
  coord_flip() + theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7"))



price_monthabb <- seasonal_df %>% select(monthabb, AveragePrice) %>% 
  group_by(monthabb) %>% group_by(avg.price=mean(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=avg.price))  + geom_bar(stat="identity", fill="#FF685F") + 
  coord_flip() + theme_minimal() + 
  theme(legend.position="none", plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7")) 


# The higher the price, the lower the volume.
seasonal_df$Volume_Price_Diff <- (seasonal_df$Total.Volume - seasonal_df$AveragePrice)

# If the month is higher there is a higher volume and thus a lower price.
volprice_diff <- seasonal_df %>% group_by(year, monthabb) %>% select(year, monthabb, type, Volume_Price_Diff) %>% 
  filter(year == c("2015", "2016", "2017")) %>%
  summarize(avg.diff=mean(Volume_Price_Diff)) %>%
  ggplot(aes(x=monthabb, y=avg.diff, group=year, color=year)) + geom_area(aes(fill=year)) + theme_minimal() +
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7"),
        legend.position="bottom", legend.background = element_rect(fill="#FFF9F5",
                                                                   size=0.5, linetype="solid", 
                                                                   colour ="black")) + 
  scale_fill_manual(values=c("#6EB3EA", "#89F058", "#FBA31C")) + scale_color_manual(values=c("#407EAF","#68B842", "#F1711E"))



pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

print(regions_monthabb, vp = vplayout(1, 1))
print(price_monthabb, vp = vplayout(1, 2))
print(volprice_diff, vp = vplayout(2, 1:2))

head(seasonal_df)

volume_conv <- seasonal_df %>% select(year, monthabb, Total.Volume, region, type) %>% 
  filter(type == "conventional", year == c("2015", "2016", "2017"), 
         region == c("Northeast", "SouthCentral", "MidSouth", "Southeast", "West")) %>% 
  group_by(year, monthabb, region) %>% summarize(avg.vol=mean(Total.Volume)) %>%
  ggplot(aes(x=monthabb, y=avg.vol)) + geom_point(color="#5D6D7E") + geom_line(group=1, color="#FE642E") +
  facet_grid(region ~ year)+ 
  theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F2F5A9"), axis.text.x = element_text(angle = 90)) + 
  labs(title="Market Volume \n Convenctional Avocados", x="Month", y="Average Volume")

volume_conv

volume_org <- seasonal_df %>% select(year, monthabb, Total.Volume, region, type) %>% 
  filter(type == "organic", year == c("2015", "2016", "2017"), 
         region == c("Northeast", "SouthCentral", "MidSouth", "Southeast", "West")) %>% 
  group_by(year, monthabb, region) %>% summarize(avg.vol=mean(Total.Volume)) %>%
  ggplot(aes(x=monthabb, y=avg.vol)) + geom_point(color="#5D6D7E") + geom_line(group=1, color="#819FF7") +
  facet_grid(region ~ year)+ 
  theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#E6E6E6"), axis.text.x = element_text(angle = 90)) + 
  labs(title="Market Volume \n Organic Avocados", x="Month", y="Average Volume")

volume_org

options(repr.plot.width=10, repr.plot.height=8) 

conv.price <- seasonal_df %>% select(type,year, monthabb, AveragePrice) %>% filter(type == "conventional", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>% summarize(avg=mean(AveragePrice))

org.price <- seasonal_df %>% select(type,year, monthabb, AveragePrice) %>% filter(type == "organic", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>% summarize(avg=mean(AveragePrice))

conv.price <- ts(conv.price$avg, start=2015, frequency=12)
org.price <- ts(org.price$avg, start=2015, frequency=12)


conv.plot <- autoplot(conv.price, color="#48a4ff") + 
  theme_economist() + theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F9E79F")) + 
  labs(title="Average Price by Month \n Conventional Avocados", y="Average Price")

org.plot <- autoplot(org.price, color="#58FA82") + 
  theme_economist() + theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F9E79F")) + 
  labs(title="Average Price by Month \n Organic Avocados", y="Average Price")


byyear.plot.conv <- ggseasonplot(conv.price, year.labels=TRUE, year.labels.left=TRUE) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F9E79F")) + 
  labs(title="Average Conventional A. Price by Year \n for each month", y="Average Price") + 
  scale_color_manual(values=c("#407EAF","#68B842", "#F1711E"))


byyear.plot.org <- ggseasonplot(org.price, year.labels=TRUE, year.labels.left=TRUE) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F9E79F")) + 
  labs(title="Average Organic A. Price by Year \n for each month", y="Average Price") + 
  scale_color_manual(values=c("#407EAF","#68B842", "#F1711E"))


plot_grid(conv.plot, byyear.plot.conv, org.plot, byyear.plot.org, nrow=2, ncol=2)

polar.conv <- ggseasonplot(conv.price, polar=TRUE) +
  ylab("Average Avocado Price") +
  ggtitle("Conventional Avocados \n Polar Plot") + theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7"),
        legend.position="bottom", legend.background = element_rect(fill="#FFF9F5",
                                                                   size=0.5, linetype="solid", 
                                                                   colour ="black")) + 
  scale_color_manual(values=c("#407EAF","#68B842", "#F1711E"))


polar.org <- ggseasonplot(org.price, polar=TRUE) +
  ylab("Average Avocado Price") +
  ggtitle("Organic Avocados \n Polar Plot") + theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7"),
        legend.position="bottom", legend.background = element_rect(fill="#FFF9F5",
                                                                   size=0.5, linetype="solid", 
                                                                   colour ="black")) + 
  scale_color_manual(values=c("#407EAF","#68B842", "#F1711E"))


plot_grid(polar.conv, polar.org, ncol=2)

# Now there are other ways to see the trends by month
options(repr.plot.width=8, repr.plot.height=6) 


monthly_conventional <- ggsubseriesplot(conv.price) +
  labs(title="Conventional Avocados", x="Months", y="Average Price") + 
  theme_economist() + theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#FEF5C7")) + 
  annotate("text", x = c(6), y = c(1.35), label = c("Upward trend \n (June - October)") , color="black", size=2.5 , angle=360, fontface="bold") + 
  geom_segment(aes(x = c(6) , y = 1.3, xend = c(6.5), yend = 1.2), colour='#D6665E', size=1,arrow = arrow(length = unit(0.35, "cm")))

monthly_organic <- ggsubseriesplot(org.price) +
  labs(title="Organic Avocados", x="Months", y="Average Price") + 
  theme_economist() + theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#FEF5C7")) + 
  annotate("text", x = c(6), y = c(1.85), label = c("Upward trend \n (June - September)") , color="black", size=2.5 , angle=360, fontface="bold") + 
  geom_segment(aes(x = c(6) , y = 1.8, xend = c(6.5), yend = 1.7), colour='#D6665E', size=1,arrow = arrow(length = unit(0.35, "cm")))


plot_grid(monthly_conventional, monthly_organic, nrow=2)


options(repr.plot.width=10, repr.plot.height=6)

seasonality_trends_conv <- window(conv.price, start=2015)
conv_plot_trends <- autoplot(seasonality_trends_conv) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5, size=12), plot.background=element_rect(fill="#F4F6F7")) + 
  labs(x="Year", y="Average Price", title="Conventional Avocados")

autocoor_conv <- ggAcf(org.price, lag=12) + 
  theme(plot.title=element_text(hjust=0.5, size=12), plot.background=element_rect(fill="#F4F6F7")) + labs(title="Autocorrelation for \n Conventional Avocados")


seasonality_trends_org <- window(org.price, start=2015)
org_plot_trends <- autoplot(seasonality_trends_org) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5, size=12), plot.background=element_rect(fill="#F4F6F7")) + 
  labs(x="Year", y="Average Price", title="Organic Avocados")

autocoor_org <- ggAcf(org.price, lag=12) + 
  theme(plot.title=element_text(hjust=0.5, size=12), plot.background=element_rect(fill="#F4F6F7")) + labs(title="Autocorrelation for \n Organic Avocados")

plot_grid(conv_plot_trends, autocoor_conv, org_plot_trends, autocoor_org, ncol=2, nrow=2)


options(repr.plot.width=10, repr.plot.height=6)

# Let's get it by week the average price
weekly_df <- original_df
weekly_df$week <- format(as.Date(original_df$Date), "%w")

conv.price.weekly <- weekly_df %>% select(type,year,AveragePrice) %>% filter(type == "conventional", year == c("2015", "2016", "2017")) 
org.price.weekly <- weekly_df %>% select(type,year,AveragePrice) %>% filter(type == "organic", year == c("2015", "2016", "2017")) 


weekly.conv.price <- ts(conv.price.weekly$AveragePrice, start=2015, frequency=12)
weekly.org.price <- ts(org.price.weekly$AveragePrice, start=2015, frequency=12)



weekly_trends_conv <- window(weekly.conv.price, start=2015)

weekly_trends_org <- window(weekly.org.price, start=2015)

conv_plot_weekly <- autoplot(weekly_trends_conv) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5, size=12), plot.background=element_rect(fill="#FFF1E0")) + 
  labs(x="Time", y="Average Price", title="Conventional Avocados \n (Weekly Time Series)")

org_plot_weekly <- autoplot(weekly_trends_org) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5, size=12), plot.background=element_rect(fill="#FFF1E0")) + 
  labs(x="Time", y="Average Price", title="Organic Avocados \n (Weekly Time Series)")

autocoor_conv <- ggAcf(weekly.conv.price, lag=156, fill="#48a4ff") + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5, size=12), plot.background=element_rect(fill="#FFF1E0")) + 
  labs(title="Autocorrelations by Weekly Lags")

autocoor_org <- ggAcf(weekly.org.price, lag=156, fill="#48a4ff") + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5, size=12), plot.background=element_rect(fill="#FFF1E0")) + 
  labs(title="Autocorrelations by Weekly Lags")

plot_grid(conv_plot_weekly, autocoor_conv,org_plot_weekly,  autocoor_org, ncol=2, nrow=2)

# Using Smoothing average
sma_conv <- sma(conventional$AveragePrice, h=10, silent=FALSE) + theme_economist()

library(fpp2)


# Let's declare our data as time series
conv <- df %>% select(Date, AveragePrice, type) %>% filter(type == "conventional")
org <- df %>% select(Date, AveragePrice, type) %>% filter(type == "organic")
# Organic Avocados
conventional <- as_tbl_time(conv, index=Date)
conventional <- as_period(conventional, '1 month')
conventional$type <- NULL
# Organic Avocados
organic <- as_tbl_time(org, index=Date)
organic <- as_period(organic, '1 month')
organic$type <- NULL

conv_ts <- ts(conventional[,2], start=c(2015, 1), frequency=12)
org_ts <- ts(organic[,2], start=c(2015, 1), frequency=12)
# The difference from month to month
# To remove the trend we take the first difference
differences_conv <- diff(conv_ts)

main_diff <- autoplot(differences_conv) + theme_minimal()

seasonality_diff <- ggseasonplot(differences_conv) + theme_minimal()

plot_grid(main_diff, seasonality_diff, nrow=2)

# ARIMA Model
# Y has trend unlike difference, it will take the difference behind the scenes d=1
# Stepwise will only use some models instead of all possible combinations
# approximation uses the model that approximates the best result to save time
arima_model_cv <- auto.arima(conv_ts, d=1, D=1, stepwise=FALSE, approximation=FALSE, trace=TRUE)
arima_model_or <- auto.arima(org_ts, d=1, D=1, stepwise=FALSE, approximation=FALSE, trace=TRUE)


print(summary(arima_model_cv))
checkresiduals(arima_model_cv) + theme_minimal()


options(repr.plot.width=10, repr.plot.height=7)

conv_forecast_sn <- autoplot(conv_ts) +
  autolayer(meanf(conv_ts, h=24),
            series="Mean", PI=FALSE) +
  autolayer(naive(conv_ts, h=24),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(conv_ts, h=24),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Conventional Avocado \n Seasonal Naive Method") +
  xlab("Date") + ylab("Price") + scale_color_manual(values=c("#FA5858", "#00BFFF", "#FF8000")) + 
  guides(colour=guide_legend(title="Forecast"))  + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7"))

org_forecast_sn <- autoplot(org_ts) +
  autolayer(meanf(org_ts, h=24),
            series="Mean", PI=FALSE) +
  autolayer(naive(org_ts, h=24),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(org_ts, h=24),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Organic Avocado \n Seasonal Naive Method") +
  xlab("Date") + ylab("Price") + scale_color_manual(values=c("#FA5858", "#00BFFF", "#FF8000")) + 
  guides(colour=guide_legend(title="Forecast"))  + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#DCFCE6"), legend.position="none")


plot_grid(conv_forecast_sn, org_forecast_sn, nrow=2)

conv_forecast_dr <- autoplot(conv_ts) +
  autolayer(meanf(conv_ts, h=24),
            series="Mean", PI=FALSE) +
  autolayer(naive(conv_ts, h=24),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(conv_ts, drift=TRUE, h=24),
            series="Drift", PI=FALSE) +
  ggtitle("Conventional Avocado \n Drift Method") +
  xlab("Date") + ylab("Price") + scale_color_manual(values=c("#ffff24", "#98fb98", "#ff6347")) + 
  guides(colour=guide_legend(title="Forecast"))  + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7"))

org_forecast_dr <- autoplot(org_ts) +
  autolayer(meanf(org_ts, h=24),
            series="Mean", PI=FALSE) +
  autolayer(rwf(org_ts, h=24),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(org_ts, drift=TRUE, h=24),
            series="Drift", PI=FALSE) +
  ggtitle("Organic Avocado \n Drift Method") +
  xlab("Date") + ylab("Price") + scale_color_manual(values=c("#ffff24", "#98fb98", "#ff6347")) + 
  guides(colour=guide_legend(title="Forecast"))  + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#DCFCE6"), legend.position="none")

plot_grid(conv_forecast_dr, org_forecast_dr, nrow=2)

rescv_nv <- residuals(naive(conv_ts))
p1 <- autoplot(rescv_nv, color="#b6ff6c") + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method \n Conventional Avocados") + theme_economist() +
  theme(plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#0D7680"),  
        axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
        axis.title=element_text(colour="white"))


resorg_nv <- residuals(naive(org_ts))
p2 <- autoplot(resorg_nv, color="#ffee6c") + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method \n Organic Avocados") + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#0D7680"), 
        axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
        axis.title=element_text(colour="white"))

plot_grid(p1, p2, nrow=2)

sqrt(0.05354)

forecast_cv <- forecast(arima_model_cv, h=24)
# Include means including the last 60 months in order to see closer the forecast.
autoplot(forecast_cv, include=60) + theme_minimal() + theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7"),
                                                            legend.position="bottom", legend.background = element_rect(fill="#FFF9F5",
                                                                                                                       size=0.5, linetype="solid", 
                                                                                                                       colour ="black")) + 
  labs(title="Forecasting using ARIMA model \n Conventional Avocados", x="Date", y="Price")

print(summary(forecast_cv))

forecast_org <- forecast(arima_model_or, h=24)
# Include means including the last 60 months in order to see closer the forecast.
autoplot(forecast_org, include=60) + theme_minimal() + theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#d0f0c0"),
                                                             legend.position="bottom", legend.background = element_rect(fill="#fffacd",
                                                                                                                        size=0.5, linetype="solid", 
                                                                                                                        colour ="black")) + 
  labs(title="Forecasting using ARIMA model \n Organic Avocados", x="Date", y="Price")

print(summary(forecast_org))