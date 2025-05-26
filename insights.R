#visualize country and gdpp
library(ggplot2)
library(dplyr)
cnt_data<-readr::read_csv(".\\umldata\\Country-data.csv")
View(cnt_data)

#Countries below $1000 GDDP and top 40 countries based on GDPP
countries_below_1000 <- cnt_data %>%
   filter(gdpp < 1000)
View(countries)

top_forty_gdpp <- cnt_data %>%
   arrange(desc(gdpp))%>%
   head(40)
View(top_forty_gdpp)

ggplot(data = top_ten_gdp, aes(x = country, y = gdpp )) + 
   geom_bar(stat =  "identity")+ labs(title = "Top ten countries with the highest gdpp", x= "country", y= "gdpp") + ylim(0,max(cnt_data$gdpp)*1.1)+
   geom_hline(yintercept = mean(cnt_data$inflation), color="red", linetype = "dashed")+theme (legend.position = "none")

#which country has the Lowest life expectancy?
lowest_life_expentancy <- cnt_data %>%
   filter(life_expec == min(life_expec,na.rm = TRUE))
print(lowest_life_expentancy)
cnt_data(cnt_data$life_expec == "Haiti",)

countries_below_70 <- cnt_data %>%
   filter(life_expec < 70)
View(countries_below_70)


#what is the correlation between GDP per capita and life expectancy?
correlation <- cor(cnt_data$gdpp, cnt_data$life_expec,use = "pairwise.complete.obs")
print(correlation)
ggplot(cnt_data, aes(x = life_expec, y = gdpp, colour = "red" ))+
   geom_point()

#Which country has the lowest exports
lowest_imports <- cnt_data %>%
   filter(imports == min(imports, na.rm = TRUE))
print(lowest_imports)

#top ten country with the highest child mortality and relationship between income and child mortality
highest_child_mortality <- cnt_data %>%
   arrange(desc(child_mort))%>%
   head(10)
View(highest_child_mortality)
 

#countries with the highest and lowest total fertility rates
highest_total_fertility <- cnt_data %>%
   filter(total_fer == max(total_fer, na.rm = TRUE))
lowest_total_fertility <- cnt_data %>%
   filter(total_fer == min(total_fer, na.rm = TRUE))
print(highest_total_fertility)
print(lowest_total_fertility)

#relationship between total fertility rate and child mortality
correlation <- cor(cnt_data$total_fer, cnt_data$child_mort, use = "pairwise.complete.obs")
print(correlation)
ggplot(cnt_data,aes(x = total_fer, y = child_mort))+
   geom_point()+
   geom_smooth(method = "lm")











