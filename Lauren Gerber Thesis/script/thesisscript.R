#Install necessary libraries and download GDP and Democracy Data from R packages 
library(devtools)
library(tidyverse)
library(forcats)
devtools::install_github("vdeminstitute/vdemdata")
library(vdemdata)
library(car)
library(gtsummary)
library(WDI)
library(psych)
#set to not report in scientific notation
options(scipen = 5)

#Make a data-set with chosen countries' GDP per capita adjusted for inflation and purchasing power parity. 
#limit year to 2023, the end of survey collection. 
gdp<- WDI(indicator="NY.GDP.PCAP.PP.KD", country=c("USA", "CAN", "AUS", "DEU", "ZAF", "BRA"), start=2023, end=2023) 

#rename "country" to "country_name" so we can later merge the data. Rename gdp to a simpler name. 
gdp<- gdp %>% rename(country_name=country, gdp=NY.GDP.PCAP.PP.KD)

#Load the vdem data-set from the package. 
vdem<- vdem

#keep only chosen countries, limit the year to 2023, and keep only the liberal democracy index in the VDEM data-set. 
#Rename "United States of America" to "United States" to keep consistency across data-sets.

countries_to_keep= c("Brazil", "Canada", "Germany", "South Africa", "United States of America", "Australia")
vdem<- vdem %>% filter(country_name %in% countries_to_keep & year=="2023")
vdem<- vdem %>% select(country_name, v2x_libdem, v2x_libdem_codelow) %>% mutate(country_name=as.factor(country_name)) %>% 
  mutate(country_name=str_replace(country_name, "United States of America", "United States"))

#Load the conspiracy data-set. 
conspdata<- read_csv("/data/CCRS Dataset.csv")

#rename variables of interest to more intuitive names. 
conspdata<- conspdata %>% rename(country_name=ID_Q2, Age=ID_Q3, Gender=ID_Q4, Education=ID_Q5, SES=ID_Q8, Employment=ID_Q9, leftright=ID_Q10,
                                 polinterest=ID_Q11, satisfactiongov=ID_Q15, satisfactiondem=ID_Q16, trustgov=ID_Q18_1, trustpol=ID_Q18_2,
                                 trustelections=ID_Q18_4,eliteppldif=ID_Q23_3, Religiosity=ID_Q25, NCCmean=ID_Q30, conspbeliefavg=ID_Q32,
                                 govmurder=ID_Q32_1, worldcontrol=ID_Q32_2, govterrorism=ID_Q32_6, war=ID_Q32_7, govcrime=ID_Q32_10, 
                                 significantevents=ID_Q32_11, drugspublic=ID_Q32_13, infohiding=ID_Q32_14, conspmentalityavg=ID_Q33)

#make average political trust index- trust in government and trust in politicians (0-10, no trust-a lot of trust).
conspdata$average_trust <- rowMeans(conspdata[, c("trustgov", "trustpol")])

#check reliability of index- good levels of internal consistency (0.89).  
average_trust_index<- data.frame(conspdata$trustpol, conspdata$trustgov)
alphaindex<- alpha(average_trust_index)
summary(alphaindex)

#make political conspiracy index (0-4, definitely not true-definitely true).
#check reliability of index- good levels of internal consistency (0.82).

conspdata$average_polconspiracies <- rowMeans(conspdata[, c("worldcontrol", "war", "significantevents")])

polconspiracies_index<- data.frame(conspdata$worldcontrol,conspdata$war, conspdata$significantevents)
alphaindex2<- alpha(polconspiracies_index)
summary(alphaindex2)

#make a variable to drop non-democratic countries. 
drop= c("Morocco", "Lebanon")

#make categorical variables categorical and drop non-democratic countries. 
conspdata<- conspdata %>% mutate(Gender=as.factor(Gender), Education=as.factor(Education), 
                                 SES=as.factor(SES), Employment=as.factor(Employment), 
                                 Religiosity=as.factor(Religiosity)) %>% filter(!(country_name %in% drop))

#merge conspiracy data with vdem data.
merged_data<- left_join(conspdata, vdem, by="country_name")

#merge gdp data with merged data. 
merged_data2<- left_join(merged_data, gdp, by="country_name")

#check for duplicates.
duplicates_all <- merged_data2 %>% filter(duplicated(merged_data2))
head(duplicates_all)

#make object of variables to include in regression and graphs. 

regressiondata<- merged_data2 %>% select(average_polconspiracies, v2x_libdem, Gender, Age, Education, Employment,
                                SES, leftright, eliteppldif, average_trust, Religiosity, NCCmean, gdp, country_name) %>% mutate(country_name=as.factor(country_name))

#elite people difference currently measured (1-5 strong disagreement (high efficacy) -> strong agreement (low efficacy)). 
#reverse variable to make it more intuitive. 
regressiondata$eliteppldifrescaled<- 6 - regressiondata$eliteppldif
  
#preliminary correlation analysis taking into account ordinal rankings.
#exclude country name- not included in main analysis! Only in object for graphing reasons. 
corrdata<- regressiondata %>% select(average_polconspiracies, v2x_libdem, Gender, Age, Education, Employment,
                                     SES, leftright, eliteppldifrescaled, average_trust, Religiosity, NCCmean, gdp)
corrdata[] <- lapply(corrdata, function(x) as.numeric(as.character(x)))
corrmatrix<- cor(corrdata, method = "spearman", use = "pairwise.complete.obs")

#make plot of correlation matrix. 
library(corrplot)
quartz()
testRes = cor.mtest(corrmatrix, conf.level = 0.95)
corrplot(corrmatrix, method = 'number', type="upper", col = COL1('Blues', 10), number.cex = 0.6, p.mat = testRes$p, sig.level = 0.10, 
         pch.col = '#F8766D33', diag=FALSE)


#rename categories names for regression data. 
regressiondata<- regressiondata %>% mutate(Gender=fct_recode(Gender, "Female"="0", "Male"= "1"), Education=fct_recode(Education, "None"="1", "Elementary"="2",
                                                                                                            "Secondary"="3", "University"="4", "Postgrad"="5"),
                                 SES=fct_recode(SES, "Lower-Class"="1", "Middle-Class"="2", "Upper-Class"="3"), Employment=fct_recode(Employment, "Full-Time"="1",
                                                                                                                                      "Part-Time"="2", "Retired"="3", 
                                                                                                                                      "Unemployed"="4", "Student"="5", "Other"="9"), 
                                 Religiosity=fct_recode(Religiosity, "Not Religious"="0", "Religious"="1"))


#check how many NAs in each variable, NAs could be meaningful for gender/religiosity. 
regressiondata %>% summarise_all(~ sum(is.na(.)))
#gender=100, Religiosity=405

#calculate percentage of NAs. 
colMeans(is.na(regressiondata))
#Mostly insignificant, 6.6% Religiosity. 

#An NA answer for gender might not always mean non-binary... but we should create a "don't know" category for religiosity. 
regressiondata$Religiosity <- fct_na_value_to_level(regressiondata$Religiosity, level = "Other/Don't Know")

#reset reference category in Education from "none" to "university".
regressiondata<- regressiondata %>% mutate(Education=fct_relevel(Education, "University"))

#multiply liberal democracy index by 100 for ease of interpretation.
regressiondata$v2x_libdemrescaled<- regressiondata$v2x_libdem *100 

#bivariate analysis. 
lm1<- lm(formula= average_polconspiracies ~ v2x_libdemrescaled, data=regressiondata) 

#table of bivariate analysis: 
library(sjPlot)
modelone<-tab_model(lm1, show.se=TRUE, show.p=TRUE, show.fstat = TRUE, show.obs=TRUE,
          string.est="Coefficients", string.pred = "Variables", string.p="P-value", string.se = "Std. Error", emph.p=TRUE,
          pred.labels = c("Intercept", "Quality of Democracy"), dv.labels = "Average Belief in Political Conspiracy Theories",
          p.style="numeric_stars", CSS = list(css.thead = "text-align:center; padding-bottom:0.2cm;", css.tdata="padding:0.11cm;"))
          
#analysis with controls. 
lm2<- lm(formula=average_polconspiracies~v2x_libdemrescaled + Gender + Age+ Education+ Employment+ SES+ leftright+ average_trust+ 
           Religiosity+NCCmean + gdp, data=regressiondata)

#table of exclusionary strategy: 
modeltwo<-tab_model(lm2, show.se=TRUE, show.p=TRUE, show.fstat = TRUE, show.obs=TRUE,  show.reflvl = TRUE,
                    string.est="Coefficients", string.pred = "Variables", string.p="P-value", string.se = "Std. Error", emph.p=TRUE,
                    pred.labels = c("Intercept", "Quality of Democracy", "Male", "Age", "No Education", "Elementary Education", "Secondary Education",
                                    "Post-Grad Degree", "Employed Part-Time", "Retired", "Unemployed", "Student", "Other Employment", 
                                    "Middle-Class", "Upper Class", "Left-Right Scale Placement", "Average Political Trust", "Religious", "NA Religion", 
                                    "Average Need For Cognitive Closure", "GDP"), 
                    dv.labels = "Average Belief in Political Conspiracy Theories",
                    p.style="numeric_stars", CSS = list(css.thead = "text-align:center; padding-bottom:0.2cm;", css.tdata="padding:0.11cm;"))


#normalize data to explore effect size of predictors. 
scaled_merged_data<- regressiondata
numeric_cols <- sapply(scaled_merged_data, is.numeric)
scaled_merged_data[numeric_cols] <- scale(scaled_merged_data[numeric_cols])

#effect size/standardized model.
lm3<- lm(formula=average_polconspiracies~v2x_libdemrescaled + Gender + Age+ Education+ Employment+ SES+ leftright+ average_trust+ 
           Religiosity+NCCmean + gdp, data=scaled_merged_data)

#table without dummy variables for effect size: 
standardizedmodel<-tab_model(lm3, terms=c("v2x_libdemrescaled", "Age", "leftright", "average_trust", "NCCmean", "gdp"), show.se=TRUE, show.p=TRUE, show.fstat = TRUE, show.obs=TRUE,
                    string.est="Coefficients", string.pred = "Variables", string.p="P-value", string.se = "Std. Error", emph.p=TRUE, 
                    pred.labels = c("Quality of Democracy", "Age", "Left-Right Scale Placement", "Average Political Trust",  
                                    "Average Need For Cognitive Closure", "GDP"), 
                    dv.labels = "Average Belief in Political Conspiracy Theories",
                    p.style="numeric_stars", CSS = list(css.thead = "text-align:center; padding-bottom:0.2cm;", css.tdata="padding:0.11cm;"))


#diagnostic plots for multivariate linear regression: 
par(mfrow = c(2, 2))
plot(lm2)

#testing for multicollinearity.  
vif(lm2)

#na's dropped dataset:
#In order to run a mediation analysis, we need to have regression models of the same size. So dropping all na's before for these specific variables is necessary: 
nasdropped<- regressiondata %>% drop_na(v2x_libdemrescaled, average_polconspiracies, eliteppldifrescaled, Gender, Age, Education, 
                                        Employment, SES, leftright, average_trust, Religiosity, NCCmean, gdp) 

#inclusionary strategy using Barron & Kenny's model:

#step one, total effect of x on y taking into account alternative explanations: 
summary(lm2)

#Step two: does X have a significant relationship with M (X->M)?  
eliteppldif<- lm(formula=eliteppldifrescaled~v2x_libdemrescaled + Gender + Age+ Education+ Employment+ SES+ leftright+ average_trust+ 
                   Religiosity+NCCmean + gdp, data=nasdropped)

#Step three: how is the relationship between X&Y affected with M? What is the relationship between M and Y?
mediationmodel<- lm(average_polconspiracies~v2x_libdemrescaled + eliteppldifrescaled+ Gender + Age 
                    + Education+ Employment+ SES+ leftright+ average_trust+ 
                      Religiosity+NCCmean + gdp, data=nasdropped)
#diagnostic plots. Relatively normal, slight tail skew but sample is large enough for it to be okay. 
plot(eliteppldif)
plot(mediationmodel)

#table for inclusionary strategy: 
inclusionarystrategytable<- tab_model(eliteppldif, mediationmodel, show.se=TRUE, show.p=TRUE, show.fstat = TRUE, show.obs=TRUE, collapse.ci=FALSE,
          string.est="Coefficients", string.pred = "Variables", string.p="P-value", string.se = "Std. Error", emph.p=TRUE,
          pred.labels = c("Intercept", "Quality of Democracy", "Male", "Age", "No Education", "Elementary Education", "Secondary Education",
                          "Post-Grad Degree", "Employed Part-Time", "Retired", "Unemployed", "Student", "Other Employment", 
                          "Middle-Class", "Upper Class", "Left-Right Scale Placement", "Average Political Trust", "Religious", "NA Religion", 
                          "Average Need For Cognitive Closure", "GDP", "External Political Efficacy"), 
          dv.labels = c("External Political Efficacy", "Average Belief in Political Conspiracy Theories"), 
          p.style="numeric_stars", CSS = list(css.thead = "text-align:center; padding-bottom:0.2cm;", css.tdata="padding:0.11cm;",
                                              css.depvarhead= "+padding-right: 0.2cm;", css.col5= "+padding-right: 0.88cm;", css.col1="padding-right: 0.1cm;"))

#formal mediation test
library(mediation)
mediationanalysis<- mediate(eliteppldif, mediationmodel, treat="v2x_libdemrescaled", mediator="eliteppldifrescaled", boot=TRUE, sims=500)
summary(mediationanalysis)

#Although none of the variables affect the QOD coefficient, testing if NCC explains better. 
#Step One- is quality of democracy related with NCC? It is not. 
NCCmeanmediation<- lm(formula=NCCmean~v2x_libdemrescaled + Gender + Age+ Education+ Employment+ SES+ leftright+ average_trust+ 
                   Religiosity+eliteppldifrescaled+gdp, data=nasdropped)



#robust clustered standard errors test.
library(sandwich)
library(lmtest)

clustered_se<- vcovCL(lm2, cluster = ~country_name)
coeftest(lm2, vcov = clustered_se)

#doesn't change anything 



#EXTRA GRAPHS AND TABLES SECTION: 

#Sociodemographic descriptive statistics tables:  
sampletable<- regressiondata %>% tbl_summary(include=c(Education, SES, Employment, Age, Gender), 
                                        statistic= list(all_continuous()~"{mean}", all_categorical()~"{p}%"), 
                                        type = all_continuous() ~ "continuous", missing="no") %>% bold_labels() %>% bold_levels()

countrytable<- table<- regressiondata %>% tbl_summary(include=c(Education, SES, Employment, Age, Gender), 
                                                 statistic= list(all_continuous()~"{mean}", all_categorical()~"{p}%"), 
                                                 by=country_name, missing="no") %>% bold_labels() %>% bold_levels() 

#other control variables descriptive statistic tables: 
sampletableothervariables<- regressiondata %>% tbl_summary(include=c(leftright, average_trust, Religiosity, NCCmean), 
                                        statistic= list(all_continuous()~c("{mean}", "{median}", "{p25}, {p75}"), all_categorical()~"{p}%"), 
                                        label = list(leftright~"Left-Right Scale", average_trust ~ "Average Political Trust", NCCmean ~ "Average Need for Cognitive Closure"), 
                                        type = all_continuous() ~ "continuous2", missing="no") %>% bold_labels() %>% bold_levels()

countrytableothervariables<- regressiondata %>% tbl_summary(include=c(leftright, average_trust, Religiosity, NCCmean), 
                                                           statistic= list(all_continuous()~c("{mean}", "{median}", "{p25}, {p75}"), all_categorical()~"{p}%"), 
                                                           label = list(leftright~"Left-Right Scale", average_trust ~ "Average Political Trust", NCCmean ~ "Average Need for Cognitive Closure"), 
                                                           by=country_name, type=all_continuous()~"continuous2", missing="no") %>% bold_labels() %>% bold_levels()


#table for dependent variable and mediating variable. 
mainvariablestable<- regressiondata %>% tbl_summary(include=c(average_polconspiracies, eliteppldifrescaled), 
                                                           statistic= list(all_continuous()~c("{mean}", "{median}", "{p25}, {p75}")), 
                                                           label = list(average_polconspiracies~"Average Political Conspiracy Belief", eliteppldifrescaled~ "External Political Efficacy Score"), 
                                                           by=country_name, 
                                                           type = list(average_polconspiracies~"continuous2", eliteppldifrescaled ~ "continuous2"), 
                                                           missing="no") %>% bold_labels() %>% bold_levels()
                                                           

#plot for country democracy scores. 
#first create data-set with only one value of democracy score per country. 
distinct_data<- regressiondata %>% distinct(country_name, v2x_libdem, .keep_all = TRUE)


#quality of democracy graph. 
library(ggthemes)
ggplot(distinct_data, aes(x = reorder(country_name, v2x_libdem), y = factor(round(v2x_libdem, 2)), fill = factor(round(v2x_libdem, 2)))) +
  geom_bar(stat = "identity") +
  ylab("Liberal Democracy Score") +
  labs(title="Liberal Democracy Scores (0-1) by Country", subtitle="Measured in the Year 2023") +
  coord_flip() +
  theme_tufte()+ 
  scale_fill_economist() +
  theme(legend.position = "none",  plot.title = element_text(hjust = 0.5, size = 12), plot.subtitle=element_text(hjust = 0.5, size = 10),axis.title.y = element_blank())


#plots of dependent and mediating variable.
#create data-set that takes the average of average pol conspiracy beliefs/political efficacy scores for each country. 
averages_by_country <- regressiondata %>%
  group_by(country_name) %>%
  summarise(
    average_polconspiraciescountry = mean(average_polconspiracies, na.rm = TRUE),
    avg_pol_efficacy = mean(eliteppldifrescaled, na.rm = TRUE))


#average political conspiracy belief graph.  
ggplot(averages_by_country, aes(x = reorder(country_name, average_polconspiraciescountry), y = factor(round(average_polconspiraciescountry, 2)), fill = factor(round(average_polconspiraciescountry, 2)))) +
  geom_bar(stat = "identity") +
  ylab("Average Belief in Political Conspiracy Theories") +
  labs(title="Average Political Conspiracy Beliefs by Country", subtitle="Measured on a Scale from 0-4 (low->high)")+
  coord_flip() +
  theme_tufte()+ 
  scale_fill_brewer(palette = "BuGn") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 12), plot.subtitle=element_text(hjust = 0.5, size = 10), axis.title.y = element_blank())


#average external political efficacy graph. 
ggplot(averages_by_country, aes(x = reorder(country_name, avg_pol_efficacy), y = factor(round(avg_pol_efficacy, 2)), fill = factor(round(avg_pol_efficacy, 2)))) +
  geom_bar(stat = "identity") +
  ylab("Average External Political Efficacy") +
  labs(title="Average External Political Efficacy by Country", subtitle="Measured on a Scale from 1-5 (low->high)")+
  coord_flip() +
  theme_tufte()+ 
  scale_fill_brewer(palette = "BuPu") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 12), plot.subtitle=element_text(hjust = 0.5, size = 10), axis.title.y = element_blank())

#visualizing final model 
library(visreg)
visreg(mediationmodel, "v2x_libdemrescaled", # specifying x-variable to visualize
       ylab = "Average Political Conspiracy Belief (low->high)", 
       xlab = "Quality of Democracy Score 0-100", 
       gg = TRUE, # creating a ggplot
       band = TRUE) + 
  theme(plot.title = element_text(hjust = 0.5, size = 10)) + 
  ggtitle("Predicted Relationship Between Democratic Quality and Political Conspiracy Beliefs")
 

