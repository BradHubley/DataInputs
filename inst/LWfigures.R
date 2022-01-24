library("ggeffects")
library(Mar.datawrangling)
library(tidyverse)
library(lubridate)
library(stringr)
library(sqldf)
library(grid)
library(SpatialHub)

#halibut catch by gear and 5yr bins
tiff(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/figures/hal_catch_gear.tiff",
     width=7, height=7, units="in", res=500)
ggplot(hal_datOR, aes(FISH_LENGTH, fill = GEAR, col = GEAR)) +
  geom_histogram(position = "identity", alpha = 0.2, bins = 50) +
  facet_wrap(~yr_bin, scales = "free") +
  theme_test()
dev.off()

#halibut catch by gear, quarter, and NAFO
tiff(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/figures/hal_gear_quarter_NAFO.tiff",
     width=7, height=7, units="in", res=500)
ggplot(hal_datOR, aes(FISH_LENGTH, stat = "count", fill = GEAR, col = GEAR)) +
  geom_histogram(position = "identity", alpha = 0.2) +
  facet_grid(NAFO~QUARTER) +
  theme_test()
dev.off()

#halibut catch by gear, quarter, and 5yr bins
tiff(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/figures/hal_gear_quarter_year.tiff",
     width=7, height=7, units="in", res=500)
ggplot(hal_datOR, aes(FISH_LENGTH, stat = "count", fill = GEAR, col = GEAR)) +
  geom_histogram(position = "identity", alpha = 0.2) +
  facet_grid(yr_bin~QUARTER) +
  theme_test()
dev.off()

#halibut catch by different fisheries/surveys over different NAFO
tiff(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/figures/length_trip_NAFO_quarter.tiff",
     width=6, height=6, units="in", res=500)
ggplot(hal_datOR, aes(FISH_LENGTH, count = "stat"))+
  geom_histogram(aes(col = TRIPCD_ID, fill = TRIPCD_ID)) +
  facet_wrap(~NAFO) +
  theme_test()
dev.off()

#predictions from "overall model" (FE: length, sex, NAFO; RE: year, tripcd_id) showing predictions by sex
tiff(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/figures/hal_predicted_sexoverall.tiff",
     width=5, height=5, units="in", res=500)
plot(sextest)+
  theme_test()
dev.off()
#predictions from "overall model" (FE: length, sex, NAFO; RE: year, tripcd_id) showing predictions by sex with unknowns removed
tiff(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/figures/hal_predicted_sexMF.tiff",
     width=5, height=5, units="in", res=500)
plot(sextest4)+
  theme_test()
dev.off()
#predictions from "overall model" (FE: length, sex, NAFO; RE: year, tripcd_id) showing predictions by NAFO (3, 4+5)
tiff(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/figures/hal_predicted_NAFO.tiff",
     width=5, height=5, units="in", res=500)
plot(NAFOtest)+
  theme_test()
dev.off()
#predictions from "overall model" (FE: length, sex, NAFO; RE: year, tripcd_id) showing predictions by quarter
tiff(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/figures/hal_predicted_quarter.tiff",
     width=5, height=5, units="in", res=500)
ggplot(Qtest, aes(x = x, y = predicted, colour = group)) +
  geom_line() +
  labs(
    colour = get_legend_title(Qtest),
    x = get_x_title(Qtest),
    y = get_y_title(Qtest),
    title = get_title(Qtest)
  )+
  theme_test()
dev.off()

#predictions from "overall model" (FE: length, sex, NAFO; RE: year, tripcd_id) showing predictions by month
cc <- scales::seq_gradient_pal("pink", "blue", "Lab")(seq(0,1,length.out=12))
tiff(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/figures/hal_predicted_month.tiff",
     width=5, height=5, units="in", res=500)
ggplot(monthtest, aes(x = x, y = predicted, colour = group)) +
  scale_colour_manual(values=cc)+
  geom_line(size = 1) +
  geom_text(data = monthtest %>% filter(x == last(x)), aes(label = group,
                                                           x = x,
                                                           y = predicted,
                                                           color = group), nudge_x = 10, ) +
  labs(
    colour = get_legend_title(monthtest),
    x = get_x_title(monthtest),
    y = get_y_title(monthtest),
    title = get_title(monthtest)
  )+
  theme_test()
dev.off()

#distribution of data by month (coloured by sex, for interest)
tiff(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/figures/hal_month.tiff",
     width=7, height=5, units="in", res=500)
ggplot(hal_datORmf, aes(FISH_LENGTH, stat = "count", fill = SEXCD_ID, col = SEXCD_ID)) +
  geom_histogram(position = "identity", alpha = 0.2) +
  facet_grid(~MONTH) +
  theme_test()
dev.off()

#predictions from model (FE: length, sex, decade, NAFO; RE:tripcd_id) showing predictions by decade
cc <- scales::seq_gradient_pal("pink", "blue", "Lab")(seq(0,1,length.out=5))

tiff(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/figures/hal_predicted_decade.tiff",
     width=10, height=10, units="in", res=500)
ggplot(dectest, aes(x = x, y = predicted, colour = group)) +
  scale_colour_manual(values=cc)+
  geom_line(size = 1) +
  geom_text(data = dectest %>% filter(x == last(x)), aes(label = group,
                                                         x = x,
                                                         y = predicted,
                                                         color = group), nudge_x = 10, ) +
  labs(
    colour = get_legend_title(dectest),
    x = get_x_title(dectest),
    y = get_y_title(dectest),
    title = get_title(dectest)
  )
dev.off()

#distribution of data by month and NAFO
tiff(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/figures/hal_month_NAFO.tiff",
     width=10, height=10, units="in", res=500)
ggplot(hal_datORmf, aes(FISH_LENGTH, stat = "count", fill = SEXCD_ID, col = SEXCD_ID)) +
  geom_histogram(position = "identity", alpha = 0.2) +
  facet_grid(NAFO~MONTH) +
  theme_test()
dev.off()

#predicted values of model with year as fixed effect for fish at length 80cm over time period
tiff(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/figures/hal_predicted_80cm.tiff",
     width=10, height=10, units="in", res=500)
ggplot(sizeselect, aes(x = group, y = predicted)) +
  geom_point() +
  labs(
    colour = get_legend_title(sizeselect),
    x = "YEAR",
    y = "FISH_WEIGHT",
    title = "Predicted Values for Fish at Length 80cm Over Time"
  ) +
  theme_test()
dev.off()


#same predicted values with year as fixed effect for RV data only
tiff(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/figures/hal_predicted_80cm_RV.tiff",
     width=10, height=10, units="in", res=500)
ggplot(sizeselectRV, aes(x = group, y = predicted)) +
  geom_point() +
  labs(
    colour = get_legend_title(sizeselectRV),
    x = "YEAR",
    y = "FISH_WEIGHT",
    title = "Predicted Values for Fish at Length 80cm Over Time from RV Data"
  ) +
  theme_test()
dev.off()

write.csv(trips_halcaught2, "C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/data/LWtriptypes.csv")

#histogram of season by year bin
tiff(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/figures/hal_gear_quarter_year.tiff",
     width=10, height=10, units="in", res=500)
ggplot(hal_datOR, aes(FISH_LENGTH, stat = "count", fill = GEAR, col = GEAR)) +
  geom_histogram(position = "identity", alpha = 0.2) +
  facet_grid(yr_bin~QUARTER) +
  theme_test()
dev.off()



###FIGURES FOR DOCUMENT

png(paste0(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/LW_QuarterNAFO.png"),
    width = 7, height = 11,units='in',pointsize=12, res=300,type='cairo')
ggplot(hal_datOR, aes(FISH_LENGTH, stat = "count", fill = TYPE, col = TYPE)) +
  geom_histogram(position = "identity", alpha = 0.2) +
  facet_grid(NAFO~QUARTER) +
  theme_test() +
  xlab("Fish Length (cm)") + ylab("Number of Halibut") +
  labs(fill="Data Source", color="Data Source") +
  scale_fill_discrete(labels = c("Observer", "RV Survey"))+
  scale_color_discrete(labels = c("Observer", "RV Survey")) +
  scale_color_manual(values=c("skyblue4", "firebrick"))+
  scale_fill_manual(values=c("skyblue4", "firebrick"))
dev.off()

png(paste0(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/LW_yrBin.png"),
    width = 7, height = 11,units='in',pointsize=12, res=300,type='cairo')
ggplot(hal_datOR, aes(FISH_LENGTH, stat = "count", fill = TYPE, col = TYPE)) +
  geom_histogram(position = "identity", alpha = 0.2) +
  facet_grid(yr_bin~.) +
  theme_test() +
  xlab("Fish Length (cm)") + ylab("Number of Halibut") +
  labs(fill="Data Source", color="Data Source") +
  scale_fill_discrete(labels = c("ISDB", "RV"))+
  scale_color_discrete(labels = c("ISDB", "RV"))+
  scale_color_manual(values=c("skyblue4", "firebrick"))+
  scale_fill_manual(values=c("skyblue4", "firebrick"))
dev.off()

###Figures for presentation

#raw data
hal_datAll$SEXCD_ID <- as.character(hal_datAll$SEXCD_ID)
tiff(paste0(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/LW_raw.tiff"),
    width = 5, height = 5,units='in',pointsize=12, res=300,type='cairo')
ggplot(hal_datAll, aes(FISH_LENGTH, FISH_WEIGHT)) +
  geom_point(aes(col=SEXCD_ID), alpha = 0.5, shape = 1) +
  theme_test() +
  xlab("Fish Length (cm)") + ylab("Fish Weight (g)") +
  labs(fill="Sex", color="Sex") +
  scale_color_manual(values=c("springgreen2", "royalblue3", "lightcoral"),
                     labels = c("Unsexed", "Male", "Female"))
dev.off()

#clean data
#sex
tiff(paste0(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/LW_sex.tiff"),
    width = 5, height = 5,units='in',pointsize=12, res=300,type='cairo')
hal_datOR$SEXCD_ID <- as.character(hal_datOR$SEXCD_ID)
ggplot(hal_datOR, aes(FISH_LENGTH, FISH_WEIGHT)) +
  geom_point(aes(col=SEXCD_ID), alpha = 0.5, shape = 1) +
  theme_test() +
  xlab("Fish Length (cm)") + ylab("Fish Weight (g)") +
  labs(fill="Sex", color="Sex") +
  scale_color_manual(values=c("springgreen2", "royalblue3", "lightcoral"),
                     labels = c("Unsexed", "Male", "Female"))
dev.off()

#NAFO
tiff(paste0(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/LW_NAFO.tiff"),
    width = 5, height = 5,units='in',pointsize=12, res=300,type='cairo')
ggplot(hal_datOR, aes(FISH_LENGTH, FISH_WEIGHT)) +
  geom_point(aes(col=NAFO_ZONE), alpha = 0.5, shape = 1) +
  theme_test() +
  xlab("Fish Length (cm)") + ylab("Fish Weight (g)") +
  labs(fill="NAFO", color="NAFO")+
  scale_color_manual(values=c("royalblue3", "firebrick"))
dev.off()

#Quarter
tiff(paste0(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/LW_QUARTER.tiff"),
    width = 5, height = 5,units='in',pointsize=12, res=600,type='cairo')
ggplot(hal_datOR, aes(FISH_LENGTH, FISH_WEIGHT)) +
  geom_point(aes(col=QUARTER), alpha = 0.7, shape = 1) +
  theme_test() +
  xlab("Fish Length (cm)") + ylab("Fish Weight (g)") +
  labs(fill="QUARTER", color="QUARTER") +
  scale_color_manual(values=c("gold1", "green4", "firebrick2", "black"))
dev.off()

#predictions for the 3 different potential fixed effects
#sex(unknowns removed)
unrm_haldat <- hal_datOR %>%
  filter(SEXCD_ID != "0")

mod_sex_unrm <- lmer(log(FISH_WEIGHT) ~ log(FISH_LENGTH) + QUARTER + SEXCD_ID + NAFO_ZONE + (1|YEAR) + (1|TRIPCD_ID), unrm_haldat)

sextest2 <- ggpredict(mod_sex_unrm, terms = c("FISH_LENGTH", "SEXCD_ID"), condition = c(QUARTER = 2, NAFO_ZONE = 3), type = "re") #since QUARTER and NAFO_ZONE are categorical I don't actually need to put these conditions in here because ggpredict() picks a reference level of each categorical predictor anyways

tiff(paste0(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/LW_predsex.tiff"),
    width = 5, height = 5,units='in',pointsize=12, res=300,type='cairo')
ggplot(sextest2, aes(x = x, y = predicted, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype=0, alpha = .15)+
  theme_test() +
  xlab("Fish Length (cm)") + ylab("Fish Weight (g)") +
  labs(fill="Sex", color="Sex") +
  scale_color_manual(values=c("royalblue3", "lightcoral"),
                     labels = c("Male", "Female")) +
  xlim(0,200) + ylim(0,125000)
dev.off()

#NAFO
NAFOtest <- ggpredict(mod_all1, terms = c("FISH_LENGTH", "NAFO_ZONE"), condition = c(QUARTER = 2, SEXCD_ID = c(0,1,2)), type = "re")

tiff(paste0(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/LW_predNAFO.tiff"),
    width = 5, height = 5,units='in',pointsize=12, res=300,type='cairo')
ggplot(NAFOtest, aes(x = x, y = predicted, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype=0, alpha = .15)+
  theme_test() +
  xlab("Fish Length (cm)") + ylab("Fish Weight (g)") +
  labs(fill="NAFO", color="NAFO") +
  scale_color_manual(values=c("royalblue3", "lightcoral")) +
  xlim(0,200) + ylim(0,125000)
dev.off()

#Quarter
Qtest <- ggpredict(mod_all1, terms = c("FISH_LENGTH", "QUARTER"), condition = c(NAFO = 3, SEXCD_ID = c(0,1,2)), type = "re")

tiff(paste0(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/LW_predQuarter.tiff"),
    width = 5, height = 5,units='in',pointsize=12, res=300,type='cairo')
ggplot(Qtest, aes(x = x, y = predicted, colour = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype=0, alpha = .15)+
  theme_test() +
  xlab("Fish Length (cm)") + ylab("Fish Weight (g)") +
  labs(fill="NAFO", color="Quarter") +
  xlim(0,200) + ylim(0,125000)
dev.off()

#clean data with model
final_model <- m5mf <- lmer(log(FISH_WEIGHT) ~ log(FISH_LENGTH) + (1|YEAR) + (1|TRIPCD_ID), hal_datOR)
model_line <- ggpredict(final_model, terms = c("FISH_LENGTH"), type = "re")
model_line <- model_line %>% rename("FISH_LENGTH" = "x", "FISH_WEIGHT" = "predicted")

tiff(paste0(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/LW_modelplot_all.tiff"),
    width = 5, height = 5,units='in',pointsize=12, res=300,type='cairo')
ggplot(NULL, aes(FISH_LENGTH, FISH_WEIGHT)) +    # Draw ggplot2 plot based on two data frames
  geom_point(data = hal_datOR, shape = 1, alpha = 0.5) +
  geom_line(data = model_line, col = "red") +
  theme_test() +
  xlab("Fish Length (cm)") + ylab("Fish Weight (g)") +
  xlim(0,260) + ylim(0,180000)
dev.off()

#raw data by source
hal_datsource <- hal_datAll %>%
  select("FISH_LENGTH", "FISH_WEIGHT", "TRIPCD_ID", "YEAR")

hal_datsource <- hal_datsource %>%
  mutate(TRIPCD_ID = replace(TRIPCD_ID, TRIPCD_ID %in% c(7001, 23, 14, 211, 7099, 230, 30, 49, 12, 7050, 7054,
                                                         7051, 70, 7002, 4511, 7055, 7060, 4320, 7061), "other_ISDB")) %>%
  mutate(TRIPCD_ID = replace(TRIPCD_ID, TRIPCD_ID %in% c(7057), "HalSurvey"))

tiff(paste0(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/LW_rawSource.tiff"),
    width = 5.5, height = 5,units='in',pointsize=12, res=300,type='cairo')
ggplot(hal_datsource, aes(FISH_LENGTH, FISH_WEIGHT)) +
  geom_point(aes(col=TRIPCD_ID), alpha = 0.5, shape = 1) +
  theme_test() +
  xlab("Fish Length (cm)") + ylab("Fish Weight (g)") +
  labs(fill="Trip Type", color="Trip Type") +
  scale_color_manual(values=c("black", "royalblue3", "firebrick"),
                     labels = c("Halibut Survey", "Other ISDB", "RV Surveys"))
dev.off()

#cleaned data by sex with model overlaid
tiff(paste0(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/LW_cleanSexMod.tiff"),
     width = 5.5, height = 5,units='in',pointsize=12, res=600,type='cairo')
ggplot(NULL, aes(FISH_LENGTH, FISH_WEIGHT)) +    # Draw ggplot2 plot based on two data frames
  geom_point(data = hal_datOR, aes(col=SEXCD_ID), alpha = 0.7, shape = 1, size = 1) +
  geom_line(data = model_line, col = "black") +
  theme_test() +
  xlab("Fish Length (cm)") + ylab("Fish Weight (g)") +
  xlim(0,260) + ylim(0,180000)  +
  labs(fill="Sex", color="Sex") +
  scale_color_manual(values=c("gold2", "steelblue1", "hotpink4"),
                     labels = c("Unsexed", "Male", "Female"))
dev.off()

#pie chart of data source
trips_halcaught2 <- trips_halcaught2 %>%
  mutate(TRIP_TYPE = replace(TRIP_TYPE, TRIP_TYPE %in% c(NA), "RV")) %>%
  mutate(TRIPCD_ID = replace(TRIPCD_ID, TRIPCD_ID %in% c(NA), "RV"))

tiff(paste0(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/LW_pie_triptype.tiff"),
     width = 5.5, height = 5,units='in',pointsize=12, res=300,type='cairo')
ggplot(data = trips_halcaught2, aes(x = "", y = n, fill = reorder(TRIP_TYPE, -n))) +
  geom_bar(stat = "identity", color = "black") +
  labs(fill= "Trip Type") +
  coord_polar("y") +
  theme_void()
dev.off()

hal_datsource_OR <- hal_datOR %>%
  select("FISH_LENGTH", "FISH_WEIGHT", "TRIPCD_ID", "YEAR")

hal_datsource_OR <- hal_datsource_OR %>%
  mutate(TRIPCD_ID = replace(TRIPCD_ID, TRIPCD_ID %in% c(7001, 23, 14, 211, 7099, 230, 30, 49, 12, 7050, 7054,
                                                         7051, 70, 7002, 4511, 7055, 7060, 4320, 7061), "other_ISDB")) %>%
  mutate(TRIPCD_ID = replace(TRIPCD_ID, TRIPCD_ID %in% c(7057), "HalSurvey"))

hal_datsource_list <- hal_datsource_OR %>% count(TRIPCD_ID)

tiff(paste0(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/LW_pie_source.tiff"),
     width = 5.5, height = 5,units='in',pointsize=12, res=300,type='cairo')
ggplot(data = hal_datsource_list, aes(x = "", y = -n, fill = TRIPCD_ID)) +
  geom_bar(stat = "identity", colour="black") +
  scale_fill_brewer(labels = c("Halibut Survey", "Other ISDB", "RV Surveys"))+
  geom_text(aes(label = c("Halibut Survey \n n = 33,898", "Other ISDB \n n = 19,920", "RV Survey \n n = 5,291")),
            position = position_stack(vjust = 0.5), size = 3)+
  coord_polar("y") +
  guides(fill=FALSE) +
  theme_void()
dev.off()

#plot showing outliers
hal_all_fig <- hal_datAll %>% select("FISH_LENGTH", "FISH_WEIGHT", "NAFO", "SEXCD_ID", "TRIPCD_ID", "YEAR", "FISH_NO", "SET_NO", "TRIP")
hal_OR_fig <- hal_datOR %>% select("FISH_LENGTH", "FISH_WEIGHT", "NAFO", "SEXCD_ID", "TRIPCD_ID", "YEAR",  "FISH_NO", "SET_NO", "TRIP")
outliers <- sqldf('SELECT * FROM hal_all_fig EXCEPT SELECT * FROM hal_OR_fig')
outliers <- anti_join(hal_all_fig, hal_OR_fig, by= c("FISH_NO", "SET_NO", "TRIP"))
hal_OR_fig <- hal_OR_fig %>% mutate(Data = "cleaned")
outliers <- outliers %>% mutate(Data = "outliers")
hal_OL_OR <- rbind.data.frame(hal_OR_fig, outliers)

tiff(paste0(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/LW_outliers.tiff"),
     width = 5.5, height = 5,units='in',pointsize=12, res=300,type='cairo')
ggplot(hal_OL_OR, aes(FISH_LENGTH, FISH_WEIGHT))+
  geom_point(aes(col=Data, shape = Data)) +
  theme_test() +
  xlab("Fish Length (cm)") + ylab("Fish Weight (g)") +
  scale_colour_manual(values=c("black", "red")) +
  scale_shape_manual(values= c(1, 16))
dev.off()

#looking at Q1 to Q3
Qpredictions <- Qtest %>%
  select("x", "predicted", "group")
Qpredictions$group <- sub("^", "Q", Qpredictions$group )
Qpredictions <- Qpredictions %>% spread("group", "predicted")

diff_Q1Q3 <- Qpredictions %>% mutate(diff = Q3-Q1)
diff_100 <- diff_Q1Q3 %>% filter(x==100)

Q_compare <- model_line %>%
  select("FISH_LENGTH", "FISH_WEIGHT") %>%
  rename("x" = "FISH_LENGTH")

Q_compare <- Q_compare %>%
  left_join(Qpredictions, by = "x")

diff_Q2OA <- Q_compare %>% mutate(diff = FISH_WEIGHT-Q2)
diff_1002OA <- diff_Q2OA %>% filter(x==100)

tiff(paste0(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/LW_Q3-Q1.tiff"),
     width = 5.5, height = 5,units='in',pointsize=12, res=300,type='cairo')
g <- ggplot(NULL, aes(x, diff)) +
  geom_line(data = diff_Q1Q3, col = "black") +
  annotate("segment", x = 100, xend = 100, y = 0, yend = 954.443159, colour = "red") +
  annotate("segment", x = 0, xend = 100, y = 954.443159, yend = 954.443159, colour = "red")+
  theme_test() +
  xlab("Fish Length (cm)") + ylab("Difference in Fish Weight (g)")+
  scale_x_continuous(expand = c(0, 0))
g+scale_y_continuous(breaks = c(954, 5000, 10000, 15000), labels = c("954", "5000", "10000", "15000"), expand = c(0,0),limits = c(0,20000))
dev.off()

tiff(paste0(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/LW_OA-Q2.tiff"),
     width = 5.5, height = 5,units='in',pointsize=12, res=300,type='cairo')
g <- ggplot(NULL, aes(x, diff)) +
  geom_line(data = diff_Q2OA, col = "blue") +
  annotate("segment", x = 100, xend = 100, y = 0, yend = 293.3355, colour = "red", linetype = "dashed", size = 0.25) +
  annotate("segment", x = 0, xend = 100, y = 293.3355, yend = 293.3355, colour = "red", linetype = "dashed", size = 0.25)+
  theme_test() +
  xlab("Fish Length (cm)") + ylab("Difference in Fish Weight (g)")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 205))
g+scale_y_continuous(breaks = c(293, 1000, 2000, 3000), labels = c("293", "1000", "2000", "3000"), expand = c(0,0),limits = c(0,3100))
dev.off()

tiff(paste0(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/LW_Qdiff.tiff"),
     width = 5.5, height = 5,units='in',pointsize=12, res=300,type='cairo')
g <- ggplot(NULL, aes(x, diff)) +
  geom_line(data = diff_Q1Q3, col = "black") +
  geom_line(data = diff_Q2OA, col = "blue") +
  annotate("segment", x = 100, xend = 100, y = 0, yend = 954.443159, colour = "red") +
  annotate("segment", x = 0, xend = 100, y = 954.443159, yend = 954.443159, colour = "red")+
  annotate("segment", x = 100, xend = 100, y = 0, yend = 293.3355, colour = "red") +
  annotate("segment", x = 0, xend = 100, y = 293.3355, yend = 293.3355, colour = "red")+
  theme_test() +
  xlab("Fish Length (cm)") + ylab("Difference in Fish Weight (g)")+
  scale_x_continuous(expand = c(0, 0))
g+scale_y_continuous(breaks = c(293, 954, 5000, 10000, 15000), labels = c("293","954", "5000", "10000", "15000"), expand = c(0,200),limits = c(0,20000))
dev.off()
#n by quarter
hal_datOR %>% group_by(QUARTER) %>% summarise(n=n())

#histrogram of fish length but quarter and year bin, coloured by gear type
tiff(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/figures/hal_gear_quarter_year.tiff",
     width=7, height=8, units="in", res=300)
ggplot(hal_datOR, aes(FISH_LENGTH, stat = "count", fill = GEAR, col = GEAR)) +
  geom_histogram(position = "identity", alpha = 0.2) +
  facet_grid(yr_bin~QUARTER) +
  theme_test() +
  xlab("Fish Length (cm)") + ylab("Number of Halibut") +
  labs(fill="Gear", color="Gear")
dev.off()

##fixed station map
xl=c(-69,-47)
yl=c(40,48)
stations_100 <- read.csv("C:/Users/harperd/Documents/Halibut/RDataVault/Survey/HalibutLonglineSurvey100FixedStations.csv")
png(file="C:/Users/harperd/Documents/Halibut/GitDataInputs/DataInputs/figures/FixedStations.png",
     width=7, height=6, units="in", res=300)
SpatialHub::bioMap(xlim = xl, ylim = yl, mapRes="MR", title = "Fixed Station Survey")
points(lat.DecDeg~lon.DecDeg,stations_100,pch=16, col = "black")
dev.off()
