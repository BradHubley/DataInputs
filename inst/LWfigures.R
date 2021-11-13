
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
