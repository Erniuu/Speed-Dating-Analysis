library(mosaic)
library(gridExtra)
library(bnstruct)

set.seed(123)

data = read.csv("/Users/cindykang/Documents/Fall 2018/MS&E 226/Project/Speed-Dating-Experiment/Speed_Dating_Data.csv", stringsAsFactors = F, na.strings = c("", "NA", "NULL"))
train_ind = sample(1:nrow(data), round(.8 * nrow(data)))
train = data[train_ind, ]
test = data[-train_ind, ] # Set aside test dataset

# Create dataset of objective compatibility attributes

obj_attr = train %>% 
  select(iid, id, wave, partner, pid, gender, age, age_o, race, race_o, samerace, int_corr, income, goal, go_out, mn_sat, imprace, imprelig, like, dec) %>% 
  left_join(train %>% select(iid, income, mn_sat, go_out, goal) %>% dplyr::rename(income_o = income, mn_sat_o = mn_sat, go_out_o = go_out, goal_o = goal) %>% unique, by = c("pid" = "iid")) %>%
  mutate(income = as.numeric(gsub(",", "", .$income)),
         income_o = as.numeric(gsub(",", "", .$income_o)),
         mn_sat = as.numeric(gsub(",", "", .$mn_sat)),
         mn_sat_o = as.numeric(gsub(",", "", .$mn_sat_o)),
         go_out = as.numeric(derivedFactor("1" = (go_out == 7),
                                           "2" = (go_out == 6),
                                           "3" = (go_out == 5),
                                           "4" = (go_out == 4),
                                           "5" = (go_out == 3),
                                           "6" = (go_out == 2),
                                           "7" = (go_out == 1),
                                           "NA" = (is.na(go_out)),
                                           .method = "unique")),
         go_out_o = as.numeric(derivedFactor("1" = (go_out_o == 7),
                                           "2" = (go_out_o == 6),
                                           "3" = (go_out_o == 5),
                                           "4" = (go_out_o == 4),
                                           "5" = (go_out_o == 3),
                                           "6" = (go_out_o == 2),
                                           "7" = (go_out_o == 1),
                                           "NA" = (is.na(go_out_o)),
                                           .method = "unique")),         
         goal = as.numeric(derivedFactor("0" = (goal %in% c(5,6)), # higher goal number = more sincere
                              "1" = (goal %in% c(1,2)),
                              "2" = (goal %in% c(3,4)),
                              "NA" = (is.na(goal)),
                              .method = "unique")),         
         goal_o = as.numeric(derivedFactor("0" = (goal_o %in% c(5,6)),
                                "1" = (goal_o %in% c(1,2)),
                                "2" = (goal_o %in% c(3,4)),
                                "NA" = (is.na(goal_o)),
                                .method = "unique")),
         age_diff = ifelse(gender == 1, age - age_o, age_o - age),  # age difference (male - female)
         income_diff = income_o - income, #  value of income difference
         go_out_diff = go_out_o - go_out, #  value of go_out difference
         sat_diff = mn_sat_o - mn_sat, # value of sat score difference
         goal_diff = goal_o - goal) %>% # value of goal difference 
  select(iid, id, wave, partner, pid, gender, age_diff, samerace, int_corr, income_diff, sat_diff, goal_diff, go_out_diff, imprace, imprelig, like, dec)



obj_attr_test = test %>% 
  select(iid, id, wave, partner, pid, gender, age, age_o, race, race_o, samerace, int_corr, income, goal, go_out, mn_sat, imprace, imprelig, like, dec) %>% 
  left_join(train %>% select(iid, income, mn_sat, go_out, goal) %>% dplyr::rename(income_o = income, mn_sat_o = mn_sat, go_out_o = go_out, goal_o = goal) %>% unique, by = c("pid" = "iid")) %>%
  mutate(income = as.numeric(gsub(",", "", .$income)),
         income_o = as.numeric(gsub(",", "", .$income_o)),
         mn_sat = as.numeric(gsub(",", "", .$mn_sat)),
         mn_sat_o = as.numeric(gsub(",", "", .$mn_sat_o)),
         go_out = as.numeric(derivedFactor("1" = (go_out == 7),
                                           "2" = (go_out == 6),
                                           "3" = (go_out == 5),
                                           "4" = (go_out == 4),
                                           "5" = (go_out == 3),
                                           "6" = (go_out == 2),
                                           "7" = (go_out == 1),
                                           "NA" = (is.na(go_out)),
                                           .method = "unique")),
         go_out_o = as.numeric(derivedFactor("1" = (go_out_o == 7),
                                             "2" = (go_out_o == 6),
                                             "3" = (go_out_o == 5),
                                             "4" = (go_out_o == 4),
                                             "5" = (go_out_o == 3),
                                             "6" = (go_out_o == 2),
                                             "7" = (go_out_o == 1),
                                             "NA" = (is.na(go_out_o)),
                                             .method = "unique")),         
         goal = as.numeric(derivedFactor("0" = (goal %in% c(5,6)), # higher goal number = more sincere
                                         "1" = (goal %in% c(1,2)),
                                         "2" = (goal %in% c(3,4)),
                                         "NA" = (is.na(goal)),
                                         .method = "unique")),         
         goal_o = as.numeric(derivedFactor("0" = (goal_o %in% c(5,6)),
                                           "1" = (goal_o %in% c(1,2)),
                                           "2" = (goal_o %in% c(3,4)),
                                           "NA" = (is.na(goal_o)),
                                           .method = "unique")),
         age_diff = ifelse(gender == 1, age - age_o, age_o - age),  # age difference (male - female)
         income_diff = income_o - income, #  value of income difference
         go_out_diff = go_out_o - go_out, #  value of go_out difference
         sat_diff = mn_sat_o - mn_sat, # value of sat score difference
         goal_diff = goal_o - goal) %>% # value of goal difference 
  select(iid, id, wave, partner, pid, gender, age_diff, samerace, int_corr, income_diff, sat_diff, goal_diff, go_out_diff, imprace, imprelig, like, dec)

# Create dataset of attribute scores given from Participant A to Participant B

pairs_attr = train %>% 
  select(iid, id, wave, partner, gender, pid, attr, sinc, intel, fun, amb, shar, like, dec)

pairs_attr_test = test %>% 
  select(iid, id, wave, partner, gender, pid, attr, sinc, intel, fun, amb, shar, like, dec)

# Create merged dataset consisting of both objective and subjective compatibility attributes

merged_attr = merge(obj_attr, pairs_attr, by = c("iid", "id", "wave", "partner", "gender", "pid", "like", "dec"))

merged_attr_test = merge(obj_attr_test, pairs_attr_test, by = c("iid", "id", "wave", "partner", "gender", "pid", "like", "dec"))

# Create plot of decision versus like score

decision_like = pairs_attr %>% 
  filter(!is.na(like)) %>% 
  mutate(like = round(like, 0),
         dec = factor(dec, levels = c(0, 1), labels = c("No", "Yes"))) %>% 
  group_by(like, dec) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::mutate(density = n/sum(n),
                label = paste0(round(density*100, 1), "%")) 

ggplot(decision_like, aes(like, y = n, fill = dec, label = label)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label), size = 3, color = "black", family = "Tahoma", position = position_stack(vjust = 0.5)) +
  scale_x_continuous(breaks = seq(0,10,2)) +
  labs(title = 'Distribution of "Like" Score and Decision \n("Would you like to see this person again?")',
       x = '"Like" Score',
       y = "Count",
       fill = "Decision") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 13, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma", size = 11.5),
        axis.text.x=element_text(colour="black", size = 11.5),
        axis.text.y=element_text(colour="black", size = 11.5),
        strip.text.x = element_text(colour="black", size = 11.5, face = "bold"))

# Create plot of univariate distribution of attribute scores

scores = pairs_attr %>%
  select(-c(like, dec)) %>% 
  mutate(partner_gender = as.factor(ifelse(gender == 1, "Female", "Male"))) %>% 
  dplyr::rename(Attractiveness = attr,
                Sincerity = sinc,
                Intelligence = intel,
                Fun = fun,
                Ambition = amb,
                `Shared Interests` = shar) %>% 
  gather(key = Attribute, value = Score, Attractiveness, Sincerity, Intelligence, Fun, Ambition, `Shared Interests`)

ggplot(scores, aes(Score, y = ..density.., fill = partner_gender)) +
  geom_histogram(bins = 10, position = "identity", alpha = 0.5) +
  facet_wrap(~Attribute) +
  scale_x_continuous(breaks = seq(0,10,2)) +
  labs(title = "Distribution of Received Attribute Scores by Gender of Ratee",
       x = "Score Received",
       y = "Density",
       fill = "Gender") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 13, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma", size = 11.5),
        axis.text.x=element_text(colour="black", size = 11.5),
        axis.text.y=element_text(colour="black", size = 11.5),
        strip.text.x = element_text(colour="black", size = 11.5, face = "bold"))
  

# Create plot of positive response rate for a participant by their average received attribute scores

scores_decision = pairs_attr %>% 
  group_by(pid) %>% 
  dplyr::summarise(num_partners = n(), # number of people who "speed dated" with participant
            pos_resp_cnt = sum(dec == 1), # number of people who marked dec = 1 for participant
            Attractiveness = mean(attr, na.rm = T), # avg attractiveness score
            Sincerity = mean(sinc, na.rm = T), # avg sincerity score
            Intelligence = mean(intel, na.rm = T), # avg intelligence score
            Fun = mean(fun, na.rm = T), # avg fun score
            Ambition = mean(amb, na.rm = T), # avg ambitiousness score
            `Shared Interests` = mean(shar, na.rm = T)) %>% # avg shared interest score)  
  mutate(`Positive Response Rate` = pos_resp_cnt / num_partners) %>% 
  gather(key = attr, value = `Average Score`, Attractiveness, Sincerity, Intelligence, Fun, Ambition, `Shared Interests`) %>% 
  arrange(pid) %>% 
  unique

lm_eqn = function(df){
  m = lm(`Positive Response Rate` ~ `Average Score`, df);
  eq <- substitute(r2, 
                   list(r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

eqns = by(scores_decision, scores_decision$attr, lm_eqn)
r_squared_df = data.frame(eq = unclass(eqns), attr = names(eqns))
r_squared_df$lab = paste(r_squared_df$attr, " (", expression(R^2), " = ", r_squared_df$eq, ") ", sep = "")

r2_labeller <- function(variable,value){
  return(r_squared_df$lab)
}

ggplot(scores_decision, aes(x = `Average Score`, y = `Positive Response Rate`)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(~attr, labeller = r2_labeller) +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  scale_y_continuous(breaks = seq(0, 1, .2)) +
  ggtitle("Positive Response Rate by Average Received Attribute Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 13, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma", size = 11.5),
        axis.text.x=element_text(colour="black", size = 11.5),
        axis.text.y=element_text(colour="black", size = 11.5),
        strip.text.x = element_text(colour="black", size = 11.5, face = "bold"))

  
# Create plot of positive response rate for a participant by their average received attribute scores
  
obj_attr_like = obj_attr %>% 
  dplyr::rename(`Age Difference` = age_diff,
         `Same Race` = samerace,
         `Interest Correlation` = int_corr,
         `Income Difference` = income_diff,
         `SAT Difference` = sat_diff,
         `Goal Difference` = goal_diff,
         `"Like" Score` = like) %>% 
  gather(key = Attribute, value = Difference, `Age Difference`, `Same Race`, `Interest Correlation`, `Income Difference`, `SAT Difference`, `Goal Difference`)

lm_eqn = function(df){
  m = lm(`"Like" Score` ~ Difference, df);
  eq <- substitute(r2, 
                   list(r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

eqns = by(obj_attr_like, obj_attr_like$Attribute, lm_eqn)
r_squared_df = data.frame(eq = unclass(eqns), attr = names(eqns))
r_squared_df$lab = paste(r_squared_df$attr, " (", expression(R^2), " = ", r_squared_df$eq, ") ", sep = "")

r2_labeller <- function(variable,value){
  return(r_squared_df$lab)
}

ggplot(obj_attr_like, aes(x = Difference, y = `"Like" Score`)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(~Attribute, labeller = r2_labeller, scales = "free") +
  #scale_x_continuous(breaks = seq(0, 10, 2)) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  ggtitle('"Like" Score by Objective Compatibility Factors') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 13, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma", size = 11.5),
        axis.text.x=element_text(colour="black", size = 11.5),
        axis.text.y=element_text(colour="black", size = 11.5),
        strip.text.x = element_text(colour="black", size = 11.5, face = "bold"))


# Create correlation plot of objective attributes and scores

p_amb = ggplot(merged_attr, aes(x = income_diff, y = amb)) +
  geom_point() +
  geom_smooth(method='lm')+
  labs(title = "Ambition Rating vs Income Difference \n(R^2 = 0.04523)",
       x = "Income Difference (Ratee - Rater)",
       y = "Ambition Rating Given") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 13, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma", size = 11.5),
        axis.text.x=element_text(colour="black", size = 11.5),
        axis.text.y=element_text(colour="black", size = 11.5),
        strip.text.x = element_text(colour="black", size = 11.5, face = "bold"))

p_sinc = ggplot(merged_attr, aes(x = goal_diff, y = sinc)) +
  geom_point() +
  geom_smooth(method='lm')+
  labs(title = "Sincerity Rating vs Income Difference\n (R^2 = 4.582e-05)",
       x = "Goal Difference (Ratee - Rater)\n (higher goal value = more sincerity)",
       y = "Sincerity Rating Given") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 13, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma", size = 11.5),
        axis.text.x=element_text(colour="black", size = 11.5),
        axis.text.y=element_text(colour="black", size = 11.5),
        strip.text.x = element_text(colour="black", size = 11.5, face = "bold"))

p_shar = ggplot(merged_attr, aes(x = int_corr, y = shar)) +
  geom_point() +
  geom_smooth(method='lm')+
  labs(title = "'Shared Interests' Rating vs Interest Correlation\n (R^2 = 0.0361)",
       x = "Interest Correlation\n (calculated using each participant's rating \nof hobbies and interests)",
       y = "'Shared Interests' Rating Given") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 13, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma", size = 11.5),
        axis.text.x=element_text(colour="black", size = 11.5),
        axis.text.y=element_text(colour="black", size = 11.5),
        strip.text.x = element_text(colour="black", size = 11.5, face = "bold"))

p_intel =  ggplot(merged_attr, aes(x = sat_diff, y = intel)) +
  geom_point() +
  geom_smooth(method='lm')+
  labs(title = "Intelligence Rating vs SAT Score Difference\n (R^2 = 0.08762)",
       x = "SAT Score Difference (Ratee - Rater)",
       y = "Intelligence Rating Given") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 13, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma", size = 11.5),
        axis.text.x=element_text(colour="black", size = 11.5),
        axis.text.y=element_text(colour="black", size = 11.5),
        strip.text.x = element_text(colour="black", size = 11.5, face = "bold"))

p_fun = ggplot(merged_attr, aes(x = go_out_diff, y = fun)) +
  geom_point() +
  geom_smooth(method='lm')+
  labs(title = "Fun Rating vs Difference in Frequency of Going Out\n (R^2 = 0.12069)",
       x = "Difference in Frequency of Going Out (Ratee - Rater)",
       y = "Fun Rating Given") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 13, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma", size = 11.5),
        axis.text.x=element_text(colour="black", size = 11.5),
        axis.text.y=element_text(colour="black", size = 11.5),
        strip.text.x = element_text(colour="black", size = 11.5, face = "bold"))

grid.arrange(p_amb, p_sinc, p_shar, p_intel, p_fun, ncol = 3)


  
#Impute Missing pid values
  
missing_pid = merged_attr[is.na(merged_attr$pid), ]
merged_attr[merged_attr$wave == 5 & merged_attr$id == 7,]
merged_attr$pid[is.na(merged_attr$pid)] <- 128

merged_attr_impute_like = as.data.frame(knn.impute(as.matrix(merged_attr[!is.na(merged_attr$like),]), k=10))

write.csv(merged_attr_impute_like, "~/Documents/Fall 2018/MS&E 226/Project/Speed-Dating-Experiment/merged_attr_impute_like.csv")

merged_attr_impute = as.data.frame(knn.impute(as.matrix(merged_attr), k=10))

write.csv(merged_attr_impute, "~/Documents/Fall 2018/MS&E 226/Project/Speed-Dating-Experiment/merged_attr_impute.csv")

merged_attr_test$pid[is.na(merged_attr_test$pid)] <- 128

write.csv(merged_attr_test, "~/Documents/Fall 2018/MS&E 226/Project/Speed-Dating-Experiment/merged_attr_test.csv")
