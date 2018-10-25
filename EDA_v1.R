set.seed(123)

data = read.csv("/Users/cindykang/Documents/Fall 2018/MS&E 226/Project/Speed-Dating-Experiment/Speed_Dating_Data.csv", stringsAsFactors = F, na.strings = c("", "NA", "NULL"))
train_ind = sample(1:nrow(data), round(.8 * nrow(data)))
train = data[train_ind, ]
test = data[-train_ind, ] # Set aside test dataset

# Create dataset of objective compatibility attributes

obj_attr = train %>% 
  select(iid, pid, gender, age, age_o, race, race_o, samerace, int_corr, income, go_out, mn_sat) %>% 
  left_join(train %>% select(iid, income, mn_sat, go_out) %>% dplyr::rename(income_o = income, mn_sat_o = mn_sat, go_out_o = go_out) %>% unique, by = c("pid" = "iid")) %>%
  mutate(income = as.numeric(gsub(",", "", .$income)),
         income_o = as.numeric(gsub(",", "", .$income_o)),
         mn_sat = as.numeric(gsub(",", "", .$mn_sat)),
         mn_sat_o = as.numeric(gsub(",", "", .$mn_sat_o)),
         age_diff = ifelse(gender == 1, age - age_o, age_o - age),  # age difference (male - female)
         income_diff = abs(income - income_o), # absolute value of income difference
         go_out_diff = abs(go_out - go_out_o), # absolute value of go_out difference
         sat_diff = abs(mn_sat - mn_sat_o)) %>% # absolute value of sat score difference
  select(iid, pid, gender, age_diff, samerace, int_corr, income_diff, sat_diff)

# Create dataset of attribute scores given from Participant A to Participant B

pairs_attr = train %>% 
  select(iid, pid, attr, sinc, intel, fun, amb, shar, like, dec)

# Create merged dataset consisting of both objective and subjective compatibility attributes

merged_attr = merge(obj_attr, pairs_attr, by = c("iid", "pid"))

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
            `Shared Interest` = mean(shar, na.rm = T)) %>% # avg shared interest score)  
  mutate(`Positive Response Rate` = pos_resp_cnt / num_partners) %>% 
  gather(key = attr, value = `Average Score`, Attractiveness, Sincerity, Intelligence, Fun, Ambition, `Shared Interest`) %>% 
  arrange(pid)

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



---
  
  
#todo : make train and test set (80/20)

options(na.action = "na.omit")
x = model.matrix(like ~ . , merged_attr)[, -c(1,2,15,16)]
y = merged_attr[complete.cases(merged_attr), ]$like

cv.out = cv.glmnet(x, y, alpha = 0)
best_lam = cv.out$lambda.min

model_ridge = glmnet(x, y, alpha = 0)

predictions = predict(model_ridge, s = best_lam, newx = x)

(rmse_train300 = sqrt((1/length(predictions)) * sum((y - predictions)^2))) # RMSE on training dataset



