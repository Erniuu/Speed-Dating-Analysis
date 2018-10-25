dat = read.csv("/Users/cindykang/Documents/Fall 2018/MS&E 226/Project/Speed-Dating-Experiment/Speed_Dating_Data.csv", stringsAsFactors = F, na.strings = c("", "NA", "NULL"))

obj_attr = dat %>% 
  select(iid, pid, gender, age, age_o, race, race_o, samerace, int_corr, income, go_out, mn_sat) %>% 
  left_join(dat %>% select(iid, income, mn_sat, go_out) %>% rename(income_o = income, mn_sat_o = mn_sat, go_out_o = go_out) %>% unique, by = c("pid" = "iid")) %>%
  mutate(income = as.numeric(gsub(",", "", .$income)),
         income_o = as.numeric(gsub(",", "", .$income_o)),
         mn_sat = as.numeric(gsub(",", "", .$mn_sat)),
         mn_sat_o = as.numeric(gsub(",", "", .$mn_sat_o)),
         age_diff = ifelse(gender == 1, age - age_o, age_o - age),  #age difference (male - female)
         income_diff = abs(income - income_o),
         go_out_diff = abs(go_out - go_out_o),
         sat_diff = abs(mn_sat - mn_sat_o)) %>% #absolute value of income difference
  select(iid, pid, gender, age_diff, samerace, int_corr, income_diff, sat_diff)

pairs_attr = dat %>% 
  select(iid, pid, attr, sinc, intel, fun, amb, shar, like, dec)

merged_attr = merge(obj_attr, pairs_attr, by = c("iid", "pid"))

#todo : make train and test set (80/20)

options(na.action = "na.omit")
x = model.matrix(like ~ . , merged_attr)[, -c(1,2,15,16)]
y = merged_attr[complete.cases(merged_attr), ]$like


#x = as.matrix(merged_attr[ , -c(1,2,15,16)])
#y = merged_attr$like
#lambda = 10^seq(10, -2, length = 100)

cv.out = cv.glmnet(x, y, alpha = 0)
best_lam = cv.out$lambda.min

model_ridge = glmnet(x, y, alpha = 0)

predictions = predict(model_ridge, s = best_lam, newx = x)

(rmse_train300 = sqrt((1/length(predictions)) * sum((y - predictions)^2))) # RMSE on training dataset

