library(pastecs)

demo = get(load("my_data_demographics.Rdata")) %>%
  as_tibble()

completed = c(1,2,3,4,5,8,10,12,14,15,17,21,22,23,24,26,30,32,33,34,36,37,
              38,39,43,44,45,46,47,51,53,55,58,59,60,61,62,63,64,66,71,73,75,77,
              78,80,81,84,86,87,90,92,97,99,100,103,107,112,120,123)

age = demo %>%
  filter(short_name == "age",
         participant %in% completed)

age$response = as.numeric(age$response)

stat.desc(age$response)
