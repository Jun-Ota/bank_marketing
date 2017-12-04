data<-read.csv("homework_data/bank_marketing_train.csv")
data$y<-ifelse(data$y=="yes", 1, 0)
summary(data)
set.seed(1234)
num_rows<-dim(data)[1]
idx<-c(1:num_rows)
train_idx<-sample(idx, size = num_rows*0.7 )
train_data<-data[train_idx, ]
validation_data<-data[-train_idx, ]

mymodel<-glm(y~age+job+marital+education+default+housing+loan+
               pdays+previous+poutcome+cons.price.idx+
               cons.conf.idx+euribor3m, 
             data = train_data, family = "binomial")

summary(mymodel)
# now AIC= 13074.08
mymodel_3<-step(mymodel)
#  AIC=13058.96
# y ~ job + education + default + poutcome + euribor3m
summary(mymodel_3)
exp(mymodel2$coefficients)
#retire,student,university.degreeが本線
#前回CPの結果nonexistent and success,金利euribor3m
score<-predict(mymodel_3, validation_data, type = "response")
score
mode(score)
summary(score)
hist(score)

ypred_flag<-ifelse(score > 0.21, 1, 0)
conf_mat<-table(validation_data$y, ypred_flag )
conf_mat

attack_num<-conf_mat[3] + conf_mat[4]
attack_num
your_cost <- attack_num * 500
your_cost
conf_mat[4] 
expected_revenue<-conf_mat[4] * 2000
expected_revenue
expected_revenue - your_cost


# =======================
data_2<-read.csv("~/Desktop/bank_marketing_test.csv")
data_2$y<-ifelse(data_2$y=="yes", 1, 0)
score<-predict(mymodel, data_2, type = "response")
score
summary(score)
hist(score)

ypred_flag<-ifelse(score > 0.21, 1, 0)
conf_mat<-table(data_2$y, ypred_flag )
conf_mat

attack_num<-conf_mat[3] + conf_mat[4]
attack_num
your_cost <- attack_num * 500
your_cost
conf_mat[4] 
expected_revenue<-conf_mat[4] * 2000
expected_revenue
expected_revenue - your_cost

