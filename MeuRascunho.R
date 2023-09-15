k_means_test <- function(data, classe, paramsList) {
  model <- clu_tune(cluster_kmeans(k = 0))
  model <- fit(model, data, paramsList)
  clu <- cluster(model, data)
  eval <- evaluate(model, clu, classe)
  eval
}

my_dict <- list(
  best_algorithm = 'best',
  params = c("item1", "item2"),
  result = 10
)

#------------ geográfica ----------- #
split_infection_region_parasite <- data %>% select (infection.county,infection.hr,
                                           infection.state,infection.country,
                                           exam.result, hemiparasite)
s_i_r_p <- split_infection_region_parasite %>% 
  filter (exam.result!="negative" & hemiparasite!="negative")
s_i_r_p$infection.county <- unclass(s_i_r_p$infection.county)
s_i_r_p$infection.hr <- unclass(s_i_r_p$infection.hr)
s_i_r_p$infection.state <- unclass(s_i_r_p$infection.state)
s_i_r_p$infection.country <- unclass(s_i_r_p$infection.country)
s_i_r_p <- na.omit(s_i_r_p) #na.omit

#------------ geográfica ----------- #
# -- Parasitas -- #
split_infection_region_parasite <- data %>% select (infection.county,infection.hr,
                                                    infection.state,infection.country,
                                                    exam.result, hemiparasite)
s_i_r_p <- na.omit(split_infection_region_parasite)
s_i_r_p <- s_i_r_p %>% filter (exam.result!="negative")
#s_i_r_p <- s_i_r_p %>% filter (hemiparasite!="negative")
s_i_r_p$infection.county <- unclass(s_i_r_p$infection.county)
s_i_r_p$infection.hr <- unclass(s_i_r_p$infection.hr)
s_i_r_p$infection.state <- unclass(s_i_r_p$infection.state)
s_i_r_p$infection.country <- unclass(s_i_r_p$infection.country)

#Escolha de parametros
params <- list( k = c(180,190,200,210) )
as.integer(params[['k']] )

# Teste full
result <- k_means_test(s_i_r_p[,1:4], s_i_r_p$hemiparasite, params)

# Clustering por quantidade de parasitas
split_infection_region_parasite <- data %>% select (infection.county,infection.hr,
                                                    infection.state,infection.country,
                                                    qty.parasites)
s_i_r_p <- na.omit(split_infection_region_parasite)
# -- Smoothing na quantidade de parasitas
ob <- smoothing_cluster(n = 10)
ob <- fit(ob, s_i_r_p$qty.parasites)
qnt_parasites_bins <- transform(ob, s_i_r_p$qty.parasites)
s_i_r_p$qty.parasites <- qnt_parasites_bins
s_i_r_p$infection.county <- unclass(s_i_r_p$infection.county)
s_i_r_p$infection.hr <- unclass(s_i_r_p$infection.hr)
s_i_r_p$infection.state <- unclass(s_i_r_p$infection.state)
s_i_r_p$infection.country <- unclass(s_i_r_p$infection.country)
#Escolha de parametros
params <- list( k = c(180,190,200,210) )
as.integer(params[['k']] )
result <- k_means_test(s_i_r_p[,1:4], s_i_r_p$qty.parasites, params)

# Clustering demográfico
split_demografico <- data %>% select (age, gender, race, 
                                      #occupation, education.level,
                                pregnancy, autochthonous.case, 
                                #exam.result)
                                #hemiparasite)
                                #previous.treatment)
                                symptom)

split_demog <- na.omit(split_demografico)

#split_demog$occupation <- unclass(split_demog$occupation)
#split_demog$education.level <- unclass(split_demog$education.level)
# -------- Organizando as grávidas ------------- #
split_demog <- split_demog %>% filter (pregnancy != "ignored gestational ")
split_demog <- split_demog %>%
  mutate(pregnancy = ifelse(grepl("trimester", pregnancy) , as.integer(1), as.integer(0)))

split_demog$pregnancy <- unclass(split_demog$pregnancy)
split_demog$autochthonous.case <- unclass(split_demog$autochthonous.case)
split_demog$age <- unclass(split_demog$age)
split_demog$gender <- unclass(split_demog$gender)
split_demog$race <- unclass(split_demog$race)

#Tornando os resultados de exames positivos ou negativos
#split_demog <- split_demog %>%
#  mutate(exam.result = ifelse(exam.result != 'negative', 'positive', 'negative'))

#Separando os parasitas
#split_demog <- split_demog %>% filter (hemiparasite != "negative")

#Escolha de parametros
params <- list( k = c(10,50,100,200) )
as.integer(params[['k']] )
#result <- k_means_test(split_demog[,1:5], split_demog$exam.result, params)
#result <- k_means_test(split_demog[,1:5], split_demog$hemiparasite, params)
#result <- k_means_test(split_demog[,1:5], split_demog$previous.treatment, params)
result <- k_means_test(split_demog[,1:5], split_demog$symptom, params)

# Clustering temporal
split_temporal <- data %>% select (notification.month, notification.year,
                                   exam.month, exam.year, treatment.month,
                                   symptom.month, symptom.year,
                                   exam.interval, treatment.interval,
                                   notification.interval,
                                   #symptom)
                                   exam.result)
                                   #hemiparasite)
split_tempo <- na.omit(split_temporal)

split_tempo$notification.month <- unclass(split_tempo$notification.month)
split_tempo$notification.year <- unclass(split_tempo$notification.year)
split_tempo$exam.month <- unclass(split_tempo$exam.month)
split_tempo$exam.year <- unclass(split_tempo$exam.year)
split_tempo$treatment.month <- unclass(split_tempo$treatment.month)
split_tempo$symptom.month <- unclass(split_tempo$symptom.month)
split_tempo$symptom.year <- unclass(split_tempo$symptom.year)
split_tempo$exam.interval <- unclass(split_tempo$exam.interval)
split_tempo$treatment.interval <- unclass(split_tempo$treatment.interval)
split_tempo$notification.interval <- unclass(split_tempo$notification.interval)

#split_tempo$symptom <- unclass(split_tempo$symptom)
#split_tempo <- split_tempo %>%
#  mutate(exam.result = ifelse(exam.result != 'negative', 'positive', 'negative'))
split_tempo$exam.result <- unclass(split_tempo$exam.result)

#split_tempo$hemiparasite <- unclass(split_tempo$hemiparasite)

params <- list( k = c(12,100,500, 1000) )
as.integer(params[['k']] )
#result <- k_means_test(split_tempo[,1:11], split_tempo$symptom, params)
#result <- k_means_test(split_tempo[,1:11], split_tempo$exam.result, params)
#result <- k_means_test(split_tempo[,1:11], split_tempo$hemiparasite, params)

# Clustering da doença
split_sick <- data %>% select (symptom, qty.parasites, exam.type,
                                   hemiparasite, detection.type,
                                   #symptom),
                                   exam.result)
split_sick <- na.omit(split_sick)
ob <- smoothing_cluster(n = 10)
ob <- fit(ob, split_sick$qty.parasites)
split_sick$qty.parasites <- transform(ob, split_sick$qty.parasites)
split_sick$qty.parasites <- as.integer(round(split_sick$qty.parasites))
split_sick$qty.parasites <- unclass(split_sick$qty.parasites)
split_sick$symptom <- unclass(split_sick$symptom)
split_sick$exam.type <- unclass(split_sick$exam.type)
split_sick$hemiparasite <- unclass(split_sick$hemiparasite)
split_sick$detection.type <- unclass(split_sick$detection.type)
split_sick$exam.result <- unclass(split_sick$exam.result)
split_sick <- na.omit(split_sick)
params <- list( k = c(200,400,800,1600) )
as.integer(params[['k']] )
result <- k_means_test(split_sick[,1:5], split_sick$exam.result, params)

# --  Pattern Prediction -- #

#Iris example
source("https://raw.githubusercontent.com/cefet-rj-dal/daltoolbox-examples/main/jupyter.R")
load_library("daltoolbox")
iris <- datasets::iris
head(iris)
slevels <- levels(iris$Species)
slevels
set.seed(1)
sr <- sample_random()
sr <- train_test(sr, iris)
iris_train <- sr$train
iris_test <- sr$test
tbl <- rbind(table(iris[,"Species"]), 
             table(iris_train[,"Species"]), 
             table(iris_test[,"Species"]))
rownames(tbl) <- c("dataset", "training", "test")
head(tbl)
model <- cla_dtree("Species", slevels)
model <- fit(model, iris_train)
train_prediction <- predict(model, iris_train)
iris_train_predictand <- adjust_class_label(iris_train[,"Species"])
train_eval <- evaluate(model, iris_train_predictand, train_prediction)
print(train_eval$metrics)
test_prediction <- predict(model, iris_test)
iris_test_predictand <- adjust_class_label(iris_test[,"Species"])
test_eval <- evaluate(model, iris_test_predictand, test_prediction)
print(test_eval$metrics)

#Training on Malaria Set
load("/home/data/malaria/malaria.RData")
#Comparando quantidade de dados nulos e não nulos
data.na <- na.omit(data)
#22923977/466127 = 49.17968     >> Só 2% dos dados são mantidos
results_list <- list()
for (feature in data) {
  x <- na.omit(feature)
  results_list <- append(results_list, list(x))
}

num_itens <- list()
for (feature in results_list) {
  x <- length(feature)
  num_itens <- append(num_itens, x)
}

feature_names <- names(data)
check_df <- data.frame(matrix(unlist(num_itens), ncol = length(num_itens), byrow = FALSE))
colnames(check_df) <- feature_names
new_column <- c("Valores") #Casa de Ferreiro
check_df <- data.frame(NewColumnName = new_column, check_df) #Espeto de Pau


library("daltoolbox")
library("RColorBrewer")
library("ggplot2")
colors <- brewer.pal(4, 'Set1')
font <- theme(text = element_text(size=16))
library("dplyr")
options(scipen = 999)

grf <- plot_groupedbar(check_df) + font #Não conseguiu plotar 40 colres
plot(grf)

#Separando grupos
pequenos <- c("notification.hr", "notification.county", "exam.type", "exam.result",
             "hemiparasite", "detection.type", "cvl.case", "notification.state",
             "notification.month", "notification.year", "previous.treatment")
grandes <- c("NewColumnName", "notification.hr", "notification.county", "exam.type",
             "exam.result",
             "hemiparasite", "detection.type", "cvl.case", "notification.state",
             "notification.month", "notification.year", "previous.treatment")

# Delete columns by excluding them using negative index
check_df_pequeno <- check_df[, !names(check_df) %in% pequenos]
grf2 <- plot_groupedbar(check_df_pequeno) + font #Não conseguiu plotar 40 colres
plot(grf2)

check_df_grande <- check_df[, names(check_df) %in% grandes]
grf3 <- plot_groupedbar(check_df_grande) + font #Não conseguiu plotar 40 colres
plot(grf3)

#Criando o grupo 1
grupo_grande <- c("notification.hr", "notification.county", "exam.type","exam.result",
              "hemiparasite", "detection.type", "cvl.case", "notification.state",
              "notification.month", "notification.year", "previous.treatment")
grupo_1 <- na.omit( data[, names(data) %in% grupo_grande] )

#Criando o grupo 2
grupo_pequeno <- c("race", "qty.parasites")
grupo_2 <- na.omit( data[, !names(data) %in% grupo_pequeno] )

#Dataset Grupo 1
set.seed(1)
sr1 <- sample_random()
sr1 <- train_test(sr1, grupo_1)
sr1_train <- sr1$train
sr1_test <- sr1$test
slevels <- levels(grupo_1$exam.result)

#Dataset Grupo 2
sr2 <- sample_random()
sr2 <- train_test(sr2, grupo_2)
sr2_train <- sr2$train
sr2_test <- sr2$test
slevels <- levels(grupo_2$exam.result)

#Majority train grupo 1
model1 <- cla_majority("exam.result", slevels)
model1 <- fit(model1, sr1_train)
train_prediction1 <- predict(model1, sr1_train)
train_predictand1 <- adjust_class_label(sr1_train[,"exam.result"])
train_eval <- evaluate(model1, train_predictand1, train_prediction1)
print(train_eval$metrics)

metricas <- data.frame(
  name = "train_g1",
  accuracy = train_eval$accuracy,
  f1 = train_eval$f1,
  sensitivity = train_eval$sensitivity,
  specificity = train_eval$specificity,
  precision = train_eval$precision,
  recall = train_eval$recall
)

test_prediction <- predict(model1, sr1_test)
test_predictand1 <- adjust_class_label(sr1_test[,"exam.result"])
test_eval <- evaluate(model1, test_predictand1, test_prediction)

new_row <- data.frame(
  name = "test_g1",
  accuracy = test_eval$accuracy,
  f1 = test_eval$f1,
  sensitivity = test_eval$sensitivity,
  specificity = test_eval$specificity,
  precision = test_eval$precision,
  recall = test_eval$recall
)
metricas <- rbind(metricas, new_row)

#Majority treinamento grupo 2
model <- cla_majority("exam.result", slevels)
model <- fit(model, sr2_train)
train_prediction <- predict(model, sr2_train)
train_predictand <- adjust_class_label(sr2_train[,"exam.result"])
train_eval <- evaluate(model, train_predictand, train_prediction)

new_row <- data.frame(
  name = "train_g2",
  accuracy = train_eval$accuracy,
  f1 = train_eval$f1,
  sensitivity = train_eval$sensitivity,
  specificity = train_eval$specificity,
  precision = train_eval$precision,
  recall = train_eval$recall
)
metricas <- rbind(metricas, new_row)

test_prediction <- predict(model, sr2_test)
test_predictand <- adjust_class_label(sr2_test[,"exam.result"])
test_eval <- evaluate(model, test_predictand, test_prediction)

new_row <- data.frame(
  name = "test_g2",
  accuracy = test_eval$accuracy,
  f1 = test_eval$f1,
  sensitivity = test_eval$sensitivity,
  specificity = test_eval$specificity,
  precision = test_eval$precision,
  recall = test_eval$recall
)
metricas <- rbind(metricas, new_row)


#Plot majority results
grf <- plot_groupedbar(metricas) + font
plot(grf)

t_metricas <- t(metricas)
colnames(t_metricas) <- t_metricas[1, ]
t_metricas <- t_metricas[-1, ]
t_metricas <- cbind(c("Accuracy","F1", "Sensitivity", "Specificity",
                                  "Precision", "Recall"), t_metricas )

grf <- plot_groupedbar(t_metricas) + font
plot(grf)

new_metrics <- subset(t_metricas, select = -train_g1)
new_metrics <- subset(new_metrics, select = -train_g2)
colnames(new_metrics)[2] <- "Majority_G1"
colnames(new_metrics)[3] <- "Majority_G2"

#Naive Bayes
model <- cla_nb("exam.result", slevels)
model <- fit(model, sr1_train)
train_prediction <- predict(model, sr1_train)
train_predictand <- adjust_class_label(sr1_train[,"exam.result"])
train_eval <- evaluate(model, train_predictand, train_prediction)
print(train_eval$metrics)
test_prediction <- predict(model, sr1_test)
test_predictand <- adjust_class_label(sr1_test[,"exam.result"])
test_eval <- evaluate(model, test_predictand, test_prediction)
print(test_eval$metrics)

new_metrics <- cbind(new_metrics, c(test_eval$accuracy, test_eval$f1,
                                    test_eval$sensitivity,test_eval$specificity,
                                    test_eval$precision,test_eval$recall))
colnames(new_metrics)[4] <- "NaiveBayes_G1"
rownames(new_metrics) <- NULL

grf <- plot_groupedbar(new_metrics) + font
plot(grf)


#Naive Bayes Grupo 2
model <- cla_nb("exam.result", slevels2)
model <- fit(model, sr2_train)
train_prediction <- predict(model, sr2_train)
train_predictand <- adjust_class_label(sr2_train[,"exam.result"])
train_eval <- evaluate(model, train_predictand, train_prediction)
print(train_eval$metrics)
test_prediction <- predict(model, sr2_test)
test_predictand <- adjust_class_label(sr2_test[,"exam.result"])
test_eval <- evaluate(model, test_predictand, test_prediction)
print(test_eval$metrics)

backup_metrics <- new_metrics
new_metrics <- cbind(new_metrics, c(test_eval$accuracy, test_eval$f1,
                                    test_eval$sensitivity,test_eval$specificity,
                                    test_eval$precision,test_eval$recall))
colnames(new_metrics)[5] <- "NaiveBayes_G2"
rownames(new_metrics) <- NULL

new_metrics_df <- as.data.frame(new_metrics)
colnames(new_metrics_df) <- c("Metric", "Majority_G1", "Majority_G2", "NaiveBayes_G1", "NaiveBayes_G2")
rownames(new_metrics_df) <- NULL

# Ensure the column is treated as character
new_metrics_df$Majority_G1 <- as.character(new_metrics_df$Majority_G1)
new_metrics_df$Majority_G2 <- as.character(new_metrics_df$Majority_G2)
new_metrics_df$NaiveBayes_G1 <- as.character(new_metrics_df$NaiveBayes_G1)
new_metrics_df$NaiveBayes_G2 <- as.character(new_metrics_df$NaiveBayes_G2)

# Convert the column to numeric and round to 3 decimals
new_metrics_df$Majority_G1 <- round(as.numeric(new_metrics_df$Majority_G1), 3)
new_metrics_df$Majority_G2 <- round(as.numeric(new_metrics_df$Majority_G2), 3)
new_metrics_df$NaiveBayes_G1 <- round(as.numeric(new_metrics_df$NaiveBayes_G1), 3)
new_metrics_df$NaiveBayes_G2 <- round(as.numeric(new_metrics_df$NaiveBayes_G2), 3)

new_metrics_df$Majority_G1 <- as.double(new_metrics_df$Majority_G1)
new_metrics_df$Majority_G2 <- as.double(new_metrics_df$Majority_G2)
new_metrics_df$NaiveBayes_G1 <- as.double(new_metrics_df$NaiveBayes_G1)
new_metrics_df$NaiveBayes_G2 <- as.double(new_metrics_df$NaiveBayes_G2)

grf <- plot_groupedbar(new_metrics_df) + font
plot(grf)

# RANDOM FOREST
#Não funcionou

#Árvores
#model1 <- cla_dtree("exam.result", slevels)
#model1 <- fit(model1, sr1_train)
#train_prediction1 <- predict(model1, sr1_train)

#Não foi possível treinar algoritmos de árvores de decisão porque algumas features
# categóricas tinham mais de 32 factors
factor_columns <- names(sr1_train)[sapply(sr1_train, is.factor)]
# Iterate through factor columns and count the number of levels
for (col_name in factor_columns) {
  num_levels <- nlevels(sr1_train[[col_name]])
  cat("Number of levels in '", col_name, "':", num_levels, "\n")
}

