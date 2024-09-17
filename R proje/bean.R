# Gerekli k??t??phaneleri y??kleme
install.packages("readxl")
install.packages("class")
install.packages("caTools")
install.packages("ggplot2")

# K??t??phaneleri ??a????rma
library(readxl)
library(class)
library(caTools)
library(ggplot2)

# Veriyi y??kleme
library(readxl)
dry <- read_excel("dry.xlsx")
View(dry)

# Veri setinin ilk birka?? satt??r??n?? yazd??rma
head(dataset)

# ??lk iki s??tunu ve son s??tunu se??me
selected_data <- dataset[, c("Area", "Perimeter", "Class")]
summary(selected_data)
table(selected_data$Class)

# Eksik verileri tespit etme ve doldurma
is.na(selected_data)
sum(is.na(selected_data))
colSums(is.na(selected_data))

# Ayk??r?? verileri kald??rma
Q1_Area <- quantile(selected_data$Area, 0.25)
Q3_Area <- quantile(selected_data$Area, 0.75)
IQR_Area <- IQR(selected_data$Area)

Q1_Perimeter <- quantile(selected_data$Perimeter, 0.25)
Q3_Perimeter <- quantile(selected_data$Perimeter, 0.75)
IQR_Perimeter <- IQR(selected_data$Perimeter)

filtered_data <- selected_data[
  selected_data$Area >= (Q1_Area - 1.5 * IQR_Area) & selected_data$Area <= (Q3_Area + 1.5 * IQR_Area) &
    selected_data$Perimeter >= (Q1_Perimeter - 1.5 * IQR_Perimeter) & selected_data$Perimeter <= (Q3_Perimeter + 1.5 * IQR_Perimeter), 
]

# Ayk??r?? verilerden ar??nd??r??lm???? boxplot ??izme
ggplot(filtered_data, aes(x = factor(1), y = Area)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of Area without Outliers", x = "", y = "Area")

ggplot(filtered_data, aes(x = factor(1), y = Perimeter)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of Perimeter without Outliers", x = "", y = "Perimeter")

# Hedef niteli??i fakt??r olarak tan??tma - Kategorik de??i??kenler i??in encoding


filtered_data$Class= factor(filtered_data$Class,
                            levels = c('BARBUNYA', 'BOMBAY','CALI','DERMASON','HOROZ','SEKER','SIRA'),
                            labels = c(1:7))

# Veri ayr????t??rma ??? E??itim ve Test veri seti
set.seed(123)
split <- sample.split(filtered_data$Class, SplitRatio = 0.80)
training_set <- subset(filtered_data, split == TRUE)
test_set <- subset(filtered_data, split == FALSE)

# Veri d??n??????m?? - Standartla??t??rma - Data Transformation - Feature Scaling - Standardization
training_set[, c("Area", "Perimeter")] <- scale(training_set[, c("Area", "Perimeter")])
test_set[, c("Area", "Perimeter")] <- scale(test_set[, c("Area", "Perimeter")])

# E??itim seti ??zerinden geli??tirilen k-NN'nin test verisi ??zerindeki uyumunu g??rmek i??in s??n??flama tahminlerini hesaplatmak
y_pred <- knn(train = training_set[, -3], test = test_set[, -3], cl = training_set$Class, k = 5, prob = TRUE)

# Performans de??erlendirme i??in karma????kl??k matrisi
cm <- table(test_set$Class, y_pred)
print(cm)
acc <- (cm[1,1] + cm[2,2]+cm[3,3]+cm[4,4]+cm[5,5]+cm[6,6]+cm[7,7]) / sum(cm) # do??ruluk katsay??s??
err <- 1 - acc # hata oran??
print(paste("Do??ruluk Oran??: ", acc))
print(paste("Hata Oran??: ", err))