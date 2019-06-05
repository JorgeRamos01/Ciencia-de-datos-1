train<-read.table("oef.train") #Cargamos los datos de entrenamiento
test<-read.table("oef.test")  #Cargamos los datos de prueba
y_train <- train[,1]   
y_test <-test[,1]
y_train1 <- factor(y_train)
#Generamos nuestra matriz Y de entramiento que tiene 0's en todo el rengl?n excepto
#en el valor que queremos predecir
Y_train <- model.matrix(~y_train1-1) 
#Calculamos PCA sobre los 256 pixeles de las imagenes de entrenamiento
compon_princ <- princomp(train[,2:257])

#Esta funci?n permite calcular el error cuadrado medio del modelo para cada p
model_eval<-function(p){
  z<-compon_princ$loadings[,1:p]
  Z<-as.matrix(train[,2:257]) %*%z
  b<-lm(Y_train ~ . -1, data=as.data.frame(Z))  #Entrenamos quitando el intercepto
  B<-b$coefficients
  y_train_estim<- Z %*% B
  #Buscamos cual es el valor m?ximo en cada rengl?n (argmax)
  y_train_estim <- apply(y_train_estim, 1, which.max)-1 
  #Obtenemos el error cuadrado medio para el conjunto de entrenamiento
  err_cuad_train<- sum(abs(y_train-y_train_estim))/length(y_train)
  #Evaluamos con el conjunto de entrenamiento
  y_test_estim <- (as.matrix(test[,2:257])%*%z)%*%B
  y_test_estim <- apply(y_test_estim, 1, which.max)-1
  #Calculamos el error cuadrado medio para el conjunto de prueba
  err_cuad_test<-sum(abs(y_test-y_test_estim))/length(y_test)
  error<-c(err_cuad_train, err_cuad_test)
  error
}

#Generamos los errores para p en desde 1 hasta 256
error_train<-rep(1,256)
error_test<-rep(1,256)
for (i in 1:256){
  a<-model_eval(i)
  error_train[i]<-a[1]
  error_test[i]<-a[2]
}

plot(1:256, error_train, main="Error cuadrado medio Vs P", xlab="Número de componentes principales", ylab="MSE", type="l",col="blue")
points(1:256, error_test, col="red", type="l")
abline(v=25)
legend(150,2.5, legend=c("MSE training", "MSE test"),  col=c("blue", "red"), lty=1:2, cex=0.8)
