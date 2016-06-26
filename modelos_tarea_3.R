library(boot)
library(lpSolve)

# Autor: Georvic Tur
# Carnet: 12-11402
# Correo: alexanderstower@gmail.com


#Primera Parte

M <- matrix(0 ,nrow=12, ncol=24)

M[1,1]=1
M[1,2]=1
M[1,3]=1
M[1,4]=1
M[1,5]=-1
M[1,21]=-1
M[2,1]=-1
M[2,5]=1
M[2,6]=1
M[2,7]=1
M[2,8]=1
M[2,9]=-1
M[3,6]=-1
M[3,9]=1
M[3,10]=1
M[3,11]=1
M[3,12]=1
M[3,13]=-1
M[4,10]=-1
M[4,13]=1
M[4,14]=1
M[4,15]=1
M[4,16]=1
M[4,17]=-1
M[5,14]=-1
M[5,17]=1
M[5,18]=1
M[5,19]=1
M[5,20]=1
M[5,22]=-1
M[6,2]=-1
M[6,18]=-1
M[6,21]=1
M[6,22]=1
M[6,23]=1
M[6,24]=1
M[7,3]=1
M[7,7]=1
M[8,8]=1
M[8,11]=1
M[9,12]=1
M[9,15]=1
M[10,16]=1
M[10,19]=1
M[11,20]=1
M[11,23]=1
M[12,4]=1
M[12,24]=1



oferta <- c(0,5,10,15,20,25)
demanda <- rep(sum(oferta)/length(oferta), length(oferta))

b <- c(oferta, demanda)

costos = c(5,5,7,7,5,5,1,1,5,5,7,7,5,5,1,1,5,5,7,7,5,5,1,1)

dirs <- rep("=",12)



RES <- lp("min",costos,M,dirs,b)

RES[["solution"]]


AAA<-matrix(RES[["solution"]], nrow=6,ncol=4,byrow=TRUE)

write.table(AAA, file = "pregunta1.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")


            
# Segunda Parte

costos2 = rep(3,24)

RES2 <- lp("min",costos2,M,dirs,b)

BBB<-matrix(RES2[["solution"]], nrow=6,ncol=4,byrow=TRUE)

write.table(BBB, file = "pregunta2.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")


# Tercera Parte

#Carnet: 12-11402
oferta2 <- c(1,2,1,1,4,0)
demanda2 <- rep(sum(oferta2)/length(oferta2), length(oferta2))

b2 <- c(oferta2, demanda2)

RES3 <- lp("min",costos,M,dirs,b2)

CCC<-matrix(RES3[["solution"]], nrow=6,ncol=4,byrow=TRUE)

write.table(CCC, file = "pregunta3.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")

# Cuarta Parte

