# Ejercicio 1.
SpearHeads <- data.frame(spearheads)
print(spearheads)
View(SpearHeads)
str(SpearHeads)
class(SpearHeads)


# Ejercicio 2.
names(SpearHeads)[names(SpearHeads) == "Mat"] <- "Materiales"
names(SpearHeads)[names(SpearHeads) == "Con"] <- "Contexto"
names(SpearHeads)[names(SpearHeads) == "Cond"] <- "Conservación"
names(SpearHeads)[names(SpearHeads) == "Loo"] <- "Loop"
names(SpearHeads)[names(SpearHeads) == "Peg"] <- "Remache"
names(SpearHeads)[names(SpearHeads) == "Date"] <- "Fecha"
names(SpearHeads)[names(SpearHeads) == "Maxle"] <- "Longitud_Max"
names(SpearHeads)[names(SpearHeads) == "Socle"] <- "Longitud_encaje"
names(SpearHeads)[names(SpearHeads) == "Maxwi"] <- "Ancho_max"
names(SpearHeads)[names(SpearHeads) == "Upsoc"] <- "Ancho_encaje"
names(SpearHeads)[names(SpearHeads) == "Maxwit"] <- "Ancho_max_encaje"
names(SpearHeads)[names(SpearHeads) == "Weight"] <- "Peso"

# Ejercicio 3.
SpearHeads$Contexto=factor(SpearHeads$Contexto, levels=c (1,2,3), labels=c("S/C", "Habitacional", "Fuerario"))
SpearHeads$Conservación=factor(SpearHeads$Conservación, levels=c (1,2,3,4), labels=c("Excelente", "Bueno", "Regular", "Malo"))
SpearHeads$Remache=factor(SpearHeads$Remache, levels=c (1,2), labels=c('Si', 'No'))
SpearHeads$Materiales=factor(SpearHeads$Materiales, levels=c (1,2), labels=c('Bronce', 'Hierro'))

View(SpearHeads)
# Ejercicio 4.
freq.cont=table(SpearHeads$Contexto)
View(freq.cont)

freq.con=table(SpearHeads$Conservación)
View(freq.con)

freq.mat=table(SpearHeads$Materiales)
View(freq.mat)

# Ejercicio 5. 
cross.contmat = table (SpearHeads$Contexto,SpearHeads$Conservación)
View(cross.contcon)
cross.conmat = table (SpearHeads$Conservación,SpearHeads$Materiales)
View(cross.conmat)

# Ejercicio 6. #los datos no aparecen en porcentaje asi que multiplicamos los valores de las columnas por 100
prop.mat=prop.table(freq.mat)
View(prop.mat)
prop.mat <- as.data.frame(prop.mat)
prop.mat$Porcentaje <- prop.mat$Freq * 100
prop.mat

prop.cont=prop.table(freq.cont)
View(prop.cont)
prop.cont <- as.data.frame(prop.cont)
prop.cont$Porcentaje <- prop.cont$Freq * 100
prop.cont

prop.con=prop.table(freq.con)
View(prop.con)
prop.con <- as.data.frame(prop.con)
prop.con$Porcentaje <- prop.con$Freq * 100
prop.con

# Ejercicio 7. 
prop.cross.contmat=round(prop.table(cross.contmat) *100,0)
View(prop.cross.contmat)
prop.cross.conmat=round(prop.table(cross.conmat) *100,0)
View(prop.cross.conmat)

# Ejercicio 8.
bar.con=barplot(table(SpearHeads$Conservación))
bar.cont=barplot(table(SpearHeads$Contexto))


# Ejercicio 9.
barh.mate=barplot(table(SpearHeads$Materiales), horiz=TRUE,
                  main = "Frecuencia de materiales",
                  xlab = "Frecuencia (%)",
                  ylab = "Periodos",
                  col = "blue")

barh.rema=barplot(table(SpearHeads$Remache), horiz=TRUE,
                  main = "Frecuencia de remaches",
                  xlab = "Frecuencia (%)",
                  ylab = "Remaches",
                  col = "black" )

# Ejercicio 10.
bar.con= barplot(cross.conmat, width = 0.85, ylim=c (0, sum(cross.conmat[,1]*1.1),
          main = "Estado de conservación vs. Materiales",
          ylab = "Frecuencia",
          legend=T)
View(bar.con)

# Ejercicio 11.
pie(table(SpearHeads$Conservación),
    main= "distribución de los estados de conservación",
    col= c("red", "yellow", "lightblue", "green"),
    labels = paste(names(freq.con), "(", (prop.con), "%)"))

# Ejercicio 12.
windows(width =25, height =25)
vari_continuas =SpearHeads[sapply(SpearHeads, is.numeric)]
hist.variables = hist(unlist(vari_continuas),
                      main = "Probabilidad de variables continuas",
                      xlab = "Valor",
                      prob = TRUE,
                      col = "violet")

