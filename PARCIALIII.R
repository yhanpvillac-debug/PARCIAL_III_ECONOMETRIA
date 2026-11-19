#############################################################
######                                                 ######
#                 UNIVERSIDAD DEL QUINDÍO                   #
#                  PROGRAMA DE ECONOMÍA                     #
#                      ECONOMETRIA                          #
######                                                 ######
#############################################################

# BY: Yhan Pablo Villa Cuartas
# BY: Brayan Estiven Agudelo Arbelaez


# Parcial III - Econometría I | Ecuación de salarios (Mincer), GEIH 2025
# Eje Cafetero: Caldas (17), Risaralda (66), Quindío (63)
# install.packages(c("lmtest","car","tseries","sandwich","ggplot2"))

DATA_INGRESOS = read.csv("C:/Users/yhanp/Downloads/geih_mincer_final.csv", sep = ",")  


library(lmtest); library(car); library(tseries); library(sandwich); library(ggplot2)

d <- read.csv(file.choose())
d$dpto <- factor(d$dpto, levels = c(63, 17, 66), labels = c("Quindio", "Caldas", "Risaralda"))

## (b) Descriptivo --------------------------------------------------------
vars <- c("lwage", "educ", "exper", "mujer", "formal")
summary(d[, vars])
sapply(d[, vars], sd)
round(cor(d[, vars]), 2)

hist(d$lwage, breaks = 50, main = "ln(salario por hora)", xlab = "")
hist(d$educ, breaks = 20, main = "Años de educación", xlab = "")
boxplot(lwage ~ mujer, data = d, names = c("Hombre", "Mujer"), ylab = "ln(salario/hora)")
boxplot(lwage ~ formal, data = d, names = c("Informal", "Formal"), ylab = "ln(salario/hora)")
boxplot(lwage ~ dpto, data = d, ylab = "ln(salario/hora)")
ggplot(d[sample(nrow(d), 5000), ], aes(educ, lwage)) + geom_jitter(alpha = .2) +
  geom_smooth(method = "lm") + labs(x = "Años de educación", y = "ln(salario/hora)")
ggplot(d[sample(nrow(d), 5000), ], aes(exper, lwage)) + geom_point(alpha = .2) +
  geom_smooth(method = "loess") + labs(x = "Experiencia potencial", y = "ln(salario/hora)")

## (c) MCO ----------------------------------------------------------------
m <- lm(lwage ~ educ + exper + I(exper^2) + mujer + formal + dpto, data = d)
summary(m)
confint(m)

## (d) Supuestos ----------------------------------------------------------
resettest(m, power = 2:3, type = "fitted")      # especificación / linealidad
vif(m)                                          # multicolinealidad
bptest(m)                                       # Breusch-Pagan
bptest(m, ~ fitted(m) + I(fitted(m)^2))         # White (forma reducida)
dwtest(m)                                       # Durbin-Watson
bgtest(m, order = 2)                            # Breusch-Godfrey
jarque.bera.test(resid(m))                      # normalidad
hist(resid(m), breaks = 60, main = "Residuos"); qqnorm(resid(m)); qqline(resid(m))

## Corrección por heterocedasticidad: errores robustos
coeftest(m, vcov = vcovHC(m, type = "HC3"))