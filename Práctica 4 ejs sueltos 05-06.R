library(brms)
library(dplyr)
library(readr)
library(ggplot2)
library(bayesplot)
library(patchwork)
library(gridGraphics)
library(GGally)
library(car)
library(dplyr)
library(tidyverse)


url <- "https://raw.githubusercontent.com/estadisticaunr/estadistica-bayesiana/main/datos/weather_WU.csv"
weather <- readr::read_csv(url)

ggplot(weather)+
  geom_point(aes(x=temp9am, y=temp3pm, color=location))

ggplot(weather)+
  geom_point(aes(x=location, y=temp3pm))


# Y = Temperatura a las 3pm
# X_1 = Temperatura a las 9am
# X_2 = Ciudad,   Uluru, Wollongong


modelo1 <- brm(temp3pm ~ temp9am, weather)
modelo2 <- brm(temp3pm ~ location, weather)
modelo2.2 <- brm(temp3pm ~ 0+location, weather) # mu_i = beta_1*Z_1i + beta_2*Z_2i
modelo3 <- brm(temp3pm ~ temp9am + location, weather)
modelo4 <- brm(temp3pm ~ temp9am * location, weather)

plot(modelo1)
plot(modelo2)
plot(modelo3)
plot(modelo4)

pairs(modelo3, off_diag_args = list(size = 1/3, alpha = 1/3))



# Crear nuevo conjunto de datos para obtener predicciones (y visualizaciones)
x_min <- min(weather$temp9am)
x_max <- max(weather$temp9am)
df_new <- data.frame(temp9am = seq(x_min, x_max, length.out = 100))

# Predecir la media (obtener posterior de mu)
df_new_mean <- fitted(modelo1, newdata = df_new) |>
  as.data.frame() |>
  bind_cols(df_new) |>
  select(estimate = Estimate, lower = Q2.5, upper = Q97.5, temp9am)

# Mostrar el intervalo de credibilidad para la recta de regresión
ggplot(df_new_mean) + 
  geom_ribbon(
    aes(x = temp9am, ymin = lower, ymax = upper), 
    fill = "grey80", 
    alpha = 0.8
  ) +
  geom_line(aes(x = temp9am, y = estimate), linewidth = 1, color = "red") + 
  geom_point(aes(x = temp9am, y = temp3pm), alpha = 0.6, size = 2, data = weather) +
  labs(x = "publicidad", y = "ventas")

# Predecir nuevas observaciones (obtener predictiva a posteriori)
df_new_predict <- predict(modelo1, newdata = df_new) |>
  as.data.frame() |>
  bind_cols(df_new) |>
  select(estimate = Estimate, lower = Q2.5, upper = Q97.5, temp9am)

# Mostrar el intervalo de credibilidad para la recta de regresión 
# y para nuevas observaciones
ggplot(df_new_mean) + 
  geom_ribbon(
    aes(x = temp9am, ymin = lower, ymax = upper),
    fill = "grey80",
    data = df_new_predict
  ) +
  geom_ribbon(
    aes(x = temp9am, ymin = lower, ymax = upper), 
    fill = "grey50", 
    alpha = 0.8
  ) +
  geom_line(aes(x = temp9am, y = estimate), linewidth = 1, color = "red") + 
  geom_point(aes(x = temp9am, y = temp3pm), alpha = 0.6, size = 2, data = weather) +
  labs(x = "publicidad", y = "ventas")


mcmc_intervals_data(modelo3, pars = c("b_Intercept", "b_temp9am", "b_locationWollongong"))


# ^b_: que comientce con "b_"
dfinterval1 <- mcmc_intervals_data(modelo1, regex_pars = "^b_", pars = "sigma")
dfinterval2 <- mcmc_intervals_data(modelo2, regex_pars = "^b_", pars = "sigma")
dfinterval3 <- mcmc_intervals_data(modelo3, regex_pars = "^b_", pars = "sigma")
dfinterval4 <- mcmc_intervals_data(modelo4, regex_pars = "^b_", pars = "sigma")

dfinterval1$modelo <- "Modelo 1"
dfinterval2$modelo <- "Modelo 2"
dfinterval3$modelo <- "Modelo 3"
dfinterval4$modelo <- "Modelo 4"

dfinterval <- rbind(dfinterval1, dfinterval2, dfinterval3, dfinterval4)

ggplot(dfinterval, aes(color=modelo, y=parameter))+
  geom_point(aes(x=m), position = position_dodge(0.4))+ 
  geom_linerange(aes(xmin=ll, xmax=hh), position = position_dodge(0.4))+
  geom_linerange(aes(xmin=l, xmax=h), linewidth=1, position = position_dodge(0.4))+
  facet_wrap(~parameter, scales = "free")+
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())


# aparecen todos los modelos, con los parámetros de cada una. 
# Si un modelo no tiene la interaccion, este no aparecerá en el modelo  

# el posterior de sigma es mas bajo en el modelo 1. 
# Esto es xq la variabilidad residual es menor
# la variable temp a las 9 am explica mas que la ciudad.
# cuando tenemos ambas esta se achica mucho. 

# Vemos que la interaccion no es significativa, pues el intervalo cubre al cero.
# Vemos que siguen una pendiente parecida. 

# El intercepto no significa lo mismo en cada modelo, por lo que no serían comparables.

pp_check(modelo1, ndraws=100)
pp_check(modelo2, ndraws=100)
pp_check(modelo3, ndraws=100)
pp_check(modelo4, ndraws=100)

loo1 <- loo(modelo1)
loo2 <- loo(modelo2)
loo3 <- loo(modelo3)
loo4 <- loo(modelo4)

c(loo1$elpd_loo,loo2$elpd_loo,loo3$elpd_loo,loo4$elpd_loo)
loo_compare(loo1, loo2, loo3, loo4)

# la mejor capacidad predictiva la da el modelo 3, sin interacción y con ambas variables




##### Ejercicio 12
# 1. Cargar los datos 
# 2. Medidas descriptivas de las variables
# 3. Ver datos faltantes, observaciones atípicas, datos erróneos
# 4. Visualizaciones (univariados y multivariados)


fishmarket <- readr::read_csv("https://raw.githubusercontent.com/estadisticaunr/estadistica-bayesiana/main/datos/fish-market.csv")

ggpairs(fishmarket, mapping = aes(color = Species))

modelo <- (lm(Weight~Length3+Length1+Length2, fishmarket))
Anova(modelo, type=2)
vif(modelo)


summary(datos2[-1])

datos2 <- fishmarket[-41,]

datos2 %>% group_by(Species) %>% 
  summarise(n=n(),
            Media_Weight=mean(Weight),
            Media_Length1=mean(Length1),
            Media_Length2=mean(Length2),
            Media_Length3=mean(Length3),
            Media_Height=mean(Height),
            Media_Width=mean(Width))


ggplot(datos2)+
  geom_point(aes(y=Weight, x=Height, col=Species))
ggplot(datos2)+
  geom_point(aes(y=Weight, x=Lenght3, col=Species))
ggplot(datos2)+
  geom_point(aes(y=Weight, x=Width, col=Species))+
  facet_wrap(~Species)


ggplot(datos2)+
  geom_point(aes(y=log(Weight), x=log(Height), col=Species))
ggplot(datos2)+
  geom_point(aes(y=Weight, x=Lenght3, col=Species))
ggplot(datos2)+
  geom_point(aes(y=log(Weight), x=log(Width), col=Species))+
  facet_wrap(~Species)

perch <- datos2[datos2$Species == "Perch",]

summary(lm(log(Weight)~Species*log(Height)*log(Width), datos2))
brm()

modeloperch <- (lm(log(Weight)~log(Height)*log(Width), perch))
influencePlot(modeloperch)
ggplot(perch)+
  geom_point(aes(y=log(Weight), x=log(Width)))+
  geom_point(aes(y=log(Weight), x=log(Width)), data=perch[c(19),], col="orange")


# Interpretación de beta_0: cuando x = 1, log(y) = beta_0
# Si un pescado mide 1cm, su peso será de e^beta_0
# A partir de acá se puede proponer la dist a priori

# planteamos la diferencia para un x+1, para interpretar el beta_1
# log(mu_0) = Beta_0 + Beta_1*Log(X)
# log(mu_1) = Beta_0 + Beta_1*Log(X*1.01)
# log(mu_1) = Beta_0 + Beta_1*Log(X*1.01) <-  Beta_1(Log(1.01) + Log(X))
# log(mu_1) = Beta_0 + Beta_1*Log(X) + Beta_1*Log(1.01)

# log(mu_0)-log(mu_1) = Beta_1*Log(1.01)
# Log(mu_0 / mu_1) = Beta_1*Log(1.01)
# mu_0 / mu_1 = e^(Beta_1*Log(1.01)) = 1.01^Beta_1
# mu_0 = 1.01^Beta_1 * mu_1

# (mu1-mu0)/mu0 = (1.01^beta1 - 1)
# cuando x crezca un 1%, la media de y se va a incrementar en un (1.01^beta1 - 1)%


priors <- c(
  prior(normal(0, 10), class = "b", coef = "Intercept"),
  prior(normal(0, 5), class = "b", coef = "log(Length1)")
)

mod1 <- brm(log(Weight)~0+Species+log(Length1):Species, data = datos2)
mod1
plot(mod1)  


