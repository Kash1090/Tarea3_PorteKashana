# Funciones de la Tarea 3

# alerta.movil
# Recibe: df (transacciones con id_tx, fecha), ventana (entero), k.alerta (numerico)
# Devuelve: data frame con fecha, n.tx, media, desv, alerta
alerta.movil <- function(df, ventana, k.alerta) {
  d <- aggregate(id_tx ~ fecha, data = df, FUN = length)
  names(d) <- c("fecha", "n.tx")
  d <- d[order(d$fecha), ]
  n <- nrow(d)
  d$media <- NA_real_
  d$desv  <- NA_real_
  for (t in (ventana + 1):n) {
    x <- d$n.tx[(t - ventana):(t - 1)]   # solo dias anteriores, sin incluir t
    d$media[t] <- mean(x)
    d$desv[t]  <- sd(x)
  }
  d$alerta <- !is.na(d$media) & d$n.tx > d$media + k.alerta * d$desv
  d
}