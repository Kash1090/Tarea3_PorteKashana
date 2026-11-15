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

# benford.mad
# Recibe: x (vector numerico de montos)
# Devuelve: la MAD respecto a la ley de Benford (numero)
benford.mad <- function(x) {
  x <- x[!is.na(x) & x > 0]
  d <- x %/% 10^floor(log10(x))
  p <- tabulate(d, nbins = 9) / length(d)
  b <- log10(1 + 1 / (1:9))
  mean(abs(p - b))
}

# benford.canton
# Recibe: df (transacciones), canal (texto), cantones (data frame con cod_canton y canton)
# Devuelve: data frame con cod_canton, canton, n, mad, clase; ordenado de mayor a menor MAD
benford.canton <- function(df, canal, cantones) {
  sub <- df[df$canal == canal, ]
  partes <- split(sub$monto_crc, sub$cod_canton)
  res <- data.frame(
    cod_canton = names(partes),
    n   = sapply(partes, length),
    mad = sapply(partes, benford.mad),
    row.names = NULL
  )
  res$clase <- as.character(cut(res$mad,
                                breaks = c(-Inf, 0.006, 0.012, 0.015, Inf),
                                labels = c("conformidad cercana", "conformidad aceptable",
                                           "conformidad marginal", "no conformidad")))
  cantones$cod_canton <- as.character(cantones$cod_canton)
  res <- merge(res, cantones[, c("cod_canton", "canton")], by = "cod_canton")
  res <- res[order(-res$mad), c("cod_canton", "canton", "n", "mad", "clase")]
  rownames(res) <- NULL
  res
<<<<<<< HEAD
}

# valor.acumulado
# Recibe: df (transacciones), tasa.anual (efectiva anual, decimal), canal (texto)
# Devuelve: lista con tabla (mes, monto, factor), tasa.mensual, va, interes
valor.acumulado <- function(df, tasa.anual, canal) {
  sub <- df[df$canal == canal, ]
  mes <- as.integer(format(as.Date(sub$fecha), "%m"))
  monto <- tapply(sub$monto_crc, factor(mes, levels = 1:12), sum)
  monto[is.na(monto)] <- 0
  tasa.mensual <- (1 + tasa.anual)^(1 / 12) - 1
  tabla <- data.frame(mes = 1:12, monto = as.numeric(monto),
                      factor = (1 + tasa.mensual)^(12 - (1:12)))
  va <- sum(tabla$monto * tabla$factor)
  list(tabla = tabla, tasa.mensual = tasa.mensual,
       va = va, interes = va - sum(tabla$monto))
} 
=======
}
>>>>>>> feat/ej2-benford
