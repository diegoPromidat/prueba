box::use(
  utils[read.table],
  readr[read_rds],
  tidyr[extract],
  dplyr[select, mutate, case_when],
  forcats[fct_expand],
  RSDA[classic.to.sym]
)

#' @export
limpieza <- function(datos, transaccion = F) {
  as_factor <- function(x) factor(replace(x,is.na(x), "no.aplica"), ordered = T)
  
  datos <- datos |>
    extract(BUSINESS_DATE,c("BUSINESS_DATE_DIA","BUSINESS_DATE_MES"),
            regex  = "([[:digit:]]+)/([[:digit:]]+)/[[:digit:]]+") |>
    extract(VALUED_DATE,c("VALUED_DATE_DIA","VALUED_DATE_MES","VALUED_DATE_ANIO"),
            regex  = "([[:digit:]]+)/([[:digit:]]+)/([[:digit:]]+)") |>
    extract(HORA, c("HORA_HORA","HORA_MIN"), regex  = "([[:digit:]]+):([[:digit:]]+):[[:digit:]]+")|>
    mutate(BUSINESS_DATE_DIA = as_factor(BUSINESS_DATE_DIA),
           BUSINESS_DATE_MES = as_factor(BUSINESS_DATE_MES),
           VALUED_DATE_DIA = as_factor(VALUED_DATE_DIA),
           VALUED_DATE_MES = as_factor(VALUED_DATE_MES),
           VALUED_DATE_ANIO = as_factor(VALUED_DATE_ANIO),
           TXN_CODE_ID =  as.factor(TXN_CODE_ID),
           TRANSACTION_REASON = as.factor(TRANSACTION_REASON),
           HORA_HORA = as_factor(HORA_HORA),
           HORA_MIN = as_factor(HORA_MIN))|>
    mutate(OPERATION_ID = as.factor(OPERATION_ID),
           MONEDA_ID_TRAN = as.factor(datos$MONEDA_ID_TRAN),
           MONEDA_ID_ACCOUNT = as.factor(datos$MONEDA_ID_ACCOUNT),
           REASON_ID = as.factor(datos$REASON_ID),
           PRODUCT = as.factor(datos$PRODUCT),
           SUBPRODUCT = as.factor(datos$SUBPRODUCT),
           STATUS = as.factor(datos$STATUS),
           ACCOUNT = as.factor(datos$ACCOUNT),
           NUM_CHEQUE = as.factor(datos$NUM_CHEQUE),
           CUSTOMER_ID = as.factor(CUSTOMER_ID),
           AMOUNT = case_when(MONEDA_TRAN_DESCRIPTION == "COLONES" ~ AMOUNT/QUOT_BUY2, T~AMOUNT )) |>
    select(-c(CODTIPO_PERSONA_SUGEF, REFERENCE_NUMBER, EDAD, NACIONALIDAD, ACCOUNT,
              OPERATION_ID,NUM_CHEQUE, CUENTA_ORIGEN, ESTADO_CIVIL, PROFESION,
              CUENTA_DESTINO, NIVEL_EDUCACION, PEP,FATCA, EMPLEADO, 
              TIPO_PERSONA_SUGEF, ACCOUNT, REFERENCE_NUMBER, PURE_ACTION, PRODUCT,
              SUBPRODUCT, MONEDA_ID_TRAN, MONEDA_ID_ACCOUNT, CUENTA_ORIGEN,
              CUENTA_DESTINO, NUM_CHEQUE, STATUS, QUOT_BUY2))
  
  names. <- c(
    "PERSON_TYPE", "IDENTIFICATION_TYPE", "TRANSACTION_REASON",
    "TRANSACTION_REASON_DESCRIPTION", "TXN_CODE_ID", 
    "TRANSACTION_CORE_DESCRIPTION", "REASON_ID", "REASON_DESCRIPTION", 
    "PURE_ACTION_DESCRIPTION", "PRODUCT_DESCRIPTION", "SUBPRODUCT_DESCRIPTION",
    "AMOUNT", "MONEDA_TRAN_DESCRIPTION", "MONEDA_ID_ACCOUNT_DESCRIPTION", 
    "BUSINESS_DATE_DIA", "BUSINESS_DATE_MES", "VALUED_DATE_DIA", 
    "VALUED_DATE_MES", "VALUED_DATE_ANIO", "COD_BANCO_ORIGEN", 
    "COD_BANCO_DESTINO", "DESCRIPCION_STATUS", "HORA_HORA", "HORA_MIN",
    "MONTHLY_AMMOUNT_IN_CASH")
  
  aux_anio <- datos$VALUED_DATE_ANIO
  aux_dia  <- datos$VALUED_DATE_DIA
  aux_mes  <- datos$VALUED_DATE_MES
  
  for (name in names.) {
    if(is.factor(datos[[name]])) {
      datos[[name]] <- fct_expand(datos[[name]], as.character(levels(datos[[name]])))
    }
  }
  
  if(!transaccion) {
    datos$ID <- NULL
  }
  
  return(datos)
}

#' @export
cargar_perfiles <- function() {
  datos <- read.table("app/static/datos/Banca_Aprendizaje_V1.csv", T, sep = ",",
                      dec = ".", stringsAsFactors = T, row.names = 1)
  datos <- limpieza(datos)
  datos <- classic.to.sym(datos, concept = "CUSTOMER_ID")
  return(datos)
}

#' @export
cargar_distancias <- function() {
  return(read_rds("app/static/datos/distancias_maximas.rds"))
}