## Análisis de tuits, comentarios youtube y tiktoks con diferentes modelos de IA

## Código elaborado por Toaki Hoz Canabal

## readme: este código se enfoca en realizar lo del código de análisis de emociones con ollama mall pero con diferentes modelos 

## setup -----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") #Cambiar locale para prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica

# Instalamos {ollama}, necesario para usar mall: https://hauselin.github.io/ollama-r/#installation
# install.packages("ollamar")

## Instalamos {mall}: https://mlverse.github.io/mall/#sentiment  -------
# install.packages("mall")


# Caargamos paquetes ------
if(!require('pacman')) install.packages('pacman')
pacman::p_load(blastula, collapse, dplyr, gt, mall, ollamar)


## Instalamos modelos ------
list_models() # para ver modelos actuales 

# INstalé los modelos que consideré los más idóneos


## deepseek-r1:8b  --------

# IMPUTA PUROS NA
# IMPUTA PUROS NA
# IMPUTA PUROS NA
# IMPUTA PUROS NA
# IMPUTA PUROS NA
# IMPUTA PUROS NA
# IMPUTA PUROS NA
# IMPUTA PUROS NA
# IMPUTA PUROS NA
# IMPUTA PUROS NA
# IMPUTA PUROS NA
# IMPUTA PUROS NA

##



# mistral:latest 4.1 GB  -------
# IMPUTA PUROS NA
# IMPUTA PUROS NA
# IMPUTA PUROS NA
# IMPUTA PUROS NA
# IMPUTA PUROS NA
# IMPUTA PUROS NA
# IMPUTA PUROS NA
# IMPUTA PUROS NA
# IMPUTA PUROS NA
# IMPUTA PUROS NA
# IMPUTA PUROS NA
# IMPUTA PUROS NA
.rs.restartR()
rm(list = ls())

# AL cargar el modelo: 
# ── mall session object 
# Backend: Ollama
# LLM session: model:mistral:latest
# R session: cache_folder:C:\Users\bhoz\AppData\Local\Temp\RtmpsnmooW\_mall_cache37d84ebfd7d


# cargamos tiktoks
tiktoks <- readRDS("~/colmex/Isaac Cineros/Analisis de sentimeinto con llm (varios modelos)/0_datos_de_carga/tiktok_con_lenguajes.rds")

## Split a nuestro objeto, nos da una lista por renglón
p1 <- tiktoks |> 
  fmutate(id= seq_row(tiktoks)) |> 
  _[, c("comment", "id")] |> 
  rsplit(~id, flatten = T)



# Definimos el tamaño del grupo 
tamano_grupo <- 20

# Creamos los índices para dividir p1 en grupos de tamaño_grupo
p2 <- split(p1, ceiling(seq_along(p1) / tamano_grupo))

# Recorremos cada objeto de p2 usando lapply
lista_final <- list()

# Creamos vector
vector <- seq_along(p2)

# Inicializa el contador
contador <- 0

## For de análisis 
for (i in vector) {
  tryCatch({
    # Aplicamos lapply a cada objeto de p2[[i]]
    lista_final[[i]] <- lapply(p2[[i]], \(x) {
      x |> 
        llm_vec_sentiment()  # Aplica la función llm_vec_sentiment a cada elemento
    })
    
    # Imprime el progreso
    print(paste0("Acabamos el objeto ", i, " de p2"))
    
    Sys.sleep(1)  # 1 segundo de espera (puedes ajustar el tiempo según sea necesario)
    
    # Incrementa el contador
    contador <- contador + 1
    
    # Guarda cada 600 iteraciones
    if (contador %% 600 == 0) {
      saveRDS(lista_final, paste0("01_datos_resultantes_llm_trabajados/deepseek/deepseek_tiktok_", i, ".rds"))
      print(paste0("Guardado después de ", contador, " iteraciones."))
    }
    
    # Solo cuando la última iteración se complete, ejecutamos las acciones
    if (i == length(vector)) {
      # 1. Guardar el objeto 'lista_final' en un archivo .rds
      saveRDS(lista_final, paste0("01_datos_resultantes_llm_trabajados/deepseek/deepseek_tiktok_", length(vector), ".rds"))
      
      #2. Mensaje para indicar que el proceso terminó
      print("Todas las iteraciones han finalizado de tiktok")
      
      
      # DAmos unos momentos para que sí guarde
      Sys.sleep(3)
      
      
      # creamos el cuerpo del correo
      msg <- compose_email(body = md(paste0("Terminó de correr el análisis de tiktok")))
      
      # Mandamos correo
      msg %>% 
        smtp_send(from = "toakihozcanabal@gmail.com",
                  to = "toaki07@hotmail.com", # enviar el correo con smtp_send
                  subject = "Finalización tiktok",
                  credentials = my_creeds
        )
      
      # Imprimos para el txt 
      print("Correo enviado de tiktok")
    }
    
  }, error = function(e) {
    # Si ocurre un error, imprímelo y continúa con el siguiente
    print(paste0("Error en el objeto ", i, ": ", e$message))
  })
}



## LLAMA -----
.rs.restartR()
rm(list = ls())

# AL cargar el modelo: 



# cargamos tiktoks
tiktoks <- readRDS("~/colmex/Isaac Cineros/Analisis de sentimeinto con llm (varios modelos)/0_datos_de_carga/tiktok_con_lenguajes.rds")

## Split a nuestro objeto, nos da una lista por renglón
p1 <- tiktoks |> 
  fmutate(id= seq_row(tiktoks)) |> 
  _[, c("comment", "id")] |> 
  rsplit(~id, flatten = T)



# Definimos el tamaño del grupo 
tamano_grupo <- 20

# Creamos los índices para dividir p1 en grupos de tamaño_grupo
p2 <- split(p1, ceiling(seq_along(p1) / tamano_grupo))

# Recorremos cada objeto de p2 usando lapply
lista_final <- list()

# Creamos vector
vector <- seq_along(p2)

# Inicializa el contador
contador <- 0

## For de análisis 
for (i in vector) {
  tryCatch({
    # Aplicamos lapply a cada objeto de p2[[i]]
    lista_final[[i]] <- lapply(p2[[i]], \(x) {
      x |> 
        llm_vec_sentiment()  # Aplica la función llm_vec_sentiment a cada elemento
    })
    
    # Imprime el progreso
    print(paste0("Acabamos el objeto ", i, " de p2"))
    
    Sys.sleep(1)  # 1 segundo de espera (puedes ajustar el tiempo según sea necesario)
    
    # Incrementa el contador
    contador <- contador + 1
    
    # Guarda cada 600 iteraciones
    if (contador %% 600 == 0) {
      saveRDS(lista_final, paste0("01_datos_resultantes_llm_trabajados/deepseek/deepseek_tiktok_", i, ".rds"))
      print(paste0("Guardado después de ", contador, " iteraciones."))
    }
    
    # Solo cuando la última iteración se complete, ejecutamos las acciones
    if (i == length(vector)) {
      # 1. Guardar el objeto 'lista_final' en un archivo .rds
      saveRDS(lista_final, paste0("01_datos_resultantes_llm_trabajados/deepseek/deepseek_tiktok_", length(vector), ".rds"))
      
      #2. Mensaje para indicar que el proceso terminó
      print("Todas las iteraciones han finalizado de tiktok")
      
      
      # DAmos unos momentos para que sí guarde
      Sys.sleep(3)
      
      
      # creamos el cuerpo del correo
      msg <- compose_email(body = md(paste0("Terminó de correr el análisis de tiktok")))
      
      # Mandamos correo
      msg %>% 
        smtp_send(from = "toakihozcanabal@gmail.com",
                  to = "toaki07@hotmail.com", # enviar el correo con smtp_send
                  subject = "Finalización tiktok",
                  credentials = my_creeds
        )
      
      # Imprimos para el txt 
      print("Correo enviado de tiktok")
    }
    
  }, error = function(e) {
    # Si ocurre un error, imprímelo y continúa con el siguiente
    print(paste0("Error en el objeto ", i, ": ", e$message))
  })
}
