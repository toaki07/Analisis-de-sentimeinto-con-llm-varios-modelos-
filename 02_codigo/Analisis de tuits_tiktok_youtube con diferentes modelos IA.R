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

# Cargamos credenciales -------
Sys.setenv(GMAIL_PWD = 'pxeb uvcq jdjk epjm')
Sys.setenv(accountGmail = 'toakihozcanabal@gmail.com')

my_creeds <- creds_envvar(user = Sys.getenv("accountGmail"),
                          pass_envvar = "GMAIL_PWD",
                          provider = 'gmail')


## Instalamos modelos ------
test_connection() 
list_models() # para ver modelos actuales 

# Instalé los modelos que consideré los más idóneos





# deepseek-r1:8b  --------
── mall session object 
Backend: Ollama
LLM session: model:deepseek-r1:8b
R session: cache_folder:C:\Users\bhoz\AppData\Local\Temp\Rtmpe4mHOS\_mall_cache13bc23d8ce4

## # Twitter -------
twitter_todos <- readRDS("0_datos_de_carga/twiter_todos_con_lenguajes.rds")

## Limpiamos idiamos que no funcionan en su mayoría (dada una revisión rápida) 
twitter_todos <- twitter_todos |> 
  tidyr::unnest(lenguaje) |> 
  filter(!lenguaje %in% c("danish", "english", "frisian", "german", "hubgarian", "icelandic", "nepali", "norwegian", "tagalog", "turkish", "welsh")) 

# quitamos NA 
twitter_todos <- twitter_todos |> 
  # select(comment_content, content_cleaned, lenguaje) |> 
  filter(!is.na(lenguaje))
## BAjamos a 226k tuits !! ojo con esto 

twitter_todos <- twitter_todos |> 
  select(comment_content) |> 
  mutate(comment_content = stringr::str_replace_all(comment_content, "#\\w+", ""), #  uita hasgtags pero se puede mejor con gsub
         comment_content = stringr::str_replace_all(comment_content, "@\\w+", ""), #  quita arrobas
         comment_content = stringr::str_squish(comment_content), #para quitar espacio
         comment_content = gsub("https://\\S+", "", comment_content)) |>  # para quitar links
  filter(comment_content != "") 

# ## Generamos P
# p <- twitter_todos


# Generamos lista
p1 <-  
  twitter_todos|> 
  fmutate(id= seq_row(twitter_todos)) |> 
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

# ## Este funciona alvvvvvvvv ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###                                                                    
llm_vec_custom(p2[[1]]$`6`, prompt = paste("sentimiento de la oración: positivo, negativo o neutral. El resultado dámelo en una palabra")
)
### ### ### ### ### ### ### ###  ### ### ### ### ### ### ### ###  ### ### ### ### ### ### ### ### 
# Corro desde el número 2397
# lista_final <- p
# vector <- vector[-c(1:2397)]


## For de análisis 
for (i in vector) {
  tryCatch({
    # Aplicamos lapply a cada objeto de p2[[i]]
    lista_final[[i]] <- lapply(p2[[i]], \(x) {
      x |> 
        llm_vec_custom(prompt = paste("sentimiento de la oración: positivo, negativo o neutral. El resultado dámelo en una palabra")
        )  # Aplica la función llm_vec_sentiment a cada elemento
    })
    
    # Imprime el progreso
    print(paste0("Acabamos el objeto ", i, " de p2"))
    
    Sys.sleep(1)  # 1 segundo de espera (puedes ajustar el tiempo según sea necesario)
    
    # Incrementa el contador
    contador <- contador + 1
    
    # Guarda cada 600 iteraciones
    if (contador %% 600 == 0) {
      saveRDS(lista_final, paste0("01_datos_resultantes_llm_trabajados/deepseek/1_sentimiento_twitter_1_", i, ".rds"))
      print(paste0("Guardado después de ", contador, " iteraciones."))
    }
    
    # Solo cuando la última iteración se complete, ejecutamos las acciones
    if (i == length(vector)) {
      # 1. Guardar el objeto 'lista_final' en un archivo .rds
      saveRDS(lista_final, paste0("01_datos_resultantes_llm_trabajados/deepseek/1_sentimiento_twitter_1_", length(vector), ".rds"))
      
      #2. Mensaje para indicar que el proceso terminó
      print("Todas las iteraciones han finalizado de twitter")
      
      
      # DAmos unos momentos para que sí guarde
      Sys.sleep(3)
      
      
      # creamos el cuerpo del correo
      msg <- compose_email(body = md(paste0("Terminó de correr el análisis de twitter con deepseek")))
      
      # Mandamos correo
      msg %>% 
        smtp_send(from = "toakihozcanabal@gmail.com",
                  to = "toaki07@hotmail.com", # enviar el correo con smtp_send
                  subject = "Finalización twitter",
                  credentials = my_creeds
        )
      
      # Imprimos para el txt 
      print("Correo enviado de twitter")
    }
    
  }, error = function(e) {
    # Si ocurre un error, imprímelo y continúa con el siguiente
    print(paste0("Error en el objeto ", i, ": ", e$message))
  })
}



## # tiktoks ------
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





## Este funciona alvvvvvvvv ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###                                                                    
llm_vec_custom(p2[[1]]$`4`, prompt = paste("sentimiento de la oración: positivo, negativo o neutral. El resultado dámelo en una palabra")
)
### ### ### ### ### ### ### ###  ### ### ### ### ### ### ### ###  ### ### ### ### ### ### ### ### 

## For de análisis 
for (i in vector) {
  tryCatch({
    # Aplicamos lapply a cada objeto de p2[[i]]
    lista_final[[i]] <- lapply(p2[[i]], \(x) {
      x |> 
        llm_vec_custom(prompt = "¿es positivo, negativo o neutral? No me des oraciones, dame una sola palabra")  # Aplica la función llm_vec_sentiment a cada elemento
    })
    
    # Imprime el progreso
    print(paste0("Acabamos el objeto ", i, " de p2"))
    
    Sys.sleep(1)  # 1 segundo de espera (puedes ajustar el tiempo según sea necesario)
    
    # Incrementa el contador
    contador <- contador + 1
    
    # Guarda cada 600 iteraciones
    if (contador %% 600 == 0) {
      saveRDS(lista_final, paste0("01_datos_resultantes_llm_trabajados/deepseek/1_sentimiento_tiktok_complejo_1_", i, ".rds"))
      print(paste0("Guardado después de ", contador, " iteraciones."))
    }
    
    # Solo cuando la última iteración se complete, ejecutamos las acciones
    if (i == length(vector)) {
      # 1. Guardar el objeto 'lista_final' en un archivo .rds
      saveRDS(lista_final, paste0("01_datos_resultantes_llm_trabajados/deepseek/1_sentimiento_tiktok_complejo_1_", length(vector), ".rds"))
      
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


# ── mall session object 
# Backend: Ollama
# LLM session: model:deepseek-r1:8b
# R session: cache_folder:C:\Users\bhoz\AppData\Local\Temp\RtmpOgHBJ1\_mall_cache23985e113d30
##









# mistral:latest 4.1 GB  -------

## # Twitter -------
twitter_todos <- readRDS("0_datos_de_carga/twiter_todos_con_lenguajes.rds")

## Limpiamos idiamos que no funcionan en su mayoría (dada una revisión rápida) 
twitter_todos <- twitter_todos |> 
  tidyr::unnest(lenguaje) |> 
  filter(!lenguaje %in% c("danish", "english", "frisian", "german", "hubgarian", "icelandic", "nepali", "norwegian", "tagalog", "turkish", "welsh")) 

# quitamos NA 
twitter_todos <- twitter_todos |> 
  # select(comment_content, content_cleaned, lenguaje) |> 
  filter(!is.na(lenguaje))
## BAjamos a 226k tuits !! ojo con esto 

twitter_todos <- twitter_todos |> 
  select(comment_content) |> 
  mutate(comment_content = stringr::str_replace_all(comment_content, "#\\w+", ""), #  uita hasgtags pero se puede mejor con gsub
         comment_content = stringr::str_replace_all(comment_content, "@\\w+", ""), #  quita arrobas
         comment_content = stringr::str_squish(comment_content), #para quitar espacio
         comment_content = gsub("https://\\S+", "", comment_content)) |>  # para quitar links
  filter(comment_content != "") 

# ## Generamos P
# p <- twitter_todos


# Generamos lista
p1 <-  
  twitter_todos|> 
  fmutate(id= seq_row(twitter_todos)) |> 
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

# ## Este funciona alvvvvvvvv ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###                                                                    
llm_vec_custom(p2[[1]]$`6`, prompt = paste("sentimiento de la oración: positivo, negativo o neutral. El resultado dámelo en una palabra")
)
### ### ### ### ### ### ### ###  ### ### ### ### ### ### ### ###  ### ### ### ### ### ### ### ### 
# Corro desde el número 2397
# lista_final <- p
# vector <- vector[-c(1:2397)]


## For de análisis 
for (i in vector) {
  tryCatch({
    # Aplicamos lapply a cada objeto de p2[[i]]
    lista_final[[i]] <- lapply(p2[[i]], \(x) {
      x |> 
        llm_vec_custom(prompt = paste("sentimiento de la oración: positivo, negativo o neutral. El resultado dámelo en una palabra")
        )  # Aplica la función llm_vec_sentiment a cada elemento
    })
    
    # Imprime el progreso
    print(paste0("Acabamos el objeto ", i, " de p2"))
    
    Sys.sleep(1)  # 1 segundo de espera (puedes ajustar el tiempo según sea necesario)
    
    # Incrementa el contador
    contador <- contador + 1
    
    # Guarda cada 600 iteraciones
    if (contador %% 600 == 0) {
      saveRDS(lista_final, paste0("01_datos_resultantes_llm_trabajados/mistral/1_sentimiento_twitter_1_", i, ".rds"))
      print(paste0("Guardado después de ", contador, " iteraciones."))
    }
    
    # Solo cuando la última iteración se complete, ejecutamos las acciones
    if (i == length(vector)) {
      # 1. Guardar el objeto 'lista_final' en un archivo .rds
      saveRDS(lista_final, paste0("01_datos_resultantes_llm_trabajados/mistral/1_sentimiento_twitter_1_", length(vector), ".rds"))
      
      #2. Mensaje para indicar que el proceso terminó
      print("Todas las iteraciones han finalizado de twitter")
      
      
      # DAmos unos momentos para que sí guarde
      Sys.sleep(3)
      
      
      # creamos el cuerpo del correo
      msg <- compose_email(body = md(paste0("Terminó de correr el análisis de twitter con mistral")))
      
      # Mandamos correo
      msg %>% 
        smtp_send(from = "toakihozcanabal@gmail.com",
                  to = "toaki07@hotmail.com", # enviar el correo con smtp_send
                  subject = "Finalización twitter",
                  credentials = my_creeds
        )
      
      # Imprimos para el txt 
      print("Correo enviado de twitter")
    }
    
  }, error = function(e) {
    # Si ocurre un error, imprímelo y continúa con el siguiente
    print(paste0("Error en el objeto ", i, ": ", e$message))
  })
}

── mall session object 
Backend: Ollama
LLM session: model:mistral:latest
R session: cache_folder:C:\Users\bhoz\AppData\Local\Temp\Rtmpyqg57W\_mall_cache3c785e34728d
 N

## # Youtube ------
 youtubes <- readRDS(file = "0_datos_de_carga/youtube_con_lenguajes.rds") 
 
 ## Split a nuestro objeto, nos da una lista por renglón
 p1 <- youtubes |> 
   fmutate(id= seq_row(youtubes)) |> 
   janitor::clean_names() |> 
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
         llm_vec_custom(prompt = paste("sentimiento de la oración: positivo, negativo o neutral. El resultado dámelo en una palabra"))  # Aplica la función llm_vec_sentiment a cada elemento
     })
     
     # Imprime el progreso
     print(paste0("Acabamos el objeto ", i, " de p2"))
     
     Sys.sleep(1)  # 1 segundo de espera (puedes ajustar el tiempo según sea necesario)
     
     # Incrementa el contador
     contador <- contador + 1
     
     # Guarda cada 600 iteraciones
     if (contador %% 600 == 0) {
       saveRDS(lista_final, paste0("01_datos_resultantes_llm_trabajados/mistral/1_sentimiento_youtube_1_", i, ".rds"))
       print(paste0("Guardado después de ", contador, " iteraciones."))
     }
     
     # Solo cuando la última iteración se complete, ejecutamos las acciones
     if (i == length(vector)) {
       # 1. Guardar el objeto 'lista_final' en un archivo .rds
       saveRDS(lista_final, paste0("01_datos_resultantes_llm_trabajados/mistral/1_sentimiento_youtube_1_", length(vector), ".rds"))
       
       #2. Mensaje para indicar que el proceso terminó
       print("Todas las iteraciones han finalizado de youtube")
       
       
       # DAmos unos momentos para que sí guarde
       Sys.sleep(3)
       
       
       # creamos el cuerpo del correo
       msg <- compose_email(body = md(paste0("Terminó de correr el análisis de youtube")))
       
       # Mandamos correo
       msg %>% 
         smtp_send(from = "toakihozcanabal@gmail.com",
                   to = "toaki07@hotmail.com", # enviar el correo con smtp_send
                   subject = "Finalización youtube",
                   credentials = my_creeds
         )
       
       # Imprimos para el txt 
       print("Correo enviado de youtube")
       
       # #2. Mensaje para indicar que el proceso terminó
       # print("Todas las iteraciones han finalizado, el sistema se apagará.")
       # 
       # # Restaurar la salida a la consola
       # # sink()
       # 
       # # DAmos unos momentos para que sí guarde
       # Sys.sleep(3)
       # 
       # 
       # # 3. Apagar la computadora
       # system("shutdown /s /f /t 0")
       
     }
     
   }, error = function(e) {
     # Si ocurre un error, imprímelo y continúa con el siguiente
     print(paste0("Error en el objeto ", i, ": ", e$message))
   })
 }
 
 
## # tiktoks -----
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

## promt de análisis


# ## Este funciona alvvvvvvvv ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###                                                                    
llm_vec_custom(p2[[1]]$`6`, prompt = paste("sentimiento de la oración: positivo, negativo o neutral. El resultado dámelo en una palabra")
)
### ### ### ### ### ### ### ###  ### ### ### ### ### ### ### ###  ### ### ### ### ### ### ### ### 
# Corro desde el número 2397
# lista_final <- p
# vector <- vector[-c(1:2397)]
 

## For de análisis 
for (i in vector) {
  tryCatch({
    # Aplicamos lapply a cada objeto de p2[[i]]
    lista_final[[i]] <- lapply(p2[[i]], \(x) {
      x |> 
        llm_vec_custom(prompt = paste("sentimiento de la oración: positivo, negativo o neutral. El resultado dámelo en una palabra")
                       )  # Aplica la función llm_vec_sentiment a cada elemento
    })
    
    # Imprime el progreso
    print(paste0("Acabamos el objeto ", i, " de p2"))
    
    Sys.sleep(1)  # 1 segundo de espera (puedes ajustar el tiempo según sea necesario)
    
    # Incrementa el contador
    contador <- contador + 1
    
    # Guarda cada 600 iteraciones
    if (contador %% 600 == 0) {
      saveRDS(lista_final, paste0("01_datos_resultantes_llm_trabajados/mistral/1_sentimiento_tiktok_1_", i, ".rds"))
      print(paste0("Guardado después de ", contador, " iteraciones."))
    }
    
    # Solo cuando la última iteración se complete, ejecutamos las acciones
    if (i == length(vector)) {
      # 1. Guardar el objeto 'lista_final' en un archivo .rds
      saveRDS(lista_final, paste0("01_datos_resultantes_llm_trabajados/mistral/1_sentimiento_tiktok_1_", length(vector), ".rds"))
      
      #2. Mensaje para indicar que el proceso terminó
      print("Todas las iteraciones han finalizado de tiktok")
      
      
      # DAmos unos momentos para que sí guarde
      Sys.sleep(3)
      
      
      # creamos el cuerpo del correo
      msg <- compose_email(body = md(paste0("Terminó de correr el análisis de tiktok con mistral")))
      
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

── mall session object 
Backend: Ollama
LLM session: model:mistral:latest
R session: cache_folder:C:\Users\bhoz\AppData\Local\Temp\Rtmpyqg57W\_mall_cache3c785e34728d




# LLAMA -----
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

llm_vec_custom(p2[[1]]$`1`, prompt = "¿es positivo, negativo o neutral? No me des oraciones, dame una sola palabra") 

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
