FROM rocker/r-ver:latest

# Instalar dependencias del sistema necesarias
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libsodium-dev \
    zlib1g-dev \
    && apt-get clean

# Instalar los paquetes de R
RUN R -e "install.packages(c('plumber', 'ggplot2', 'dplyr', 'readr'), repos='https://cloud.r-project.org/')"

# Configurar el directorio de trabajo
WORKDIR /app

# Copiar los archivos del proyecto
COPY api.R /app/api.R
COPY logistic_model.rds /app/logistic_model.rds

# Exponer el puerto 8000
EXPOSE 8000

# Comando para ejecutar el servidor Plumber
CMD ["R", "-e", "pr <- plumber::plumb('/app/api.R'); pr$run(host='0.0.0.0', port=8000)"]