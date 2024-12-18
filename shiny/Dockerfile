# Usar la imagen base de R
FROM rocker/r-ver:4.3.0

# Instalar las dependencias necesarias del sistema
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpng-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Instalar los paquetes de R necesarios para la aplicación Shiny, uno por uno
RUN R -e "install.packages('shiny', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinythemes', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('ggplot2', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('dplyr', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('readr', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('httr', repos='https://cloud.r-project.org/')"

# Establecer el directorio de trabajo dentro del contenedor
WORKDIR /app

# Copiar los archivos locales de la aplicación Shiny (app.R) al contenedor
COPY app.R /app/app.R

# Exponer el puerto 3838 (puerto por defecto de Shiny)
EXPOSE 3838

# Comando para ejecutar la aplicación Shiny
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]