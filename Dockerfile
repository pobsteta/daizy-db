FROM pobsteta/r-full

MAINTAINER Pascal Obstétar "pascal.obstetar@gmail.com"

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'dplyr', 'tibble', 'pool', 'rlang', 'DT', 'rhandsontable', 'shinysky', 'gdata', 'tool', 'devtools', 'RSQLite'), repos='https://cloud.r-project.org/')"

# github repository
RUN R -e "devtools::install_github('AnalytixWare/ShinySky')"

# copy the app to the image
RUN mkdir /root/daizy-db
COPY daizy-db /root/daizy-db
COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/daizy-db')"]
