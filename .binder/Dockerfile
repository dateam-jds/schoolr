FROM rocker/binder:4.0.2
LABEL maintainer='Muhammad Aswan Syahputra'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}



RUN wget https://github.com/dateam-jds/schoolr/raw/main/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2021-03-19/')); devtools::install_deps()"

RUN rm DESCRIPTION.1; exit 0
