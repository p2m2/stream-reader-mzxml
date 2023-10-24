FROM inraep2m2/scala-sbt:latest

LABEL author="Olivier Filangi"
LABEL mail="olivier.filangi@inrae.fr"
ENV MILL_VERSION="0.10.4"

COPY . /stream-reader-mzxml/
WORKDIR /stream-reader-mzxml
RUN sbt assembly

CMD ["java","-cp","./assembly/pack.jar"]