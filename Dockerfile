FROM inraep2m2/scala-sbt:latest

LABEL author="Olivier Filangi"
LABEL mail="olivier.filangi@inrae.fr"

COPY . /app/
WORKDIR /app
RUN sbt assembly
ENTRYPOINT ["java","-cp","assembly/pack.jar"]