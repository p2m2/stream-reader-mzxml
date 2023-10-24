FROM inraep2m2/scala-sbt:latest

LABEL author="Olivier Filangi"
LABEL mail="olivier.filangi@inrae.fr"

COPY ./assembly/pack.jar /app/
WORKDIR /app

ENTRYPOINT ["java","-cp","pack.jar"]