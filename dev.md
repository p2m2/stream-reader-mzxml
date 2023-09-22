## scalaxb

- install scalaxb = https://github.com/eed3si9n/scalaxb/
- generate class from XSD
  wget https://sashimi.sourceforge.net/schema_revision/mzXML_3.2/mzXML_3.2.xsd
  mkdir separations
  pushd separations
  wget http://sashimi.sourceforge.net/schema_revision/mzXML_3.2/separations/separation_technique_1.0.xsd
  popd
  wget http://sashimi.sourceforge.net/schema_revision/mzXML_3.2/general_types_1.0.xsd

scalaxb mzXML_3.2.xsd separations/separation_technique_1.0.xsd general_types_1.0.xsd -p mzxml