INITI_IsoComCRMTable <- read.csv(file ='www/IsotopicData/RAW_INITIAL_IsoComCRM.csv')
INITI_IsoComCRMTable['Element'][INITI_IsoComCRMTable['Element'] == ''] <- NA
INITI_IsoComCRMTable['CRM.name'][INITI_IsoComCRMTable['CRM.name'] == ''] <- NA


INITI_IsoComCRMTable <- zoo::na.locf(INITI_IsoComCRMTable)

# PArtir la tabla en info y resultados, aparteee
