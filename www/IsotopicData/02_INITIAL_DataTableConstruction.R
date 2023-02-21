INITI_IsoComCRMTable <- read.csv(file ='www/IsotopicData/RAW_INITIAL_IsoComCRM.csv')
INITI_IsoComCRMTable['Element'][INITI_IsoComCRMTable['Element'] == ''] <- NA
INITI_IsoComCRMTable['CRM.name'][INITI_IsoComCRMTable['CRM.name'] == ''] <- NA
INITI_IsoComCRMTable['CRM.lot'][INITI_IsoComCRMTable['CRM.lot'] == ''] <- NA
INITI_IsoComCRMTable['CRM.producer'][INITI_IsoComCRMTable['CRM.producer'] == ''] <- NA
INITI_IsoComCRMTable['CRM.description'][INITI_IsoComCRMTable['CRM.description'] == ''] <- NA
INITI_IsoComCRMTable['CRM.presentation'][INITI_IsoComCRMTable['CRM.presentation'] == ''] <- NA
INITI_IsoComCRMTable['UncertType'][INITI_IsoComCRMTable['UncertType'] == ''] <- NA
INITI_IsoComCRMTable['k.factor'][INITI_IsoComCRMTable['k.factor'] == ''] <- NA

INITI_IsoComCRMTable <- zoo::na.locf(INITI_IsoComCRMTable)

