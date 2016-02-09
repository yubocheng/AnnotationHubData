# The correct recipe(step 1 & 2) is inside grasp2db software package. 

## FIXME: does not pass BiocVersion
## STEP 3:  Call the helper to set up the newResources() method
makeAnnotationHubResource("Grasp2ImportPreparer",
                          grasp2db:::.makeGrasp2ToAHM)
