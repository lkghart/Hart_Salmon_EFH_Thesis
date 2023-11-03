V4_extent.rds is changed to "AOI_extent.rds"
region2_polygon is changed to "AOI_polygon.rds"
user_region2.rds is changed to "user_region.rds"
v3_basis_subset is changed to "basis_subset.rds"
prediction_grid2 to gam_prediction_grid

Important! Changing Model 3 to Model 4 to align with paper.

Chinook_CH1_GAM_Predictions.rds to Ch1_GAM_Predictions_Response.rds

spec_VAST_CV.rds changed to spec,"_VAST_dynamic_model_CV.rds"))

## Note to self for Monday
Figure out how to untrack files
find . -name /data/BASIS/* -print0 | xargs -0 git rm -f --ignore-unmatch