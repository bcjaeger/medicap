

key_data <- tibble::tribble(
  ~variable                    , ~type  , ~label                                 , ~outcome, ~exposure , ~subset , ~group   , ~time                   ,
  "year_ABDHMO"                , "intg" , "Follow-up duration (year)"            , FALSE   , FALSE     , FALSE   , FALSE    , NA_character_           ,
  "Age"                        , "catg" , "Age, years"                           , FALSE   , TRUE      , TRUE    , TRUE     , NA_character_           ,
  "Gender"                     , "catg" , "Gender"                               , FALSE   , TRUE      , TRUE    , TRUE     , NA_character_           ,
  "Race"                       , "catg" , "Race"                                 , FALSE   , TRUE      , TRUE    , TRUE     , NA_character_           ,
  "Pre_index_statin_intensity" , "ctns" , "Statin intensity for prevalent users" , TRUE    , TRUE      , FALSE   , TRUE     , NA_character_           ,
  "Pre_index_statin"           , "bnry" , "Prevalent statin use"                 , TRUE    , TRUE      , TRUE    , TRUE     , NA_character_           ,
  "Post_index_statin"          , "ttev" , "Statin use during follow-up"          , TRUE    , FALSE     , FALSE   , FALSE    , "Post_index_statin_days",
  "Pre_index_PCSK9i_dose"      , "ctns" , "Dose of prevalent PCSK9 Inhibitor"    , TRUE    , FALSE     , FALSE   , FALSE    , NA_character_
)

key_list <- table.glue::as_inline(key_data,
                                  tbl_variables = 'variable',
                                  tbl_values = setdiff(names(key_data), 'variable'))



