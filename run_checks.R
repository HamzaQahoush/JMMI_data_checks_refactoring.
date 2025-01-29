

source(here::here('src/00_data_checks_utils.R'))


general_info <- c("uuid","enumerator","organization_name_so","date","marketplace_tx","admin1_label_so","admin2_label")
general_info_old <- c("uuid","enumerator_id_All","select_one_organization_name_All","date_survey_All","market_name_All","governorate_ID_All","district_ID_All")

aor_masterlist <- read.xlsx(here::here("./inputs/AOR master list.xlsx")) #
general_info <- general_info_old # temporary to test 

raw_data <- read_excel(here::here('inputs/REACH_JMMI_January_2025_raw_data.xlsx')) %>% mutate(
  aor_All = case_when(
    aor_All == "DFA" ~ "AA",
    aor_All == "IRG" ~ "GOY",
    TRUE ~ aor_All
  ) 
) %>% dplyr::rename(uuid=`_uuid`)

#raw_data['aor_All'][[1]][[1]] <- "AA"

final_df <- read_excel(here::here('inputs/Final_new_calc.xlsx')) 
# filter data for last three months date 
last_3_months <- (seq(max(final_df$date), length = 3, by = "-1 months"))

final_df <- final_df %>% filter(date %in% last_3_months) 







################ Run check item
#### create a df that will used to read the column name for prices we an put all items

items_spec <- data.frame(
  item                = c("vegetable_oil", "onion", "potato", "rice"), 
  vendor_sell_yn      = c("sell_sunflower_oil_All", "sell_onion_All", 
                          "sell_potato_All", "sell_rice_All"),
  vendor_sell_std_yn  = c("quantity_1L_sunflower_oil_All", "quantity_1Kg_onion_All", 
                          "quantity_1Kg_potato_All", "quantity_1Kg_rice_All"),
  q_other             = c("quantity_other_sunflower_oil_All", "quantity_other_onion_All", 
                          "quantity_other_potato_All", "quantity_other_rice_All"),
  price_other         = c("price_sunflower_oil_All", "price_onion_All", 
                          "price_potato_All", "price_rice_All"),
  calc_price_item     = c("calc_price_sunflower_oil_All", "calc_price_onion_All",
                          "calc_price_potato_All", "calc_price_rice_All")
)


#for single check we can use ::
# onion_check <- check_item_price(
#   df              = raw_data,
#   final_df        = final_df,
#   item            = "onion",
#   vendor_sell_yn   = "sell_onion_All",
#   vendor_sell_std_yn = "quantity_1Kg_onion_All",
#   q_other         = "quantity_other_onion_All",
#   price_other     = "price_onion_All",
#   calc_price_item = "calc_price_onion_All"
# )



# write.xlsx(onion_check,"onion_check.xlsx",overwrite = T)



### to loop over list of items

# iterate over columns in parallel, passing them as arguments
results_list <- pmap(
  .l = items_spec,
  .f = function(item, vendor_sell_yn, vendor_sell_std_yn, q_other, price_other, calc_price_item) {
    check_item_price(
      df                = raw_data,
      final_df          = final_df,
      item              = item,
      vendor_sell_yn    = vendor_sell_yn,
      vendor_sell_std_yn= vendor_sell_std_yn,
      q_other           = q_other,
      price_other       = price_other,
      calc_price_item   = calc_price_item
    )
  }
)

# pmap() returns a list of results, one per row of items_spec
#  name each element by the item, you can do:
names(results_list) <- items_spec$item



all_item_checks <- bind_rows(results_list, .id = "item_name")





######################
check_aor_in_data(raw_data,aor_masterlist,"aor_All","district_ID_All")

##########

wage_checks <- wage_data_check(raw_data,final_df,"unskilled_yn_All","daily_wages_All","daily_wage_price_All","monthly_wage_price_All","calc_daily_wage_All")
################################## exchange price check :: 
check_exchange_rate_tst <- one_variable_check(raw_data = raw_data, final_df = final_df,variable_to_check ="exchange_rate_result_All",check_label = "exchange_rates")

##############################################
check_food_stock_tst <- one_variable_check(raw_data = raw_data, final_df = final_df,variable_to_check ="food_restock_days_min_All",check_label = "food_restock")
#################
check_wash_restock <- one_variable_check(raw_data = raw_data, final_df = final_df,variable_to_check ="wash_restock_days_min_All",check_label = "wash_restock")
#######################
