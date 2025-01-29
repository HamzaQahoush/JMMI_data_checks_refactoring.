
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")


# Install needed packages
# my_pkgs <- c("lubridate","here","openxlsx","writexl","purrr","readxl",
#              "janitor","tidyverse","devtools","reshape","stringr","dplyr")


######## for first time just use 
#renv::init()
#for (pkg in my_pkgs) {
 # renv::install(pkg,confirm="Y")
#}
# Restore or initialize the local environment
# If first time, do `renv::init()`, otherwise typically:
renv::restore()

### to load packages
lapply(my_pkgs, function(pkg) {
  library(pkg, character.only = TRUE)
})


# 4) If you add new packages, temporarily uncomment these lines:
# renv::install("some_new_package")
# renv::snapshot()











###### helper functions to check item price ######### Hamza Script################

## function to get AOR from raw data
get_AOR <- function(raw_data, AOR) {
  raw_data %>%
    dplyr::filter(aor_All == AOR)

}


### standard measures for items as df
std_units <<- data.frame(
  item = c(
    "wheat_flour",
    "rice",
    "dry_beans",
    "Canned_Beans",
    "lentil",
    "vegetable_oil",
    "sugar",
    "Salt",
    "potato",
    "onions",
    "bottled_water",
    "treated_water",
    "soap",
    "laundry_powder",
    "sanitary_napkins",
    "bleach"
  ),
  std_measure = c(
    "1 kg",
    "1 kg",
    "1 kg",
    "14.1 oz (=400 grams)",
    "1 kg",
    "1 Litre",
    "1 Kg",
    "1 Kg",
    "1 Kg",
    "1 kg",
    "0.75 litre",
    "10 Liters",
    "100 Grams",
    "100 Grams",
    "10 pieces",
    "1 Litre"
  )
)


message_to_partners <- data.frame(variable=c("exchange_rate","restock"),msg = "The exchange rate of, YER for 1 USD seems too high or too low")

## get the max price of an item , by AOR
get_AOR_max <- function(final_data, AOR) {
  final_data %>%
    dplyr::filter(aor == AOR) %>%
    dplyr::select(!matches("date|government|district|SMEB|lumpsum|num_obs|exchange_rates_new")) %>%
    dplyr::group_by(aor) %>%
    dplyr::summarise_all(max, na.rm = TRUE)
}

## get the min price of an item , by AOR
get_AOR_min <- function(final_data, AOR) {
  final_data %>%
    dplyr::filter(aor == AOR) %>%
    dplyr::select(!matches("date|government|district|SMEB|lumpsum|num_obs|exchange_rates_new")) %>%
    dplyr::group_by(aor) %>%
    dplyr::summarise_all(min, na.rm = TRUE)
}


flag_item_outliers_chunk <- function(final_df,
                                     current_df,
                                     aor_in,
                                     item,
                                     calc_price_col) {
  
  # 1) Coerce to numeric and remove rows where the price is NA or non-positive
  #    (since log of 0 or negative is invalid)
  df_clean <- current_df %>%
    dplyr::filter(!is.na(.data[[calc_price_col]])) %>%
    dplyr::mutate(
      "{calc_price_col}" := as.numeric(.data[[calc_price_col]])
    ) %>%
    # If we do NOT want to filter out zero or negative, remove this line,
    # but we should be aware `log()` will fail or produce -Inf for ≤ 0
    dplyr::filter(.data[[calc_price_col]] > 0)
  
  # 2) Branch logic:
  # If item is in the "restock" category, use the special check
  if (item %in% c("food_restock","fuel_restock","wash_restock")) {
    
    df_flagged <- df_clean %>%
      dplyr::mutate(
        "{item}_check" := dplyr::if_else(
          # Example logic: >20 is flagged, or log-based outlier
          .data[[calc_price_col]] > 20 |
            log(.data[[calc_price_col]]) >
            quantile(log(.data[[calc_price_col]]), 0.75, na.rm = TRUE) +
            3 * IQR(log(.data[[calc_price_col]]), na.rm = TRUE) |
            log(.data[[calc_price_col]]) <
            quantile(log(.data[[calc_price_col]]), 0.25, na.rm = TRUE) -
            3 * IQR(log(.data[[calc_price_col]]), na.rm = TRUE),
          1L, 0L
        )
      )
    
  } else {
    # Otherwise, use min/max from final_df
    item_max_val <- get_AOR_max(final_data = final_df, AOR = aor_in)[[item]]
    item_min_val <- get_AOR_min(final_data = final_df, AOR = aor_in)[[item]]
    
    # Check for missing or NA boundaries
    if (is.null(item_max_val) || is.null(item_min_val) ||
        is.na(item_max_val)   || is.na(item_min_val)) {
      warning(
        "No valid min/max found for item='", item,
        "' in AOR='", aor_in, "'. Returning empty data frame."
      )
      return(dplyr::tibble())  # or return(df_clean) if you want to skip flagging
    }
    
    df_flagged <- df_clean %>%
      dplyr::mutate(
        "{item}_check" := dplyr::if_else(
          .data[[calc_price_col]] > item_max_val |
            .data[[calc_price_col]] < item_min_val |
            log(.data[[calc_price_col]]) >
            quantile(log(.data[[calc_price_col]]), 0.75, na.rm = TRUE) +
            3 * IQR(log(.data[[calc_price_col]]), na.rm = TRUE) |
            log(.data[[calc_price_col]]) <
            quantile(log(.data[[calc_price_col]]), 0.25, na.rm = TRUE) -
            3 * IQR(log(.data[[calc_price_col]]), na.rm = TRUE),
          1L, 0L
        )
      )
  }
  
  # 3) Return only flagged rows
  df_flagged %>%
    dplyr::filter(.data[[paste0(item, "_check")]] == 1L)
}



## create empty df for cleaning log
create_item_log_partners <- function() {
  data.frame(
    uuid       = character(),
    enum       = character(),
    org        = character(),
    date       = character(),
    market     = character(),
    gov        = character(),
    dist       = character(),
    issue      = character(),
    variable   = character(),
    old_value  = character(),
    new_value  = character(),
    comments   = character(),
    fix        = character(),
    checked_by = character(),
    stringsAsFactors = FALSE
  )
}



# create filled clog with some data: enum,ngo
create_filled_item_log_partners <- function(item_checks) {
  data.frame(
    uuid = item_checks$uuid,
    enum = item_checks$enumerator,
    org = item_checks$organization_name_so,
    date = item_checks$date,
    market = item_checks$marketplace_tx,
    gov = item_checks$admin1_label_so,
    dist = item_checks$admin2_label,
    issue = item_checks$issue_type,
    variable = item_checks$variable,
    old_value = item_checks$value,
    new_value = item_checks$new_value,
    comments = item_checks$comments,
    fix = item_checks$fix,
    checked_by = item_checks$checked_by
  )
}

create_filled_item_log_partners_old <- function(item_checks) {
  data.frame(
    uuid = item_checks$uuid,
    enum = item_checks$enumerator_id_All,
    org = item_checks$select_one_organization_name_All,
    date = item_checks$date_survey_All,
    market = item_checks$market_name_All,
    gov = item_checks$governorate_ID_All,
    dist = item_checks$district_ID_All,
    issue = item_checks$issue_type,
    variable = item_checks$variable,
    old_value = item_checks$value,
    new_value = item_checks$new_value,
    comments = item_checks$comments,
    fix = item_checks$fix,
    checked_by = item_checks$checked_by
  )
}







get_flagged_data_for_aor <- function(raw_data,
                                     final_df,
                                     AOR,
                                     general_info_cols,
                                     vendor_sell_std_yn,
                                     vendor_sell_yn,
                                     q_other, 
                                     price_other,
                                     calc_price_item,
                                     item) {
  # 1) Subset to the requested AOR
  data_aor <- get_AOR(raw_data, AOR = AOR)
  
  # If no rows for this AOR, return empty right away
  if (nrow(data_aor) == 0) {
    message("No rows found for AOR='", AOR, "' -> returning empty")
    return(data.frame())
  }
  
  # 2) Ensure the needed columns actually exist
  needed_cols <- c(
    general_info_cols,
    vendor_sell_std_yn,
    vendor_sell_yn,
    q_other,
    price_other,
    calc_price_item
  )
  
  missing_cols <- setdiff(needed_cols, names(data_aor))
  if (length(missing_cols) > 0) {
    stop(
      "Data is missing these columns for AOR='", AOR, "': ",
      paste0(missing_cols, collapse = ", ")
    )
  }
  
  # 3) Subset and convert to numeric
  data_subset <- data_aor %>%
    dplyr::select(dplyr::any_of(needed_cols)) %>%
    dplyr::mutate(
      # convert the chosen price column to numeric
      "{calc_price_item}" := as.numeric(.data[[calc_price_item]])
    )
  
  # 4) Call  outlier chunk function
  flagged <- flag_item_outliers_chunk(
    final_df = final_df,
    current_df = data_subset,
    aor_in = AOR,
    item = item,
    calc_price_col = calc_price_item
  )
  
  return(flagged)
}


check_item_price <- function(df,
                             final_df,
                             item,
                             vendor_sell_yn,
                             vendor_sell_std_yn,
                             q_other, 
                             price_other,
                             calc_price_item) {
  IRG_data_subset_flagged <- get_flagged_data_for_aor(
    raw_data            = df,
    final_df            = final_df,
    AOR                 = "GOY",
    general_info_cols   = general_info,
    vendor_sell_std_yn  = vendor_sell_std_yn,
    vendor_sell_yn      = vendor_sell_yn,
    q_other             = q_other,
    price_other         = price_other,
    calc_price_item     = calc_price_item,
    item                = item
  )
  
  # 2) Flag DFA data (AOR="AA")
  DFA_data_subset_flagged <- get_flagged_data_for_aor(
    raw_data            = df,
    final_df            = final_df,
    AOR                 = "AA",
    general_info_cols   = general_info,
    vendor_sell_std_yn  = vendor_sell_std_yn,
    vendor_sell_yn      = vendor_sell_yn,
    q_other             = q_other,
    price_other         = price_other,
    calc_price_item     = calc_price_item,
    item                = item
  )
  
  # 3) Bind them together
  binded_checks_aors <- dplyr::bind_rows(IRG_data_subset_flagged, DFA_data_subset_flagged)
  
  # 4) Look up the standard measure for your item
  #    If no match, you can handle that gracefully
  std_item <- std_units$std_measure[grepl(item, std_units$item)]
  if (length(std_item) == 0) {
    std_item <- "???"
    warning("No match found in std_units for item='", item, "'.")
  }
  
  # 5) If no flagged rows, skip the rest and show "No issues"
  if (nrow(binded_checks_aors) == 0) {
    cat(crayon::green("> No issues related to item prices found.\n"))
    # Return an empty log or some placeholder
    return(data.frame())
  }
  #browser()
  # 6) Otherwise, build the logs
  item_checks <- binded_checks_aors %>%
    dplyr::mutate(
      issue_type = dplyr::if_else(
        .data[[vendor_sell_yn]] == "yes",
        paste0(
          "The entered price of ", std_item,
          " of ( ",item ," is ", .data[[calc_price_item]], " ) seems too high or too low"
        ),
        paste0(
          "The calculated price of ", std_item,
          " of  ( ",item," is ", .data[[calc_price_item]], " ) seems too high or too low; ",
          "you've entered ( ", .data[[price_other]], " ) as the price of ( ", .data[[q_other]], " )"
        )
      ),
      checked_by = "WA",
      new_value = " ",
      comments = " ",
      fix = "Checked with partners"
    )
  
  # 7) Gather columns
  item_checks <- item_checks %>%
    tidyr::gather(
      key   = "variable",
      value = "value",
      all_of(c(vendor_sell_std_yn, vendor_sell_yn, q_other, price_other, calc_price_item))
    )
  
  # 8) Create final partner log
  item_log_partners <- create_filled_item_log_partners_old(item_checks) %>%
    dplyr::filter(variable != vendor_sell_std_yn)
  
  # 9) Print how many issues
  no_issues <- item_log_partners %>%
    dplyr::select(uuid, issue) %>%
    unique()
  cat(crayon::red(paste0("> ", nrow(no_issues), " issues related to ",item ," prices found.\n")))
  
  return(item_log_partners)
}




########################################################################################

check_aor_in_data <- function(raw_data,aor_masterlist,data_aor_col,data_admin2){
  aor_checks <- raw_data %>% dplyr::select(all_of(general_info),data_aor_col) %>% dplyr::left_join(aor_masterlist, by =data_admin2 ) %>% 
    dplyr::mutate(aor_check = ifelse(.data[[data_aor_col]]== .data$aor ,0,1))  %>%
    dplyr::filter(aor_check == 1)
  
  #browser()
  
  
  
  ###### creating logs
  if(nrow(aor_checks)>=1) {
    
    aor_checks <- aor_checks %>% mutate(  
      updated.Date = as.Date(updated.Date),
      issue_type   = paste0(
        "The reported region ... (", .data[[data_aor_col]], 
        ") ... from (", .data$aor, ") ... in ", 
        format(updated.Date, "%B %Y")
      ),
      checked_by="WA",
      new_value=" ",
      comments=" ",
      fix="Checked with partners"
    )    
    
    
    
    
    aor_checks <- aor_checks %>% gather(key = "variable",
                                        value = "value",
                                        `updated.Date`,aor,data_aor_col)
    
    
    
    aor_log <- create_filled_item_log_partners_old(aor_checks)
    
    aor_log_partners <- aor_log %>%
      filter(variable == "data_aor_col")
    
    no_issues <- aor_log_partners %>% dplyr::select(uuid,issue) %>% unique
    cat(crayon::red(paste0("> ",nrow(aor_log)/3," aor updates or changes found.")))
    cat("\n")
    
  } else {
    
    aor_log <- create_item_log_partners()
    aor_log_partners <- aor_log
    
    cat(crayon::green(paste0("> No aor updates or changes found.")))
    cat("\n")
  }
  
  return(aor_checks)
  
}
########################################################################## wage
get_flagged_data_for_wage <- function(raw_data  ,
                                      final_df  ,
                                      AOR ,
                                      general_info_cols,
                                      unskilled_yn_All,
                                      daily_wage_price_All,
                                      daily_wages_All ,
                                      monthly_wage_price_All,
                                      calc_daily_wage_All,
                                      item = "daily_wage_rate") {
  # 1) Subset to the requested AOR
  data_aor <- get_AOR(raw_data, AOR = AOR)
  
  # If no rows for this AOR, return empty right away
  if (nrow(data_aor) == 0) {
    message("No rows found for AOR='", AOR, "' -> returning empty")
    return(data.frame())
  }
  
  # 2) Ensure the needed columns actually exist
  needed_cols <- c(
    general_info_cols,
    unskilled_yn_All,
    daily_wages_All,
    monthly_wage_price_All,
    calc_daily_wage_All
  )
  
  missing_cols <- setdiff(needed_cols, names(data_aor))
  if (length(missing_cols) > 0) {
    stop(
      "Data is missing these columns for AOR='", AOR, "': ",
      paste0(missing_cols, collapse = ", ")
    )
  }
  
  # 3) Subset and convert to numeric
  data_subset <- data_aor %>%
    dplyr::select(dplyr::any_of(needed_cols)) %>%
    dplyr::mutate(
      # convert the chosen price column to numeric
      "{calc_daily_wage_All}" := as.numeric(.data[[calc_daily_wage_All]])
    )
  
  # 4) Call  outlier chunk function
  flagged <- flag_item_outliers_chunk(
    final_df = final_df,
    current_df = data_subset,
    aor_in = AOR,
    item = item,
    calc_price_col = calc_daily_wage_All
  )
  
  return(flagged)
}


##################################################################################
wage_data_check <- function(df,
                       final_df,
                       unskilled_yn_All,
                       daily_wages_All,
                       daily_wage_price_All,
                       monthly_wage_price_All,
                       calc_daily_wage_All
) {
  
  IRG_data_subset_flagged <- get_flagged_data_for_wage(
    raw_data            = df,
    final_df            = final_df,
    AOR                 = "GOY",
    general_info_cols   = general_info,
    unskilled_yn_All=unskilled_yn_All,
    daily_wages_All=daily_wages_All,
    daily_wage_price_All=daily_wage_price_All,
    monthly_wage_price_All=monthly_wage_price_All,
    calc_daily_wage_All=calc_daily_wage_All,
    item = "daily_wage_rate"
  )
  
  
  # 2) Flag DFA data (AOR="AA")
  DFA_data_subset_flagged <- get_flagged_data_for_wage(
    raw_data            = df,
    final_df            = final_df,
    AOR                 = "AA",
    general_info_cols   = general_info,
    unskilled_yn_All=unskilled_yn_All,
    daily_wages_All=daily_wages_All,
    daily_wage_price_All=daily_wage_price_All,
    monthly_wage_price_All=monthly_wage_price_All,
    calc_daily_wage_All=calc_daily_wage_All,
    item = "daily_wage_rate"
  )
  
  # 3) Bind them together
  binded_checks_aors <- dplyr::bind_rows(IRG_data_subset_flagged, DFA_data_subset_flagged)
  
  
  # 5) If no flagged rows, skip the rest and show "No issues"
  if (nrow(binded_checks_aors) == 0) {
    cat(crayon::green("> No issues related to item prices found.\n"))
    # Return an empty log or some placeholder
    return(data.frame())
  }
  
  # 6) Otherwise, build the logs
  wage_checks <- binded_checks_aors %>%
    dplyr::mutate(
      issue_type = dplyr::if_else(
        .data[[daily_wages_All]] == "daily",
        paste0(
          "The entered value of daily wage of ", calc_daily_wage_All,
          " seems too high or too low"
        ),
        paste0(
          "The calculated value of daily wage ", calc_daily_wage_All,
          " seems too high or too low you've entered ( ", .data[[monthly_wage_price_All]], " ) seems ; ",
          "you've entered ( ", .data[[monthly_wage_price_All]], " ) as  the value of ( ", .data[[daily_wages_All]], " )"
        )
      ),
      checked_by = "WA",
      new_value  = " ",
      comments   = " ",
      fix        = "Checked with partners"
    )
  
  # 7) Gather columns
  wage_checks <- wage_checks %>% gather(key = "variable",
                                        value = "value",
                                        unskilled_yn_All,daily_wages_All,monthly_wage_price_All,calc_daily_wage_All)   
  # 8) Create final partner log 
  item_log_partners <- create_filled_item_log_partners_old(wage_checks) %>%
    dplyr::filter(variable != daily_wages_All)
  
  # 9) Print how many issues
  no_issues <- item_log_partners %>% dplyr::select(uuid, issue) %>% unique()
  cat(crayon::red(paste0("> ", nrow(no_issues), " issues related to wage prices found.\n")))
  
  return(item_log_partners)
}





# final_df,
# unskilled_yn_All,
# daily_wages_All,
# daily_wage_price_All,
# monthly_wage_price_All,
# calc_daily_wage_All
############################################################## exchange rate check
get_flagged_data_one_variable <- function(raw_data  ,
                                      final_df  ,
                                      AOR ,
                                      general_info_cols,
                                      variable_to_check,
                                      item ) {
 
  # 1) Subset to the requested AOR
  data_aor <- get_AOR(raw_data, AOR = AOR)
  
  # If no rows for this AOR, return empty right away
  if (nrow(data_aor) == 0) {
    message("No rows found for AOR='", AOR, "' -> returning empty")
    return(data.frame())
  }
  
  # 2) Ensure the needed columns actually exist
  needed_cols <- c(
    general_info_cols,
    variable_to_check
  )
  
  missing_cols <- setdiff(needed_cols, names(data_aor))
  if (length(missing_cols) > 0) {
    stop(
      "Data is missing these columns for AOR='", AOR, "': ",
      paste0(missing_cols, collapse = ", ")
    )
  }
  
  # 3) Subset and convert to numeric
  data_subset <- data_aor %>%
    dplyr::select(dplyr::any_of(needed_cols)) %>%
    dplyr::mutate(
      # convert the chosen price column to numeric
      "{variable_to_check}" := as.numeric(.data[[variable_to_check]])
    )
  
  # 4) Call  outlier chunk function
  flagged <- flag_item_outliers_chunk(
    final_df = final_df,
    current_df = data_subset,
    aor_in = AOR,
    item = item,
    calc_price_col = variable_to_check
  )
  
  return(flagged)
}


one_variable_check <- function(raw_data,
                                final_df,variable_to_check,check_label){


  IRG_data_subset_flagged <- get_flagged_data_one_variable(
    raw_data            = raw_data,
    final_df            = final_df,
    AOR                 = "GOY",
    general_info_cols   = general_info,
  variable_to_check = variable_to_check,
    item = check_label
  )
  
  
  # 2) Flag DFA data (AOR="AA")
  DFA_data_subset_flagged <- get_flagged_data_one_variable(
    raw_data            = raw_data,
    final_df            = final_df,
    AOR                 = "AA",
    general_info_cols   = general_info,
    variable_to_check = variable_to_check,
    item = check_label
  )
  
  # 3) Bind them together
  binded_checks_aors <- dplyr::bind_rows(IRG_data_subset_flagged, DFA_data_subset_flagged)
  
  
  # 5) If no flagged rows, skip the rest and show "No issues"
  if (nrow(binded_checks_aors) == 0) {
    cat(crayon::green("> No issues related to item prices found.\n"))
    # Return an empty log or some placeholder
    return(data.frame())
  }
  
  # 6) Otherwise, build the logs
  variable_check <- binded_checks_aors %>%
    mutate(
      issue_type = case_when(
        check_label == "exchange_rates" ~
          paste0(
            "The exchange rate of ", .data[[variable_to_check]],
            " YER for 1 USD seems too high or too low"
          ),
        TRUE ~ paste0(
          "Please check the restock time ", .data[[variable_to_check]]
        )
      ),
      checked_by = "WA",
      new_value  = " ",
      comments   = " ",
      fix        = "Checked with partners"
    )
  

  # 7) Gather columns
  variable_check <- variable_check %>% gather(key = "variable",
                                        value = "value",variable_to_check
                                        )   
  # 8) Create final partner log 
  item_log_partners <- create_filled_item_log_partners_old(variable_check) %>%
    dplyr::filter(variable == variable_to_check)
  
  # 9) Print how many issues
  no_issues <- item_log_partners %>% dplyr::select(uuid, issue) %>% unique()
  cat(crayon::red(paste0("> ", nrow(no_issues), " issues related to" ,check_label, " found.\n")))
  
  return(item_log_partners)



}

