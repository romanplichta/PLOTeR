#' @title read_tomst
#'
#' @description
#' `read_tomst()` Returns data frame from csv files of Tomst structure.
#'
#' @details Data are automatically named and converted into default structure.
#'
#' @param file a character vector of file names; e.g., c("data_92241809_2025_04_04_0.csv", "data_92241809_2025_04_04_1.csv"). If missing, all csv files in the working directory matching the name pattern "data_8digitserialnumber_*.csv" will be loaded.
#' @param path a character vector of full path name; the default corresponds to the working directory, getwd().
#' @param radius_units the units of dendrometer radius used when downloading data using Tomst software. Use "auto" to automatically convert radius values into micrometers (default). Use either "tomst" or "um" to convert manually. Specifically, if the data were downloaded from Tomst software in micrometers, use "um", if data were downloaded in Tomst units, use "tomst". This function always attempts to convert data into micrometers.If values lower than 1270 are found together with "tomst" units, warning message will be displayed.The conversion formula from "tomst" units into micrometers is: micrometers = (tomst-1278)*(8890/(34000-1278))
#' @param interval parameter used to fill in missing data_stamps as the desired interval. "auto" detects the interval automatically. If multiple intervals are automatically detected, the prevailing one is used. Variables will contain NA for the filled in data_stamps. Shorter intervals than the desired one are kept in data.
#' @param TMS_calibration recalculation of Tomst TMS soil moisture count into volumetric soil moisture based on calibration equations (version available on TOMST.com 06.10.2025). Loamy_Sand_A is used as default.
#'
#' @return Data.frame with long type of data.
#'
#' @examples
#' df <- read_tomst(radius_units = "tomst", delim = ".")
#' df <- read_tomst("data_94000000_0.csv", delim = ".")
#'
#' @import readr
#' @importFrom plyr ldply
#' @import dplyr
#' @import lubridate
#' @import tidyr
#'
#' @export
read_tomst = function (path, file, radius_units  = c("auto","um", "tomst"), interval = c("auto","1 min", "5 min", "15 min", "1 hour"), TMS_calibration = c("Loamy_Sand_A", "Loamy_Sand_B","Sandy_Loam_A","Sandy_Loam_B","Loam","Sil_Loam", "Peat", "Sand"), delim){
  if(missing(path)){
    path <- getwd()
  }
  if(missing(file)){
    files = vector()
    files <- list.files(path = path, pattern=glob2rx("data*.csv$"))
  }else{
    files <- file
  }
  if(length(files) == 0){
    stop("Working directory: no file in appropriate format")
  }else{
    files = paste0(path,"/",files)
    rm(path)
    TMS = list()
    for(i in files) {
      x <- readr::read_delim(i, delim =";",
                             col_names = F,
                             col_types = readr::cols(.default = "c"))
      if(length(x)<7){
        NULL
      }else{
        x<-as.data.frame(x)[c(2,4,5,6,7,8)]
        TMS[[i]] <-x
      }
    }
    if(length(TMS) == 0){
      stop("Working directory: no file in appropriate format")
    }else{
      TMS2 = list()
      for(i in unique(substr(basename(names(TMS)),6,13))) {
        x <- dplyr::bind_rows(TMS[(grep(names(TMS),pattern = i, value = T))])
        TMS2[[i]] <-x
      }
      rm(TMS, x, files, i)
      df = plyr::ldply(TMS2, data.frame)
      rm(TMS2)
      colnames(df) = c(".id", "date_time", "T1", "T2", "T3", "Moisture", "orig_radius_units")
      if(missing(delim)){
        delim = Sys.localeconv()["decimal_point"]
      }else{
        if(!(delim %in% c(".", ",")) | length(delim) > 1){
          stop("Wrong decimal separator. Check delim argument")
        }else{
          delim = delim
        }
      }
      if(delim == ","){
        df = df %>% dplyr::mutate(across(c(T1, T2, T3, Moisture), function(x) gsub("\\.", paste0(delim), x))) %>% as.data.frame()
      }else{
        df = df %>% dplyr::mutate(across(c(T1, T2, T3, Moisture), function(x) gsub(",", paste0(delim), x))) %>% as.data.frame()
      }
      df = subset(df, !is.na(date_time))
      df$date_time = lubridate::parse_date_time(df$date_time , c("YmdHM", "dmYHM", "YmdHMS", "dmYHMS", "dmY"), tz = 'UTC')
      df = subset(df, date_time > "1990-01-01 00:00:00 UTC")
      df = subset(df, !is.na(date_time))
      df = df %>% group_by(.id) %>% dplyr::distinct(.id, date_time, .keep_all = T) %>% as.data.frame()
      df[c(".id", "Moisture")] = lapply(df[c(".id", "Moisture")], as.numeric)
      df$orig_radius_units = as.character(df$orig_radius_units)
      if(any(radius_units %in% c("tomst", "um", "auto"))){
        if(radius_units[1] == "auto"){
          df$Radius = ifelse(df$.id > 93000000, NA,
                             ifelse(df$orig_radius_units == "205",(df$Moisture-1278)*(8890/(34000-1278)),
                                      ifelse(df$orig_radius_units == "206", df$Moisture,df$Moisture)))
        }
        else{
        if(radius_units[1] == "tomst"){
          df$Radius = ifelse(df$.id > 93000000, NA, (df$Moisture-1278)*(8890/(34000-1278)))
        }else{
          if(radius_units[1] == "um"){
            df$Radius = ifelse(df$.id > 93000000, NA, df$Moisture)
          }else{
            stop("Wrong units. Use 'auto', 'tomst' or 'um'")
          }
        }
      }
      }else{
        stop("Wrong units. Use 'auto', 'tomst' or 'um'")
      }
      df$Moisture = ifelse(df$.id >= 93000000 | df$.id %in% c(90181123, 91181123), df$Moisture, NA)
      # Soil type calibration lines----
      # Soil type start
      tms_calib_data = list('Loamy_Sand_A' = matrix(data = c(-1.90e-8, 2.66e-4, -1.54e-1), nrow = 1, ncol = 3, dimnames = list(c("Loamy_Sand_A"), c("a", "b", "c"))),
                            'Loamy_Sand_B' = matrix(data = c(-2.30E-8, 2.82E-4, -1.67E-1), nrow = 1, ncol = 3, dimnames = list(c("Loamy_Sand_B"), c("a", "b", "c"))),
                            'Sandy_Loam_A' = matrix(data = c(-3.80E-8, 3.39E-4, -2.15E-1), nrow = 1, ncol = 3, dimnames = list(c("Sandy_Loam_A"), c("a", "b", "c"))),
                            'Sandy_Loam_B' = matrix(data = c(-9.00E-10, 2.62E-4, -1.59E-1), nrow = 1, ncol = 3, dimnames = list(c("Sandy_Loam_B"), c("a", "b", "c"))),
                            'Loam' = matrix(data = c(-5.10E-8, 3.98E-4, -2.91E-1), nrow = 1, ncol = 3, dimnames = list(c("Loam"), c("a", "b", "c"))),
                            'Sil_Loam' = matrix(data = c(1.70E-8, 1.18E-4, -1.01E-1), nrow = 1, ncol = 3, dimnames = list(c("Sil_Loam"), c("a", "b", "c"))),
                            'Peat' = matrix(data = c(1.23E-7, -1.45E-4, 2.03E-1), nrow = 1, ncol = 3, dimnames = list(c("Peat"), c("a", "b", "c"))),
                            'Sand' = matrix(data = c(-3.00E-9, 1.61E-4, -1.10E-1), nrow = 1, ncol = 3, dimnames = list(c("Sand"), c("a", "b", "c"))))
      if(any(TMS_calibration %in% c("Loamy_Sand_A", "Loamy_Sand_B","Sandy_Loam_A","Sandy_Loam_B","Loam","Sil_Loam", "Peat", "Sand"))){
        df$Moisture = (tms_calib_data[[TMS_calibration[1]]][1]*df$Moisture^2+tms_calib_data[[TMS_calibration[1]]][1]*df$Moisture+tms_calib_data[[TMS_calibration[1]]][1])*100
      }else{
        stop("TMS_calibration. Use 'Loamy_Sand_A', 'Loamy_Sand_B','Sandy_Loam_A','Sandy_Loam_B','Loam','Sil_Loam', 'Peat', 'Sand'.")
      }
      # Soil type end
      df$T1 = as.numeric(df$T1)
      df$T2 = as.numeric(ifelse(df$.id >= 93000000, df$T2, NA))
      df$T3 = as.numeric(ifelse(df$.id >= 93000000, df$T3, NA))
      df = df %>% mutate(orig_radius_units = as.character(if_else(.id >= 93000000, NA,if_else(orig_radius_units == "205","tomst",if_else(orig_radius_units == "206", "um", orig_radius_units)))))
      df$.id = as.factor(df$.id)
      df = droplevels(df)
      # Measurement interval. This fnct will add lines in selected interval to complete missing data-stamps----
      # interval start
      if(any(interval %in% c("auto", "1 min", "5 min", "15 min", "1 hour"))){
        if(interval[1] == "auto"){
          resol = df %>% dplyr::select(.id, date_time) %>% dplyr::group_by(.id) %>%
            dplyr::mutate(time_diff = difftime(date_time, lag(date_time), units = "mins") ) %>%
            filter(!is.na(time_diff)) %>%
            dplyr:: filter(time_diff >= 1 & time_diff <= 1440) %>%
            dplyr::mutate(tot = n()) %>%
            dplyr::group_by(time_diff, .add = T) %>%
            dplyr::mutate(per = n()) %>%
            dplyr::distinct(.id, time_diff, .keep_all = T) %>%
            dplyr::summarise(time_diff_per = (per/tot)*100) %>%
            dplyr::slice_max(time_diff_per, n = 1, with_ties = FALSE) %>%
            select(-time_diff_per) %>%
            as.data.frame
          if(is.na(resol$time_diff[1])|is.null(resol$time_diff[1])){
            stop("Wrong interval. Unable to detect interval. Please, check your data.")
          }else{
            df = df %>% dplyr::group_by(.id) %>%
              left_join(resol, by = ".id") %>%
              tidyr::complete(date_time = seq.POSIXt(c(min(date_time)+(as.difftime(dplyr::first(time_diff))-lubridate::minute(min(date_time)))), max(date_time), by=as.difftime(dplyr::first(time_diff), units = "mins"),tz = 'UTC')) %>%
              dplyr::arrange(date_time, .by_group = T) %>% dplyr::select(-time_diff) %>% as.data.frame()
            rm(resol)
          }
        }
        else{
          min_sequence = function(data = data, interval = "1 min"){
            x = min(data$date_time)
            x_minutes = as.numeric(lubridate::minute(min(data$date_time)))
            y = as.numeric(as.difftime(lubridate::duration(paste0(interval))/60, units = "mins"))
            z = x+lubridate::minutes(floor(y*(ceiling(x_minutes/y)-(x_minutes/y))))
            return(z)
          }
          df = df %>%
            dplyr::group_by(.id) %>%
            dplyr::group_modify(~ {
              start_time <- min_sequence(.x, interval = interval)
              .x %>% tidyr::complete(date_time = seq.POSIXt(start_time, max(.x$date_time), by = interval, tz = "UTC")) %>%
                dplyr::arrange(date_time)}) %>% dplyr::ungroup() %>% dplyr::arrange(.id) %>% as.data.frame()
        }
      }else{
        stop("Wrong interval. Use 'auto', '1 min', '5 min', '15 min', '1 hour'")
      }
      # interval end
      df = df %>% select(where(~!all(is.na(.x)))) %>% as.data.frame()
      # Radius units lines----
      # Radius units start
      if(any(colnames(df) %in% "Radius")){
        if(radius_units[1] == "auto"){
          if(all(df$orig_radius_units %in% c("tomst", "um", NA))){
            message(paste0("Radius units were automatically converted into micrometers"))
          }else{
            message(paste0("Check radius units for unexpected values"))
          }
        }else{
        if(radius_units[1] == "tomst"){
          if(any(df$Radius < -2.173461 & round(df$Radius,4) != -277.9314, na.rm = T)){
            message(paste0("Data contains low values. Check whether data were truly downloaded in ", radius_units[1], " units."))
          }else{
            message(paste0(radius_units[1], " units were converted into micrometers and used for Radius calculation."))
          }
          }else{
          if(radius_units[1] == "um"){
            if(any(df$Radius < 0, na.rm = T)){
              message(paste0("Micrometers were used for Radius calculation."))
              message(paste0("Data contains low values. Check Radius data."))
            }else{
              message(paste0("Micrometers were used for Radius calculation."))
            }
            }else{
            stop("Wrong units. Use 'auto', 'tomst' or 'um'")
          }
          }
      }
      }
      # Radius units end
      return(df)
    }
  }
}

