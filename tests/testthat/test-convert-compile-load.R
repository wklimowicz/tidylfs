test_that("Converting and compiling runs succesfully", {


  test_data <- tibble::tribble(
    ~ACTHR, ~ACTHR2, ~ACTPOT, ~ACTUOT, ~ACTWKDY1, ~ACTWKDY2,
    ~ACTWKDY3, ~ACTWKDY4, ~ACTWKDY5, ~ACTWKDY6, ~ACTWKDY7,
    ~ADD, ~ADDJOB, ~ADVHST, ~AGE, ~AGEDFE, ~AGES, ~AOFL16,
    ~AOFL19, ~AOHL16, ~AOHL19, ~APPREN, ~APPSAM, ~ATFRM2,
    ~ATFROM, ~ATTEND, ~AXFA, ~AXFB, ~AXPA, ~AXPB, ~AYFL19,
    ~AYHL19, ~BACTHR, ~BANDG, ~BANDG2, ~BANDN, ~BANDN2, ~BEFOR,
    ~BEFORF, ~BENFTS, ~BONCMP1, ~BONCMP2, ~BONCMP3, ~BONCMP4,
    ~BTEC, ~BUSHR, ~CAIND, ~CALQRTR, ~CALW1YR, ~CALWEEK,
    ~CAMEYR, ~CANDG, ~CASENO, ~CASHFUL, ~CASHTIM,
    ~CGNOW, ~CGQUL, ~CLAIMS, ~CMBDEG01, ~CMBDEG02, ~CMBDEG03,
    ~CMBDEG04, ~CMBDEG05, ~CMBDEG06, ~CMBDEG07, ~CMBDEG08,
    ~CMBDEG09, ~CMBDEG10, ~CMBDEG11, ~CMBDEG12, ~CMBMAIN,
    ~CONMON, ~CONMPY, ~CONSEY, ~COUNTRY, ~COURSE, ~CRY01,
    ~CRYO, ~CRYOX, ~CSE, ~CURCODE, ~CURED, ~CYMR, ~CYMS, ~CYMU,
    ~CYMW, ~DAYSPZ, ~DEGNOW, ~DEGQUL, ~DEGREE, ~DIFFHR,
    ~DIFJOB, ~DIFQUL, ~DISCURR, ~DOBD, ~DOBM, ~DOBY, ~DTEOFBTH,
    ~DURUN, ~DURUN2, ~DVHRPNUM, ~ED13WK, ~ED1FUT, ~ED4WK,
    ~EDAGE, ~EMPLEN, ~EMPMON, ~ENROLL, ~ERNCM01, ~ERNCM02,
    ~ERNCM03, ~ERNCM04, ~ERNCM05, ~ERNCM06, ~ERNCM07, ~ERNCM08,
    ~ERNCM09, ~ERNCM10, ~ERNCM11, ~ERNFILT, ~ETH01, ~ETHAS,
    ~ETHBL, ~ETHCEN15, ~ETHCEN6, ~ETHMX, ~ETHWH, ~EVDAY,
    ~EVEROT, ~EVERWK, ~EVEVE, ~EVHM98, ~EVNGHT, ~EVSAT, ~EVSUN,
    ~EXTFU, ~EXTHRS, ~FAMLY031, ~FAMLY032, ~FAMLY033, ~FAMUNIT,
    ~FDPCH15, ~FDPCH16, ~FDPCH19, ~FDPCH2, ~FDPCH4, ~FDPCH9,
    ~FLED9D, ~FLEX9D1, ~FLEX9D2, ~FLEX9D3, ~FLEXW1, ~FLEXW2,
    ~FLEXW3, ~FLEXW4, ~FLEXW5, ~FLEXW6, ~FLEXW7, ~FLEXW8,
    ~FLEXW9, ~FMPLUS, ~FTPT, ~FTPTW, ~FTPTWK, ~FURN, ~FUTUR13,
    ~FUTUR4, ~GB, ~GCSE, ~GNVNOW, ~GNVQ, ~GNVQUL, ~GOR3,
    ~GORONE, ~GORWK2R, ~GORWKR, ~GOVTOF, ~GOVTOF2, ~GOVTOR,
    ~GROSS99, ~GRSEXP, ~GRSPRD, ~GRSSWK, ~GRSSWK2, ~HALLRES,
    ~HDPCH19, ~HEAL01, ~HEAL02, ~HEAL03, ~HEAL04, ~HEAL05,
    ~HEAL06, ~HEAL07, ~HEAL08, ~HEAL09, ~HEAL10, ~HEAL11,
    ~HEAL12, ~HEAL13, ~HEAL14, ~HEAL15, ~HEAL16, ~HEAL17,
    ~HEALIM, ~HEALPB01, ~HEALPB02, ~HEALPB03, ~HEALPB04,
    ~HEALPB05, ~HEALPB06, ~HEALPB07, ~HEALPB08, ~HEALPB09,
    ~HEALPB10, ~HEALTH, ~HEALYL, ~HEALYR, ~HGHNOW, ~HGHQUL,
    ~HHLD, ~HIGHO, ~HIQUAL, ~HIQUALD, ~HITQUA, ~HOHID, ~HOME,
    ~HOME2, ~HOMED1, ~HOMED2, ~HOMED21, ~HOMED22, ~HOMED23,
    ~HOMED3, ~HOURLY, ~HOURPAY, ~HOUT, ~HOWGET, ~HPRMB, ~HRP,
    ~HRPID, ~HRRATE, ~HSNGGB1, ~HSNGGB2, ~HSNGNI, ~HST,
    ~HSTNOW, ~HSTQUL, ~ILLDAYS1, ~ILLDAYS2, ~ILLDAYS3,
    ~ILLDAYS4, ~ILLDAYS5, ~ILLDAYS6, ~ILLDAYS7, ~ILLFRI,
    ~ILLMON, ~ILLOFF, ~ILLSAT, ~ILLSUN, ~ILLTHU, ~ILLTUE,
    ~ILLWED, ~ILLWK, ~ILODEFR, ~INCNOW, ~INCSUP, ~INDD92L,
    ~INDD92M, ~INDD92S, ~INDG92L, ~INDG92M, ~INDG92S, ~INDM92L,
    ~INDM92M, ~INDM92O, ~INDM92S, ~INDS92L, ~INDS92M, ~INDS92S,
    ~INDSECT, ~INECACR, ~IOUTCOME, ~IREND2, ~JBAWAY, ~JOBBEG,
    ~JOBED, ~JOBLRN1, ~JOBLRN2, ~JOBLRN3, ~JOBTMP, ~JOBTMP2,
    ~JOBTRN, ~JOBTYP, ~JOBTYP2, ~JSADUR, ~JSATYP, ~LAND96,
    ~LEFTM, ~LEFTW, ~LEFTYR, ~LEISCL, ~LESPAY, ~LESPAY2,
    ~LESPAY3, ~LEVQUAL, ~LIKEWK, ~LIMITA, ~LIMITK, ~LIVTOG,
    ~LKFTPA, ~LKFTPC, ~LKSELA, ~LKSELC, ~LKTIMA, ~LKTIMB,
    ~LKWFWM, ~LKYT4, ~LNGLIM, ~LOOK4, ~LOOKM1, ~LOOKM2,
    ~LOOKM3, ~LPCODE, ~LSSOTH, ~M3CRY, ~M3CRYO, ~MAINMA,
    ~MAINME, ~MAINMS, ~MANAG2, ~MANAGER, ~MANAGLR, ~MARCHK,
    ~MARDY, ~MARSEX, ~MARSTT, ~MATLVE, ~METHAL01, ~METHAL02,
    ~METHAL03, ~METHAL04, ~METHAL05, ~METHAL06, ~METHAL07,
    ~METHAL08, ~METHAL09, ~METHAL10, ~METHAL11, ~METHAL12,
    ~METHAL13, ~METHAL14, ~METHM, ~METHMP01, ~METHMP02,
    ~METHMP03, ~METHMP04, ~METHMP05, ~METHMP06, ~METHMP07,
    ~METHMP08, ~METHMP09, ~METHMP10, ~METHMP11, ~METHPY,
    ~METHSE1, ~METHSE2, ~METHSE3, ~METHSE4, ~METHSE5, ~METHSE6,
    ~MODAPP, ~MOVED, ~MPNLR02, ~MPNR02, ~MPNSR02, ~NATIDB,
    ~NATIDE, ~NATIDI, ~NATIDO, ~NATIDS, ~NATIDW, ~NATION,
    ~NATO, ~NATOX, ~NDTYPE, ~NET99, ~NETPRD, ~NETWK, ~NETWK2,
    ~NEWDEAL, ~NEWQUL, ~NOCUST, ~NOLOOK, ~NOWANT, ~NSECM,
    ~NSECMMJ, ~NUMAL, ~NUMAS, ~NUMHHLD, ~NUMOL, ~NUMSCE,
    ~NURSE, ~NUTS3, ~NVQHI, ~NVQKN2, ~NVQLE2, ~NVQLEV, ~NVQQUL,
    ~NVQSAM, ~NVQSVQ, ~NVQUN, ~OMCONT, ~OMROLE, ~ONETEN,
    ~OTHWP, ~OTWPNO2, ~OVABL, ~OVHRS, ~OVNST, ~OVRTME, ~OVSKHR,
    ~OVST, ~OWNBUS, ~OYCIRC, ~OYCRY, ~OYCRYO, ~OYEQM3, ~OYFTPT,
    ~OYMNGE, ~OYMPE02, ~OYMPR02, ~OYMPS02, ~OYRESC, ~OYSIND,
    ~OYSOCC, ~OYSOLO, ~OYSTAT, ~OYSUPVI, ~PAIDHRA, ~PAIDHRU,
    ~PAYSSP, ~PCA, ~PDWAGE, ~PENBEN31, ~PENBEN32, ~PENBEN33,
    ~PENBEN34, ~PERSNO, ~PIWT14, ~POTHR, ~PREFHR, ~PRXREL,
    ~PUBLICR, ~PWT14, ~PYSLIP, ~QALPL99, ~QGCSE, ~QGNVQ,
    ~QLPLO99, ~QRTR, ~QUALCH1, ~QUALCH2, ~QUALCH3, ~QUALS01,
    ~QUALS02, ~QUALS03, ~QUALS04, ~QUALS05, ~QUALS06, ~QUALS07,
    ~QUALS08, ~QUALS09, ~QUALS10, ~QUALS11, ~QULHI, ~QULNOW,
    ~QUOTA, ~RDIC92, ~RDMPNO2, ~RECNO, ~REDANY, ~REDCLOS,
    ~REDCLS, ~REDIND, ~REDINDYR, ~REDMNGE, ~REDMPN2, ~REDOCC,
    ~REDP1, ~REDP2, ~REDP3, ~REDPAID, ~REDSOLO, ~REDSTAT,
    ~REDSUPV, ~REDUND, ~REDYLFT, ~REFDTE, ~REFWKD, ~REFWKM,
    ~REFWKY, ~REG3, ~REGONE, ~REGWK2R, ~REGWKR, ~RELBUS,
    ~RELH96, ~RELHFU, ~RELHRP, ~RELIG, ~RELP, ~RENT96, ~RESBBY,
    ~RESMTH, ~RESPNO, ~RESTEMR, ~RESTME, ~RSA, ~RSANOW,
    ~RSAQUL, ~SAMELAD, ~SAMQUL, ~SC2KLMJ, ~SC2KLMN, ~SC2KMMJ,
    ~SC2KMMN, ~SC2KOMJ, ~SC2KOMN, ~SC2KSMJ, ~SC2KSMN, ~SCHM99,
    ~SCNOW, ~SCNTGA, ~SCQUL, ~SCTVEC, ~SECEX, ~SECGA, ~SECGB,
    ~SECGRO, ~SECJMBR, ~SECJOB, ~SECNET, ~SECTOR, ~SECTRO03,
    ~SELF1, ~SELF2, ~SELF3, ~SELF4, ~SEX, ~SHFTWK99, ~SHFTYP,
    ~SIC80L, ~SIC80M, ~SIC80O, ~SIC80S, ~SINCOM, ~SKDSBN31,
    ~SKDSBN32, ~SKDSBN33, ~SKDSBN34, ~SKDSBN35, ~SKDSBN36,
    ~SKDSBN37, ~SLEARN1, ~SLEARN2, ~SLEARN3, ~SMESIT, ~SMEST2,
    ~SMSXFU, ~SNGDEG, ~SOC2KAP, ~SOC2KL, ~SOC2KM, ~SOC2KO,
    ~SOC2KR, ~SOC2KS, ~SOLO2, ~SOLOLR, ~SOLOR, ~START, ~STAT2,
    ~STATLR, ~STATR, ~STUCUR, ~SUBCOD1, ~SUBCOD2, ~SUBCOD3,
    ~SUBCOD4, ~SUBCOD5, ~SUBCOD6, ~SUBCOD7, ~SUBCOD8, ~SUMHRS,
    ~SUPVIS, ~SUPVIS2, ~TEACH1, ~TEACH2, ~TEACH3, ~TECLEC,
    ~TECNOW, ~TECQUL, ~TELEQA, ~TELEQB, ~TELQA2, ~TELQB2,
    ~TEMLEN, ~TEN96, ~THISWV, ~TOTAC1, ~TOTAC2, ~TOTHRS,
    ~TOTUS1, ~TOTUS2, ~TPBEN031, ~TPBEN032, ~TPBEN033,
    ~TPBEN034, ~TPBEN035, ~TPBEN036, ~TPBEN037, ~TPBEN038,
    ~TRATIR, ~TRHR93, ~TRNDAY, ~TRNLEN, ~TRNOPP, ~TRONJB,
    ~TRSITE, ~TTACHR, ~TTUSHR, ~TTWA, ~TYEMPS, ~TYPHST,
    ~TYPINT, ~UACNTY, ~UALA, ~UALADGB, ~UALD3, ~UALDO, ~UALDWK,
    ~UALDWK2, ~UNDABL, ~UNDEMP, ~UNDHRS, ~UNDNST, ~UNDSKHR,
    ~UNDST, ~UNDY981, ~UNDY982, ~UNDY983, ~UNDY984, ~UNDY985,
    ~UNDY986, ~UNDY987, ~UNDY988, ~UNDY989, ~UNEMBN1, ~UNEMBN2,
    ~UNWYMN, ~UOTHR, ~URESMC, ~USESLP, ~USGRS99, ~USNET99,
    ~USUGPAY, ~USUHR, ~USUNPAY, ~USUWRK1, ~USUWRK2, ~USUWRK3,
    ~VARYHR, ~VCQPLO, ~VOCQPL, ~W1YR, ~WAIT, ~WARD98, ~WAVFND,
    ~WCHDAY1, ~WCHDAY2, ~WCHDAY3, ~WCHDAY4, ~WCHDAY5, ~WCHDAY6,
    ~WCHDAY7, ~WCHFR, ~WCHMO, ~WCHSA, ~WCHSU, ~WCHTH, ~WCHTU,
    ~WCHWE, ~WEEK, ~WHYTMP, ~WKFRI, ~WKMON, ~WKSAT, ~WKSUN,
    ~WKTHU, ~WKTUE, ~WKWED, ~WNLEFT, ~WNLEFT2, ~WRKAGE,
    ~WRKING, ~XDISDDA, ~XR00, ~XR01, ~XR02, ~XR03, ~XR04,
    ~XR05, ~XR06, ~XR07, ~XR08, ~XR09, ~XR10, ~XR11, ~XR12,
    ~XR13, ~XR14, ~XR15, ~Y2JOB, ~YERQAL1, ~YERQAL2, ~YERQAL3,
    ~YLESS, ~YMORE, ~YNOTFT, ~YPAYL, ~YPAYM, ~YPTCIA, ~YPTJOB,
    ~YSTART, ~YTETJB, ~YTETMP, ~YVARY99, 1, 1, 1,
    1, "1", "1", "1", "1", "1",
    "1", "1", 1, "1", "1", 1, 1,
    "1", 1, 1, 1, 1, "1", "1",
    "1", "1", "1", "1", "1", "1", "1", 1,
    1, 1, "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", 1, "1", "1", 1, 1, 1,
    "1", "40001220110101", 1, 1, "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", 1, 1, "1",
    "1", "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    1, "1", "1", "1", 1, 1, 1,
    "01021935", "1", "1", 1, "1", "1",
    "1", "1", "1", 1, "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", 1, "1",
    "1", "1", "1", 1, 1, 1,
    1, 1, 1, 1, "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", 1, "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", 1, 1,
    "1", 1, "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", 1, "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", 1, "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", 1, "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "-5", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    1, "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", 1, 1, "1", "1", "1",
    "1", "1", "1", "1", "1", "1", 1,
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", 1,
    1, "1", "1", "1", "1", "1", "1",
    "1", 1, 1, 1, "1", "1", "1",
    1, "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", 1, "1", "1",
    1, "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "01062003",
    1, 1, 1, "1", "1", "1", "1",
    "1", "1", "1", 1, "1", "1", "1",
    "1", 1, 1, "1", "1", "1", "1",
    "1", "1", "1", "1", "1", 1,
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", 1,
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", 1, "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", 1, "1",
    "1", 1, "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    1, 1, 1, "1", "1", 1, 1,
    1, "1", "1", "1", "1", "1", "1",
    "1", "1", "-10", "-10", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", 1, "1", "1", 1, "1", "1",
    "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", 1,
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1", "1",
    "1", 1, 1, "1", "1", "1", "1",
    "1", "1", "1", "1", "1", "1", "1"
  )




  test_convert_compile <- function() {

    withr::local_envvar(DATA_DIRECTORY = ".")

    lfs_convert("test", "test_rds_data/")

    lfs_compile("test_rds_data", save_to_folder = TRUE)

    lfs <- lfs_load()
  }


  withr::with_file(list(fs::dir_create("test"),
    fs::dir_create("test_rds_data"),
    "test/2003 Q4.sav" = haven::write_sav(test_data, "test/2003 Q4.sav"),
    "test/2010 Q4.sav" = haven::write_sav(test_data, "test/2010 Q4.sav"),
    "lfs_data.fst",
    "lfs_variables_report.csv"
  ), {
    expect_error(suppressMessages(test_convert_compile()), NA)
  })


  # APS ----------------------------------------


  test_convert_compile_aps <- function() {

    withr::local_envvar(DATA_DIRECTORY = ".")

    lfs_convert("test", "test_aps_data/")

    lfs_compile("test_aps_data/", save_to_folder = TRUE, aps = TRUE)

    aps <- aps_load()
  }


  withr::with_file(list(fs::dir_create("test"),
    fs::dir_create("test_aps_data"),
    "test/APS 2003.sav" = haven::write_sav(test_data, "test/APS 2003.sav"),
    "test/APS 2010.sav" = haven::write_sav(test_data, "test/APS 2010.sav"),
    "aps_data.fst",
    "aps_variables_report.csv"
  ), {
    expect_error(suppressMessages(test_convert_compile_aps()), NA)
  })




  # expect_error(suppressMessages(test_convert_compile()), NA)

  # Cleanup
  # file <- paste0("test_rds_data.Rds")
  # if (file.exists(file)) {
  #   # Delete file if it exists
  #   file.remove(file)
  # }

  # file <- "variables_report.csv"
  # if (file.exists(file)) {
  #   # Delete file if it exists
  #   file.remove(file)
  # }
})


# df <- haven::read_sav("test_raw_data/2003_Q2.sav")
# datapasta::dmdclip(df)
