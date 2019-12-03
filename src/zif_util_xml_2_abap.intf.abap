interface ZIF_UTIL_XML_2_ABAP
  public .


  class-methods F4_OPEN_FILE
    importing
      !IV_WINDOW_TITLE type STRING default SPACE
      !IV_FILENAME type STRING optional
      !IV_EXTENSION type STRING default '*.xml'
      !IV_INITIAL_FOLDER type STRING default 'C:\'
    returning
      value(RV_FILEPATH) type STRING .
  type-pools ABAP .
  methods LOAD
    importing
      !IV_DATA type STRING
      !IV_DATA_TYPE type CHAR1
      !IV_ZIP type FLAG default ABAP_TRUE
    returning
      value(RV_OK) type FLAG .
  methods GET_XML
    returning
      value(RV_XML) type STRING .
  methods CONVERT
    exporting
      !EV_OK type FLAG
    changing
      !CR_DATA type ref to DATA .
  methods SHOW_XML
    importing
      !IV_TITLE type CL_ABAP_BROWSER=>TITLE
      !IV_SIZE type STRING default CL_ABAP_BROWSER=>LARGE
      !IO_CONTAINER type ref to CL_GUI_CONTAINER optional .
  methods UPLOAD
    importing
      !IV_FILEPATH type STRING
      !IV_DATA_TYPE type CHAR1 default 'X'
      !IV_ZIP type FLAG default ABAP_TRUE
    returning
      value(RV_OK) type FLAG .
endinterface.
