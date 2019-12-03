interface ZIF_UTIL_ABAP_2_XML
  public .

  type-pools ABAP .

  class-methods F4_SAVE_FILE
    importing
      !IV_WINDOW_TITLE type STRING default SPACE
      !IV_FILENAME type STRING optional
      !IV_EXTENSION type STRING default '*.xml'
      !IV_INITIAL_FOLDER type STRING default 'C:\'
    returning
      value(RV_FILEPATH) type STRING .
  methods LOAD
    importing
      !IR_DATA type ref to DATA
    returning
      value(RV_OK) type FLAG .
  methods CONVERT
    importing
      !IV_ZIP type FLAG default ABAP_TRUE
    exporting
      !EV_XML type STRING
      !EV_BASE64 type STRING .
  methods GET_XML
    returning
      value(RV_XML) type STRING .
  methods GET_BASE64
    returning
      value(RV_BASE64) type STRING .
  methods SHOW_XML
    importing
      !IV_TITLE type CL_ABAP_BROWSER=>TITLE
      !IV_SIZE type STRING default CL_ABAP_BROWSER=>LARGE
      !IO_CONTAINER type ref to CL_GUI_CONTAINER optional .
  methods DOWNLOAD
    importing
      !IV_FILEPATH type STRING
      !IV_DATA_TYPE type CHAR1 default 'X'
    returning
      value(RV_OK) type FLAG .
endinterface.
