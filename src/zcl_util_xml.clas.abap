class ZCL_UTIL_XML definition
  public
  final
  create private .

public section.

  interfaces ZIF_UTIL_ABAP_2_XML .
  interfaces ZIF_UTIL_XML_2_ABAP .

  aliases DOWNLOAD
    for ZIF_UTIL_ABAP_2_XML~DOWNLOAD .
  aliases F4_OPEN_FILE
    for ZIF_UTIL_XML_2_ABAP~F4_OPEN_FILE .
  aliases F4_SAVE_FILE
    for ZIF_UTIL_ABAP_2_XML~F4_SAVE_FILE .
  aliases UPLOAD
    for ZIF_UTIL_XML_2_ABAP~UPLOAD .

  constants:
    BEGIN OF cs_data_type,
        base64 TYPE char1 VALUE 'B',
        xml    TYPE char1 VALUE 'X',
      END OF cs_data_type .

  class-methods GET_ABAP_2_XML
    returning
      value(RI_ABAP_2_XML) type ref to ZIF_UTIL_ABAP_2_XML .
  class-methods GET_XML_2_ABAP
    returning
      value(RI_XML_2_ABAP) type ref to ZIF_UTIL_XML_2_ABAP .
protected section.
private section.

  data MV_DATA_TYPE type CHAR1 .
  data MR_DATA type ref to DATA .
  data MV_XML type STRING .
  data MV_BASE64 type STRING .

  type-pools ABAP .
  class-methods F4_LOCAL
    importing
      !IV_WINDOW_TITLE type STRING default SPACE
      !IV_OPEN_OR_SAVE type FLAG default ABAP_TRUE
      !IV_FILENAME type STRING optional
      !IV_EXTENSION type STRING default '*.xml'
      !IV_INITIAL_FOLDER type STRING default 'C:\'
    returning
      value(RV_FILEPATH) type STRING .
  methods CONSTRUCTOR .
  class CL_ABAP_BROWSER definition load .
  methods SHOW_XML
    importing
      !IV_TITLE type CL_ABAP_BROWSER=>TITLE
      !IV_SIZE type STRING default CL_ABAP_BROWSER=>LARGE
      !IO_CONTAINER type ref to CL_GUI_CONTAINER optional .
ENDCLASS.



CLASS ZCL_UTIL_XML IMPLEMENTATION.


method CONSTRUCTOR.
  endmethod.


METHOD f4_local.

    DATA: lt_file              TYPE filetable,
          ls_file              TYPE file_table,
          lv_rc                TYPE i,
          lv_window_title	     TYPE string VALUE space,
          lv_initial_folder	   TYPE string VALUE 'C:\',
          lv_default_extension TYPE string,
          lv_filename          TYPE string,
          lv_path              TYPE string.


    IF iv_extension IS NOT INITIAL.
      lv_default_extension = iv_extension.
    ELSE.
      lv_default_extension = '*.xml'.
    ENDIF.

    IF iv_initial_folder IS NOT INITIAL.
      lv_initial_folder = iv_initial_folder.
    ENDIF.

    IF iv_open_or_save IS NOT INITIAL.
      " Fichero a abrir
      CALL METHOD cl_gui_frontend_services=>file_open_dialog
        EXPORTING
          window_title            = lv_window_title
          initial_directory       = lv_initial_folder
          default_extension       = lv_default_extension
          default_filename        = iv_filename
          file_filter             = lv_default_extension
        CHANGING
          file_table              = lt_file
          rc                      = lv_rc
        EXCEPTIONS
          file_open_dialog_failed = 1
          cntl_error              = 2
          error_no_gui            = 3
          not_supported_by_gui    = 4
          OTHERS                  = 5.
      IF sy-subrc EQ 0.
        LOOP AT lt_file INTO ls_file.
          MOVE ls_file-filename TO rv_filepath.
          RETURN.
        ENDLOOP.
      ENDIF.

    ELSE.
      " Fichero a guardar
      cl_gui_frontend_services=>file_save_dialog(
        EXPORTING
          window_title              = lv_window_title
          initial_directory         = lv_initial_folder
          default_extension         = lv_default_extension
          default_file_name         = iv_filename
          file_filter               = lv_default_extension
        CHANGING
          filename                  = lv_filename
          path                      = lv_path
          fullpath                  = rv_filepath
        EXCEPTIONS
          cntl_error                = 1                " Control error
          error_no_gui              = 2                " No GUI available
          not_supported_by_gui      = 3                " GUI does not support this
          invalid_default_file_name = 4                " Invalid default file name
          OTHERS                    = 5 ).
    ENDIF.

  ENDMETHOD.


METHOD get_abap_2_xml.

    DATA: lo_util_xml TYPE REF TO zcl_util_xml.

    CREATE OBJECT lo_util_xml.
    ri_abap_2_xml ?= lo_util_xml.

  ENDMETHOD.


METHOD get_xml_2_abap.

    DATA: lo_util_xml TYPE REF TO zcl_util_xml.

    CREATE OBJECT lo_util_xml.
    ri_xml_2_abap ?= lo_util_xml.

  ENDMETHOD.


METHOD show_xml.

    DATA: lv_title TYPE cl_abap_browser=>title.

    IF iv_title IS NOT INITIAL.
      lv_title = iv_title.
    ELSE.
      lv_title = 'XML'.
    ENDIF.

    cl_abap_browser=>show_xml(
      EXPORTING
        xml_string   = mv_xml     " XML in String
        title        = lv_title   " Window Title
        size         = iv_size    " Size (S,M.L,XL)
        container    = io_container ).

  ENDMETHOD.


METHOD zif_util_abap_2_xml~convert.

    DATA: lo_zip     TYPE REF TO cl_abap_zip,
          lv_xstring TYPE xstring.

    FIELD-SYMBOLS: <ft_data> TYPE ANY TABLE.

    " Tabla: Siempre se pasa al fichero XML una tabla de datos
    ASSIGN mr_data->* TO <ft_data>.

    IF <ft_data> IS ASSIGNED.
      " Convertir datos a XML
      CALL TRANSFORMATION id SOURCE abap = <ft_data> RESULT XML mv_xml.
    ENDIF.

    " Cambiar la codificación del XML, ya que al pasar a Binario se recodifican
    " los datos y se debe cambiar el tag XML inicial donde se indica la codificación.
    REPLACE FIRST OCCURRENCE OF 'encoding="utf-16"' IN mv_xml WITH 'encoding="utf-8"'.

    " Pasar Binario
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = mv_xml
      IMPORTING
        buffer = lv_xstring
      EXCEPTIONS
        failed = 1
        OTHERS = 2.

    IF iv_zip EQ abap_true.
      " Comprimir en ZIP los datos binarios
      CREATE OBJECT lo_zip.
      lo_zip->add( name    = 'ABAP'
                   content = lv_xstring ).

      " Convertir a Base64 los datos comprimidos en ZIP
      mv_base64 = cl_http_utility=>if_http_utility~encode_x_base64( unencoded = lo_zip->save( ) ).

    ELSE.
      " Convertir XML pasado a Binario directamente
      mv_base64 = cl_http_utility=>if_http_utility~encode_x_base64( unencoded = lv_xstring ).

    ENDIF.

    " Pasar datos XML y Base64 a la salida
    ev_xml    = mv_xml.
    ev_base64 = mv_base64.

  ENDMETHOD.


METHOD zif_util_abap_2_xml~download.

    DATA: lt_content   TYPE soli_tab,
          lv_xstring   TYPE xstring,
          lv_len       TYPE int4,
          lv_down_data TYPE string.

    " Validar fichero de entrada informado
    IF iv_filepath IS INITIAL.
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    " Descargar los datos seleccionados en base al tipo de entrada
    IF iv_data_type EQ cs_data_type-xml.
      lv_down_data = mv_xml.
    ELSEIF iv_data_type EQ cs_data_type-base64.
      lv_down_data = mv_base64.
    ENDIF.

    " Solo descargar si hay datos
    IF lv_down_data IS INITIAL.
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    " Pasar Binario
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = lv_down_data
      IMPORTING
        buffer = lv_xstring
      EXCEPTIONS
        failed = 1
        OTHERS = 2.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_xstring
      IMPORTING
        output_length = lv_len
      TABLES
        binary_tab    = lt_content[].

    " Descargar datos en BINARIO
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = lv_len               " File length for binary files
        filename                  = iv_filepath          " Name of file
        filetype                  = 'BIN'                " File type (ASCII, binary ...)
      CHANGING
        data_tab                  = lt_content
      EXCEPTIONS
        file_write_error          = 1                    " Cannot write to file
        no_batch                  = 2                    " Cannot execute front-end function in background
        gui_refuse_filetransfer   = 3                    " Incorrect Front End
        invalid_type              = 4                    " Invalid value for parameter FILETYPE
        no_authority              = 5                    " No Download Authorization
        unknown_error             = 6                    " Unknown error
        header_not_allowed        = 7                    " Invalid header
        separator_not_allowed     = 8                    " Invalid separator
        filesize_not_allowed      = 9                    " Invalid file size
        header_too_long           = 10                   " Header information currently restricted to 1023 bytes
        dp_error_create           = 11                   " Cannot create DataProvider
        dp_error_send             = 12                   " Error Sending Data with DataProvider
        dp_error_write            = 13                   " Error Writing Data with DataProvider
        unknown_dp_error          = 14                   " Error when calling data provider
        access_denied             = 15                   " Access to File Denied
        dp_out_of_memory          = 16                   " Not enough memory in data provider
        disk_full                 = 17                   " Storage medium is full.
        dp_timeout                = 18                   " Data provider timeout
        file_not_found            = 19                   " Could not find file
        dataprovider_exception    = 20                   " General Exception Error in DataProvider
        control_flush_error       = 21                   " Error in Control Framework
        not_supported_by_gui      = 22                   " GUI does not support this
        error_no_gui              = 23                   " GUI not available
        OTHERS                    = 24 ).

    IF sy-subrc EQ 0.
      " Descarga correcta
      rv_ok = abap_true.
    ELSE.
      " Error al descargar
      rv_ok = abap_false.
    ENDIF.

  ENDMETHOD.


METHOD zif_util_abap_2_xml~f4_save_file.

    " Diálogo Guardar
    rv_filepath = f4_local( iv_window_title   = iv_window_title
                            iv_open_or_save   = abap_false
                            iv_filename       = iv_filename
                            iv_extension      = iv_extension
                            iv_initial_folder = iv_initial_folder ).

  ENDMETHOD.


METHOD zif_util_abap_2_xml~get_base64.
    rv_base64 = mv_base64.
  ENDMETHOD.


METHOD zif_util_abap_2_xml~get_xml.
    rv_xml = mv_xml.
  ENDMETHOD.


METHOD zif_util_abap_2_xml~load.

    DATA: lr_struc      TYPE REF TO data,
          lo_datadescr  TYPE REF TO cl_abap_datadescr,
          lo_tabledescr TYPE REF TO cl_abap_tabledescr.

    FIELD-SYMBOLS: <ft_data>    TYPE ANY TABLE,
                   <ft_data_in> TYPE ANY TABLE,
                   <fs_data>    TYPE any,
                   <fs_data_in> TYPE any.

    IF ir_data IS INITIAL.
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    " Capturar tipo de datos de los datos de entrada
    lo_datadescr ?= cl_abap_datadescr=>describe_by_data_ref( ir_data ).
    lo_tabledescr ?= lo_datadescr.
    " Los datos SIEMPRE se envían al conversior XML como una tabla
    CREATE DATA mr_data TYPE HANDLE lo_tabledescr.

    ASSIGN mr_data->* TO <ft_data>.
    ASSIGN ir_data->* TO <ft_data_in>.

    IF <ft_data>    IS ASSIGNED AND
       <ft_data_in> IS ASSIGNED.
      " Pasar todo el contenido de la tabla de entrada a la tabla del objeto
      <ft_data>[] = <ft_data_in>[].
      " Marcar salida OK
      rv_ok = abap_true.
    ENDIF.

  ENDMETHOD.


METHOD zif_util_abap_2_xml~show_xml.

    me->show_xml( iv_title     = iv_title
                  iv_size      = iv_size
                  io_container = io_container ).

  ENDMETHOD.


METHOD zif_util_xml_2_abap~convert.

    FIELD-SYMBOLS: <ft_table> TYPE ANY TABLE.

    " Asignar datos de entrada
    ASSIGN cr_data->* TO <ft_table>.
    IF <ft_table> IS ASSIGNED.
      " Transformar datos XML directamente a tabla ABAP de entrada modificable
      CALL TRANSFORMATION id SOURCE XML me->mv_xml RESULT abap = <ft_table>.
      ev_ok = abap_true.
    ENDIF.

  ENDMETHOD.


METHOD zif_util_xml_2_abap~f4_open_file.

    " Diálogo Abrir
    rv_filepath = f4_local( iv_window_title   = iv_window_title
                            iv_open_or_save   = abap_true
                            iv_filename       = iv_filename
                            iv_extension      = iv_extension
                            iv_initial_folder = iv_initial_folder ).

  ENDMETHOD.


METHOD zif_util_xml_2_abap~get_xml.
    rv_xml = mv_xml.
  ENDMETHOD.


METHOD zif_util_xml_2_abap~load.

    DATA: lo_zip       TYPE REF TO cl_abap_zip,
          lt_content   TYPE soli_tab,
          lv_bin_data  TYPE xstring,
          lv_bin_unzip TYPE xstring,
          lv_len       TYPE int4.

    CLEAR: mv_xml, mv_base64, rv_ok.

    IF iv_data_type EQ cs_data_type-xml.
      " Datos en formato XML
      mv_data_type = iv_data_type.
      mv_xml       = iv_data.
      rv_ok        = abap_true.

    ELSEIF iv_data_type EQ cs_data_type-base64.
      " Datos en formato Base64
      mv_data_type = iv_data_type.
      mv_base64    = iv_data.

      " Convertir de Base64 a XString
      lv_bin_data = cl_http_utility=>if_http_utility~decode_x_base64( encoded = iv_data ).

      " Datos comprimidos en ZIP?
      IF iv_zip EQ abap_true.

        " Descomprimir ZIP
        CREATE OBJECT lo_zip.
        lo_zip->load(
          EXPORTING zip              = lv_bin_data
          EXCEPTIONS zip_parse_error = 1
                     OTHERS          = 2 ).
        IF sy-subrc NE 0.
          " Error al crear objeto ZIP con datos binarios
          rv_ok = abap_false.
          RETURN.
        ENDIF.

        " Recuperar datos comprimidos
        lo_zip->get(
          EXPORTING name                     = 'ABAP'
          IMPORTING content                  = lv_bin_unzip
          EXCEPTIONS zip_index_error         = 1
                     zip_decompression_error = 2
                     OTHERS                  = 3 ).
        IF sy-subrc NE 0.
          " Error al capturar datos comprimidos en ZIP
          rv_ok = abap_false.
          RETURN.
        ENDIF.

        CLEAR: lv_bin_data.
        lv_bin_data = lv_bin_unzip.

      ENDIF.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = lv_bin_data
        IMPORTING
          output_length = lv_len
        TABLES
          binary_tab    = lt_content[].

      CALL FUNCTION 'SCMS_BINARY_TO_STRING'
        EXPORTING
          input_length = lv_len
        IMPORTING
          text_buffer  = mv_xml
        TABLES
          binary_tab   = lt_content[]
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.

      IF sy-subrc NE 0.
        " Error de conversión
        rv_ok = abap_false.
        RETURN.
      ELSE.
        " Carga de Datos OK
        rv_ok = abap_true.
      ENDIF.

    ENDIF.

  ENDMETHOD.


METHOD zif_util_xml_2_abap~show_xml.

    me->show_xml( iv_title     = iv_title
                  iv_size      = iv_size
                  io_container = io_container ).

  ENDMETHOD.


METHOD zif_util_xml_2_abap~upload.

    DATA: lt_content   TYPE soli_tab,
          lv_size      TYPE int4,
          lv_file_data TYPE string.

    IF iv_filepath IS INITIAL.
      " Fichero de carga vacío
      rv_ok = abap_false.
    ENDIF.

    IF iv_data_type NE cs_data_type-xml AND
       iv_data_type NE cs_data_type-base64.
      " Tipo de datos incorrecto
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    " Cargar fichero en formato Binario
    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = iv_filepath      " Name of file
        filetype                = 'BIN'            " File Type (ASCII, Binary)
      IMPORTING
        filelength              = lv_size          " File Length
      CHANGING
        data_tab                = lt_content       " Transfer table for file contents
      EXCEPTIONS
        file_open_error         = 1                " File does not exist and cannot be opened
        file_read_error         = 2                " Error when reading file
        no_batch                = 3                " Cannot execute front-end function in background
        gui_refuse_filetransfer = 4                " Incorrect front end or error on front end
        invalid_type            = 5                " Incorrect parameter FILETYPE
        no_authority            = 6                " No upload authorization
        unknown_error           = 7                " Unknown error
        bad_data_format         = 8                " Cannot Interpret Data in File
        header_not_allowed      = 9                " Invalid header
        separator_not_allowed   = 10               " Invalid separator
        header_too_long         = 11               " Header information currently restricted to 1023 bytes
        unknown_dp_error        = 12               " Error when calling data provider
        access_denied           = 13               " Access to File Denied
        dp_out_of_memory        = 14               " Not enough memory in data provider
        disk_full               = 15               " Storage medium is full.
        dp_timeout              = 16               " Data provider timeout
        not_supported_by_gui    = 17               " GUI does not support this
        error_no_gui            = 18               " GUI not available
        OTHERS                  = 19 ).

    IF sy-subrc NE 0.
      " Error al cargar Fichero
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    " Pasar de Binario a String
    CALL FUNCTION 'SCMS_BINARY_TO_STRING'
      EXPORTING
        input_length = lv_size
      IMPORTING
        text_buffer  = lv_file_data
      TABLES
        binary_tab   = lt_content[]
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

    IF sy-subrc NE 0.
      " Error al convertir Binario a STRING
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    " Cargar datos en el objeto conversor
    rv_ok = zif_util_xml_2_abap~load( iv_data      = lv_file_data
                                      iv_data_type = iv_data_type
                                      iv_zip       = iv_zip ).

  ENDMETHOD.
ENDCLASS.
