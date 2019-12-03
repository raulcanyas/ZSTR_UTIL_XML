*&---------------------------------------------------------------------*
*& Report ZDEMO_XML
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdemo_xml.

**********************************************************************
* Definiciones Globales
**********************************************************************
DATA: gi_abap_2_xml TYPE REF TO zif_util_abap_2_xml,
      gi_xml_2_abap TYPE REF TO zif_util_xml_2_abap.

DATA: gt_sflight   TYPE TABLE OF sflight,
      gt_sbook     TYPE TABLE OF sbook,
      gv_ok        TYPE flag,
      gv_scr_error TYPE flag,
      gv_date      TYPE c LENGTH 10,
      gv_msg       TYPE text100.

DATA: go_split     TYPE REF TO cl_gui_splitter_container,
      go_container TYPE REF TO cl_gui_container,
      go_alv       TYPE REF TO cl_gui_alv_grid,
      gt_fieldcat  TYPE lvc_t_fcat,
      gt_sort      TYPE lvc_t_sort,
      gs_layout    TYPE lvc_s_layo,
      gs_variant   TYPE disvariant.

DATA: go_split_xml     TYPE REF TO cl_gui_splitter_container,
      go_container_xml TYPE REF TO cl_gui_container,
      gv_title         TYPE cl_abap_browser=>title.


CONSTANTS: cv_xml    TYPE string VALUE '*.xml',
           cv_base64 TYPE string VALUE '*.base64'.


**********************************************************************
* Pantalla de Selección
**********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-t00.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_a2x   RADIOBUTTON GROUP g0 DEFAULT 'X' USER-COMMAND rfrs.
SELECTION-SCREEN COMMENT (30) FOR FIELD p_a2x.
PARAMETERS: p_x2a RADIOBUTTON GROUP g0.
SELECTION-SCREEN COMMENT (30) FOR FIELD p_x2a.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_sfli   RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND rfrs.
SELECTION-SCREEN COMMENT (30) FOR FIELD p_sfli.
PARAMETERS: p_sbook RADIOBUTTON GROUP g1.
SELECTION-SCREEN COMMENT (30) FOR FIELD p_sbook.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t02.
PARAMETERS: p_carrid TYPE s_carr_id MODIF ID c20.
PARAMETERS: p_date   TYPE s_date MODIF ID c21.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-t03.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_down   RADIOBUTTON GROUP g3 DEFAULT 'X' USER-COMMAND rfrs MODIF ID c30.
SELECTION-SCREEN COMMENT (30) FOR FIELD p_down MODIF ID c30.
PARAMETERS: p_show RADIOBUTTON GROUP g3 MODIF ID c30.
SELECTION-SCREEN COMMENT (30) FOR FIELD p_show MODIF ID c30.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_xml   RADIOBUTTON GROUP g4 DEFAULT 'X' USER-COMMAND rfrs MODIF ID c31.
SELECTION-SCREEN COMMENT (30) FOR FIELD p_xml MODIF ID c31.
PARAMETERS: p_base64 RADIOBUTTON GROUP g4 MODIF ID c31.
SELECTION-SCREEN COMMENT (30) FOR FIELD p_base64 MODIF ID c31.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.

PARAMETERS: p_file TYPE string LOWER CASE MODIF ID c32.

SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN END OF BLOCK b0.


**********************************************************************
* Ajustes pantalla selección y Ayudas de Búsqueda
**********************************************************************
AT SELECTION-SCREEN OUTPUT.
  PERFORM change_screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_file.



**********************************************************************
* Lógica programa
**********************************************************************
START-OF-SELECTION.

  PERFORM validate_screen CHANGING gv_scr_error.
  IF gv_scr_error IS INITIAL.
    CLEAR: gv_ok, gv_msg.

    IF p_a2x IS NOT INITIAL.
      " Seleccionar datos ABAP
      PERFORM select_abap_data.
      " Convertir a XML o Base64
      PERFORM convert CHANGING gv_ok gv_msg.

    ELSEIF p_x2a IS NOT INITIAL.
      " Carga de datos del fichero y conversión a ABAP
      PERFORM upload_and_convert CHANGING gv_ok gv_msg.

    ENDIF.

  ENDIF.

**********************************************************************
* Lógica de Presentación
**********************************************************************
END-OF-SELECTION.

  IF gv_scr_error IS INITIAL.

    IF p_a2x IS NOT INITIAL.

      IF p_down IS NOT INITIAL.
        " Descargar datos si no hay errores
        PERFORM download USING gv_ok gv_msg.

      ELSEIF p_show IS NOT INITIAL.
        " Visualizar XML si no hay errore
        PERFORM show_xml USING gv_ok gv_msg.

      ENDIF.

    ELSEIF p_x2a IS NOT INITIAL.
      " Visualizar datos del fichero cargados en pantalla ALV
      CALL SCREEN 9000.

    ENDIF.

  ENDIF.



**********************************************************************
* Rutinas generales
**********************************************************************

*&---------------------------------------------------------------------*
*&      Form  CHANGE_SCREEN
*&---------------------------------------------------------------------*
FORM change_screen .

  LOOP AT SCREEN.

    IF screen-group1 EQ 'C20'. " Filtro CARRID

      IF p_a2x IS NOT INITIAL.
        screen-invisible = '0'.
        screen-input     = '1'.
      ELSEIF p_x2a IS NOT INITIAL.
        screen-invisible = '1'.
        screen-input     = '0'.
      ENDIF.
      MODIFY SCREEN.

    ELSEIF screen-group1 EQ 'C21'. " Filtro DATE

      IF p_a2x IS NOT INITIAL.
        IF p_sfli IS NOT INITIAL.
          screen-invisible = '1'.
          screen-input     = '0'.
        ELSEIF p_sbook IS NOT INITIAL.
          screen-invisible = '0'.
          screen-input     = '1'.
        ENDIF.
      ELSEIF p_x2a IS NOT INITIAL.
        screen-invisible = '1'.
        screen-input     = '0'.
      ENDIF.
      MODIFY SCREEN.

    ELSEIF screen-group1 EQ 'C30'. " Opción de salida

      IF p_a2x IS NOT INITIAL.
        screen-invisible = '0'.
        screen-input     = '1'.

        IF p_show IS NOT INITIAL.
          p_xml    = abap_true.
          p_base64 = abap_false.
        ENDIF.

      ELSEIF p_x2a IS NOT INITIAL.
        screen-invisible = '1'.
        screen-input     = '0'.
      ENDIF.
      MODIFY SCREEN.

    ELSEIF screen-group1 EQ 'C31'. " Tipo Fichero XML o Base64

      IF p_a2x IS NOT INITIAL.
        IF p_down IS NOT INITIAL.
          screen-invisible = '0'.
          screen-input     = '1'.
        ELSEIF p_show IS NOT INITIAL.
          screen-invisible = '1'.
          screen-input     = '0'.
        ENDIF.
      ELSEIF p_x2a IS NOT INITIAL.
        screen-invisible = '0'.
        screen-input     = '1'.
      ENDIF.
      MODIFY SCREEN.

    ELSEIF screen-group1 EQ 'C32'. " Fichero

      IF p_a2x IS NOT INITIAL.
        IF p_down IS NOT INITIAL.
          screen-invisible = '0'.
          screen-input     = '1'.
        ELSEIF p_show IS NOT INITIAL.
          screen-invisible = '1'.
          screen-input     = '0'.
        ENDIF.
      ELSEIF p_x2a IS NOT INITIAL.
        screen-invisible = '0'.
        screen-input     = '1'.
      ENDIF.
      MODIFY SCREEN.

    ENDIF.

  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F4_FILE
*&---------------------------------------------------------------------*
FORM f4_file .

  DATA: lv_extension TYPE string,
        lv_filename  TYPE string.

  CLEAR: lv_extension.
  IF p_xml IS NOT INITIAL.
    " Nombre fichero por defecto
    IF p_x2a IS NOT INITIAL.
      lv_filename = '*.xml'.
    ELSEIF p_a2x IS NOT INITIAL.
      IF p_sfli IS NOT INITIAL.
        lv_filename  = 'Test_Vuelos.xml'.
      ELSEIF p_sbook IS NOT INITIAL.
        lv_filename  = 'Test_Reservas.xml'.
      ENDIF.
    ENDIF.
    " Extensión por defecto
    lv_extension = '*.xml'.
  ELSEIF p_base64 IS NOT INITIAL.
    " Nombre fichero por defecto
    IF p_x2a IS NOT INITIAL.
      lv_filename = '*.bas'.
    ELSEIF p_a2x IS NOT INITIAL.
      IF p_sfli IS NOT INITIAL.
        lv_filename  = 'Test_Vuelos.bas'.
      ELSEIF p_sbook IS NOT INITIAL.
        lv_filename  = 'Test_Reservas.bas'.
      ENDIF.
    ENDIF.
    " Extensión por defecto
    lv_extension = '*.bas'.
  ENDIF.

  IF p_x2a IS NOT INITIAL.
    " Fichero a cargar
    p_file = zcl_util_xml=>f4_open_file( iv_filename  = lv_filename
                                         iv_extension = lv_extension ).
  ELSEIF p_a2x IS NOT INITIAL.
    " Fichero a descargar
    p_file = zcl_util_xml=>f4_save_file( iv_filename  = lv_filename
                                         iv_extension = lv_extension ).
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  VALIDATE_SCREEN
*&---------------------------------------------------------------------*
FORM validate_screen CHANGING cv_scr_error TYPE flag.

  IF p_a2x IS NOT INITIAL.

    IF p_carrid IS INITIAL.
      MESSAGE s398(00) WITH 'Compañía aérea obligatoria.' DISPLAY LIKE 'E'.
      cv_scr_error = abap_true.
    ENDIF.

    IF p_sbook IS NOT INITIAL.
      IF p_date IS INITIAL.
        MESSAGE s398(00) WITH 'Fecha de Vuelo obligatoria.' DISPLAY LIKE 'E'.
        cv_scr_error = abap_true.
      ENDIF.
    ENDIF.

    IF p_down IS NOT INITIAL.
      IF p_file IS INITIAL.
        MESSAGE s398(00) WITH 'Fichero obligatorio.' DISPLAY LIKE 'E'.
        cv_scr_error = abap_true.
      ENDIF.
    ENDIF.

  ELSEIF p_x2a IS NOT INITIAL.

    IF p_file IS INITIAL.
      MESSAGE s398(00) WITH 'Fichero obligatorio.' DISPLAY LIKE 'E'.
      cv_scr_error = abap_true.
    ENDIF.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  SELECT_ABAP_DATA
*&---------------------------------------------------------------------*
FORM select_abap_data.

  CLEAR: gt_sflight[], gt_sbook[].

  IF p_sfli IS NOT INITIAL.
    SELECT * INTO TABLE gt_sflight
      FROM sflight
      WHERE carrid EQ p_carrid.

  ELSEIF p_sbook IS NOT INITIAL.
    SELECT * INTO TABLE gt_sbook
      FROM sbook
      WHERE carrid EQ p_carrid AND fldate EQ p_date.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  CONVERT
*&---------------------------------------------------------------------*
FORM convert CHANGING cv_ok  TYPE flag
                                   cv_msg TYPE text100.

  DATA: lr_data TYPE REF TO data.

  " Pasar tabla de datos a variable genérica
  IF p_sfli IS NOT INITIAL.
    GET REFERENCE OF gt_sflight INTO lr_data.
  ELSEIF p_sbook IS NOT INITIAL.
    GET REFERENCE OF gt_sbook INTO lr_data.
  ENDIF.

  " Crear instancia conversor ABAP a XML
  CLEAR: gi_abap_2_xml.
  gi_abap_2_xml = zcl_util_xml=>get_abap_2_xml( ).
  IF gi_abap_2_xml IS INITIAL.
    cv_ok  = abap_false.
    cv_msg = 'Error al instanciar conversor ABAP a XML.'.
    RETURN.
  ENDIF.

  " Cargar datos ABAP en conversor
  cv_ok = gi_abap_2_xml->load( ir_data = lr_data ).
  IF cv_ok EQ abap_false.
    cv_msg = 'Error al cargar datos ABAP.'.
    RETURN.
  ENDIF.

  " Convertir datos cargados y convertir a XML y Base64
  gi_abap_2_xml->convert( iv_zip = abap_true ).

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  UPLOAD_AND_CONVERT
*&---------------------------------------------------------------------*
FORM upload_and_convert CHANGING cv_ok  TYPE flag
                                 cv_msg TYPE text100.

  DATA: lr_data TYPE REF TO data.

  CLEAR: gi_xml_2_abap.
  gi_xml_2_abap = zcl_util_xml=>get_xml_2_abap( ).

  IF p_xml IS NOT INITIAL.
    " Cargar datos del fichero en formato XML
    cv_ok = gi_xml_2_abap->upload( iv_filepath  = p_file
                                   iv_data_type = zcl_util_xml=>cs_data_type-xml
                                   iv_zip       = abap_true ).

  ELSEIF p_base64 IS NOT INITIAL.
    " Cargar datos del fichero en formato Base64
    cv_ok = gi_xml_2_abap->upload( iv_filepath  = p_file
                                   iv_data_type = zcl_util_xml=>cs_data_type-base64
                                   iv_zip       = abap_true ).

  ENDIF.

  IF cv_ok EQ abap_false.
    " Error en la carga de datos
    IF p_xml IS NOT INITIAL.
      cv_msg = 'Error al cargar datos XML.'.
    ELSEIF p_base64 IS NOT INITIAL.
      cv_msg = 'Error al cargar datos Base64.'.
    ENDIF.
    RETURN.
  ENDIF.

  " Asignar la tabla de datos adecuada. Los formatos de los datos XML y
  " la tabla de datos deben coincidir.
  IF p_sfli IS NOT INITIAL.
    GET REFERENCE OF gt_sflight INTO lr_data.
  ELSEIF p_sbook IS NOT INITIAL.
    GET REFERENCE OF gt_sbook INTO lr_data.
  ENDIF.

  " Convertir datos del fichero cargado a la tabla ABAP correspondiente
  gi_xml_2_abap->convert( IMPORTING ev_ok  = cv_ok
                          CHANGING cr_data = lr_data ).

  IF cv_ok EQ abap_false.
    " Error en la carga de datos
    IF p_xml IS NOT INITIAL.
      cv_msg = 'Error al convertir datos XML.'.
    ELSEIF p_base64 IS NOT INITIAL.
      cv_msg = 'Error al convertir datos Base64.'.
    ENDIF.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD
*&---------------------------------------------------------------------*
FORM download USING uv_ok  TYPE flag
                    uv_msg TYPE text100.

  DATA: lv_ok TYPE flag.

  IF uv_ok EQ abap_false.
    " Error durante la conversión
    WRITE:/ '@5C@', 'Error!!'.
    WRITE:/ '@5C@', '->', uv_msg.
  ELSE.
    " Descargar datos
    CLEAR: gv_ok.
    IF p_xml IS NOT INITIAL.
      " Descargar datos en formato XML
      gv_ok = gi_abap_2_xml->download( iv_filepath  = p_file
                                       iv_data_type = zcl_util_xml=>cs_data_type-xml ).
    ELSEIF p_base64 IS NOT INITIAL.
      " Descargar datos en formato Base64
      gv_ok = gi_abap_2_xml->download( iv_filepath  = p_file
                                       iv_data_type = zcl_util_xml=>cs_data_type-base64 ).
    ENDIF.
    IF gv_ok EQ abap_true.
      WRITE:/ '@5B@', 'Conversión ABAP a XML:'.
      WRITE:/ '@5B@', 'Fichero descargado correctamente.'.
    ELSE.
      WRITE:/ '@5C@', 'Error!!'.
      WRITE:/ '@5C@', 'Error durante la descarga del Fichero.'.
    ENDIF.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  SHOW_XML
*&---------------------------------------------------------------------*
FORM show_xml USING uv_ok  TYPE flag
                    uv_msg TYPE text100.

  IF uv_ok EQ abap_false.
    " Error durante la conversión
    WRITE:/ '@5C@', 'Error!!'.
    WRITE:/ '@5C@', '->', uv_msg.
  ELSE.

    CLEAR: gv_title.
    IF p_sfli IS NOT INITIAL.
      gv_title = 'Vuelos'.
    ELSEIF p_sbook IS NOT INITIAL.
      gv_title = 'Reservas'.
    ENDIF.

    " Visualizar XML en Pantalla
    CALL SCREEN 9100.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'BASICO'.
  IF p_sfli IS NOT INITIAL.
    SET TITLEBAR 'CARGA_XML' WITH 'Vuelos Compañía:' p_carrid.
  ELSEIF p_sbook IS NOT INITIAL.
    CLEAR: gv_date.
    WRITE p_date TO gv_date.
    SET TITLEBAR 'CARGA_XML' WITH 'Reservas:' p_carrid '-' gv_date.
  ENDIF.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  ALV_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE alv_9000 OUTPUT.
  PERFORM alv_9000.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE sy-ucomm.
    WHEN '&F03'.
      SET SCREEN 0. LEAVE SCREEN.
      PERFORM clear_9000.
    WHEN '&F15' OR '&F12'.
      PERFORM clear_9000.
      SET SCREEN 0. LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  CLEAR_9000
*&---------------------------------------------------------------------*
FORM clear_9000.

  IF go_alv IS NOT INITIAL.
    go_alv->free( ).
  ENDIF.
  IF go_container IS NOT INITIAL.
    go_container->free( ).
  ENDIF.
  IF go_split IS NOT INITIAL.
    go_split->free( ).
  ENDIF.

  CLEAR: go_split, go_container, go_alv.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ALV_9000
*&---------------------------------------------------------------------*
FORM alv_9000 .

  DATA: ls_stable TYPE lvc_s_stbl.

  IF go_alv IS INITIAL.
    " Splitter de 1x2 para capturar pantalla completa
    CREATE OBJECT go_split
      EXPORTING
        metric            = '0001'
        parent            = cl_gui_container=>default_screen
        rows              = 1
        columns           = 1
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.

    go_container = go_split->get_container( row = 1 column = 1 ).

    CREATE OBJECT go_alv
      EXPORTING
        i_parent = go_container.

    gs_variant-report  = sy-repid.
    gs_layout-sel_mode = 'A'.

    IF p_sfli IS NOT INITIAL.

      PERFORM catalogo USING 'SFLIGHT'.
      CALL METHOD go_alv->set_table_for_first_display
        EXPORTING
          is_layout       = gs_layout
          is_variant      = gs_variant
          i_save          = 'A'
        CHANGING
          it_outtab       = gt_sflight[]
          it_fieldcatalog = gt_fieldcat.

    ELSEIF p_sbook IS NOT INITIAL.
      PERFORM catalogo USING 'SBOOK'.
      CALL METHOD go_alv->set_table_for_first_display
        EXPORTING
          is_layout       = gs_layout
          is_variant      = gs_variant
          i_save          = 'A'
        CHANGING
          it_outtab       = gt_sbook[]
          it_fieldcatalog = gt_fieldcat.

    ENDIF.

  ELSE.
    " Refrescar ALV y mantener posición barras desplazamiento
    ls_stable-row = 'X'.
    ls_stable-col = 'X'.

    go_alv->refresh_table_display(
      EXPORTING
        is_stable      = ls_stable
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2 ).

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  CATALOGO
*&---------------------------------------------------------------------*
FORM catalogo USING uv_tabname TYPE tabname.

  CLEAR: gt_fieldcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = uv_tabname
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9100 OUTPUT.
  SET PF-STATUS 'BASICO'.
  SET TITLEBAR 'TITLE_XML' WITH gv_title.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  XML_9100  OUTPUT
*&---------------------------------------------------------------------*
MODULE xml_9100 OUTPUT.

  IF go_split_xml IS INITIAL.
    " Splitter de 1x2 para capturar pantalla completa
    CREATE OBJECT go_split_xml
      EXPORTING
        metric            = '0001'
        parent            = cl_gui_container=>default_screen
        rows              = 1
        columns           = 1
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.

    go_container_xml = go_split_xml->get_container( row = 1 column = 1 ).

    " Visualizar XML en Container FullScreen
    " TIP -> Si no se pasa un Container se visualiza como una ventana
    "        de diálogo.
    gi_abap_2_xml->show_xml( iv_title     = space
                             io_container = go_container_xml ).

  ENDIF.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9100 INPUT.

  CASE sy-ucomm.
    WHEN '&F03'.
      SET SCREEN 0. LEAVE SCREEN.
      PERFORM clear_9100.
    WHEN '&F15' OR '&F12'.
      PERFORM clear_9100.
      SET SCREEN 0. LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  CLEAR_9100
*&---------------------------------------------------------------------*
FORM clear_9100.

  IF go_container_xml IS NOT INITIAL.
    go_container_xml->free( ).
  ENDIF.
  IF go_split_xml IS NOT INITIAL.
    go_split_xml->free( ).
  ENDIF.

  CLEAR: go_split_xml, go_container_xml.

ENDFORM.
