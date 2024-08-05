class ZINT_000_CL_BAL_LOG definition
  public
  final
  create public .

public section.

  data MV_HANDLE type BALLOGHNDL .
  data MT_LOGS type BAL_T_MSG .
  data MV_INT_TYPE type ZINT_000_DE_ITYPE .
  data MV_INT_SUBTYPE type ZINT_000_DE_ISTYPE .
  data MV_DIRECTION type ZINT_000_DE_DIRECTION .
  data MV_SEND type CHAR1 .

  methods LOG_CREATE
    importing
      value(IV_INT_TYPE) type ZINT_000_DE_ITYPE
      value(IV_INT_SUBTYPE) type ZINT_000_DE_ISTYPE
      value(IV_EXTERNAL_NUMBER) type BALNREXT optional
      value(IV_SEND) type CHAR1 optional
    returning
      value(RV_HANDLE) type BALLOGHNDL
    exceptions
      LOG_HEADER_INCONSISTENT
      LOGGING_ERROR .
  methods LOG_ADD
    importing
      value(IV_HANDLE) type BALLOGHNDL optional
      value(IS_MESSAGE) type BAL_S_MSG optional
      value(IT_MESSAGE) type BAL_T_MSG optional
    exceptions
      LOG_NOT_FOUND
      MSG_INCONSISTENT
      LOG_IS_FULL
      LOGGING_ERROR .
  methods LOG_SAVE
    importing
      value(IV_HANDLE) type BALLOGHNDL optional
      value(IV_GUID) type ZINT_000_DE_GUID optional
      value(IV_STATU) type ZINT_000_DE_STATU optional
      value(IV_COMMIT) type CHAR1 optional
      value(IV_UPDATE_TASK) type XFELD optional
      value(IV_SAVE_ALL) type XFELD optional
    exporting
      value(ES_LOGNO) type BAL_S_LGNM
      value(ET_LOGS) type BAL_T_MSG
    exceptions
      LOG_NOT_FOUND
      SAVE_NOT_ALLOWED
      NUMBERING_ERROR
      LOGGING_ERROR .
  methods LOG_DISPLAY
    importing
      value(IV_HANDLE) type BALLOGHNDL optional
    exceptions
      PROFILE_INCONSISTENT
      INTERNAL_ERROR
      NO_DATA_AVAILABLE
      NO_AUTHORITY
      LOGGING_ERROR .
  methods LOG_DELETE
    importing
      value(IV_HANDLE) type BALLOGHNDL optional
    exceptions
      LOG_NOT_FOUND
      LOGGING_ERROR .
  methods LOG_GET_ADDED_MESSAGES
    returning
      value(T_MSG) type BAL_T_MSG .
  methods LOG_REFRESH
    importing
      value(IV_HANDLE) type BALLOGHNDL optional .
  methods LOG_ERROR_FOUND
    returning
      value(RV_FOUND) type BOOLEAN .
  class-methods STATU_CHANGE
    importing
      value(IV_GUID) type ZINT_000_DE_GUID
      value(IV_STATU) type ZINT_000_DE_STATU
      value(IV_COMMIT) type CHAR1 default 'X'
      value(IV_BALLOG_ID) type BALLOGHNDL optional
      value(IV_BALLOG_NUMBER) type BALOGNR optional
      value(IV_INT_TYPE) type ZINT_000_DE_ITYPE optional
      value(IV_INT_SUBTYPE) type ZINT_000_DE_ISTYPE optional
      value(IV_SEND) type CHAR1 optional
    exporting
      value(ET_RETURN) type BAPIRET2_T .
  class-methods READ_BALLOG
    importing
      value(IV_BALLOG_NUMBER) type BALOGNR
    exporting
      value(ET_MESSAGES) type ZINT_000_TT_BALLOG_MESSAGE .
  class-methods GET_PARAMETER
    importing
      !IV_INT_TYPE type ZINT_000_DE_ITYPE
      !IV_PARAM type FIELDNAME
    exporting
      !EV_VALUE type CSEQUENCE
      !RT_VALUE type ANY TABLE .
protected section.
private section.
ENDCLASS.



CLASS ZINT_000_CL_BAL_LOG IMPLEMENTATION.


  METHOD get_parameter.

    DATA : lt_params TYPE TABLE OF zint_000_t_007.

    DATA : l_data     TYPE REF TO data.
    FIELD-SYMBOLS <lt_textvalue> TYPE STANDARD TABLE .
    FIELD-SYMBOLS <lv_sign>      TYPE any   .
    FIELD-SYMBOLS <lv_option>    TYPE any   .
    FIELD-SYMBOLS <lv_value>     TYPE any   .

    GET REFERENCE OF rt_value INTO l_data .
    ASSIGN l_data->* TO <lt_textvalue> .


    SELECT * FROM zint_000_t_007 INTO TABLE lt_params
      WHERE int_type = iv_int_type
        AND param    = iv_param.



    READ TABLE lt_params INTO DATA(ls_params) INDEX 1.
    ev_value = ls_params-value.



    LOOP AT lt_params INTO ls_params.

      IF ls_params-optty IS INITIAL.
        ls_params-optty = 'EQ'.
      ENDIF.
      IF ls_params-sign IS INITIAL.
        ls_params-sign = 'I'.
      ENDIF.

      APPEND INITIAL LINE TO <lt_textvalue> ASSIGNING FIELD-SYMBOL(<fs_value>).

      ASSIGN COMPONENT 'SIGN'   OF STRUCTURE <fs_value> TO <lv_sign>.
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <fs_value> TO <lv_option>.
      ASSIGN COMPONENT 'LOW'    OF STRUCTURE <fs_value> TO <lv_value>.

      CHECK sy-subrc EQ 0.

      <lv_sign>   = ls_params-sign.
      <lv_option> = ls_params-optty.
      <lv_value>  = ls_params-value.
    ENDLOOP.
  ENDMETHOD.


  METHOD log_add.

    IF iv_handle IS INITIAL.
      iv_handle = me->mv_handle.
    ENDIF.
***
    CALL FUNCTION 'BAL_GLB_MSG_DEFAULTS_SET'
      EXPORTING
        i_s_msg_defaults = VALUE bal_s_mdef( log_handle = iv_handle )
      EXCEPTIONS
        OTHERS           = 0.
    IF sy-subrc <> 0.
      RAISE logging_error.
    ENDIF.
***
    IF it_message[] IS INITIAL AND is_message IS NOT INITIAL.
      APPEND is_message TO it_message.
    ENDIF.
***
    LOOP AT it_message REFERENCE INTO DATA(lo_message).

      APPEND lo_message->* TO mt_logs.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = iv_handle
          i_s_msg          = lo_message->*
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.

      CASE sy-subrc.
        WHEN 1.
          RAISE log_not_found.
*
        WHEN 2.
          RAISE msg_inconsistent.
*
        WHEN 3.
          RAISE log_is_full.
*
        WHEN 4.
          RAISE logging_error.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD log_create.
    REFRESH mt_logs.
    SELECT SINGLE *
      FROM zint_000_t_002
      INTO @DATA(ls_002)
      WHERE int_type EQ @iv_int_type AND
            int_subtype EQ @iv_int_subtype.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'BAL_LOG_CREATE'
        EXPORTING
          i_s_log                 = VALUE bal_s_log( object = ls_002-balobj subobject = ls_002-balsubobj extnumber = iv_external_number )
        IMPORTING
          e_log_handle            = me->mv_handle
        EXCEPTIONS
          log_header_inconsistent = 1
          OTHERS                  = 2.

      IF sy-subrc <> 0.
        CASE sy-subrc.
          WHEN 1.
            RAISE log_header_inconsistent.
          WHEN OTHERS.
            RAISE logging_error.
        ENDCASE.
      ELSE.
        me->mv_int_type = iv_int_type.
        me->mv_int_subtype = iv_int_subtype.
        me->mv_direction = ls_002-direction.
        me->mv_send = iv_send.
        IF iv_send EQ 'X'.
          IF ls_002-direction EQ 'O'.
            log_add( is_message = VALUE #( msgty = 'S' msgid = 'ZINT_000_MC' msgno = '002' msgv1 = ls_002-description msgv2 = sy-uname msgv3 = sy-datum msgv4 = sy-uzeit ) ).
          ELSEIF ls_002-direction EQ 'I'.
            log_add( is_message = VALUE #( msgty = 'S' msgid = 'ZINT_000_MC' msgno = '004' msgv1 = ls_002-description msgv2 = sy-uname msgv3 = sy-datum msgv4 = sy-uzeit ) ).
          ENDIF.
        ENDIF.
      ENDIF.
*
      rv_handle = me->mv_handle.
    ENDIF.


  ENDMETHOD.


  method LOG_DELETE.

    IF iv_handle IS INITIAL.
      iv_handle = me->mv_handle.
    ENDIF.
***
    CALL FUNCTION 'BAL_LOG_DELETE'
      EXPORTING
        i_log_handle  = iv_handle
      EXCEPTIONS
        log_not_found = 1
        OTHERS        = 2.
    CASE sy-subrc .
      WHEN 1.
        RAISE log_not_found.
*
      WHEN 2.
        RAISE logging_error.
*
    ENDCASE.

  endmethod.


  method LOG_DISPLAY.

     DATA :  ls_display_profile TYPE bal_s_prof.
***
    IF iv_handle IS INITIAL.
      iv_handle = me->mv_handle.
    ENDIF.
***

* get a prepared profile
    CLEAR ls_display_profile.
    CALL FUNCTION 'BAL_DSP_PROFILE_STANDARD_GET'
      IMPORTING
        e_s_display_profile = ls_display_profile
      EXCEPTIONS
        OTHERS              = 0.
    IF sy-subrc <> 0.
      RAISE logging_error.
    ENDIF.

    ls_display_profile-show_all   = 'X'.
    ls_display_profile-title      = 'Günlükleri görüntüle'.
    ls_display_profile-pop_adjst  = 'X' .
    ls_display_profile-langu      = sy-langu .
    ls_display_profile-start_col  = 5   .
    ls_display_profile-start_row  = 5   .
    ls_display_profile-end_col    = 90  .
    ls_display_profile-end_row    = 30  .

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_t_log_handle       = VALUE bal_t_logh( ( iv_handle ) )
        i_s_display_profile  = ls_display_profile
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.
    CASE sy-subrc.
      WHEN 1.
        RAISE profile_inconsistent.
*
      WHEN 2.
        RAISE internal_error.
*
      WHEN 3.
        RAISE no_data_available.
*
      WHEN 4.
        RAISE no_authority.
*
      WHEN 5.
        RAISE logging_error.
*
    ENDCASE.

  endmethod.


  method LOG_ERROR_FOUND.
       CLEAR rv_found.
    LOOP AT mt_logs TRANSPORTING NO FIELDS WHERE msgty CA 'AEX'.
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0.
      rv_found = 'X'.
    ENDIF.
  endmethod.


  method LOG_GET_ADDED_MESSAGES.
     t_msg = mt_logs.
  endmethod.


  method LOG_REFRESH.
       IF iv_handle IS NOT INITIAL.

      CALL FUNCTION 'BAL_LOG_REFRESH'
        EXPORTING
          i_log_handle  = iv_handle
        EXCEPTIONS
          log_not_found = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

    ENDIF.
  endmethod.


  METHOD log_save.

    DATA: lv_msg_out TYPE char255.

    IF iv_handle IS INITIAL.
      iv_handle = me->mv_handle.
    ENDIF.
    DATA: lt_logno TYPE  bal_t_lgnm.
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_in_update_task = iv_update_task
        i_t_log_handle   = VALUE bal_t_logh( ( iv_handle ) )
        i_save_all       = iv_save_all
      IMPORTING
        e_new_lognumbers = lt_logno
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.
    DATA(lv_subrc) = sy-subrc.
    IF lv_subrc NE 0 AND sy-msgid IS NOT INITIAL AND sy-msgno IS NOT INITIAL.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = sy-msgid
          msgnr               = sy-msgno
          msgv1               = sy-msgv1
          msgv2               = sy-msgv2
          msgv3               = sy-msgv3
          msgv4               = sy-msgv4
        IMPORTING
          message_text_output = lv_msg_out.
    ENDIF.

    IF lt_logno IS NOT INITIAL.
      READ TABLE lt_logno INTO DATA(ls_logno) INDEX 1.
      es_logno = ls_logno.
    ENDIF.

    IF iv_guid IS NOT INITIAL AND me->mv_int_type IS NOT INITIAL AND me->mv_int_subtype IS NOT INITIAL.
      IF me->mv_send EQ 'X'.
        "İlk mesaj kaydıdır

        DATA: ls_log TYPE zint_000_t_003.

        SELECT SINGLE MAX( counter )
          FROM zint_000_t_003
          INTO @DATA(lv_counter)
          WHERE int_type EQ @me->mv_int_type AND
          int_subtype EQ @me->mv_int_subtype AND
          guid EQ @iv_guid.
        lv_counter = lv_counter + 1.

        ls_log = VALUE #(
        guid          = iv_guid
        counter       = lv_counter
        ballog_id     = iv_handle
        ballog_number = ls_logno-lognumber
        int_type      = me->mv_int_type
        int_subtype   = me->mv_int_subtype
        datum         = sy-datum
        uzeit         = sy-uzeit
        username      = sy-uname
        statu         = iv_statu ).

        IF iv_statu IS INITIAL AND me->mv_send EQ 'X'.
          IF me->mv_direction EQ 'O'.
            ls_log-statu = '10'.
          ELSEIF me->mv_direction EQ 'I'.
            ls_log-statu = '20'.
          ENDIF.
        ENDIF.
        MODIFY zint_000_t_003 FROM ls_log.

        statu_change(
        EXPORTING
          iv_guid          = iv_guid
          iv_statu         = ls_log-statu
          iv_commit        = ' '
          iv_ballog_id     = ls_log-ballog_id
          iv_ballog_number = ls_log-ballog_number
          iv_int_type      = me->mv_int_type
          iv_int_subtype   = me->mv_int_subtype
          iv_send          = me->mv_send
          ).
      ELSE.
        "Guid için işleme mesajıdır
        "11 ve 12 tipini işleyen programlarda çağırılan statu_change metodu kapatıldı ve buraya alındı.
        IF me->mv_int_type = '01' AND ( me->mv_int_subtype = '11' OR me->mv_int_subtype = '12' ).
          statu_change(
          EXPORTING
            iv_guid          = iv_guid
            iv_statu         = iv_statu
            iv_commit        = ' '
            iv_ballog_id     = iv_handle
            iv_ballog_number = ls_logno-lognumber
            iv_int_type      = me->mv_int_type
            iv_int_subtype   = me->mv_int_subtype
            iv_send          = me->mv_send
            ).
        ENDIF.
      ENDIF.
    ENDIF.

    CASE lv_subrc .
      WHEN 0.
      WHEN 1.
        RAISE log_not_found.
      WHEN 2.
*        RAISE save_not_allowed.
        et_logs = me->mt_logs[].
        DATA: lt_900   TYPE TABLE OF zint_000_t_900,
              ls_900   TYPE zint_000_t_900,
              lv_sayac TYPE int4.

        LOOP AT et_logs INTO DATA(ls_logs).
          lv_sayac = lv_sayac + 1.

          CLEAR ls_900.
          ls_900 = VALUE #(
            int_type    = me->mv_int_type
            int_subtype = me->mv_int_subtype
            guid        = iv_guid
            counter     = lv_sayac
            send        = me->mv_send
            type        = ls_logs-msgty
            id          = ls_logs-msgid
            m_number    = ls_logs-msgno
            message_v1  = ls_logs-msgv1
            message_v2  = ls_logs-msgv2
            message_v3  = ls_logs-msgv3
            message_v4  = ls_logs-msgv4
            ret_msg     = lv_msg_out
          ).

          APPEND ls_900 TO lt_900.
        ENDLOOP.
        IF sy-subrc EQ 0 AND lt_900 IS NOT INITIAL.
          MODIFY zint_000_t_900 FROM TABLE lt_900.
        ENDIF.
      WHEN 3.
        RAISE numbering_error.
      WHEN 4.
        RAISE logging_error.
    ENDCASE.

    IF iv_commit EQ abap_true.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD read_ballog.

    DATA: ls_return  TYPE bapiret2,
          lt_logkeys TYPE STANDARD TABLE OF bapialf,
          lt_logdata TYPE STANDARD TABLE OF bapialg.
    APPEND INITIAL LINE TO lt_logkeys REFERENCE INTO DATA(lr_logkey).
    lr_logkey->lognumber = iv_ballog_number.

    CALL FUNCTION 'BAPI_APPLICATIONLOG_GETDETAIL'
      EXPORTING
        language    = sy-langu
        textformat  = 'ASC'
        linkpattern = ' '
      IMPORTING
        return      = ls_return
      TABLES
        logkeys     = lt_logkeys
        logdata     = lt_logdata.

  DATA: ls_messages TYPE ZINT_000_S_BALLOG_MESSAGE.
  LOOP AT lt_logdata INTO DATA(ls_logdata).
    CLEAR: ls_messages.
    ls_messages = VALUE #(
      type = ls_logdata-type
      message = ls_logdata-message
    ).
    APPEND ls_messages TO et_messages.
  ENDLOOP.

  ENDMETHOD.


  METHOD statu_change.

*/  Quadroda çift geliyor statuyü bozuyor.
    IF iv_int_type    EQ '01' AND
       iv_int_subtype EQ '92' .
      SELECT SINGLE *
        FROM zint_000_t_003
        INTO @DATA(ls_003)
        BYPASSING BUFFER
        WHERE guid EQ @iv_guid
          AND int_type EQ @iv_int_type
          AND int_subtype EQ @iv_int_subtype.
      IF sy-subrc EQ 0 .
        DATA(lv_rc) = 0 .
      ENDIF.

    ELSE.
      SELECT SINGLE *
        FROM zint_000_t_003
        INTO @ls_003
        BYPASSING BUFFER
        WHERE guid EQ @iv_guid
          AND int_type EQ @iv_int_type
          AND int_subtype EQ @iv_int_subtype.
      IF sy-subrc EQ 0 .
        lv_rc = 0 .
      ENDIF.
    ENDIF.

    IF lv_rc = 0 AND iv_send EQ ' '.
      "Var olan kayıt üzerinde statü değişikliği yapılıyor.
*      IF iv_statu NE ls_003-statu.
      DATA: ls_log TYPE zint_000_t_005.
      "Statü değişikliği yapılıyor.
      SELECT SINGLE MAX( counter )
        FROM zint_000_t_005
        INTO @DATA(lv_counter)
        WHERE int_type EQ @ls_003-int_type AND
              int_subtype EQ @ls_003-int_subtype AND
              guid EQ @ls_003-guid.
      lv_counter = lv_counter + 1.
      ls_log = VALUE #(
        int_type = ls_003-int_type
        int_subtype = ls_003-int_subtype
        guid = ls_003-guid
        counter = lv_counter
        statu = iv_statu
        eski_statu = ls_003-statu
        datum = sy-datum
        uzeit = sy-uzeit
        username = sy-uname
        ballog_id = iv_ballog_id
        ballog_number = iv_ballog_number
        ).
      UPDATE zint_000_t_003
      SET statu = iv_statu
      WHERE int_type = ls_003-int_type AND
            int_subtype = ls_003-int_subtype AND
            guid = ls_003-guid.
      MODIFY zint_000_t_005 FROM ls_log.
      IF iv_commit EQ 'X'.
        COMMIT WORK.
      ENDIF.
*      ELSE.
*        APPEND INITIAL LINE TO et_return REFERENCE INTO DATA(lr_return).
*        lr_return->type = 'E'.
*      ENDIF.
    ELSEIF iv_send EQ 'X'.
      "İlk kayıt atılacaktır.
      ls_log = VALUE #(
          int_type = iv_int_type
          int_subtype = iv_int_subtype
          guid = iv_guid
          counter = '1'
          statu = iv_statu
          eski_statu = ' '
          datum = sy-datum
          uzeit = sy-uzeit
          username = sy-uname
          ballog_id = iv_ballog_id
          ballog_number = iv_ballog_number
          ).
      MODIFY zint_000_t_005 FROM ls_log.
      IF iv_commit EQ 'X'.
        COMMIT WORK.
      ENDIF.
    ELSE.
      "Hata
    ENDIF.

  ENDMETHOD.
ENDCLASS.
