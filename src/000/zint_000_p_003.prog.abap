*&---------------------------------------------------------------------*
*& Report ZINT_000_P_003
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zint_000_p_003.

START-OF-SELECTION.
  DATA: lr_direction  TYPE RANGE OF zint_000_t_002-direction.
  DATA: lr_statu      TYPE RANGE OF zint_000_t_003-statu,
        lt_email_file TYPE TABLE OF zint_000_s_mail,
        lr_int        TYPE RANGE OF zint_000_de_istype,
        lt_log        TYPE TABLE OF zint_000_t_011,
        lv_datum(10)      type c,
        lv_uzeit(8)      type c.

  lr_direction = VALUE #( sign = 'I' option = 'EQ' ( low = 'I' )
                                                   ( low = 'O' ) ).

  lr_statu = VALUE #( sign = 'I' option = 'EQ' ( low = '12')
                                               ( low = '22')
                                               ( low = '23')
                                               ( low = '29') ).

  SELECT a~int_type , a~int_subtype , b~description , b~direction , a~guid , a~statu , a~datum , a~uzeit , a~username , b~rework , b~technical_name
    FROM zint_000_t_003 AS a INNER JOIN zint_000_t_002 AS b ON
    b~int_type EQ a~int_type AND
    b~int_subtype EQ a~int_subtype
    INNER JOIN zint_000_t_001 AS c ON
    c~int_type EQ a~int_type
    INTO TABLE @DATA(lt_003)
    WHERE a~int_type EQ '10' AND
          a~statu IN @lr_statu AND
          b~direction IN @lr_direction.

  CHECK lt_003 IS NOT INITIAL.
  SORT lt_003 BY datum DESCENDING uzeit DESCENDING.

  lr_int[] = VALUE #( FOR GROUPS <g_int> OF <wa> IN lt_003 GROUP BY ( int_subtype = <wa>-int_subtype )
                         ( sign = 'I' option = 'EQ' low = <g_int>-int_subtype )   ).

  SELECT lt5~*
  FROM zint_000_t_005 AS lt5
  INNER JOIN @lt_003 AS lt3 ON lt3~int_type    EQ lt5~int_type
                           AND lt3~int_subtype EQ lt5~int_subtype
                           AND lt3~guid        EQ lt5~guid
    WHERE lt5~ballog_number NE @space
      AND lt5~statu         IN @lr_statu
    INTO TABLE @DATA(lt_005).

  SORT lt_005 BY counter DESCENDING.

  SELECT *
    FROM zint_000_t_011
    INTO TABLE @DATA(lt_011).

  LOOP AT lr_int INTO DATA(ls_int).
    CLEAR: lt_email_file[].

    SELECT * FROM zint_000_t_mail
      WHERE int_subtype EQ @ls_int-low
      INTO TABLE @DATA(lt_user).

    DATA(lt_email_list) = VALUE yacf_tt_0006(
      FOR <ls_t001> IN lt_user (
*        full_name = ycl_acf_user=>get_full_name( <ls_t001>-kullanici_adi )
        email_address = ycl_acf_user=>get_email_address( <ls_t001>-kullanici_adi ) ) ).

*<<< Ç.CEYLAN/A.KOC added on 20.09.22 FOR 9002556584 US-Entegrasyon hataları begin
      LOOP AT lt_user INTO DATA(ls_user) WHERE int_subtype EQ ls_int-low.
        IF ls_user-kullanici_adi EQ space.
        APPEND INITIAL LINE TO lt_email_list  ASSIGNING FIELD-SYMBOL(<lfs_email_list>).
           <lfs_email_list>-email_address = ls_user-email.
        ENDIF.
      ENDLOOP.
* <<< Ç.CEYLAN/A.KOC added on 20.09.22 FOR 9002556584 US-Entegrasyon hataları end

      SORT lt_email_list.                                      "" Doğa Almira 27.09.2022
      DELETE lt_email_list WHERE email_address IS INITIAL.


    LOOP AT lt_005 INTO DATA(ls_005) WHERE int_subtype EQ ls_int-low.
      READ TABLE lt_011 INTO DATA(ls_011) WITH KEY int_type      = ls_005-int_type
                                                   int_subtype   = ls_005-int_subtype
                                                   guid          = ls_005-guid
                                                   counter       = ls_005-counter
                                                   ballog_number = ls_005-ballog_number.
*      IF sy-subrc EQ 0. "" Doğa Almira 15.08.2022
*        CONTINUE.
*      ENDIF.
        CONCATENATE ls_005-datum+6(2)
                    ls_005-datum+4(2)
                    ls_005-datum+0(4)
                    into lv_datum SEPARATED BY '.'.
        CONCATENATE ls_005-uzeit+0(2)
                    ls_005-uzeit+2(2)
                    ls_005-uzeit+4(2)
                    into lv_uzeit SEPARATED BY ':'.

      APPEND INITIAL LINE TO lt_log ASSIGNING FIELD-SYMBOL(<fs_log>).
      <fs_log> = CORRESPONDING #( ls_005 ).

      zint_000_cl_bal_log=>read_ballog(
        EXPORTING iv_ballog_number = ls_005-ballog_number
        IMPORTING et_messages = DATA(lt_messages) ).

      SORT lt_messages.

      DELETE lt_messages WHERE type NE 'E'.

      DELETE ADJACENT DUPLICATES FROM lt_messages COMPARING ALL FIELDS.

      LOOP AT lt_messages INTO DATA(ls_message).
        lt_email_file = VALUE #( BASE lt_email_file (
          int_subtype  = ls_005-int_subtype
          guid         = ls_005-guid
          message      = ls_message-message
          datum        = lv_datum
          uzeit        = lv_uzeit
        ) ).

      ENDLOOP.
    ENDLOOP.

    IF lt_email_file[] IS INITIAL.
      CONTINUE.
    ENDIF.

    DATA(lo_email) = NEW ycl_acf_email( ).

    lo_email->set_sender( i_full_name = zcl_mm_031_sas=>c_sender_name
                          i_email_address = zcl_mm_031_sas=>c_sender_email ).

    lo_email->set_recipient_list( i_email_list = lt_email_list ).

    DATA(ls_excel_attachment) = lo_email->set_attachment_excel( i_data = lt_email_file i_title = 'entegrasyon-hata-mesajlari.xlsx' ).
    lo_email->set_attachment( ls_excel_attachment ).

    DATA(lv_subject) = VALUE text60( ).
    lv_subject = |TR-US Entegrasyon Hataları Hk.|.
    lo_email->set_subject( lv_subject ).

    DATA(lv_body) = VALUE text255( ).
    lv_body = |Sayın Yetkili, { cl_abap_char_utilities=>newline }{ cl_abap_char_utilities=>newline }|
         && | TR-US entegrasyonu hatalı guid bilgileri paylaşılmıştır.{ cl_abap_char_utilities=>newline }|
         && |Kontrol ederek gerekli aksiyonların alınmasını rica ederiz. { cl_abap_char_utilities=>newline }{ cl_abap_char_utilities=>newline }|.
    lo_email->set_body( lv_body ).

    lv_body = |Bu eposta MAVİ sistemi tarafından otomatik olarak gönderilmiştir. Lütfen cevap vermeyiniz.  { cl_abap_char_utilities=>newline }|
           && |Saygılarımızla. { cl_abap_char_utilities=>newline }|.
    lo_email->set_body( lv_body ).

    "emaili gönder
    lo_email->send( EXCEPTIONS error = 1 ).

    lo_email->free( ).
  ENDLOOP.

  IF lt_log[] IS NOT INITIAL.
    MODIFY zint_000_t_011 FROM TABLE lt_log.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.
