*****************************************
* Leopold Hock | 05.08.2020 | https://www.leopoldhock.de
* This is a complete example for how an oData interface has to look on the ABAP backend side for a 1:1 Database Table/EntitySet solution.
* It is built for the SFLIGHT mock data set. oData Service/Entity/EntitySet based on SFLIGHT needs to be already implemented. This is just
* the working code for all the redefined methods of the oData service provider class.
*****************************************

*****************************************
* GET_ENTITYSET
*****************************************
  METHOD sflightset_get_entityset.
    SELECT * FROM sflight UP TO 20 ROWS INTO CORRESPONDING FIELDS OF TABLE et_entityset.
  ENDMETHOD.
  
*****************************************
* GET_ENTITY
*****************************************
  METHOD sflightset_get_entity.
    READ TABLE it_key_tab WITH KEY name = 'Carrid' ASSIGNING FIELD-SYMBOL(<carrid>).
    READ TABLE it_key_tab WITH KEY name = 'Connid' ASSIGNING FIELD-SYMBOL(<connid>).
    READ TABLE it_key_tab WITH KEY name = 'Fldate' ASSIGNING FIELD-SYMBOL(<fldate>).
    IF <carrid> IS ASSIGNED AND <connid> IS ASSIGNED AND <fldate> IS ASSIGNED.
      SELECT SINGLE * FROM sflight INTO CORRESPONDING FIELDS OF er_entity
        WHERE carrid = <carrid>-value
        AND connid = <connid>-value
        AND fldate = <fldate>-value.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception.
      endif.
    ELSE.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
    ENDIF.
  ENDMETHOD.

*****************************************
* UPDATE
*****************************************
  METHOD sflightset_update_entity.
    io_data_provider->read_entry_data( IMPORTING es_data = er_entity ).
    MODIFY sflight FROM er_entity.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ELSE.
      ROLLBACK WORK.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
    ENDIF.
  ENDMETHOD.

*****************************************
* CREATE
*****************************************
  METHOD sflightset_update_entity.
    io_data_provider->read_entry_data( IMPORTING es_data = er_entity ).
    MODIFY sflight FROM er_entity.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ELSE.
      ROLLBACK WORK.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
    ENDIF.
  ENDMETHOD.
  
*****************************************
* DELETE
*****************************************
  METHOD sflightset_delete_entity.
    READ TABLE it_key_tab WITH KEY name = 'Carrid' ASSIGNING FIELD-SYMBOL(<carrid>).
    READ TABLE it_key_tab WITH KEY name = 'Connid' ASSIGNING FIELD-SYMBOL(<connid>).
    READ TABLE it_key_tab WITH KEY name = 'Fldate' ASSIGNING FIELD-SYMBOL(<fldate>).
    IF <carrid> IS ASSIGNED AND <connid> IS ASSIGNED AND <fldate> IS ASSIGNED.
      DELETE FROM sflight WHERE carrid = <carrid>-value
              AND connid = <connid>-value
              AND fldate = <fldate>-value.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ELSE.
        ROLLBACK WORK.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception.
    ENDIF.
  ENDMETHOD.
