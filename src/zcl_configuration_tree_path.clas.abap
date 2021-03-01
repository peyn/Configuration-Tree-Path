CLASS zcl_configuration_tree_path DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS ac_command_copy TYPE syst_ucomm VALUE 'ZZPATHCOPY' ##NO_TEXT.
    CONSTANTS ac_command_paste TYPE syst_ucomm VALUE 'ZZPATHPASTE' ##NO_TEXT.
    CONSTANTS ac_language_english TYPE spras VALUE 'E' ##NO_TEXT.
    CONSTANTS ac_language_german TYPE spras VALUE 'D' ##NO_TEXT.
    CONSTANTS ac_user_parameter_separator TYPE memoryid VALUE 'ZCFG_PATH_SEPARATOR' ##NO_TEXT.

    CLASS-METHODS handle_command
      IMPORTING
        !iv_command            TYPE syst_ucomm
        !iv_language           TYPE syst_langu
        !it_nodes              TYPE hier_iface_t
        !it_all_nodes          TYPE hier_iface_t
        !it_all_texts          TYPE hier_texts_t
      CHANGING
        !cv_execute_command    TYPE flag
        !cv_update_actual_node TYPE flag
        !cs_actual_node        TYPE hier_iface .
    CLASS-METHODS modify_context_menu
      CHANGING
        !ct_menu TYPE streecntxt_tab .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      gty_paths TYPE TABLE OF text4096 .

    CLASS-METHODS get_default_separator
      RETURNING
        VALUE(rv_separator) TYPE string .
    CLASS-METHODS get_possible_separators
      RETURNING
        VALUE(rt_separators) TYPE aco_tt_string .
    CLASS-METHODS handle_command_copy
      IMPORTING
        !iv_command   TYPE syst_ucomm
        !iv_language  TYPE syst_langu
        !it_nodes     TYPE hier_iface_t
        !it_all_nodes TYPE hier_iface_t
        !it_all_texts TYPE hier_texts_t .
    CLASS-METHODS handle_command_paste
      IMPORTING
        !iv_command            TYPE syst_ucomm
        !iv_language           TYPE syst_langu
        !it_nodes              TYPE hier_iface_t OPTIONAL
        !it_all_nodes          TYPE hier_iface_t
        !it_all_texts          TYPE hier_texts_t
      CHANGING
        !cv_update_actual_node TYPE flag
        !cs_actual_node        TYPE hier_iface .
    CLASS-METHODS handle_command_paste_do
      IMPORTING
        !iv_current_path TYPE i DEFAULT 0
        !iv_language     TYPE spras
        !it_paths        TYPE gty_paths
        !it_nodes        TYPE shi_all_nodes
        !it_texts        TYPE hier_texts_t
        !io_tree         TYPE REF TO cl_stree_data
      CHANGING
        !cs_parent       TYPE hier_iface OPTIONAL .
ENDCLASS.



CLASS ZCL_CONFIGURATION_TREE_PATH IMPLEMENTATION.


  METHOD get_default_separator.
    DATA:
      lt_parameters TYPE ustyp_t_parameters.

    CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
      EXPORTING
        user_name           = sy-uname
      TABLES
        user_parameters     = lt_parameters
      EXCEPTIONS
        user_name_not_exist = 1
        OTHERS              = 2.
    IF sy-subrc = 0.
      READ TABLE lt_parameters REFERENCE INTO DATA(ld_parameter)
        WITH KEY parid = ac_user_parameter_separator.
      IF sy-subrc = 0 AND ld_parameter->parva IS NOT INITIAL.
        rv_separator = ld_parameter->parva.
        RETURN.
      ENDIF.
    ENDIF.

    DATA(lt_separators) = get_possible_separators( ).
    READ TABLE lt_separators INTO rv_separator INDEX 1.
  ENDMETHOD.


  METHOD get_possible_separators.
    APPEND |>| TO rt_separators.
    APPEND |»| TO rt_separators.
    APPEND |→| TO rt_separators.
    APPEND |->| TO rt_separators.
  ENDMETHOD.


  METHOD handle_command.
    CASE iv_command.
      WHEN ac_command_copy.
        CALL METHOD handle_command_copy
          EXPORTING
            iv_command   = iv_command
            iv_language  = iv_language
            it_nodes     = it_nodes
            it_all_nodes = it_all_nodes
            it_all_texts = it_all_texts.

        cv_execute_command = abap_false.
      WHEN ac_command_paste.
        CALL METHOD handle_command_paste
          EXPORTING
            iv_command            = iv_command
            iv_language           = iv_language
            it_nodes              = it_nodes
            it_all_nodes          = it_all_nodes
            it_all_texts          = it_all_texts
          CHANGING
            cv_update_actual_node = cv_update_actual_node
            cs_actual_node        = cs_actual_node.

        cv_execute_command = abap_false.
    ENDCASE.
  ENDMETHOD.


  METHOD handle_command_copy.
    DATA:
      lv_rc    TYPE i,
      lv_path  TYPE text4096,
      lv_text  TYPE text4096,
      ls_node  TYPE hier_iface,
      lt_paths TYPE TABLE OF text4096.

    CHECK iv_command = ac_command_copy.

    READ TABLE it_nodes INTO ls_node INDEX 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lv_separator) = get_default_separator( ).

    DO.
      lv_text = |NODE { ls_node-node_id } DESCRIPTION MISSING|.
      DO 4 TIMES.
        CASE sy-index.
          WHEN 1.
            DATA(lv_language) = iv_language.
          WHEN 2.
            CHECK lv_language <> sy-langu.
            lv_language = sy-langu.
          WHEN 3.
            CHECK lv_language <> ac_language_english.
            lv_language = ac_language_english.
          WHEN 4.
            CHECK lv_language <> ac_language_german.
            lv_language = ac_language_german.
        ENDCASE.

        READ TABLE it_all_texts REFERENCE INTO DATA(ld_text)
          WITH KEY spras = lv_language node_id = ls_node-node_id.
        IF sy-subrc = 0.
          EXIT.
        ENDIF.

        CLEAR ld_text.
      ENDDO.
      IF ld_text IS BOUND.
        lv_text = ld_text->text.
      ENDIF.

      IF lv_path IS INITIAL.
        lv_path = lv_text.
      ELSE.
        lv_path = |{ lv_text } { lv_separator } { lv_path }|.
      ENDIF.

      IF ls_node-parent_key IS INITIAL.
        EXIT.
      ENDIF.

      READ TABLE it_all_nodes INTO ls_node
        WITH KEY node_key = ls_node-parent_key.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
    ENDDO.

    APPEND lv_path TO lt_paths.
    CALL METHOD cl_gui_frontend_services=>clipboard_export
      IMPORTING
        data                 = lt_paths
      CHANGING
        rc                   = lv_rc
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        no_authority         = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    " 1&2&3&4
    MESSAGE s000(0q) WITH 'Path copied successfully'(t01).
  ENDMETHOD.


  METHOD handle_command_paste.
    DATA:
      ls_parent TYPE hier_iface,
      lt_paths  TYPE gty_paths.

    FIELD-SYMBOLS:
      <lo_tree> TYPE REF TO cl_stree_data.

    CHECK iv_command = ac_command_paste.

    CALL METHOD cl_gui_frontend_services=>clipboard_import
      IMPORTING
        data                 = lt_paths
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    READ TABLE lt_paths INTO DATA(ls_path) INDEX 1.
    IF sy-subrc <> 0.
      " 1&2&3&4
      MESSAGE s000(0q) WITH 'Nothing found in clipboard'(t02).
      RETURN.
    ENDIF.

    LOOP AT get_possible_separators( ) INTO DATA(lv_separator).
      SPLIT ls_path AT lv_separator INTO TABLE lt_paths IN CHARACTER MODE.
      CHECK lines( lt_paths ) > 1.
      EXIT.
    ENDLOOP.
    IF lines( lt_paths ) = 1.
      " 1&2&3&4
      MESSAGE s000(0q) WITH 'Unable to split path'(t03).
      RETURN.
    ENDIF.

    ASSIGN ('(SAPLSHI01)G_TREE_DATA') TO <lo_tree>.
    IF sy-subrc <> 0.
      " 1&2&3&4
      MESSAGE s000(0q) WITH 'Unable to find tree'(t04).
      RETURN.
    ENDIF.

    DATA(lt_all_nodes) = <lo_tree>->all_nodes[].
    DATA(lt_all_texts) = <lo_tree>->all_texts[].

    CALL METHOD handle_command_paste_do
      EXPORTING
        iv_language = iv_language
        it_paths    = lt_paths
        it_nodes    = lt_all_nodes
        it_texts    = lt_all_texts
        io_tree     = <lo_tree>
      CHANGING
        cs_parent   = ls_parent.

    CHECK ls_parent IS NOT INITIAL.
    cv_update_actual_node = abap_true.
    cs_actual_node = ls_parent.

    " 1&2&3&4
    MESSAGE s000(0q) WITH 'Finished parsing path'(t05).
  ENDMETHOD.


  METHOD handle_command_paste_do.
    DATA:
      lv_path TYPE string.

    DATA(lv_current_path) = iv_current_path + 1.
    LOOP AT it_paths INTO lv_path
      FROM lv_current_path.

      DATA(lv_tabix) = sy-tabix.
      DATA(lv_found) = abap_false.

      SHIFT lv_path LEFT DELETING LEADING space.

      " 4 times because 4 language options
      DO 4 TIMES.
        CASE sy-index.
          WHEN 1.
            DATA(lv_language) = iv_language.
          WHEN 2.
            CHECK lv_language <> sy-langu.
            lv_language = sy-langu.
          WHEN 3.
            CHECK lv_language <> ac_language_english.
            lv_language = ac_language_english.
          WHEN 4.
            CHECK lv_language <> ac_language_german.
            lv_language = ac_language_german.
        ENDCASE.

        " search for node text in the list of texts
        LOOP AT it_texts REFERENCE INTO DATA(ld_text)
          WHERE spras = lv_language
            AND text = lv_path.

          " check if parent id of the node is correct
          READ TABLE it_nodes REFERENCE INTO DATA(ld_node)
            WITH KEY node_id = ld_text->node_id.
          IF sy-subrc <> 0 OR ld_node->parent_key <> cs_parent-node_key.

            " node not found or parent not correct - look for other node
            CONTINUE.
          ENDIF.

          lv_found = abap_true.

          " unmark the previously marked node
          IF cs_parent IS NOT INITIAL.
            CALL METHOD io_tree->tree->node_set_style
              EXPORTING
                node_key = cs_parent-node_key
                style    = cl_gui_column_tree=>style_default.
          ENDIF.

          cs_parent = ld_node->*.
          io_tree->node_key = ld_node->node_key.

          " mark the current node
          CALL METHOD io_tree->tree->node_set_style
            EXPORTING
              node_key = io_tree->node_key
              style    = cl_gui_column_tree=>style_emphasized.

          " last node was found - exit the logic
          IF lv_tabix = lines( it_paths ) OR ld_node->node_type = 'IMG'.
            RETURN.
          ENDIF.

          " check if child nodes are already read
          READ TABLE it_nodes TRANSPORTING NO FIELDS
            WITH KEY parent_key = ld_node->node_key.
          IF sy-subrc = 0.
            CALL METHOD io_tree->tree->expand_node
              EXPORTING
                node_key       = io_tree->node_key
                expand_subtree = abap_false.

            " Make sure expanded node is also visible
            CALL METHOD io_tree->tree->ensure_visible
              EXPORTING
                node_key = io_tree->node_key.
          ELSE.

            " expand the node and read its children
            PERFORM handle_expand_no_children
              IN PROGRAM saplshi01.

            DATA(lt_nodes) = io_tree->all_nodes[].
            DATA(lt_texts) = io_tree->all_texts[].

            " continue searching with the new nodes' information
            CALL METHOD handle_command_paste_do
              EXPORTING
                iv_current_path = lv_tabix
                iv_language     = iv_language
                it_paths        = it_paths
                it_nodes        = lt_nodes
                it_texts        = lt_texts
                io_tree         = io_tree
              CHANGING
                cs_parent       = cs_parent.

            RETURN.
          ENDIF.

          EXIT.
        ENDLOOP.

        CHECK lv_found = abap_true.
        EXIT.
      ENDDO.

      " node with text wasn't found
      " exit the LOOP and let the system set the last found node as current
      CHECK lv_found = abap_false.
      RETURN.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_context_menu.
    APPEND INITIAL LINE TO ct_menu REFERENCE INTO DATA(ld_menu).
    APPEND INITIAL LINE TO ct_menu REFERENCE INTO ld_menu.
    ld_menu->menu_fcode = ac_command_paste.
    ld_menu->menu_text = 'Paste Node Path'(m01).
    ld_menu->is_no_ref = abap_true.
    APPEND INITIAL LINE TO ct_menu REFERENCE INTO ld_menu.
    ld_menu->menu_fcode = ac_command_copy.
    ld_menu->menu_text = 'Copy Node''s Path'(m02).
    ld_menu->is_no_ref = abap_true.
  ENDMETHOD.
ENDCLASS.
