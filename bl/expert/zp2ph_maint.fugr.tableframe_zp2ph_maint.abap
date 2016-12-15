*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZP2PH_MAINT
*   generation date: 13.12.2016 at 19:23:59
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZP2PH_MAINT        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.