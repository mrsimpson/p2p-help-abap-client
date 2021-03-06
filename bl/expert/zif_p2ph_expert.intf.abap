interface ZIF_P2PH_EXPERT
  public .


  interfaces /BOBF/IF_LIB_CONSTANTS .

  constants:
    BEGIN OF SC_ACTION,
      BEGIN OF AREA,
 CREATE_AREA                    TYPE /BOBF/ACT_KEY VALUE '14187758B6551ED6A6D3EAD7FE4D6BAC',
 DELETE_AREA                    TYPE /BOBF/ACT_KEY VALUE '14187758B6551ED6A6D3EAD7FE4DEBAC',
 SAVE_AREA                      TYPE /BOBF/ACT_KEY VALUE '14187758B6551ED6A6D3EAD7FE4E6BAC',
 UPDATE_AREA                    TYPE /BOBF/ACT_KEY VALUE '14187758B6551ED6A6D3EAD7FE4DABAC',
 VALIDATE_AREA                  TYPE /BOBF/ACT_KEY VALUE '14187758B6551ED6A6D3EAD7FE4E2BAC',
      END OF AREA,
      BEGIN OF ROOT,
 CREATE_ROOT                    TYPE /BOBF/ACT_KEY VALUE '14187758B6551ED6A6D3E85B86584BAA',
 DELETE_ROOT                    TYPE /BOBF/ACT_KEY VALUE '14187758B6551ED6A6D3E85B8658CBAA',
 LOCK_ROOT                      TYPE /BOBF/ACT_KEY VALUE '14187758B6551ED6A6D3E85B86578BAA',
 SAVE_ROOT                      TYPE /BOBF/ACT_KEY VALUE '14187758B6551ED6A6D3E85B86594BAA',
 UNLOCK_ROOT                    TYPE /BOBF/ACT_KEY VALUE '14187758B6551ED6A6D3E85B8657CBAA',
 UPDATE_ROOT                    TYPE /BOBF/ACT_KEY VALUE '14187758B6551ED6A6D3E85B86588BAA',
 VALIDATE_ROOT                  TYPE /BOBF/ACT_KEY VALUE '14187758B6551ED6A6D3E85B86590BAA',
      END OF ROOT,
    END OF SC_ACTION .
  constants:
    BEGIN OF SC_ACTION_ATTRIBUTE,
        BEGIN OF ROOT,
        BEGIN OF LOCK_ROOT,
 GENERIC                        TYPE STRING VALUE 'GENERIC',
 EDIT_MODE                      TYPE STRING VALUE 'EDIT_MODE',
 ALL_NONE                       TYPE STRING VALUE 'ALL_NONE',
 SCOPE                          TYPE STRING VALUE 'SCOPE',
 FORCE_INVALIDATION             TYPE STRING VALUE 'FORCE_INVALIDATION',
 LOCK_PARAMETER_BUFFER          TYPE STRING VALUE 'LOCK_PARAMETER_BUFFER',
        END OF LOCK_ROOT,
        BEGIN OF UNLOCK_ROOT,
 GENERIC                        TYPE STRING VALUE 'GENERIC',
 EDIT_MODE                      TYPE STRING VALUE 'EDIT_MODE',
 ALL_NONE                       TYPE STRING VALUE 'ALL_NONE',
 SCOPE                          TYPE STRING VALUE 'SCOPE',
 FORCE_INVALIDATION             TYPE STRING VALUE 'FORCE_INVALIDATION',
 LOCK_PARAMETER_BUFFER          TYPE STRING VALUE 'LOCK_PARAMETER_BUFFER',
        END OF UNLOCK_ROOT,
      END OF ROOT,
    END OF SC_ACTION_ATTRIBUTE .
  constants:
    BEGIN OF SC_ALTERNATIVE_KEY,
      BEGIN OF AREA,
 AREA                           TYPE /BOBF/OBM_ALTKEY_KEY VALUE '14187758B6551EE6A8CE28ADE8E3B2CB',
      END OF AREA,
      BEGIN OF ROOT,
 UNAME                          TYPE /BOBF/OBM_ALTKEY_KEY VALUE '14187758B6551ED6A6DB089D83AEDD2E',
      END OF ROOT,
    END OF SC_ALTERNATIVE_KEY .
  constants:
    BEGIN OF SC_ASSOCIATION,
      BEGIN OF AREA,
 MESSAGE                        TYPE /BOBF/OBM_ASSOC_KEY VALUE '14187758B6551ED6A6D3EAD7FE4D0BAC',
 PROPERTY                       TYPE /BOBF/OBM_ASSOC_KEY VALUE '14187758B6551ED6A6D3EAD7FE4D4BAC',
 TO_PARENT                      TYPE /BOBF/OBM_ASSOC_KEY VALUE '14187758B6551ED6A6D3EA47EDCF0BAB',
 TO_ROOT                        TYPE /BOBF/OBM_ASSOC_KEY VALUE '14187758B6551ED6A6D3EA47EDCF2BAB',
      END OF AREA,
      BEGIN OF AREA_MESSAGE,
 TO_PARENT                      TYPE /BOBF/OBM_ASSOC_KEY VALUE '14187758B6551ED6A6D3EAD7FE4EABAC',
      END OF AREA_MESSAGE,
      BEGIN OF AREA_PROPERTY,
 TO_PARENT                      TYPE /BOBF/OBM_ASSOC_KEY VALUE '14187758B6551ED6A6D3EAD7FE4ECBAC',
      END OF AREA_PROPERTY,
      BEGIN OF ROOT,
 AREA                           TYPE /BOBF/OBM_ASSOC_KEY VALUE '14187758B6551ED6A6D3EA47EDCECBAB',
 LOCK                           TYPE /BOBF/OBM_ASSOC_KEY VALUE '14187758B6551ED6A6D3E85B86576BAA',
 MESSAGE                        TYPE /BOBF/OBM_ASSOC_KEY VALUE '14187758B6551ED6A6D3E85B86572BAA',
 PROPERTY                       TYPE /BOBF/OBM_ASSOC_KEY VALUE '14187758B6551ED6A6D3E85B86582BAA',
      END OF ROOT,
      BEGIN OF ROOT_LOCK,
 TO_PARENT                      TYPE /BOBF/OBM_ASSOC_KEY VALUE '14187758B6551ED6A6D3E85B8659ABAA',
      END OF ROOT_LOCK,
      BEGIN OF ROOT_MESSAGE,
 TO_PARENT                      TYPE /BOBF/OBM_ASSOC_KEY VALUE '14187758B6551ED6A6D3E85B86598BAA',
      END OF ROOT_MESSAGE,
      BEGIN OF ROOT_PROPERTY,
 TO_PARENT                      TYPE /BOBF/OBM_ASSOC_KEY VALUE '14187758B6551ED6A6D3E85B8659CBAA',
      END OF ROOT_PROPERTY,
    END OF SC_ASSOCIATION .
  constants:
    BEGIN OF SC_ASSOCIATION_ATTRIBUTE,
      BEGIN OF AREA,
        BEGIN OF PROPERTY,
 ALL_NODE_PROPERTY              TYPE STRING VALUE 'ALL_NODE_PROPERTY',
 ALL_NODE_ATTRIBUTE_PROPERTY    TYPE STRING VALUE 'ALL_NODE_ATTRIBUTE_PROPERTY',
 ALL_ASSOCIATION_PROPERTY       TYPE STRING VALUE 'ALL_ASSOCIATION_PROPERTY',
 ALL_ASSOCIATION_ATTRIBUTE_PROP TYPE STRING VALUE 'ALL_ASSOCIATION_ATTRIBUTE_PROP',
 ALL_ACTION_PROPERTY            TYPE STRING VALUE 'ALL_ACTION_PROPERTY',
 ALL_ACTION_ATTRIBUTE_PROPERTY  TYPE STRING VALUE 'ALL_ACTION_ATTRIBUTE_PROPERTY',
 ALL_QUERY_PROPERTY             TYPE STRING VALUE 'ALL_QUERY_PROPERTY',
 ALL_QUERY_ATTRIBUTE_PROPERTY   TYPE STRING VALUE 'ALL_QUERY_ATTRIBUTE_PROPERTY',
 ALL_SUBTREE_PROPERTY           TYPE STRING VALUE 'ALL_SUBTREE_PROPERTY',
        END OF PROPERTY,
      END OF AREA,
      BEGIN OF ROOT,
        BEGIN OF PROPERTY,
 ALL_NODE_PROPERTY              TYPE STRING VALUE 'ALL_NODE_PROPERTY',
 ALL_NODE_ATTRIBUTE_PROPERTY    TYPE STRING VALUE 'ALL_NODE_ATTRIBUTE_PROPERTY',
 ALL_ASSOCIATION_PROPERTY       TYPE STRING VALUE 'ALL_ASSOCIATION_PROPERTY',
 ALL_ASSOCIATION_ATTRIBUTE_PROP TYPE STRING VALUE 'ALL_ASSOCIATION_ATTRIBUTE_PROP',
 ALL_ACTION_PROPERTY            TYPE STRING VALUE 'ALL_ACTION_PROPERTY',
 ALL_ACTION_ATTRIBUTE_PROPERTY  TYPE STRING VALUE 'ALL_ACTION_ATTRIBUTE_PROPERTY',
 ALL_QUERY_PROPERTY             TYPE STRING VALUE 'ALL_QUERY_PROPERTY',
 ALL_QUERY_ATTRIBUTE_PROPERTY   TYPE STRING VALUE 'ALL_QUERY_ATTRIBUTE_PROPERTY',
 ALL_SUBTREE_PROPERTY           TYPE STRING VALUE 'ALL_SUBTREE_PROPERTY',
        END OF PROPERTY,
      END OF ROOT,
    END OF SC_ASSOCIATION_ATTRIBUTE .
  constants:
    SC_BO_KEY  TYPE /BOBF/OBM_BO_KEY VALUE '14187758B6551ED6A6D3E85B86568BAA' .
  constants:
    SC_BO_NAME TYPE /BOBF/OBM_NAME VALUE 'ZP2PH_EXPERT' .
  constants:
    BEGIN OF SC_DETERMINATION,
      BEGIN OF ROOT,
 DET_USER_DETAILS               TYPE /BOBF/DET_KEY VALUE '14187758B6551ED6A7F35EF6B605B05C',
      END OF ROOT,
    END OF SC_DETERMINATION .
  constants:
    SC_MODEL_VERSION TYPE /BOBF/CONF_VERSION VALUE '00000' .
  constants:
    BEGIN OF SC_NODE,
 AREA                           TYPE /BOBF/OBM_NODE_KEY VALUE '14187758B6551ED6A6D3EA47EDCEABAB',
 AREA_MESSAGE                   TYPE /BOBF/OBM_NODE_KEY VALUE '14187758B6551ED6A6D3EAD7FE4CEBAC',
 AREA_PROPERTY                  TYPE /BOBF/OBM_NODE_KEY VALUE '14187758B6551ED6A6D3EAD7FE4D2BAC',
 ROOT                           TYPE /BOBF/OBM_NODE_KEY VALUE '14187758B6551ED6A6D3E85B8656CBAA',
 ROOT_LOCK                      TYPE /BOBF/OBM_NODE_KEY VALUE '14187758B6551ED6A6D3E85B86574BAA',
 ROOT_MESSAGE                   TYPE /BOBF/OBM_NODE_KEY VALUE '14187758B6551ED6A6D3E85B86570BAA',
 ROOT_PROPERTY                  TYPE /BOBF/OBM_NODE_KEY VALUE '14187758B6551ED6A6D3E85B86580BAA',
    END OF SC_NODE .
  constants:
    BEGIN OF SC_NODE_ATTRIBUTE,
      BEGIN OF AREA,
  NODE_DATA                      TYPE STRING VALUE 'NODE_DATA',
  AREA                           TYPE STRING VALUE 'AREA',
  EXPERTISE                      TYPE STRING VALUE 'EXPERTISE',
      END OF AREA,
      BEGIN OF ROOT,
  NODE_DATA                      TYPE STRING VALUE 'NODE_DATA',
  UNAME                          TYPE STRING VALUE 'UNAME',
  AVAILABLE                      TYPE STRING VALUE 'AVAILABLE',
  TRANSIENT_NODE_DATA            TYPE STRING VALUE 'TRANSIENT_NODE_DATA',
  FULL_NAME                      TYPE STRING VALUE 'FULL_NAME',
  EMAIL_ADDRESS                  TYPE STRING VALUE 'EMAIL_ADDRESS',
      END OF ROOT,
    END OF SC_NODE_ATTRIBUTE .
  constants:
    BEGIN OF SC_NODE_CATEGORY,
      BEGIN OF AREA,
 AREA                           TYPE /BOBF/OBM_NODE_CAT_KEY VALUE '14187758B6551ED6A6D3EAD7FE4CCBAC',
      END OF AREA,
      BEGIN OF ROOT,
 ROOT                           TYPE /BOBF/OBM_NODE_CAT_KEY VALUE '14187758B6551ED6A6D3E85B8656EBAA',
      END OF ROOT,
    END OF SC_NODE_CATEGORY .
  constants:
    BEGIN OF SC_QUERY,
      BEGIN OF ROOT,
 SELECT_BY_AREA                 TYPE /BOBF/OBM_QUERY_KEY VALUE '14187758B6551ED6A6DB395B8CE77DA7',
 SELECT_BY_ELEMENTS             TYPE /BOBF/OBM_QUERY_KEY VALUE '14187758B6551ED6A6DB37C9D99D9DA7',
      END OF ROOT,
    END OF SC_QUERY .
  constants:
    BEGIN OF SC_QUERY_ATTRIBUTE,
      BEGIN OF ROOT,
        BEGIN OF SELECT_BY_AREA,
 VIEW_FIELDS                    TYPE STRING VALUE 'VIEW_FIELDS',
 MANDT                          TYPE STRING VALUE 'MANDT',
 DB_KEY                         TYPE STRING VALUE 'DB_KEY',
 AREA_KEY                       TYPE STRING VALUE 'AREA_KEY',
 UNAME                          TYPE STRING VALUE 'UNAME',
 AVAILABLE                      TYPE STRING VALUE 'AVAILABLE',
 AREA                           TYPE STRING VALUE 'AREA',
 EXPERTISE                      TYPE STRING VALUE 'EXPERTISE',
        END OF SELECT_BY_AREA,
        BEGIN OF SELECT_BY_ELEMENTS,
 UNAME                          TYPE STRING VALUE 'UNAME',
 AVAILABLE                      TYPE STRING VALUE 'AVAILABLE',
        END OF SELECT_BY_ELEMENTS,
      END OF ROOT,
    END OF SC_QUERY_ATTRIBUTE .
  constants:
    BEGIN OF SC_VALIDATION,
      BEGIN OF AREA,
 ALTERNATIVE_KEY                TYPE /BOBF/VAL_KEY VALUE '14187758B6551EE6A8CE274EFFD592C8',
 EXPERTISE_CODE_VALUES          TYPE /BOBF/VAL_KEY VALUE '14187758B6551EE6A8CD1F62F93F109F',
 EXPERTISE_LEVEL                TYPE /BOBF/VAL_KEY VALUE '14187758B6551EE6A8CE255DEE8692BC',
      END OF AREA,
      BEGIN OF ROOT,
 ALTERNATIVE_KEY                TYPE /BOBF/VAL_KEY VALUE '14187758B6551ED6A6DAF64A381D7CFA',
      END OF ROOT,
    END OF SC_VALIDATION .
endinterface.
