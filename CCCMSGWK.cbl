      ******************************************************************
      *                                                                *
      * CCCMSGWK - Check error messages for payroll run                *
      *                                                                *
      ******************************************************************
      ******************************************************************
      *                                                                *
      * Description: Working storage for selects for CCCMSGCK          *
      *                                                                *
      *                                                                *
      ******************************************************************



      /*****************************************************************
      *  CCC PNS 02/23/98  LOOK FOR ERROR MESSAGES FOR RETURN CODE     *
      ******************************************************************
       01  S-RTNCD.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'CCCMSGCK_S_MSG'.

           02  BIND-SETUP.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  RUN-ID              PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X       VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  SELECT-X            PIC X.
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      *  CCC PNS 02/23/98  LOOK FOR ERR MSGS FOR RETURN CODE OFF CYCLE *
      ******************************************************************
       01  S-RTNCDOFF.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'CCCMSGCK_S_MSGOFF'.

           02  BIND-SETUP.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC XX      VALUE ALL 'S'.
               03  FILLER              PIC XX      VALUE ALL 'M'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  COMPANY             PIC X(10).
               03  PAYGROUP            PIC X(10).
               03  PAY-END-DT          PIC X(10).
               03  PAGE-NO             PIC 9999                COMP.
               03  PAGE-NO-THRU        PIC 9999                COMP.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X       VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  SELECT-X            PIC X.
               03  FILLER              PIC X       VALUE 'Z'.



