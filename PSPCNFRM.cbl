       IDENTIFICATION DIVISION.

       PROGRAM-ID. PSPCNFRM.

       ENVIRONMENT DIVISION.

      ******************************************************************
      *                                                                *
      *                                                                *
      *                                                                *
      *                                                                *
      * This software and related documentation are provided under a   *
      * license agreement containing restrictions on use and           *
      * disclosure and are protected by intellectual property          *
      * laws. Except as expressly permitted in your license agreement  *
      * or allowed by law, you may not use, copy, reproduce,           *
      * translate, broadcast, modify, license, transmit, distribute,   *
      * exhibit, perform, publish or display any part, in any form or  *
      * by any means. Reverse engineering, disassembly, or             *
      * decompilation of this software, unless required by law for     *
      * interoperability, is prohibited.                               *
      * The information contained herein is subject to change without  *
      * notice and is not warranted to be error-free. If you find any  *
      * errors, please report them to us in writing.                   *
      *                                                                *
      * Copyright (C) 1988, 2009, Oracle and/or its affiliates.        *
      * All Rights Reserved.                                           *
      *                                                                *
      ******************************************************************
      *                                                                *
      *          $Date:  2009/04/09:05:47:26                           *
      *       $Release:  HR91                                          *
      *      $Revision:  102                                           *
      *                                                                *
      ******************************************************************
      *  MODIFICATION LOG:                                             *
      *    02/23/98 PNS Check pay messages to properly update process  *
      *       scheduler status at completion of run.  Changes first    *
      *       put in to 5.1 now copied to 7.0                          *
      *    11/12/98 PNS Copied again to 7.01 code.  Changes are limited*
      *       to SD000-SET-RUN-STAT-SUCCESSFUL SECTION. and a section  *
      *       in working storage.                                      *
      *    12/14/98 PNS Copied again for Tax Update 98-E               *
      *    06/18/99 PNS And again for HR 7.02 App Update               *
      *    10/08/01 PNS And again for HR 8.01 Upgrade                  *
      *    06/01/06 PNS And again for HR 8.9 Upgrade                   *
      *    10/09/10 PNS And again for HR 9.1 Upgrade                   *
      ******************************************************************

      ******************************************************************
      *                                                                *
      *                   PROGRAM DESCRIPTION:                         *
      *                                                                *
      * RUN A PAYROLL CONFIRMATION.                                    *
      *                                                                *
      ******************************************************************


       DATA DIVISION.


       WORKING-STORAGE SECTION.


       01  PROGRAM-IDENTITY            PIC X(8)    VALUE 'PSPCNFRM'.


       01  W-WK.
           02  TIME-OUT                PIC 99B99B99/99.


       01  W-DSP.
           02  PAGE-NO                 PIC ZZZZ9.


       01  W-PASS.
           02  COUNTRY                 PIC XXX.


       01  W-SW.
           02  FETCH-CAL-SW            PIC X       VALUE SPACE.
               88  FETCH-CAL-END                   VALUE 'E'.


       01  W-NET-PARAM.
           02  RETURN-CD               PIC 99                  COMP.
               88  RETURN-CD-SUCCESS               VALUE 0.
               88  RETURN-CD-FAILURE               VALUE 99.


       01  W-PRC-INSTANCE.
           02  PROCESS-INSTANCE-ERRMSG PIC 9(10).


      /*****************************************************************
      *            PSPCKNBR - OPERATION TO PERFORM                     *
      ******************************************************************

       01  PSPCKNBR-L-PASS.
           02  OPERATION               PIC X.
               88  OP-VERIFY-FORMS     VALUE 'F'.
               88  OP-UPD-FORMS-TBL    VALUE 'U'.


      /*****************************************************************
      *            PAY_CONF_RUNCTL BUFFER AND STMT                     *
      ******************************************************************
       01  S-RUNCTL.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPCNFRM_S_RUNCTL'.

           02  BIND-SETUP.
               03  FILLER              PIC X(30)   VALUE ALL 'C'.
               03  FILLER              PIC X(30)   VALUE ALL 'H'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  OPRID               PIC X(30).
               03  BATCH-RUN-ID        PIC X(30).
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC XXXX    VALUE ALL 'I'.
               03  FILLER              PIC XXXX    VALUE ALL 'N'.
               03  FILLER              PIC X       VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  RUN-ID              PIC X(10).
               03  PROCESS-ON-OFF      PIC X.
                   88 PROCESS-ON-RUNID             VALUE 'O'.
                   88 PROCESS-OFF-RUNID            VALUE 'F'.
                   88 NO-RUNID                     VALUE 'N'.
               03  COMPANY             PIC X(10).
               03  PAYGROUP            PIC X(10).
               03  PAY-END-DT          PIC X(10).
               03  PAGE-NO             PIC 99999               COMP.
               03  PAGE-NO-THRU        PIC 99999               COMP.
               03  REVERSALS           PIC X.
                   88  REVERSALS-YES               VALUE 'Y'.
                   88  REVERSALS-NO                VALUE 'N'.

               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      *            PAY_CONF_RUNCTL SQL DELETE STMT                     *
      ******************************************************************
       01  D-RUNCTL.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPCNFRM_D_RUNCTL'.

           02  BIND-SETUP.
               03  FILLER              PIC X(30)   VALUE ALL 'C'.
               03  FILLER              PIC X(30)   VALUE ALL 'H'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  OPRID               PIC X(30).
               03  BATCH-RUN-ID        PIC X(30).
               03  FILLER              PIC X       VALUE 'Z'.

      /*****************************************************************
      *            PAY_CALENDAR BUFFER AND STMT                        *
      ******************************************************************
       01  S-CALOFF.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPCNFRM_S_CALOFF'.

           02  BIND-SETUP.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  COMPANY             PIC X(10).
               03  PAYGROUP            PIC X(10).
               03  PAY-END-DT          PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE ALL 'H'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  RUN-ID              PIC X(10).
               03  PAY-OFF-CYCLE-CAL   PIC X.
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      *            PAY_CHECK BUFFER AND STMT                           *
      ******************************************************************
       01  S-PAGE.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPCNFRM_S_PAGE'.

           02  BIND-SETUP.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC XXXX    VALUE ALL 'I'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  COMPANY             PIC X(10).
               03  PAYGROUP            PIC X(10).
               03  PAY-END-DT          PIC X(10).
               03  PAGE-NO             PIC 99999               COMP.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC XXXX    VALUE ALL 'I'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  PAGE-NO             PIC 99999               COMP.
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      *            PAY_CALENDAR BUFFER AND STMT                        *
      ******************************************************************
       01  S-CAL.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPCNFRM_S_CAL'.
           02  SQL-STMT-CONT           PIC X(18)   VALUE
                                                   'PSPCNFRM_S_CALCONT'.
           02  SQL-STMT-OFF2           PIC X(18)   VALUE
                                                   'PSPCNFRM_S_CALOFF2'.
           02  SQL-STMT-CNT2           PIC X(18)   VALUE
                                                   'PSPCNFRM_S_CALCNT2'.

           02  BIND-SETUP.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  PAYCONT             PIC X(10)   VALUE ALL 'Z'.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  RUN-ID              PIC X(10).
               03  COMPANY-GT          PIC X(10)   VALUE 'Z'.
               03  COMPANY             PIC X(10).
               03  PAYGROUP            PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC X       VALUE 'C'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  COMPANY             PIC X(10).
               03  PAYGROUP            PIC X(10).
               03  PAY-END-DT          PIC X(10).
               03  PAY-OFF-CYCLE-CAL   PIC X.
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      *            PAY_PAGE OFF CYCLE BUFFER AND STMT                  *
      ******************************************************************
       01  S-OFFPAGE.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPCNFRM_S_OFFPAGE'.

           02  BIND-SETUP.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC XXXX    VALUE ALL 'I'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  COMPANY             PIC X(10).
               03  PAYGROUP            PIC X(10).
               03  PAY-END-DT          PIC X(10).
               03  PAGE-NO             PIC 99999               COMP.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X       VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  SELECT-X            PIC X.
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      *            PAY_CHECK OFF CYCLE BUFFER AND STMT                 *
      ******************************************************************
       01  S-OFF.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPCNFRM_S_OFF'.

           02  BIND-SETUP.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  COMPANY             PIC X(10).
               03  PAYGROUP            PIC X(10).
               03  PAY-END-DT          PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X       VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  SELECT-X            PIC X.
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      *            PAY_CALENDAR SQL UPDATE STMT                        *
      ******************************************************************
       01  U-CALOFF.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPCNFRM_U_CALOFF'.

           02  BIND-SETUP.
               03  FILLER              PIC X       VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  PAY-OFF-CYCLE       PIC X.
                   88  PAY-OFF-CYCLE-YES           VALUE 'Y'.
                   88  PAY-OFF-CYCLE-NO            VALUE 'N'.

               03  COMPANY             PIC X(10).
               03  PAYGROUP            PIC X(10).
               03  PAY-END-DT          PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      *            PAY_CALENDAR SQL UPDATE STMT                        *
      ******************************************************************
       01  U-CAL.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPCNFRM_U_CAL'.

           02  BIND-SETUP.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  COMPANY             PIC X(10).
               03  PAYGROUP            PIC X(10).
               03  PAY-END-DT          PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      * CCC Check for Error Messages                                   *
      ******************************************************************

           COPY CCCMSGWK.

      /*****************************************************************
      *  FIND IF MESSAGES EXIST FOR PROCESS COMPLETED  BUFFER AND STMT *
      ******************************************************************
       01  S-CNT-MSG.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPCNFRM_S_CNT_MSG'.

           02  BIND-SETUP.
               03  FILLER              PIC XXXX    VALUE ALL 'I'.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  PROCESS-INSTANCE    PIC S9(9)               COMP.
               03  COMPANY             PIC X(10).
               03  PAYGROUP            PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-SETUP.
               03  FILLER              PIC X       VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  SELECT-X            PIC X.
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      *            RAPID ENTRY PAYSHEET DELETES: SQL BUFFER AND STMT   *
      ******************************************************************
       01  D-RPDET.
           02  SQL-STMT-DEL            PIC X(18)   VALUE
                                           'PSPCNFRM_D_RPDET'.

           02  SQL-STMT-DELINV         PIC X(18)   VALUE
                                           'PSPCNFRM_D_INVLD'.

           02  SQL-STMT-DELRUN         PIC X(18)   VALUE
                                           'PSPCNFRM_D_RPRUN'.

           02  SQL-STMT-DELHDR         PIC X(18)   VALUE
                                           'PSPCNFRM_D_RPHDR'.
           02  BIND-SETUP.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE ALL 'H'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  RUN-ID              PIC X(10).
               03  OFF-CYCLE           PIC X.
               03  FILLER              PIC X       VALUE 'Z'.



      /*****************************************************************
      *            NETWORK COMMUNICATION                               *
      ******************************************************************
       01  NETRT.                      COPY PTCNETRT.


      /*****************************************************************
      *            SQL COMMUNICATION                                   *
      ******************************************************************
       01  SQLRT.                      COPY PTCSQLRT.


      /*****************************************************************
      *            PAYROLL SELECTION                                   *
      ******************************************************************
       01  PSLCT.                      COPY PSCPSLCT.


      /*****************************************************************
      *            DEDUCTION ARRAY                                     *
      ******************************************************************
       01  DARRY.                      COPY PSCDARRY.


      /*****************************************************************
      *            PROCESS SCHEDULER REQUEST STATUS INTERFACE          *
      ******************************************************************
       01  USTAT.                      COPY PTCUSTAT.


      /*****************************************************************
      *            ADDITIONAL LIBRARY FUNCTION PARAMETERS              *
      ******************************************************************
       01  LIBWK.                      COPY PTCLIBWK.


      /*****************************************************************
      *                                                                *
       PROCEDURE DIVISION.
      *                                                                *
      ******************************************************************
      *                                                                *
       AA000-MAIN SECTION.
       AA000.
      *                                                                *
      ******************************************************************

           COPY PTCLIBFX.

           COPY PSCVERSN.

           COPY PTCNCHEK.

           SET PAYROLL-STEP-CONFIRM OF PSLCT  TO  TRUE
           PERFORM DA000-SELECT-RUNCTL
           ACCEPT TIME-OUT OF W-WK  FROM  TIME
           INSPECT TIME-OUT OF W-WK CONVERTING SPACE TO ':'
           INSPECT TIME-OUT OF W-WK CONVERTING '/' TO '.'

           IF OFF-CYCLE-YES OF PSLCT

               IF REVERSALS-YES OF S-RUNCTL

                   DISPLAY 'Reversal Confirmation started for Run: '
                           RUN-ID OF PSLCT
               ELSE
                   DISPLAY 'Off-Cycle Confirmation started for Run: '
                           RUN-ID OF PSLCT
               END-IF
           ELSE
               DISPLAY 'On-Cycle Confirmation started for Run: '
                       RUN-ID OF PSLCT
           END-IF

           DISPLAY ' at ' TIME-OUT OF W-WK
                   '.'
           PERFORM GA000-START-RUN

           IF NOT RTNCD-USER OF SQLRT

               IF NO-RUNID OF PSLCT

                   IF REVERSALS-YES OF S-RUNCTL

                       PERFORM MA000-PROCESS-CONFIRMATION
                               UNTIL FETCH-CAL-END OF W-SW
                   ELSE
                       PERFORM WITH TEST AFTER
                               VARYING PAGE-NO OF PSLCT
                                       FROM  PAGE-NO OF PSLCT  BY  1
                                   UNTIL PAGE-NO OF PSLCT
                                           =  PAGE-NO-THRU OF PSLCT

                           PERFORM JA000-LOOKUP-OFF-CYCLE-PAGE

                           IF RTNCD-END OF SQLRT

                               SET RTNCD-OK OF SQLRT  TO  TRUE
                           ELSE
                               PERFORM MA000-PROCESS-CONFIRMATION
                           END-IF
                       END-PERFORM
                   END-IF
               ELSE
                   PERFORM DM000-SELECT-PAY-CALENDAR
                   IF PROCESS-ON-RUNID OF PSLCT

                           MOVE SQL-STMT-CONT OF S-CAL
                                   TO  SQL-STMT OF S-CAL
                   ELSE
                           MOVE SQL-STMT-CNT2 OF S-CAL
                                   TO SQL-STMT OF S-CAL
                   END-IF
                   MOVE ALL 'H'  TO  PAYCONT OF BIND-SETUP OF S-CAL
                   PERFORM MA000-PROCESS-CONFIRMATION
                           UNTIL FETCH-CAL-END OF W-SW
               END-IF
           END-IF

           IF RTNCD-USER OF SQLRT
                   OR NO-RUNID OF PSLCT
                   OR REVERSALS-YES OF S-RUNCTL

               CONTINUE
           ELSE
               IF (PAY-OFF-CYCLE-CAL OF PSLCT = 'Y'
                       AND  OFF-CYCLE-YES OF PSLCT)
                       OR  (OFF-CYCLE-NO OF PSLCT)

                   PERFORM TA000-DELETE-RAPID-PAYSHEETS
               END-IF
           END-IF

           PERFORM SA000-TERM

           COPY PSCRTNCD.

           .
       MAIN-EXIT.
           STOP RUN.


      /*****************************************************************
      *                                                                *
       DA000-SELECT-RUNCTL SECTION.
       DA000.
      *                                                                *
      ******************************************************************

           CALL 'PTPSQLRT' USING   ACTION-CONNECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
           IF RTNCD-ERROR OF SQLRT

               MOVE 'SELECT-RUNCTL(CONNECT)'  TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           IF PROCESS-INSTANCE OF SQLRT  NOT =  ZERO

               PERFORM DB000-SET-RUN-STAT-PROCESSING
               MOVE PROCESS-INSTANCE OF SQLRT
                       TO  PROCESS-INSTANCE-ERRMSG OF PSLCT
           ELSE

               CALL 'PTPRUNID' USING   SQLRT
                                       PROCESS-INSTANCE-ERRMSG
                                               OF W-PRC-INSTANCE
               MOVE PROCESS-INSTANCE-ERRMSG OF W-PRC-INSTANCE
                       TO PROCESS-INSTANCE-ERRMSG OF PSLCT
           END-IF

           IF RUNNING-REMOTE-CALL OF NETRT

               PERFORM DA005-GET-RUNCTL-PARAM
           ELSE
               MOVE OPRID OF SQLRT  TO  OPRID OF S-RUNCTL
               MOVE BATCH-RUN-ID OF SQLRT  TO  BATCH-RUN-ID OF S-RUNCTL

               CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                       SQLRT
                                       SQL-CURSOR-COMMON OF SQLRT
                                       SQL-STMT OF S-RUNCTL
                                       BIND-SETUP OF S-RUNCTL
                                       BIND-DATA OF S-RUNCTL
                                       SELECT-SETUP OF S-RUNCTL
                                       SELECT-DATA OF S-RUNCTL
               IF RTNCD-ERROR OF SQLRT

                   MOVE 'SELECT-RUNCTL(SELECT)'
                           TO  ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF

               INITIALIZE SELECT-DATA OF S-RUNCTL

               CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                       SQLRT
                                       SQL-CURSOR-COMMON OF SQLRT
               IF RTNCD-ERROR OF SQLRT

                   IF RTNCD-END OF SQLRT

                       DISPLAY 'Confirmation Run Control Missing.'
                       DISPLAY ' for Operator ID  ' OPRID OF S-RUNCTL
                       DISPLAY ' and Batch Run ID '
                               BATCH-RUN-ID OF S-RUNCTL
                       SET RTNCD-USER-ERROR OF SQLRT  TO  TRUE
                       MOVE 'SELECT-RUNCTL'
                           TO ERR-SECTION OF SQLRT
                       PERFORM ZZ000-SQL-ERROR
                   ELSE
                       MOVE 'SELECT-RUNCTL(FETCH)'
                               TO  ERR-SECTION OF SQLRT
                       PERFORM ZZ000-SQL-ERROR
                   END-IF
               ELSE
                   PERFORM DD000-RUNCTL-ACCEPTED
               END-IF
           END-IF

           MOVE CORR SELECT-DATA OF S-RUNCTL  TO  PSLCT

           IF PAGE-NO-THRU OF PSLCT  =  ZERO

               MOVE PAGE-NO OF PSLCT  TO  PAGE-NO-THRU OF PSLCT
           END-IF

           IF RUN-ID OF PSLCT  =  SPACE

               SET NO-RUNID OF PSLCT  TO  TRUE
               SET OFF-CYCLE-YES OF PSLCT  TO  TRUE
               PERFORM DG000-SELECT-OFF-CALENDAR
               IF PAY-OFF-CYCLE-CAL OF S-CALOFF  =  'N'

                   MOVE 'N'  TO  PAY-OFF-CYCLE-CAL OF PSLCT
               ELSE
                   MOVE 'Y'  TO  PAY-OFF-CYCLE-CAL OF PSLCT
               END-IF

               IF REVERSALS-YES OF S-RUNCTL

                   PERFORM DJ000-SELECT-PAGE
               END-IF
           ELSE

               IF PROCESS-ON-RUNID OF PSLCT

                   SET OFF-CYCLE-NO OF PSLCT  TO  TRUE
               ELSE
                   SET OFF-CYCLE-YES OF PSLCT  TO  TRUE
                   MOVE SQL-STMT-OFF2 OF S-CAL
                           TO  SQL-STMT OF S-CAL
               END-IF

               MOVE RUN-ID OF PSLCT  TO  RUN-ID OF S-CAL
               PERFORM DM000-SELECT-PAY-CALENDAR

               IF PAY-OFF-CYCLE-CAL OF S-CAL  =  'Y'

                   MOVE 'Y'  TO  PAY-OFF-CYCLE-CAL OF PSLCT
               ELSE
                   MOVE 'N'  TO  PAY-OFF-CYCLE-CAL OF PSLCT
               END-IF
           END-IF

           .
       SELECT-RUNCTL-EXIT.


      /*****************************************************************
      *                                                                *
       DA005-GET-RUNCTL-PARAM SECTION.
       DA005.
      *                                                                *
      ******************************************************************

           MOVE 'RETURN_CD'  TO  DATA-NAME OF NETRT
           MOVE 2  TO  DATA-LEN OF NETRT
           SET TYPE-SMALLINT OF NETRT  TO  TRUE

           CALL 'PTPNETRT' USING   ACTION-GET OF NETRT
                                   NETRT
                                   RETURN-CD OF W-NET-PARAM
           IF NET-ERROR OF NETRT

               MOVE 'GET-RUNCTL-PARAM(RETURN_CD)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZP000-NET-ERROR
           END-IF

           MOVE 'COMPANY'  TO  DATA-NAME OF NETRT
           MOVE 10  TO  DATA-LEN OF NETRT
           SET TYPE-CHAR OF NETRT  TO  TRUE

           CALL 'PTPNETRT' USING   ACTION-GET OF NETRT
                                   NETRT
                                   COMPANY OF S-RUNCTL
           IF NET-ERROR OF NETRT

               MOVE 'GET-RUNCTL-PARAM(COMPANY)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZP000-NET-ERROR
           END-IF

           MOVE 'PAYGROUP'  TO  DATA-NAME OF NETRT
           MOVE 10  TO  DATA-LEN OF NETRT
           SET TYPE-CHAR OF NETRT  TO  TRUE

           CALL 'PTPNETRT' USING   ACTION-GET OF NETRT
                                   NETRT
                                   PAYGROUP OF S-RUNCTL
           IF NET-ERROR OF NETRT

               MOVE 'GET-RUNCTL-PARAM(PAYGROUP)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZP000-NET-ERROR
           END-IF

           MOVE 'PAY_END_DT'  TO  DATA-NAME OF NETRT
           MOVE 10  TO  DATA-LEN OF NETRT
           SET TYPE-DATE OF NETRT  TO  TRUE

           CALL 'PTPNETRT' USING   ACTION-GET OF NETRT
                                   NETRT
                                   PAY-END-DT OF S-RUNCTL
           IF NET-ERROR OF NETRT

               MOVE 'GET-RUNCTL-PARAM(PAY_END_DT)'
                       TO  ERR-SECTION OF SQLRT
                  PERFORM ZP000-NET-ERROR
           END-IF

           MOVE 'PAGE_NUM'  TO  DATA-NAME OF NETRT
           MOVE 4  TO  DATA-LEN OF NETRT
           SET TYPE-INT OF NETRT  TO  TRUE

           CALL 'PTPNETRT' USING   ACTION-GET OF NETRT
                                   NETRT
                                   PAGE-NO OF S-RUNCTL
           IF NET-ERROR OF NETRT

               MOVE 'GET-RUNCTL-PARAM(PAGE_NUM)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZP000-NET-ERROR
           END-IF

           MOVE 'LINE_NUM'  TO  DATA-NAME OF NETRT
           MOVE 2  TO  DATA-LEN OF NETRT
           SET TYPE-SMALLINT OF NETRT  TO  TRUE

           CALL 'PTPNETRT' USING   ACTION-GET OF NETRT
                                   NETRT
                                   LINE-NO OF PSLCT
           IF NET-ERROR OF NETRT

               MOVE 'GET-RUNCTL-PARAM(LINE_NUM)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZP000-NET-ERROR
           END-IF

           MOVE PAGE-NO OF S-RUNCTL TO PAGE-NO OF PSLCT

           MOVE SPACE  TO  RUN-ID OF SELECT-DATA OF S-RUNCTL
           MOVE ZERO  TO  PAGE-NO-THRU OF S-RUNCTL
           SET REVERSALS-NO OF SELECT-DATA OF S-RUNCTL  TO  TRUE

           .
       GET-RUNCTL-PARAM-EXIT.


      /*****************************************************************
      *                                                                *
       DB000-SET-RUN-STAT-PROCESSING SECTION.
       DB000.
      *                                                                *
      ******************************************************************

           INITIALIZE USTAT
           MOVE PROCESS-INSTANCE OF SQLRT  TO  PROCESS-INSTANCE OF USTAT
           SET RUN-STATUS-PROCESSING OF USTAT  TO  TRUE

           CALL 'PTPUSTAT' USING   SQLRT
                                   USTAT
           IF RTNCD-ERROR OF SQLRT

               MOVE 'SET-RUN-STAT-PROCESSING(PTPUSTAT)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           .
       SET-RUN-STAT-PROCESSING-EXIT.


      /*****************************************************************
      *                                                                *
       DD000-RUNCTL-ACCEPTED SECTION.
       DD000.
      *                                                                *
      ******************************************************************

           MOVE OPRID OF SQLRT  TO  OPRID OF D-RUNCTL
           MOVE BATCH-RUN-ID OF SQLRT  TO  BATCH-RUN-ID OF D-RUNCTL

           CALL 'PTPSQLRT' USING   ACTION-UPDATE OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF D-RUNCTL
                                   BIND-SETUP OF D-RUNCTL
                                   BIND-DATA OF D-RUNCTL
           IF RTNCD-ERROR OF SQLRT

               MOVE 'RUNCTL-ACCEPTED(DELETE)'  TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           CALL 'PTPSQLRT' USING   ACTION-COMMIT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
           IF RTNCD-ERROR OF SQLRT

               MOVE 'RUNCTL-ACCEPTED(COMMIT)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           .
       RUNCTL-ACCEPTED-EXIT.


      /*****************************************************************
      *                                                                *
       DG000-SELECT-OFF-CALENDAR SECTION.
       DG000.
      *                                                                *
      ******************************************************************

           MOVE CORR SELECT-DATA OF S-RUNCTL  TO  BIND-DATA OF S-CALOFF

           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF S-CALOFF
                                   BIND-SETUP OF S-CALOFF
                                   BIND-DATA OF S-CALOFF
                                   SELECT-SETUP OF S-CALOFF
                                   SELECT-DATA OF S-CALOFF
           IF RTNCD-ERROR OF SQLRT

               MOVE 'SELECT-OFF-CALENDAR(SELECT)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           INITIALIZE SELECT-DATA OF S-CALOFF

           CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
           IF RTNCD-ERROR OF SQLRT

               IF RTNCD-END OF SQLRT

                   DISPLAY 'Pay Calendar for Company: '
                           COMPANY OF S-RUNCTL
                   DISPLAY '                 Pay Group: '
                           PAYGROUP OF S-RUNCTL
                   DISPLAY '                 Pay End Date: '
                           PAY-END-DT OF S-RUNCTL
                   DISPLAY '  was not found.'
                   SET RTNCD-USER-ERROR OF SQLRT  TO  TRUE
                   PERFORM ZZ000-SQL-ERROR
               ELSE
                   MOVE 'SELECT-OFF-CALENDAR(FETCH)'
                           TO  ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF
           ELSE

               IF RUN-ID OF S-CALOFF  =  SPACE

                   DISPLAY 'Pay Calendar for Company: '
                           COMPANY OF S-RUNCTL
                   DISPLAY '                 Pay Group: '
                           PAYGROUP OF S-RUNCTL
                   DISPLAY '                 Pay End Date: '
                           PAY-END-DT OF S-RUNCTL
                   DISPLAY '  has no Run ID.'
                   SET RTNCD-USER-ERROR OF SQLRT  TO  TRUE
                   PERFORM ZZ000-SQL-ERROR
               ELSE
                   MOVE RUN-ID OF S-CALOFF  TO  RUN-ID OF PSLCT
               END-IF
           END-IF

           .
       SELECT-OFF-CALENDAR-EXIT.


      /*****************************************************************
      *                                                                *
       DJ000-SELECT-PAGE SECTION.
       DJ000.
      *                                                                *
      ******************************************************************

           MOVE CORR SELECT-DATA OF S-RUNCTL  TO  BIND-DATA OF S-PAGE
           MOVE PAGE-NO OF PSLCT  TO  PAGE-NO OF BIND-DATA OF S-PAGE

           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF S-PAGE
                                   BIND-SETUP OF S-PAGE
                                   BIND-DATA OF S-PAGE
                                   SELECT-SETUP OF S-PAGE
                                   SELECT-DATA OF S-PAGE
           IF RTNCD-ERROR OF SQLRT

               MOVE 'SELECT-PAGE(SELECT)'  TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           INITIALIZE SELECT-DATA OF S-PAGE

           CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
           IF RTNCD-ERROR OF SQLRT

               IF RTNCD-END OF SQLRT

                   SET RTNCD-OK  TO  TRUE
                   SET FETCH-CAL-END OF W-SW  TO  TRUE
               ELSE
                   MOVE 'SELECT-PAGE(FETCH)'  TO  ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF
           ELSE
               MOVE PAGE-NO OF SELECT-DATA OF S-PAGE
                       TO  PAGE-NO OF PSLCT
               MOVE PAGE-NO OF SELECT-DATA OF S-PAGE
                       TO  PAGE-NO-THRU OF PSLCT
           END-IF

           .
       SELECT-PAGE-EXIT.


      /*****************************************************************
      *                                                                *
       DM000-SELECT-PAY-CALENDAR SECTION.
       DM000.
      *                                                                *
      ******************************************************************

           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF S-CAL
                                   BIND-SETUP OF S-CAL
                                   BIND-DATA OF S-CAL
                                   SELECT-SETUP OF S-CAL
                                   SELECT-DATA OF S-CAL
           IF RTNCD-ERROR OF SQLRT

               MOVE 'SELECT-PAY-CALENDAR(SELECT)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           INITIALIZE SELECT-DATA OF S-CAL

           CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
           IF RTNCD-ERROR OF SQLRT

               IF RTNCD-END OF SQLRT

                   SET RTNCD-OK  TO  TRUE
                   SET FETCH-CAL-END OF W-SW  TO  TRUE
               ELSE
                   MOVE 'SELECT-PAY-CALENDAR(FETCH)'
                           TO  ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF
           ELSE
               MOVE CORR SELECT-DATA OF S-CAL  TO  PSLCT
           END-IF

           .
       SELECT-PAY-CALENDAR-EXIT.


      /*****************************************************************
      *                                                                *
       GA000-START-RUN SECTION.
       GA000.
      *                                                                *
      ******************************************************************

           CALL 'PSPSTRUN' USING   NETRT
                                   SQLRT
                                   PSLCT
           IF RTNCD-ERROR OF SQLRT

               MOVE 'START-RUN(PSPSTRUN)'  TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           .
       START-RUN-EXIT.


      /*****************************************************************
      *                                                                *
       JA000-LOOKUP-OFF-CYCLE-PAGE SECTION.
       JA000.
      *                                                                *
      ******************************************************************

           MOVE COMPANY OF PSLCT  TO  COMPANY OF S-OFFPAGE
           MOVE PAYGROUP OF PSLCT  TO  PAYGROUP OF S-OFFPAGE
           MOVE PAY-END-DT OF PSLCT  TO  PAY-END-DT OF S-OFFPAGE
           MOVE PAGE-NO OF PSLCT  TO  PAGE-NO OF S-OFFPAGE

           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF S-OFFPAGE
                                   BIND-SETUP OF S-OFFPAGE
                                   BIND-DATA OF S-OFFPAGE
                                   SELECT-SETUP OF S-OFFPAGE
                                   SELECT-DATA OF S-OFFPAGE
           IF RTNCD-ERROR OF SQLRT

               MOVE 'LOOKUP-OFF-CYCLE-PAGE(SELECT)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           INITIALIZE SELECT-DATA OF S-OFFPAGE

           CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
           IF RTNCD-ERROR OF SQLRT
                   AND NOT RTNCD-END OF SQLRT

               MOVE 'LOOKUP-OFF-CYCLE-PAGE(FETCH)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           .
       LOOKUP-OFF-CYCLE-PAGE-EXIT.


      /*****************************************************************
      *                                                                *
       MA000-PROCESS-CONFIRMATION SECTION.
       MA000.
      *                                                                *
      ******************************************************************

           ACCEPT TIME-OUT OF W-WK  FROM  TIME
           INSPECT TIME-OUT OF W-WK CONVERTING SPACE TO ':'
           INSPECT TIME-OUT OF W-WK CONVERTING '/' TO '.'
           DISPLAY 'Confirmation started for Company: '
                   COMPANY OF PSLCT
           DISPLAY '                         Pay Group: '
                   PAYGROUP OF PSLCT
           DISPLAY '                         Pay End Date: '
                   PAY-END-DT OF PSLCT

           IF NO-RUNID OF PSLCT

               MOVE PAGE-NO OF PSLCT  TO  PAGE-NO OF W-DSP
               DISPLAY '                         PAGE_NUM: '
                       PAGE-NO OF W-DSP
           END-IF

           DISPLAY ' at ' TIME-OUT OF W-WK
                   '.'

           CALL 'PSPPYWK1' USING   SQLRT
                                   PSLCT
                                   DARRY
                                   W-PASS
           IF RTNCD-ERROR OF SQLRT

               MOVE 'PROCESS-CONFIRMATION(PSPPYWK1)'
                        TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           IF RTNCD-OK OF SQLRT

               IF OFF-CYCLE-NO OF PSLCT

                  PERFORM MH000-VERIFY-CHECK-NBR
               END-IF
           END-IF

           IF NO-RUNID OF PSLCT

               PERFORM MD000-SELECT-OFF-CALENDAR
               PERFORM MD100-UPDATE-OFF-CALENDAR

               IF PAY-OFF-CYCLE-CAL OF PSLCT  =  'Y'

                   PERFORM MG000-UPDATE-PAY-CALENDAR

               END-IF

               IF REVERSALS-YES OF S-RUNCTL

                   PERFORM DJ000-SELECT-PAGE
               END-IF
           ELSE

               IF PROCESS-OFF-RUNID OF PSLCT
                       AND NOT RTNCD-USER OF SQLRT

                   SET PAY-OFF-CYCLE-NO OF U-CALOFF  TO  TRUE
                   PERFORM MD100-UPDATE-OFF-CALENDAR
               END-IF

               IF NOT RTNCD-USER OF SQLRT
                       AND  (PROCESS-ON-RUNID OF PSLCT
                           OR  (PROCESS-OFF-RUNID OF PSLCT
                           AND  PAY-OFF-CYCLE-CAL OF PSLCT  =  'Y'))

                   PERFORM MG000-UPDATE-PAY-CALENDAR
               END-IF

               MOVE COMPANY OF SELECT-DATA OF S-CAL
                       TO  COMPANY OF BIND-DATA OF S-CAL
               MOVE COMPANY OF SELECT-DATA OF S-CAL
                       TO  COMPANY-GT OF BIND-DATA OF S-CAL
               MOVE PAYGROUP OF SELECT-DATA OF S-CAL
                       TO  PAYGROUP OF BIND-DATA OF S-CAL
               PERFORM DM000-SELECT-PAY-CALENDAR
           END-IF

           PERFORM MJ000-COMMIT-CALENDAR


           .
       PROCESS-CONFIRMATION-EXIT.

      /*****************************************************************
      *                                                                *
       MD000-SELECT-OFF-CALENDAR SECTION.
       MD000.
      *                                                                *
      ******************************************************************

           MOVE CORR PSLCT  TO  BIND-DATA OF S-OFF

           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF S-OFF
                                   BIND-SETUP OF S-OFF
                                   BIND-DATA OF S-OFF
                                   SELECT-SETUP OF S-OFF
                                   SELECT-DATA OF S-OFF
           IF RTNCD-ERROR OF SQLRT

               MOVE 'UPDATE-OFF-CALENDAR(SELECT)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           INITIALIZE SELECT-DATA OF S-OFF

           CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
           IF RTNCD-ERROR OF SQLRT

               IF RTNCD-END OF SQLRT

                   SET RTNCD-OK  TO  TRUE
                   SET PAY-OFF-CYCLE-NO OF U-CALOFF  TO  TRUE
               ELSE
                   MOVE 'UPDATE-OFF-CALENDAR(FETCH)'
                           TO  ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF
           ELSE
               SET PAY-OFF-CYCLE-YES OF U-CALOFF  TO  TRUE
           END-IF

           .
       SELECT-OFF-CALENDAR-EXIT.


      /*****************************************************************
      *                                                                *
       MD100-UPDATE-OFF-CALENDAR SECTION.
       MD100.
      *                                                                *
      ******************************************************************

           MOVE CORR PSLCT  TO  BIND-DATA OF U-CALOFF

           CALL 'PTPSQLRT' USING   ACTION-UPDATE OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF U-CALOFF
                                   BIND-SETUP OF U-CALOFF
                                   BIND-DATA OF U-CALOFF
           IF RTNCD-ERROR OF SQLRT

               MOVE 'UPDATE-OFF-CALENDAR'  TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           .
       UPDATE-OFF-CALENDAR-EXIT.


      /*****************************************************************
      *                                                                *
       MG000-UPDATE-PAY-CALENDAR SECTION.
       MG000.
      *                                                                *
      ******************************************************************

           MOVE CORR PSLCT  TO  BIND-DATA OF U-CAL

           CALL 'PTPSQLRT' USING   ACTION-UPDATE OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF U-CAL
                                   BIND-SETUP OF U-CAL
                                   BIND-DATA OF U-CAL
           IF RTNCD-ERROR OF SQLRT

               MOVE 'UPDATE-PAY-CALENDAR'  TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           .
       UPDATE-PAY-CALENDAR-EXIT.


      /*****************************************************************
      *                                                                *
       MH000-VERIFY-CHECK-NBR SECTION.
       MH000.
      *                                                                *
      ******************************************************************

           SET OP-UPD-FORMS-TBL OF PSPCKNBR-L-PASS TO TRUE

           CALL 'PSPCKNBR' USING   SQLRT
                                   PSLCT
                                   PSPCKNBR-L-PASS

           IF RTNCD-ERROR OF SQLRT

               MOVE 'VERIFY-CHECK-NBR'  TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           .
       VERIFY-CHECK-NBR-EXIT.



      /*****************************************************************
      *                                                                *
       MJ000-COMMIT-CALENDAR SECTION.
       MJ000.
      *                                                                *
      ******************************************************************

           CALL 'PTPSQLRT' USING   ACTION-COMMIT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
           IF RTNCD-ERROR OF SQLRT

               MOVE 'COMMIT-CALENDAR'  TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           .
       COMMIT-CALENDAR-EXIT.


      /*****************************************************************
      *                                                                *
       SA000-TERM SECTION.
       SA000.
      *                                                                *
      ******************************************************************

           IF PROCESS-INSTANCE OF SQLRT  NOT =  ZERO

               PERFORM SD000-SET-RUN-STAT-SUCCESSFUL
           END-IF

           IF RUNNING-REMOTE-CALL OF NETRT

               PERFORM SA050-FIND-ERR-MSG-FND
               PERFORM SA200-RET-SUCCESS
           END-IF

           CALL 'PTPSQLRT' USING   ACTION-DISCONNECT-ALL OF SQLRT
                                   SQLRT
           IF RTNCD-ERROR OF SQLRT

               MOVE 'TERM'  TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           IF RUNNING-REMOTE-CALL OF NETRT

               PERFORM ZZ100-NET-TERMINATE
           END-IF

           ACCEPT TIME-OUT OF W-WK  FROM  TIME
           INSPECT TIME-OUT OF W-WK CONVERTING SPACE TO ':'
           INSPECT TIME-OUT OF W-WK CONVERTING '/' TO '.'
           DISPLAY 'Confirmation ended at ' TIME-OUT OF W-WK
                   '.'

           .
       TERM-EXIT.


      /*****************************************************************
      *                                                                *
       SA050-FIND-ERR-MSG-FND SECTION.
       SA050.
      *                                                                *
      ******************************************************************

           MOVE PROCESS-INSTANCE-ERRMSG OF PSLCT
                   TO  PROCESS-INSTANCE OF S-CNT-MSG
           MOVE COMPANY OF PSLCT TO COMPANY OF S-CNT-MSG
           MOVE PAYGROUP OF PSLCT TO PAYGROUP OF S-CNT-MSG

           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF S-CNT-MSG
                                   BIND-SETUP OF S-CNT-MSG
                                   BIND-DATA OF S-CNT-MSG
                                   SELECT-SETUP OF S-CNT-MSG
                                   SELECT-DATA OF S-CNT-MSG
           IF RTNCD-ERROR OF SQLRT

               MOVE 'FIND-ERR-MSG-FND(SELECT)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           INITIALIZE SELECT-DATA OF S-CNT-MSG

           CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
           IF RTNCD-ERROR OF SQLRT

               IF RTNCD-END OF SQLRT

                   SET RTNCD-OK OF SQLRT  TO  TRUE
               ELSE
                   MOVE 'FIND-ERR-MSG-FND(FETCH)'
                           TO  ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF
           ELSE
               PERFORM SA100-PUT-PRCSINSTANCE
           END-IF

           .
       FIND-ERR-MSG-FND-EXIT.


      /*****************************************************************
      *                                                                *
       SA100-PUT-PRCSINSTANCE SECTION.
       SA100.
      *                                                                *
      ******************************************************************

           MOVE 'PRCSINSTANCE'  TO  DATA-NAME OF NETRT
           MOVE 4  TO  DATA-LEN OF NETRT
           SET TYPE-INT OF NETRT  TO  TRUE

           CALL 'PTPNETRT' USING   ACTION-PUT OF NETRT
                                   NETRT
                                   PROCESS-INSTANCE-ERRMSG
                                           OF PSLCT
           IF NET-ERROR OF NETRT

               MOVE 'PUT-PRCSINSTANCE'  TO  ERR-SECTION OF SQLRT
               PERFORM ZP000-NET-ERROR
           END-IF

           .
       PUT-PRCSINSTANCE-EXIT.


      /*****************************************************************
      *                                                                *
       SA200-RET-SUCCESS SECTION.
       SA200.
      *                                                                *
      ******************************************************************

           SET RETURN-CD-SUCCESS OF W-NET-PARAM  TO  TRUE
           MOVE 'RETURN_CD'  TO  DATA-NAME OF NETRT
           MOVE 2  TO  DATA-LEN OF NETRT
           SET TYPE-SMALLINT OF NETRT  TO  TRUE

           CALL 'PTPNETRT' USING   ACTION-PUT OF NETRT
                                   NETRT
                                   RETURN-CD OF W-NET-PARAM
           IF NET-ERROR OF NETRT

               MOVE 'RET-SUCCESS'  TO  ERR-SECTION OF SQLRT
               PERFORM ZP000-NET-ERROR
           END-IF

           .
       RET-SUCCESS-EXIT.


      /*****************************************************************
      *                                                                *
       SD000-SET-RUN-STAT-SUCCESSFUL SECTION.
       SD000.
      *                                                                *
      ******************************************************************

           SET RUN-STATUS-SUCCESSFUL OF USTAT  TO  TRUE

      /*****************************************************************
      * CCC Check for Error Messages                                   *
      ******************************************************************

           COPY CCCMSGCK.

           CALL 'PTPUSTAT' USING   SQLRT
                                   USTAT
           IF RTNCD-ERROR OF SQLRT

               MOVE 'SET-RUN-STAT-SUCCESSFUL(PTPUSTAT)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           .
       SET-RUN-STAT-SUCCESSFUL-EXIT.


      /*****************************************************************
      *                                                                *
       TA000-DELETE-RAPID-PAYSHEETS SECTION.
       TA000.
      *                                                                *
      ******************************************************************

           MOVE RUN-ID OF PSLCT TO RUN-ID OF D-RPDET
           MOVE OFF-CYCLE OF PSLCT TO OFF-CYCLE OF D-RPDET

           CALL 'PTPSQLRT' USING   ACTION-UPDATE OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT-DEL OF D-RPDET
                                   BIND-SETUP OF D-RPDET
                                   BIND-DATA OF D-RPDET

           IF RTNCD-ERROR OF SQLRT

               MOVE 'DELETE-RPDET'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF


           CALL 'PTPSQLRT' USING   ACTION-UPDATE OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT-DELINV OF D-RPDET
                                   BIND-SETUP OF D-RPDET
                                   BIND-DATA OF D-RPDET

           IF RTNCD-ERROR OF SQLRT

               MOVE 'DELETE-RPDETINV'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           CALL 'PTPSQLRT' USING   ACTION-UPDATE OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT-DELRUN OF D-RPDET
                                   BIND-SETUP OF D-RPDET
                                   BIND-DATA OF D-RPDET

           IF RTNCD-ERROR OF SQLRT

               MOVE 'DELETE-RPDELRUN'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF


           CALL 'PTPSQLRT' USING   ACTION-UPDATE OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT-DELHDR OF D-RPDET
                                   BIND-SETUP OF D-RPDET
                                   BIND-DATA OF D-RPDET

           IF RTNCD-ERROR OF SQLRT

               MOVE 'DELETE-RPDELHDR'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           PERFORM MJ000-COMMIT-CALENDAR

           .
       DELETE-RAPID-PAYSHEETS-EXIT.


      /*****************************************************************
      *                                                                *
       ZP000-NET-ERROR SECTION.
       ZP000.
      *                                                                *
      *                                                                *
      ******************************************************************

           SET RTNCD-USER-ERROR OF SQLRT  TO  TRUE
           PERFORM ZZ000-SQL-ERROR

           .
       NET-ERROR-EXIT.


      /*****************************************************************
      *                                                                *
       ZZ000-SQL-ERROR SECTION.
       ZZ000.
      *                                                                *
      * SQL ERROR PROCESSING                                           *
      *                                                                *
      ******************************************************************

           CALL 'PTPSQLRT' USING   ACTION-ERROR OF SQLRT
                                   SQLRT

           IF RUNNING-REMOTE-CALL OF NETRT

               IF RET-CODE-OK OF NETRT

                   PERFORM ZZ050-NET-RESET
               END-IF

               PERFORM ZZ100-NET-TERMINATE
           END-IF

           ACCEPT TIME-OUT OF W-WK  FROM  TIME
           INSPECT TIME-OUT OF W-WK CONVERTING SPACE TO ':'
           INSPECT TIME-OUT OF W-WK CONVERTING '/' TO '.'
           DISPLAY 'Confirmation did not finish at '
                   TIME-OUT OF W-WK
                   ' !!!!!'

           COPY PSCRTNCD.

           .
       SQL-ERROR-EXIT.
           STOP RUN.


      /*****************************************************************
      *                                                                *
       ZZ050-NET-RESET SECTION.
       ZZ050.
      *                                                                *
      *                                                                *
      ******************************************************************

           CALL 'PTPNETRT' USING   ACTION-RESET OF NETRT
                                   NETRT

           .
       NET-RESET-EXIT.


      /*****************************************************************
      *                                                                *
       ZZ100-NET-TERMINATE SECTION.
       ZZ100.
      *                                                                *
      *                                                                *
      ******************************************************************

           CALL 'PTPNETRT' USING   ACTION-DONE OF NETRT
                                   NETRT

           .
       NET-TERMINATE-EXIT.
