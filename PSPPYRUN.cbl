       IDENTIFICATION DIVISION.

       PROGRAM-ID. PSPPYRUN.

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
      *          $Date:  2009/04/09:05:58:59                           *
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
      *    10/08/01 PNS Copied again to 8.01 code.                     *
      *    06/01/06 PNS Copied again to 8.9 code.                      *
      *    10/09/10 PNS Copied again to 9.1 code.                      *
      *                                                                *
      ******************************************************************

      ******************************************************************
      *                                                                *
      *                   PROGRAM DESCRIPTION:                         *
      *                                                                *
      * RUN A PAY CALCULATION.                                         *
      *                                                                *
      ******************************************************************

      ******************************************************************
      * MODIFIED FOR EDUCATION & GOVERNMENT                            *
      * HP90003       FICA CREDIT ENHANCEMENT                          *
      * MODIFIED FOR FEDERAL                                           *
      * FED0999       FICA CREDIT ENHANCEMENT                          *
      ******************************************************************


       DATA DIVISION.


       WORKING-STORAGE SECTION.


       01  PROGRAM-IDENTITY            PIC X(8)    VALUE 'PSPPYRUN'.


       01  W-WK.
           02  TIME-OUT                PIC 99B99B99/99.
           02  WK-PAY-PRD              PIC 9.
           02  WK-DATE.
               03  WK-YR               PIC X(4).
               03  WK-NUM-YR    REDEFINES WK-YR   PIC 9(4).
               03  FILLER              PIC X.
               03  WK-MO               PIC X(2).
               03  FILLER              PIC X.
               03  WK-DAY              PIC X(2).
           02  WK-RESULT               PIC S9(4)V9(5).
           02  WK-RESULT-INTEGER       PIC S9(4).

NOCBGN     02  WK-DED-PERIODS.                                          HP90003
               03  FILLER              PIC X(5)    VALUE '12345'.       HP90003
           02  WK-DED-PRD-ARRAY REDEFINES WK-DED-PERIODS.               HP90003
               03  WK-DED-PERIOD       PIC X       OCCURS 5.            HP90003
NOCEND     02  WK-IDX                  PIC 9999    COMP.                HP90003

       01  W-DSP.
           02  PAGE-NO                 PIC ZZZZ9.


       01  W-SW.
           02  FETCH-CAL-SW            PIC X       VALUE SPACE.
               88  FETCH-CAL-END                   VALUE 'E'.


       01  W-PASS.
           02  COUNTRY                 PIC XXX.


       01  W-NET-PARAM.
           02  RETURN-CD               PIC 99                  COMP.
               88  RETURN-CD-SUCCESS               VALUE 0.
               88  RETURN-CD-FAILURE               VALUE 99.


       01  W-PRC-INSTANCE.
           02  PROCESS-INSTANCE-ERRMSG PIC 9(10).


      /*****************************************************************
      *            PAY_CALC_RUNCTL BUFFER AND STMT                     *
      ******************************************************************
       01  S-RUNCTL.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYRUN_S_RUNCTL'.

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
               03  FILLER              PIC X       VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC XXXX    VALUE ALL 'I'.
               03  FILLER              PIC XXXX    VALUE ALL 'N'.
               03  FILLER              PIC X       VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE ALL 'H'.
               03  FILLER              PIC X       VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  RUN-ID              PIC X(10).
               03  PROCESS-ON-OFF      PIC X.
                   88 PROCESS-ON-RUNID             VALUE 'O'.
                   88 PROCESS-OFF-RUNID            VALUE 'F'.
                   88 NO-RUNID                     VALUE 'N'.
               03  PRELIMINARY-CALC    PIC X.
                   88 PRELIMINARY-CALC-YES         VALUE 'Y'.
                   88 PRELIMINARY-CALC-NO          VALUE 'N'.

               03  COMPANY             PIC X(10).
               03  PAYGROUP            PIC X(10).
               03  PAY-END-DT          PIC X(10).
               03  PAGE-NO             PIC 99999               COMP.
               03  PAGE-NO-THRU        PIC 99999               COMP.
               03  CALC-SELECT         PIC X.
               03  XFER-CALC-ERRS      PIC X.
               03  PROC-ERR086         PIC X.
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      *            PAY_CALC_RUNCTL SQL DELETE STMT                     *
      ******************************************************************
       01  D-RUNCTL.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYRUN_D_RUNCTL'.

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
                                                   'PSPPYRUN_S_CALOFF'.

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
               03  FILLER              PIC XXX     VALUE ALL 'H'.
               03  FILLER              PIC X(6)    VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE ALL 'H'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  RUN-ID              PIC X(10).
               03  COUNTRY             PIC XXX.
               03  AGGR-ID             PIC X(6).
               03  PAY-OFF-CYCLE-CAL   PIC X.
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      *            PAY_CALENDAR BUFFER AND STMT                        *
      ******************************************************************
       01  S-CAL.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYRUN_S_CAL'.
           02  SQL-STMT-CONT           PIC X(18)   VALUE
                                                   'PSPPYRUN_S_CALCONT'.
           02  SQL-STMT-OFF2           PIC X(18)   VALUE
                                                   'PSPPYRUN_S_CALOFF2'.
           02  SQL-STMT-CNT2           PIC X(18)   VALUE
                                                   'PSPPYRUN_S_CALCNT2'.

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
               03  FILLER              PIC XXX     VALUE ALL 'C'.
               03  FILLER              PIC X(6)    VALUE ALL 'H'.
               03  FILLER              PIC X       VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  COMPANY             PIC X(10).
               03  PAYGROUP            PIC X(10).
               03  PAY-END-DT          PIC X(10).
               03  COUNTRY             PIC XXX.
               03  AGGR-ID             PIC X(6).
               03  PAY-OFF-CYCLE-CAL   PIC X.
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      *            PAY_PAGE OFF CYCLE BUFFER AND STMT                  *
      ******************************************************************
       01  S-OFFPAGE.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYRUN_S_OFFPAGE'.

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
                                                   'PSPPYRUN_S_OFF'.

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
                                                   'PSPPYRUN_U_CALOFF'.

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
                                                   'PSPPYRUN_U_CAL'.
           02  SQL-STMT-PRE            PIC X(18)   VALUE
                                                   'PSPPYRUN_U_CALPRE'.
           02  SQL-STMT-CAL            PIC X(18)   VALUE
                                                   'PSPPYRUN_U_CAL'.

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
                                                   'PSPPYRUN_S_CNT_MSG'.

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
      *            PAY_PERIOD BUFFER AND STMT                          *
      ******************************************************************
       01  S-PAYPRD.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYRUN_S_PAYPRD'.

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
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  PAY-PERIOD          PIC X.
               03  PAY-END-DT          PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************HP90003
      *            DED_PERIOD BUFFER AND STMT                          *HP90003
      ******************************************************************HP90003
       01  S-PRDCNT.                                                    HP90003
           02  SQL-CURSOR              PIC 9999    VALUE ZERO  COMP.    HP90003
           02  SQL-STMT                PIC X(18)   VALUE                HP90003
                                                   'PSPPYRUN_S_PRDCNT'. HP90003
                                                                        HP90003
           02  BIND-SETUP.                                              HP90003
               03  FILLER              PIC X(10)   VALUE ALL 'C'.       HP90003
               03  FILLER              PIC X(10)   VALUE ALL 'H'.       HP90003
               03  FILLER              PIC X(10)   VALUE ALL 'D'.       HP90003
               03  FILLER              PIC X(10)   VALUE ALL 'A'.       HP90003
               03  FILLER              PIC X       VALUE ALL 'C'.       HP90003
               03  FILLER              PIC X       VALUE ALL 'H'.       HP90003
               03  FILLER              PIC X       VALUE 'Z'.           HP90003
                                                                        HP90003
           02  BIND-DATA.                                               HP90003
               03  COMPANY             PIC X(10).                       HP90003
               03  PAYGROUP            PIC X(10).                       HP90003
               03  YEAR-BEGIN-DT       PIC X(10).                       HP90003
               03  YEAR-END-DT         PIC X(10).                       HP90003
               03  DED-PERIOD          PIC X.                           HP90003
               03  PAY-CONFIRM-RUN     PIC X.                           HP90003
               03  FILLER              PIC X       VALUE 'Z'.           HP90003
                                                                        HP90003
           02  SELECT-SETUP.                                            HP90003
               03  FILLER              PIC XX      VALUE ALL 'S'.       HP90003
               03  FILLER              PIC X       VALUE 'Z'.           HP90003
                                                                        HP90003
           02  SELECT-DATA.                                             HP90003
               03  DED-PERIOD-COUNT    PIC 999                 COMP.    HP90003
               03  FILLER              PIC X       VALUE 'Z'.           HP90003


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

           SET PAYROLL-STEP-CALC OF PSLCT  TO  TRUE
           PERFORM DA000-SELECT-RUNCTL
           ACCEPT TIME-OUT OF W-WK  FROM  TIME
           INSPECT TIME-OUT OF W-WK CONVERTING SPACE TO ':'
           INSPECT TIME-OUT OF W-WK CONVERTING '/' TO '.'

           IF OFF-CYCLE-YES OF PSLCT

               DISPLAY 'Off-Cycle Calculation started for Run: '
                       RUN-ID OF PSLCT
           ELSE
               DISPLAY 'On-Cycle Calculation started for Run: '
                       RUN-ID OF PSLCT
           END-IF

           DISPLAY ' at ' TIME-OUT OF W-WK
                   '.'
           PERFORM GA000-START-RUN

           IF RTNCD-OK OF SQLRT

               IF NO-RUNID OF PSLCT

                   PERFORM WITH TEST AFTER
                           VARYING PAGE-NO OF PSLCT
                                   FROM  PAGE-NO OF PSLCT  BY  1
                               UNTIL PAGE-NO OF PSLCT
                                       =  PAGE-NO-THRU OF PSLCT

                       PERFORM JA000-LOOKUP-OFF-CYCLE-PAGE

                       IF RTNCD-END OF SQLRT

                           SET RTNCD-OK OF SQLRT  TO  TRUE
                       ELSE
                           PERFORM MA000-PROCESS-PAY
                       END-IF
                   END-PERFORM
               ELSE
                   PERFORM DM000-SELECT-PAY-CALENDAR
                   IF PROCESS-ON-RUNID OF PSLCT

                       MOVE SQL-STMT-CONT OF S-CAL
                               TO  SQL-STMT OF S-CAL
                   ELSE
                       MOVE SQL-STMT-CNT2 OF S-CAL
                               TO  SQL-STMT OF S-CAL
                   END-IF
                   MOVE ALL 'H'  TO  PAYCONT OF BIND-SETUP OF S-CAL
                   PERFORM MA000-PROCESS-PAY
                           UNTIL FETCH-CAL-END OF W-SW
               END-IF

               IF NOT-OVER-XFER-LIMIT   OF PSLCT

                   CALL 'PSPCNFER'     USING SQLRT
                                             PSLCT
                   IF RTNCD-ERROR OF SQLRT

                       MOVE 'MAIN(PSPCNFER)'  TO  ERR-SECTION OF SQLRT
                       PERFORM ZZ000-SQL-ERROR
                   END-IF
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

                       DISPLAY 'Calculation Run Control Missing.'
                       DISPLAY ' for Operator ID  ' OPRID OF S-RUNCTL
                       DISPLAY ' and Batch Run ID '
                               BATCH-RUN-ID OF S-RUNCTL
                       SET RTNCD-USER-ERROR OF SQLRT  TO  TRUE
                       MOVE 'SELECT-RUNCTL' TO ERR-SECTION OF SQLRT
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
           ELSE
               MOVE RUN-ID OF PSLCT  TO  RUN-ID OF S-CAL
               MOVE AGGR-ID OF PSLCT  TO  AGGR-ID OF S-CAL

               IF PROCESS-ON-RUNID OF PSLCT

                   SET OFF-CYCLE-NO OF PSLCT  TO  TRUE
               ELSE
                   MOVE SQL-STMT-OFF2 OF S-CAL
                           TO  SQL-STMT OF S-CAL
                   SET OFF-CYCLE-YES OF PSLCT  TO  TRUE
               END-IF

               PERFORM DM000-SELECT-PAY-CALENDAR

               IF PAY-OFF-CYCLE-CAL OF S-CAL  =  'N'

                   MOVE 'N'  TO  PAY-OFF-CYCLE-CAL OF PSLCT

                   IF PRELIMINARY-CALC-YES OF S-RUNCTL
                           AND  PROCESS-ON-RUNID OF PSLCT

                       MOVE SQL-STMT-PRE OF U-CAL
                               TO  SQL-STMT OF U-CAL
                   END-IF
               ELSE
                   MOVE 'Y'  TO   PAY-OFF-CYCLE-CAL OF PSLCT
               END-IF
           END-IF

           IF    XFER-CALC-ERRS-YES     OF PSLCT

             SET NOT-OVER-XFER-LIMIT OF PSLCT TO TRUE
           ELSE

             SET OVER-XFER-LIMIT     OF PSLCT TO TRUE
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


           MOVE 'PROC_ERR086'  TO  DATA-NAME OF NETRT
           MOVE 1  TO  DATA-LEN OF NETRT
           SET TYPE-CHAR OF NETRT  TO  TRUE

           CALL 'PTPNETRT' USING   ACTION-GET OF NETRT
                                   NETRT
                                   PROC-ERR086 OF S-RUNCTL
           IF NET-ERROR OF NETRT

               MOVE 'GET-RUNCTL-PARAM(PROC_ERR086)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZP000-NET-ERROR
           END-IF


           SET PRELIMINARY-CALC-YES OF S-RUNCTL  TO  TRUE
           MOVE SPACE  TO  RUN-ID OF S-RUNCTL
           MOVE ZERO  TO  PAGE-NO-THRU OF S-RUNCTL
           MOVE 'A'  TO  CALC-SELECT OF S-RUNCTL

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
                   MOVE AGGR-ID OF S-CALOFF  TO  AGGR-ID OF PSLCT
               END-IF
           END-IF

           .
       SELECT-OFF-CALENDAR-EXIT.


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

                   SET RTNCD-OK OF SQLRT  TO  TRUE
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
       MA000-PROCESS-PAY SECTION.
       MA000.
      *                                                                *
      ******************************************************************

           ACCEPT TIME-OUT OF W-WK  FROM  TIME
           INSPECT TIME-OUT OF W-WK CONVERTING SPACE TO ':'
           INSPECT TIME-OUT OF W-WK CONVERTING '/' TO '.'
           DISPLAY 'Calculation started for Company: '
                   COMPANY OF PSLCT
           DISPLAY '                        Pay Group: '
                   PAYGROUP OF PSLCT
           DISPLAY '                        Pay End Date: '
                   PAY-END-DT OF PSLCT

           IF NO-RUNID OF PSLCT

               MOVE PAGE-NO OF PSLCT  TO  PAGE-NO OF W-DSP
               DISPLAY '                        PAGE_NUM: '
                       PAGE-NO OF W-DSP
           END-IF

           DISPLAY ' at ' TIME-OUT OF W-WK
                   '.'

           IF NO-RUNID OF PSLCT

               MOVE COUNTRY OF S-CALOFF  TO  COUNTRY OF W-PASS
           ELSE
               MOVE COUNTRY OF S-CAL  TO  COUNTRY OF W-PASS
           END-IF

           PERFORM NA000-GET-PAYPRD-DATA

           IF PUBLIC-SECTOR-YES OF PSLCT  OR                            HP90003
              (GOVERNMENT OF PSLCT AND                                  FED0999
               US-FEDERAL-GOVT OF PSLCT)                                FED0999

              PERFORM NB200-COUNT-DED-PERIODS                           HP90003
           END-IF                                                       HP90003

           CALL 'PSPPYWK1' USING   SQLRT
                                   PSLCT
                                   DARRY
                                   W-PASS
           IF RTNCD-ERROR OF SQLRT

               MOVE 'PROCESS-PAY(PSPPYWK1)' TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           IF OFF-CYCLE-YES OF PSLCT

               PERFORM MD000-UPDATE-OFF-CALENDAR
           ELSE
               PERFORM MG000-UPDATE-PAY-CALENDAR

           END-IF

           IF PROCESS-ON-RUNID OF PSLCT
                   OR  PROCESS-OFF-RUNID OF PSLCT

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
       PROCESS-PAY-EXIT.


      /*****************************************************************
      *                                                                *
       MD000-UPDATE-OFF-CALENDAR SECTION.
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

                   SET RTNCD-OK OF SQLRT  TO  TRUE
                   SET PAY-OFF-CYCLE-NO OF U-CALOFF  TO  TRUE
               ELSE
                   MOVE 'UPDATE-OFF-CALENDAR(FETCH)'
                           TO  ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF
           ELSE
               SET PAY-OFF-CYCLE-YES OF U-CALOFF  TO  TRUE
           END-IF

           MOVE CORR PSLCT  TO  BIND-DATA OF U-CALOFF

           CALL 'PTPSQLRT' USING   ACTION-UPDATE OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF U-CALOFF
                                   BIND-SETUP OF U-CALOFF
                                   BIND-DATA OF U-CALOFF
           IF RTNCD-ERROR OF SQLRT

               MOVE 'UPDATE-OFF-CALENDAR(UPDATE)'
                       TO  ERR-SECTION OF SQLRT
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
       NA000-GET-PAYPRD-DATA SECTION.
       NA000.
      *                                                                *
      ******************************************************************

           MOVE CORR PSLCT  TO  BIND-DATA OF S-PAYPRD
           MOVE PAY-END-DT OF PSLCT  TO  WK-DATE OF W-WK
           IF WK-MO OF W-WK  =  '04' OR '06' OR '09' OR '11'

               MOVE '30'  TO  WK-DAY OF W-WK
           ELSE

               IF WK-MO OF W-WK  =  '02'

                   COMPUTE WK-RESULT OF W-WK
                           =  WK-NUM-YR OF W-WK
                           /  4

                   MOVE WK-RESULT OF W-WK
                           TO  WK-RESULT-INTEGER OF W-WK

                   IF WK-RESULT OF W-WK
                           >  WK-RESULT-INTEGER OF W-WK

                       MOVE '28'  TO WK-DAY OF W-WK
                   ELSE

                       MOVE '29'  TO  WK-DAY OF W-WK
                   END-IF
               ELSE

                   MOVE '31'  TO WK-DAY OF W-WK
               END-IF
           END-IF

           MOVE WK-DATE OF W-WK
               TO  PAY-END-DT OF BIND-DATA OF S-PAYPRD

           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF S-PAYPRD
                                   BIND-SETUP OF S-PAYPRD
                                   BIND-DATA OF S-PAYPRD
                                   SELECT-SETUP OF S-PAYPRD
                                   SELECT-DATA OF S-PAYPRD
           IF RTNCD-ERROR OF SQLRT

               MOVE 'SELECT-PAYPRD'  TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           INITIALIZE SELECT-DATA OF S-PAYPRD
           MOVE ZERO  TO  PAY-PRDS-IN-MONTH OF PSLCT
           MOVE ZERO  TO  THIS-PAY-PRD OF PSLCT

           CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
           IF RTNCD-ERROR OF SQLRT

               MOVE 'FETCH-PAYPRD-DATA(PYRUN)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           ELSE
               PERFORM NA100-SET-PAYPRD-DATA
               MOVE WK-PAY-PRD OF W-WK  TO  PAY-PRDS-IN-MONTH OF PSLCT
           END-IF

           IF PAY-END-DT OF SELECT-DATA OF S-PAYPRD
                   =  PAY-END-DT OF PSLCT

               MOVE WK-PAY-PRD OF W-WK  TO  THIS-PAY-PRD OF PSLCT
           ELSE

               PERFORM UNTIL PAY-END-DT OF SELECT-DATA OF S-PAYPRD
                       <  PAY-END-DT OF PSLCT

                   INITIALIZE SELECT-DATA OF S-PAYPRD


                   CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                           SQLRT
                                           SQL-CURSOR-COMMON OF SQLRT
                   IF RTNCD-ERROR OF SQLRT

                       IF RTNCD-END OF SQLRT

                           SET RTNCD-OK OF SQLRT  TO  TRUE
                       ELSE
                           MOVE 'FETCH-PAYPRD-DATA(PYRUN)'
                                   TO  ERR-SECTION OF SQLRT
                           PERFORM ZZ000-SQL-ERROR
                       END-IF
                   ELSE

                       IF PAY-END-DT OF SELECT-DATA OF S-PAYPRD
                               =  PAY-END-DT OF PSLCT

                           PERFORM NA100-SET-PAYPRD-DATA
                           MOVE WK-PAY-PRD OF W-WK
                                   TO  THIS-PAY-PRD OF PSLCT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF

           IF THIS-PAY-PRD OF PSLCT  =  ZERO

               MOVE 1  TO  THIS-PAY-PRD OF PSLCT
           END-IF

           .
       GET-PAYPRD-DATA-EXIT.


      /*****************************************************************
      *                                                                *
       NA100-SET-PAYPRD-DATA SECTION.
       NA100.
      *                                                                *
      ******************************************************************

           MOVE ZERO  TO  WK-PAY-PRD OF W-WK

           EVALUATE PAY-PERIOD OF S-PAYPRD

               WHEN '1'

                   MOVE 1  TO  WK-PAY-PRD OF W-WK

               WHEN '2'

                   MOVE 2  TO  WK-PAY-PRD OF W-WK

               WHEN '3'

                   MOVE 3  TO  WK-PAY-PRD OF W-WK

               WHEN '4'

                   MOVE 4  TO  WK-PAY-PRD OF W-WK

               WHEN '5'

                   MOVE 5  TO  WK-PAY-PRD OF W-WK

           END-EVALUATE

           .
       SET-PAYPRD-DATA-EXIT.


      /*****************************************************************HP90003
      *                                                                *HP90003
       NB200-COUNT-DED-PERIODS SECTION.                                 HP90003
       NB200.                                                           HP90003
      *                                                                *HP90003
      ******************************************************************HP90003
                                                                        HP90003
           MOVE CORR PSLCT  TO  BIND-DATA OF S-PRDCNT                   HP90003
           MOVE PAY-END-DT OF PSLCT  TO  WK-DATE OF W-WK                HP90003
           MOVE '01'                 TO  WK-MO   OF W-WK                HP90003
           MOVE '01'                 TO  WK-DAY  OF W-WK                HP90003
           MOVE WK-DATE OF W-WK      TO  YEAR-BEGIN-DT OF S-PRDCNT      HP90003
           MOVE '12'                 TO  WK-MO   OF W-WK                HP90003
           MOVE '31'                 TO  WK-DAY  OF W-WK                HP90003
           MOVE WK-DATE OF W-WK      TO  YEAR-END-DT  OF S-PRDCNT       HP90003
                                                                        HP90003
           MOVE ZERO TO TOT-CONF-DED-PRD-CNT OF PSLCT                   HP90003
           MOVE ZERO TO TOT-FUTURE-DED-PRD-CNT OF PSLCT                 HP90003
                                                                        HP90003
           MOVE 'N'                  TO  PAY-CONFIRM-RUN OF S-PRDCNT    HP90003
           PERFORM VARYING WK-IDX FROM 1 BY 1 UNTIL WK-IDX > 5          HP90003
                                                                        HP90003
               MOVE ZERO TO FUTURE-DED-PRD-CNT OF PSLCT(WK-IDX)         HP90003
               PERFORM NB250-RETRIEVE-COUNTS                            HP90003
                                                                        HP90003
               MOVE  DED-PERIOD-COUNT OF S-PRDCNT                       HP90003
                               TO FUTURE-DED-PRD-CNT OF PSLCT(WK-IDX)   HP90003
                                                                        HP90003
               COMPUTE TOT-FUTURE-DED-PRD-CNT OF PSLCT                  HP90003
                   =   TOT-FUTURE-DED-PRD-CNT OF PSLCT                  HP90003
                   +   FUTURE-DED-PRD-CNT OF PSLCT(WK-IDX)              HP90003
           END-PERFORM                                                  HP90003
                                                                        HP90003
           MOVE 'Y'                  TO  PAY-CONFIRM-RUN OF S-PRDCNT    HP90003
           PERFORM VARYING WK-IDX FROM 1 BY 1 UNTIL WK-IDX > 5          HP90003
                                                                        HP90003
               MOVE ZERO TO CONF-DED-PRD-CNT OF PSLCT(WK-IDX)           HP90003
               PERFORM NB250-RETRIEVE-COUNTS                            HP90003
                                                                        HP90003
               MOVE  DED-PERIOD-COUNT OF S-PRDCNT                       HP90003
                               TO CONF-DED-PRD-CNT OF PSLCT(WK-IDX)     HP90003
                                                                        HP90003
               COMPUTE TOT-CONF-DED-PRD-CNT OF PSLCT                    HP90003
                   =   TOT-CONF-DED-PRD-CNT OF PSLCT                    HP90003
                   +   CONF-DED-PRD-CNT OF PSLCT(WK-IDX)                HP90003
           END-PERFORM                                                  HP90003
                                                                        HP90003
           .                                                            HP90003
       COUNT-DED-PERIODS-EXIT.                                          HP90003
                                                                        HP90003
      /*****************************************************************HP90003
      *                                                                *HP90003
       NB250-RETRIEVE-COUNTS SECTION.                                   HP90003
       NB250.                                                           HP90003
      *                                                                *HP90003
      ******************************************************************HP90003
                                                                        HP90003
           MOVE WK-DED-PERIOD OF WK-DED-PRD-ARRAY (WK-IDX)              HP90003
                                   TO DED-PERIOD   OF S-PRDCNT          HP90003
                                                                        HP90003
           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT               HP90003
                                   SQLRT                                HP90003
                                   SQL-CURSOR-COMMON OF SQLRT           HP90003
                                   SQL-STMT OF S-PRDCNT                 HP90003
                                   BIND-SETUP OF S-PRDCNT               HP90003
                                   BIND-DATA OF S-PRDCNT                HP90003
                                   SELECT-SETUP OF S-PRDCNT             HP90003
                                   SELECT-DATA OF S-PRDCNT              HP90003
           IF RTNCD-ERROR OF SQLRT                                      HP90003
                                                                        HP90003
               MOVE 'SELECT-PRDCNT'  TO  ERR-SECTION OF SQLRT           HP90003
               PERFORM ZZ000-SQL-ERROR                                  HP90003
           END-IF                                                       HP90003
                                                                        HP90003
           INITIALIZE SELECT-DATA OF S-PRDCNT                           HP90003
                                                                        HP90003
           CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT                HP90003
                                   SQLRT                                HP90003
                                   SQL-CURSOR-COMMON OF SQLRT           HP90003
           IF RTNCD-ERROR OF SQLRT                                      HP90003
                                                                        HP90003
               MOVE 'FETCH-PRDCNT-DATA(PYRUN)'                          HP90003
                       TO  ERR-SECTION OF SQLRT                         HP90003
               PERFORM ZZ000-SQL-ERROR                                  HP90003
           END-IF                                                       HP90003
           .                                                            HP90003
       RETRIEVE-COUNTS-EXIT.                                            HP90003


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
           DISPLAY 'Calculation ended at ' TIME-OUT OF W-WK
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
           DISPLAY 'Calculation did not finish at '
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
