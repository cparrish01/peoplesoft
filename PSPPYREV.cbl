       IDENTIFICATION DIVISION.

       PROGRAM-ID. PSPPYREV.

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
      *          $Date:  2009/04/09:05:58:53                           *
      *       $Release:  HR91                                          *
      *      $Revision:  101                                           *
      *                                                                *
      ******************************************************************
      *  MODIFICATION LOG:                                             *
      *    02/23/98 PNS Check pay messages to properly update process  *
      *       scheduler status at completion of run.  Changes first    *
      *       put in to 5.1 now copied to 7.0                          *
      *    10/08/01 PNS copied again to 8.01 code.                     *
      *    06/01/06 PNS copied again to 8.9 code.                      *
      *    10/09/10 PNS copied again to 9.1 code.                      *
      *                                                                *
      ******************************************************************

      ******************************************************************
      *                                                                *
      *                   PROGRAM DESCRIPTION:                         *
      *                                                                *
      * RUN A SET OF CHECK REVERSALS FOR A PAY PERIOD.                 *
      *                                                                *
      ******************************************************************


       DATA DIVISION.


       WORKING-STORAGE SECTION.


       01  PROGRAM-IDENTITY            PIC X(8)    VALUE 'PSPPYREV'.


       01  W-WK.
           02  TIME-OUT                PIC 99B99B99/99.


       01  W-NET-PARAM.
           02  RETURN-CD               PIC 99                  COMP.
               88  RETURN-CD-SUCCESS               VALUE 0.
               88  RETURN-CD-FAILURE               VALUE 99.


       01  W-PRC-INSTANCE.
           02  PROCESS-INSTANCE-ERRMSG PIC 9(10).


      /*****************************************************************
      *            PAY_REV_RUNCTL BUFFER AND STMT                      *
      ******************************************************************
       01  S-RUNCTL.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYREV_S_RUNCTL'.

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
               03  FILLER              PIC X(10)   VALUE ALL 'H'.
               03  FILLER              PIC X(10)   VALUE ALL 'D'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  COMPANY             PIC X(10).
               03  PAYGROUP            PIC X(10).
               03  PAY-END-DT          PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      *            PAY_REV_RUNCTL SQL DELETE STMT                      *
      ******************************************************************
       01  D-RUNCTL.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYREV_D_RUNCTL'.

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
       01  S-CAL.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYREV_S_CAL'.

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
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  RUN-ID              PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      *            PAY_CHECK_REVRS SQL DELETE STMT                     *
      ******************************************************************
       01  D-CKREV.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYREV_D_CKREV'.

           02  BIND-SETUP.
               03  FILLER              PIC X(30)   VALUE ALL 'C'.
               03  FILLER              PIC X(30)   VALUE ALL 'H'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  BIND-DATA.
               03  OPRID               PIC X(30).
               03  BATCH-RUN-ID        PIC X(30).
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      *            PAY_CHECK OFF CYCLE BUFFER AND STMT                 *
      ******************************************************************
       01  S-OFF.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYREV_S_OFF'.

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
       01  U-CAL.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYREV_U_CAL'.

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
      * CCC Check for Error Messages                                   *
      ******************************************************************

           COPY CCCMSGWK.

      /*****************************************************************
      *  FIND IF MESSAGES EXIST FOR PROCESS COMPLETED  BUFFER AND STMT *
      ******************************************************************
       01  S-CNT-MSG.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYREV_S_CNT_MSG'.

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
      *            PAYROLL SELECTION                                   *
      ******************************************************************
       01  PSLCT.                      COPY PSCPSLCT.


      /*****************************************************************
      *            NETWORK BLOCK                                       *
      ******************************************************************
       01  NETRT.                      COPY PTCNETRT.


      /*****************************************************************
      *            SQL COMMUNICATION                                   *
      ******************************************************************
       01  SQLRT.                      COPY PTCSQLRT.


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

           SET PAYROLL-STEP-REVERSAL OF PSLCT  TO  TRUE
           SET OFF-CYCLE-YES OF PSLCT  TO  TRUE
           PERFORM DA000-SELECT-RUNCTL
           ACCEPT TIME-OUT OF W-WK  FROM  TIME
           INSPECT TIME-OUT OF W-WK CONVERTING SPACE TO ':'
           INSPECT TIME-OUT OF W-WK CONVERTING '/' TO '.'
           DISPLAY 'Pay Reversal started for Company: '
                   COMPANY OF S-RUNCTL
           DISPLAY '                         Pay Group: '
                   PAYGROUP OF S-RUNCTL
           DISPLAY '                         Pay End Date: '
                   PAY-END-DT OF S-RUNCTL
           DISPLAY ' at ' TIME-OUT OF W-WK
                   '.'

           PERFORM GA000-START-RUN

           IF RTNCD-OK OF SQLRT

               PERFORM JA000-PROCESS-REVERSAL
               PERFORM MA000-COMMIT-REVERSAL
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
               PERFORM DD000-SELECT-CALENDAR
               MOVE CORR SELECT-DATA OF S-RUNCTL  TO  PSLCT
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

                       DISPLAY 'Pay Reversal Run Control Missing.'
                       DISPLAY ' for Operator ID  ' OPRID OF S-RUNCTL
                       DISPLAY ' and Batch Run ID '
                       DISPLAY BATCH-RUN-ID OF S-RUNCTL
                       SET RTNCD-USER-ERROR OF SQLRT  TO  TRUE
                       PERFORM ZZ000-SQL-ERROR
                   ELSE
                       MOVE 'SELECT-RUNCTL(FETCH)'
                               TO  ERR-SECTION OF SQLRT
                       PERFORM ZZ000-SQL-ERROR
                   END-IF
               ELSE
                   PERFORM DD000-SELECT-CALENDAR
                   MOVE CORR SELECT-DATA OF S-RUNCTL  TO  PSLCT
                   PERFORM DG000-RUNCTL-ACCEPTED
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

           MOVE 'PAYGROUP_REV'  TO  DATA-NAME OF NETRT
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

           MOVE 'PAY_END_DT_REV'  TO  DATA-NAME OF NETRT
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
       DD000-SELECT-CALENDAR SECTION.
       DD000.
      *                                                                *
      ******************************************************************

           MOVE CORR SELECT-DATA OF S-RUNCTL  TO  BIND-DATA OF S-CAL

           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF S-CAL
                                   BIND-SETUP OF S-CAL
                                   BIND-DATA OF S-CAL
                                   SELECT-SETUP OF S-CAL
                                   SELECT-DATA OF S-CAL
           IF RTNCD-ERROR OF SQLRT

               MOVE 'SELECT-CALENDAR(SELECT)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           INITIALIZE SELECT-DATA OF S-CAL

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
                   MOVE 'SELECT-CALENDAR(FETCH)'
                           TO  ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF
           ELSE

               IF RUN-ID OF S-CAL  =  SPACE

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
                   MOVE RUN-ID OF S-CAL  TO  RUN-ID OF PSLCT
               END-IF
           END-IF

           .
       SELECT-CALENDAR-EXIT.


      /*****************************************************************
      *                                                                *
       DG000-RUNCTL-ACCEPTED SECTION.
       DG000.
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

               MOVE 'RUNCTL-ACCEPTED(COMMIT)'  TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           .
       RUNCTL-ACCEPTED-EXIT.


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
       JA000-PROCESS-REVERSAL SECTION.
       JA000.
      *                                                                *
      ******************************************************************

           CALL 'PSPCKREV' USING   NETRT
                                   SQLRT
                                   PSLCT
           IF RTNCD-ERROR OF SQLRT

               MOVE 'PROCESS-REVERSAL(PSPCKREV)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           PERFORM JD000-UPDATE-PAY-CALENDAR
           PERFORM JG000-DELETE-CHECK-REVRS

           .
       PROCESS-REVERSAL-EXIT.


      /*****************************************************************
      *                                                                *
       JD000-UPDATE-PAY-CALENDAR SECTION.
       JD000.
      *                                                                *
      ******************************************************************

           MOVE CORR SELECT-DATA OF S-RUNCTL  TO  BIND-DATA OF S-OFF

           CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF S-OFF
                                   BIND-SETUP OF S-OFF
                                   BIND-DATA OF S-OFF
                                   SELECT-SETUP OF S-OFF
                                   SELECT-DATA OF S-OFF
           IF RTNCD-ERROR OF SQLRT

               MOVE 'UPDATE-PAY-CALENDAR(SELECT)'
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
                   SET PAY-OFF-CYCLE-NO OF U-CAL  TO  TRUE
               ELSE
                   MOVE 'UPDATE-PAY-CALENDAR(FETCH)'
                           TO  ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF
           ELSE
               SET PAY-OFF-CYCLE-YES OF U-CAL  TO  TRUE
           END-IF

           MOVE CORR SELECT-DATA OF S-RUNCTL  TO  BIND-DATA OF U-CAL

           CALL 'PTPSQLRT' USING   ACTION-UPDATE OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF U-CAL
                                   BIND-SETUP OF U-CAL
                                   BIND-DATA OF U-CAL
           IF RTNCD-ERROR OF SQLRT

               MOVE 'UPDATE-PAY-CALENDAR(UPDATE)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           .
       UPDATE-PAY-CALENDAR-EXIT.


      /*****************************************************************
      *                                                                *
       JG000-DELETE-CHECK-REVRS SECTION.
       JG000.
      *                                                                *
      ******************************************************************

           MOVE OPRID OF SQLRT  TO  OPRID OF D-CKREV
           MOVE BATCH-RUN-ID OF SQLRT  TO  BATCH-RUN-ID OF D-CKREV

           CALL 'PTPSQLRT' USING   ACTION-UPDATE OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
                                   SQL-STMT OF D-CKREV
                                   BIND-SETUP OF D-CKREV
                                   BIND-DATA OF D-CKREV
           IF RTNCD-ERROR OF SQLRT

               MOVE 'DELETE-CHECK-REVRS'  TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           .
       DELETE-CHECK-REVRS-EXIT.


      /*****************************************************************
      *                                                                *
       MA000-COMMIT-REVERSAL SECTION.
       MA000.
      *                                                                *
      ******************************************************************

           CALL 'PTPSQLRT' USING   ACTION-COMMIT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
           IF RTNCD-ERROR OF SQLRT

               MOVE 'COMMIT-REVERSAL'  TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           .
       COMMIT-REVERSAL.


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
           DISPLAY 'Pay Reversal ended at ' TIME-OUT OF W-WK
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
           DISPLAY 'Pay Reversal did not finish at '
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
      * TERMINATE DUE TO NETWORK ERROR                                 *
      *                                                                *
      ******************************************************************

           CALL 'PTPNETRT' USING   ACTION-DONE OF NETRT
                                   NETRT

           .
       NET-TERMINATE-EXIT.
