       IDENTIFICATION DIVISION.

       PROGRAM-ID. PSPPYBLD.

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
      *          $Date:  2009/04/09:05:57:56                           *
      *       $Release:  HR91                                          *
      *      $Revision:  101                                           *
      *                                                                *
      ******************************************************************
      *  MODIFICATION LOG:                                             *
      *    02/23/98 PNS Check pay messages to properly update process  *
      *       scheduler status at completion of run.  Changes first    *
      *       put in to 5.1 now copied to 7.0                          *
      *    10/08/01 PNS Copied again to 8.01 code.                     *
      *    06/01/06 PNS Copied again to 8.9 code.                      *
      *    10/15/10 PNS Copied again to 9.1 code.                      *
      *                                                                *
      ******************************************************************

      ******************************************************************
      *                                                                *
      *                   PROGRAM DESCRIPTION:                         *
      *                                                                *
      * BUILD THE PAY FILE FOR A PAY RUN.                              *
      *                                                                *
      ******************************************************************


       DATA DIVISION.


       WORKING-STORAGE SECTION.


       01  PROGRAM-IDENTITY            PIC X(8)    VALUE 'PSPPYBLD'.

       01  W-WK.
           02  TIME-OUT                PIC 99B99B99/99.


       01  W-SW.
           02  FETCH-CAL-SW            PIC X       VALUE SPACE.
               88  FETCH-CAL-END                   VALUE 'E'.


       01  W-NET-PARAM.
           02  RETURN-CD               PIC 99                  COMP.
               88  RETURN-CD-SUCCESS               VALUE 0.
               88  RETURN-CD-FAILURE               VALUE 99.

           02  PAYDATA.
               03  COMPANY             PIC X(10).
               03  PAYGROUP            PIC X(10).
               03  PAY-END-DT          PIC X(10).
               03  EMPLID              PIC X(20).
               03  EMPL-RCD-NO         PIC 999                 COMP.
               03  LOAD-TL-PRCS        PIC X.
                   88  LOAD-TL-PRCS-YES            VALUE 'Y'.
               03  LDTL-RUN-CNTL-ID    PIC X(30)   VALUE SPACE.


       01  W-PRC-INSTANCE.
           02  PROCESS-INSTANCE-ERRMSG PIC 9(10).


      /*****************************************************************
      *            PAYSHEET_RUNCTL BUFFER AND STMT                     *
      ******************************************************************
       01  S-RUNCTL.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYBLD_S_RUNCTL'.

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
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  RUN-ID              PIC X(10).
               03  FILLER              PIC X       VALUE 'Z'.



      /*****************************************************************
      *            PAYSHEET_RUNCTL SQL DELETE STMT                     *
      ******************************************************************
       01  D-RUNCTL.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYBLD_D_RUNCTL'.

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
                                                   'PSPPYBLD_S_CALOFF'.

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
               03  FILLER              PIC X(10)   VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE ALL 'H'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  RUN-ID              PIC X(10).
               03  COUNTRY             PIC XXX.
               03  AGGR-ID             PIC X(10).
               03  PAY-OFF-CYCLE-CAL   PIC X.
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      *            PAY_CALENDAR BUFFER AND STMT                        *
      ******************************************************************
       01  S-CAL.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYBLD_S_CAL'.
           02  SQL-STMT-CONT           PIC X(18)   VALUE
                                                   'PSPPYBLD_S_CALCONT'.
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
               03  FILLER              PIC X(6)    VALUE ALL 'C'.
               03  FILLER              PIC X       VALUE 'Z'.

           02  SELECT-DATA.
               03  COMPANY             PIC X(10).
               03  PAYGROUP            PIC X(10).
               03  PAY-END-DT          PIC X(10).
               03  AGGR-ID             PIC X(6).
               03  FILLER              PIC X       VALUE 'Z'.


      /*****************************************************************
      *            PAY_CALENDAR SQL UPDATE STMT                        *
      ******************************************************************
       01  U-CAL.
           02  SQL-STMT                PIC X(18)   VALUE
                                                   'PSPPYBLD_U_CAL'.

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
                                                   'PSPPYBLD_S_CNT_MSG'.

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
      *            NETWORK COMMUNICATION                               *
      ******************************************************************
       01  NETRT.                      COPY PTCNETRT.


      /*****************************************************************
      *            SQL COMMUNICATION                                   *
      ******************************************************************
       01  SQLRT.                      COPY PTCSQLRT.


      /*****************************************************************
      *            PAY DATA ARRAY                                      *
      ******************************************************************
       01  PARRY.                      COPY PSCPARRY.


      /*****************************************************************
      *            SETID ARRAY                                         *
      ******************************************************************
       01  SETAR.                      COPY PSCSETAR.


      /*****************************************************************
      *        EARNING CODE ARRAY                                      *
      *            REQUIRED FOR CALLING LOAD FROM TIME & LABOR         *
      ******************************************************************
       01  FCERN.                      COPY PSCFCERN.


      /*****************************************************************
      *        LOAD TIME & LABOR TO PAYROLL DATA                       *
      *            REQUIRED FOR CALLING LOAD FROM TIME & LABOR         *
      ******************************************************************
       01  LDTL1.                      COPY PSCLDTL1.


      /*****************************************************************
      *        PAYROLL SELECTION                                       *
      *            REQUIRED FOR CALLING LOAD FROM TIME & LABOR         *
      ******************************************************************
       01  FCRUN.                      COPY PSCFCRUN.


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

           SET PAYROLL-STEP-BUILD OF PSLCT  TO  TRUE
           PERFORM DA000-SELECT-RUNCTL
           ACCEPT TIME-OUT OF W-WK  FROM  TIME
           INSPECT TIME-OUT OF W-WK CONVERTING SPACE TO ':'
           INSPECT TIME-OUT OF W-WK CONVERTING '/' TO '.'
           DISPLAY 'PaySheets started for Run: '
                   RUN-ID OF PSLCT
           DISPLAY ' at ' TIME-OUT OF W-WK
                   '.'

           IF OFF-CYCLE-YES OF PSLCT

               PERFORM GA000-BUILD-PAYSHEETS
               PERFORM MA000-COMMIT-OL-PAYSHEET
           ELSE
               MOVE SQL-STMT-CONT OF S-CAL  TO  SQL-STMT OF S-CAL
               MOVE ALL 'H'  TO  PAYCONT OF BIND-SETUP OF S-CAL
               PERFORM GA000-BUILD-PAYSHEETS
                       UNTIL FETCH-CAL-END OF W-SW
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
                       TO  PROCESS-INSTANCE-ERRMSG OF PSLCT
           END-IF

           IF RUNNING-REMOTE-CALL OF NETRT

               PERFORM DA005-GET-NET-PARAM
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

                       DISPLAY 'PaySheet Run Control Missing.'
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
                   MOVE CORR SELECT-DATA OF S-RUNCTL  TO  PSLCT
                   PERFORM DD000-RUNCTL-ACCEPTED
               END-IF
           END-IF

           MOVE SPACE  TO  BAL-ID-FOR-CAL-YR OF PSLCT

           IF RUN-ID OF PSLCT  =  SPACE

               SET NO-RUNID OF PSLCT  TO  TRUE
               SET OFF-CYCLE-YES OF PSLCT  TO  TRUE
               PERFORM GM000-SELECT-OFF-CALENDAR
           ELSE
               SET OFF-CYCLE-NO OF PSLCT TO TRUE
               MOVE RUN-ID OF S-RUNCTL  TO  RUN-ID OF S-CAL
               PERFORM GG000-SELECT-PAY-CALENDAR
           END-IF

           .
       SELECT-RUNCTL-EXIT.


      /*****************************************************************
      *                                                                *
       DA005-GET-NET-PARAM SECTION.
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

               MOVE 'GET-NET-PARAM(RETURN_CD)'  TO  ERR-SECTION OF SQLRT
               PERFORM ZP000-NET-ERROR
           END-IF

           MOVE 'COMPANY'  TO  DATA-NAME OF NETRT
           MOVE 10  TO  DATA-LEN OF NETRT
           SET TYPE-CHAR OF NETRT  TO  TRUE

           CALL 'PTPNETRT' USING   ACTION-GET OF NETRT
                                   NETRT
                                   COMPANY OF W-NET-PARAM
           IF NET-ERROR OF NETRT

               MOVE 'GET-NET-PARAM(COMPANY)'  TO  ERR-SECTION OF SQLRT
               PERFORM ZP000-NET-ERROR
           END-IF

           MOVE 'PAYGROUP'  TO  DATA-NAME OF NETRT
           MOVE 10  TO  DATA-LEN OF NETRT
           SET TYPE-CHAR OF NETRT  TO  TRUE

           CALL 'PTPNETRT' USING   ACTION-GET OF NETRT
                                   NETRT
                                   PAYGROUP OF W-NET-PARAM
           IF NET-ERROR OF NETRT

               MOVE 'GET-NET-PARAM(PAYGROUP)'  TO  ERR-SECTION OF SQLRT
               PERFORM ZP000-NET-ERROR
           END-IF

           MOVE 'PAY_END_DT'  TO  DATA-NAME OF NETRT
           MOVE 10  TO  DATA-LEN OF NETRT
           SET TYPE-DATE OF NETRT  TO  TRUE

           CALL 'PTPNETRT' USING   ACTION-GET OF NETRT
                                   NETRT
                                   PAY-END-DT OF W-NET-PARAM
           IF NET-ERROR OF NETRT

               MOVE 'GET-NET-PARAM(PAY_END_DT)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZP000-NET-ERROR
           END-IF

           MOVE 'EMPLID'  TO  DATA-NAME OF NETRT
           MOVE 20  TO  DATA-LEN OF NETRT
           SET TYPE-CHAR OF NETRT  TO  TRUE

           CALL 'PTPNETRT' USING   ACTION-GET OF NETRT
                                   NETRT
                                   EMPLID OF W-NET-PARAM
           IF NET-ERROR OF NETRT

               MOVE 'GET-NET-PARAM(EMPLID)'  TO  ERR-SECTION OF SQLRT
               PERFORM ZP000-NET-ERROR
           END-IF

           MOVE 'EMPL_RCD'  TO  DATA-NAME OF NETRT
           MOVE 2  TO  DATA-LEN OF NETRT
           SET TYPE-SMALLINT OF NETRT  TO  TRUE

           CALL 'PTPNETRT' USING   ACTION-GET OF NETRT
                                   NETRT
                                   EMPL-RCD-NO OF W-NET-PARAM
           IF NET-ERROR OF NETRT

               MOVE 'GET-NET-PARAM(EMPL_RCD)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZP000-NET-ERROR
           END-IF

           MOVE 'LOAD_TL_PRCS'  TO  DATA-NAME OF NETRT
           MOVE 1   TO  DATA-LEN OF NETRT
           SET TYPE-CHAR OF NETRT  TO  TRUE

           CALL 'PTPNETRT' USING   ACTION-GET OF NETRT
                                   NETRT
                                   LOAD-TL-PRCS OF W-NET-PARAM
           IF NET-ERROR OF NETRT

               MOVE 'GET-NET-PARAM(LOAD_TL_PRCS)'
                   TO  ERR-SECTION OF SQLRT
               PERFORM ZP000-NET-ERROR
           END-IF

           IF LOAD-TL-PRCS-YES OF W-NET-PARAM

               MOVE 'LDTL_RUN_CNTL_ID'  TO  DATA-NAME OF NETRT
               MOVE 30   TO  DATA-LEN OF NETRT
               SET TYPE-CHAR OF NETRT  TO  TRUE

               CALL 'PTPNETRT' USING   ACTION-GET OF NETRT
                                       NETRT
                                       LDTL-RUN-CNTL-ID OF W-NET-PARAM
               IF NET-ERROR OF NETRT

                   MOVE 'GET-NET-PARAM(LDTL-RUN-CNTL-ID)'
                       TO  ERR-SECTION OF SQLRT
                   PERFORM ZP000-NET-ERROR
               END-IF
           END-IF

           MOVE SPACE  TO  RUN-ID OF S-RUNCTL
           MOVE CORR PAYDATA OF W-NET-PARAM  TO  PSLCT

           .
       GET-NET-PARAM-EXIT.


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
       GA000-BUILD-PAYSHEETS SECTION.
       GA000.
      *                                                                *
      ******************************************************************

           ACCEPT TIME-OUT OF W-WK  FROM  TIME
           INSPECT TIME-OUT OF W-WK CONVERTING SPACE TO ':'
           INSPECT TIME-OUT OF W-WK CONVERTING '/' TO '.'
           IF OFF-CYCLE-YES OF PSLCT

               DISPLAY 'PaySheets started for Company: '
                       COMPANY OF W-NET-PARAM
               DISPLAY '                      Pay Group: '
                       PAYGROUP OF W-NET-PARAM
               DISPLAY '                      Pay End Date: '
                       PAY-END-DT OF W-NET-PARAM
               DISPLAY '                      Employee Id: '
                       EMPLID OF W-NET-PARAM
               DISPLAY '                      Employment Rcd No: '
                       EMPL-RCD-NO OF W-NET-PARAM
           ELSE
               DISPLAY 'PaySheets started for Company: '
                       COMPANY OF SELECT-DATA OF S-CAL
               DISPLAY '                      Pay Group: '
                       PAYGROUP OF SELECT-DATA OF S-CAL
               DISPLAY '                      Pay End Date: '
                       PAY-END-DT OF SELECT-DATA OF S-CAL
           END-IF

           DISPLAY ' at ' TIME-OUT OF W-WK
                   '.'

           IF OFF-CYCLE-NO OF PSLCT

               MOVE CORR SELECT-DATA OF S-CAL  TO  PSLCT
           END-IF

           CALL 'PSPPYSHT' USING   NETRT
                                   SQLRT
                                   PSLCT
                                   PARRY
                                   SETAR

           IF RTNCD-ERROR OF SQLRT

               MOVE 'BUILD-PAYSHEETS(PSPPYSHT)'
                        TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           IF OFF-CYCLE-NO OF PSLCT

               PERFORM GD000-UPDATE-PAY-CALENDAR
               MOVE COMPANY OF SELECT-DATA OF S-CAL
                       TO  COMPANY OF BIND-DATA OF S-CAL
               MOVE COMPANY OF SELECT-DATA OF S-CAL
                       TO  COMPANY-GT OF BIND-DATA OF S-CAL
               MOVE PAYGROUP OF SELECT-DATA OF S-CAL
                       TO  PAYGROUP OF BIND-DATA OF S-CAL
               PERFORM GG000-SELECT-PAY-CALENDAR
           END-IF


           .
       BUILD-PAYSHEETS-EXIT.


      /*****************************************************************
      *                                                                *
       GD000-UPDATE-PAY-CALENDAR SECTION.
       GD000.
      *                                                                *
      ******************************************************************

           IF NOT RTNCD-USER OF SQLRT

               MOVE CORR SELECT-DATA OF S-CAL  TO  BIND-DATA OF U-CAL

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
           END-IF

           CALL 'PTPSQLRT' USING   ACTION-COMMIT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
           IF RTNCD-ERROR OF SQLRT

               MOVE 'UPDATE-PAY-CALENDAR(COMMIT)'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           .
       UPDATE-PAY-CALENDAR-EXIT.


      /*****************************************************************
      *                                                                *
       GM000-SELECT-OFF-CALENDAR SECTION.
       GM000.
      *                                                                *
      ******************************************************************

           MOVE CORR PAYDATA OF W-NET-PARAM
                   TO  BIND-DATA OF S-CALOFF

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

                   DISPLAY 'Pay Calendar for Company:'
                           COMPANY OF W-NET-PARAM
                   DISPLAY '                 Pay Group:'
                           PAYGROUP OF W-NET-PARAM
                   DISPLAY '                 Pay End Date:'
                           PAY-END-DT OF W-NET-PARAM
                   DISPLAY 'was not found.'
                   SET RTNCD-USER-ERROR OF SQLRT  TO  TRUE
                   PERFORM ZZ000-SQL-ERROR
               ELSE
                   MOVE 'SELECT-OFF-CALENDAR(FETCH)'
                           TO  ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF
           ELSE

               IF RUN-ID OF S-CALOFF  =  SPACE

                   DISPLAY 'Pay Calendar for Company:'
                           COMPANY OF W-NET-PARAM
                   DISPLAY '                 Pay Group:'
                           PAYGROUP OF W-NET-PARAM
                   DISPLAY '                 Pay End Date:'
                           PAY-END-DT OF W-NET-PARAM
                   DISPLAY '  has no Run id.'
                   SET RTNCD-USER-ERROR OF SQLRT  TO  TRUE
                   PERFORM ZZ000-SQL-ERROR
               ELSE
                   MOVE RUN-ID OF S-CALOFF  TO  RUN-ID OF PSLCT
                   MOVE AGGR-ID OF S-CALOFF  TO  AGGR-ID OF PSLCT
                   IF PAY-OFF-CYCLE-CAL OF S-CALOFF  =  'N'

                       MOVE 'N'  TO  PAY-OFF-CYCLE-CAL OF PSLCT
                   ELSE
                       MOVE 'Y'  TO  PAY-OFF-CYCLE-CAL OF PSLCT
                   END-IF
               END-IF
           END-IF

           .
       SELECT-OFF-CALENDAR-EXIT.


      /*****************************************************************
      *                                                                *
       GG000-SELECT-PAY-CALENDAR SECTION.
       GG000.
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
           END-IF

           .
       SELECT-PAY-CALENDAR-EXIT.


      /*****************************************************************
      *                                                                *
       MA000-COMMIT-OL-PAYSHEET SECTION.
       MA000.
      *                                                                *
      ******************************************************************

           CALL 'PTPSQLRT' USING   ACTION-COMMIT OF SQLRT
                                   SQLRT
                                   SQL-CURSOR-COMMON OF SQLRT
           IF RTNCD-ERROR OF SQLRT

               MOVE 'COMMIT-OL-PAYSHEET'
                       TO  ERR-SECTION OF SQLRT
               PERFORM ZZ000-SQL-ERROR
           END-IF

           IF LOAD-TL-PRCS-YES OF W-NET-PARAM

               INITIALIZE LDTL1
               SET LOADED-NO OF TL-PAYSYS OF LDTL1 TO TRUE
               SET PRCS-OPT-LOAD          OF LDTL1 TO TRUE
               SET REQ-SRC-OL             OF LDTL1 TO TRUE
               MOVE OPRID OF SQLRT TO OPRID OF LDTL1
               MOVE LDTL-RUN-CNTL-ID OF W-NET-PARAM
                   TO RUN-CNTL-ID OF LDTL1

               MOVE EMPLID OF W-NET-PARAM
                   TO BUILD-ONE-EMPLID       OF PSLCT
               MOVE EMPL-RCD-NO OF W-NET-PARAM
                   TO BUILD-ONE-EMPL-RCD-NO  OF PSLCT

               CALL 'PSPLDTLG' USING   SQLRT
                                       PSLCT
                                       SETAR
                                       FCERN
                                       FCRUN
                                       LDTL1

               IF RTNCD-ERROR OF SQLRT

                   MOVE 'CHK EXISTING PAYSHEETS(PSPLDTLG)'
                        TO  ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF
           END-IF

           .
       COMMIT-OL-PAYSHEET-EXIT.


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
           DISPLAY 'PaySheets ended at ' TIME-OUT OF W-WK

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

               MOVE 'RET-SUCCESS'   TO  ERR-SECTION OF SQLRT
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
      * CLEANUP BEFORE TERMINATING                                     *
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


           DISPLAY 'PaySheets did not finish at '
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
      * TERMINATE THE NETWORK                                                 *
      *                                                                *
      ******************************************************************

           CALL 'PTPNETRT' USING   ACTION-DONE OF NETRT
                                   NETRT

           .
       NET-TERMINATE-EXIT.
