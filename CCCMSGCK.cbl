      ******************************************************************
      *                                                                *
      * CCCMSGCK - Check error messages for payroll run                *
      *                                                                *
      ******************************************************************
      ******************************************************************
      *                                                                *
      * Description: Checks if any error messages were generated for   *
      *              this payroll run and sets the run-status for the  *
      *              process scheduler accordingly.                    *
      *                                                                *
      *                                                                *
      ******************************************************************



    
           IF OFF-CYCLE-NO OF PSLCT

               MOVE CORR PSLCT  TO  BIND-DATA OF S-RTNCD

               CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                       SQLRT
                                       SQL-CURSOR-COMMON OF SQLRT
                                       SQL-STMT OF S-RTNCD
                                       BIND-SETUP OF S-RTNCD
                                       BIND-DATA OF S-RTNCD
                                       SELECT-SETUP OF S-RTNCD
                                       SELECT-DATA OF S-RTNCD
               IF RTNCD-ERROR OF SQLRT
  
                   MOVE 'CHECK MESSAGES RTNCD(SELECT)'
                           TO  ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF
  
               INITIALIZE SELECT-DATA OF S-RTNCD
               CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                       SQLRT
                                       SQL-CURSOR-COMMON OF SQLRT
               IF RTNCD-ERROR OF SQLRT
 
                   IF RTNCD-END OF SQLRT

                       SET RTNCD-OK OF SQLRT  TO  TRUE
                       SET RUN-STATUS-SUCCESSFUL OF USTAT  TO  TRUE
                   ELSE
                       MOVE 'CHECK MESSAGES RTNCD(SELECT)'
                               TO  ERR-SECTION OF SQLRT
                       PERFORM ZZ000-SQL-ERROR
                   END-IF
               ELSE
                   SET RUN-STATUS-UNSUCCESSFUL OF USTAT  TO  TRUE
                   SET CONTINUE-JOB-NO OF USTAT          TO  TRUE
               END-IF
           ELSE

               MOVE CORR PSLCT  TO  BIND-DATA OF S-RTNCDOFF

               CALL 'PTPSQLRT' USING   ACTION-SELECT OF SQLRT
                                       SQLRT
                                       SQL-CURSOR-COMMON OF SQLRT
                                       SQL-STMT OF S-RTNCDOFF
                                       BIND-SETUP OF S-RTNCDOFF
                                       BIND-DATA OF S-RTNCDOFF
                                       SELECT-SETUP OF S-RTNCDOFF
                                       SELECT-DATA OF S-RTNCDOFF
               IF RTNCD-ERROR OF SQLRT
  
                   MOVE 'CHECK MESSAGES RTNCDOFF(SELECT)'
                           TO  ERR-SECTION OF SQLRT
                   PERFORM ZZ000-SQL-ERROR
               END-IF
  
               INITIALIZE SELECT-DATA OF S-RTNCDOFF
               CALL 'PTPSQLRT' USING   ACTION-FETCH OF SQLRT
                                       SQLRT
                                       SQL-CURSOR-COMMON OF SQLRT
               IF RTNCD-ERROR OF SQLRT
 
                   IF RTNCD-END OF SQLRT

                       SET RTNCD-OK OF SQLRT  TO  TRUE
                       SET RUN-STATUS-SUCCESSFUL OF USTAT  TO  TRUE
                   ELSE
                       MOVE 'CHECK MESSAGES RTNCDOFF(SELECT)'
                               TO  ERR-SECTION OF SQLRT
                       PERFORM ZZ000-SQL-ERROR
                   END-IF
               ELSE
                   SET RUN-STATUS-UNSUCCESSFUL OF USTAT  TO  TRUE
                   SET CONTINUE-JOB-NO OF USTAT          TO  TRUE
               END-IF
           END-IF

