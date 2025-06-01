       IDENTIFICATION DIVISION.
       PROGRAM-ID. read_counter.
       AUTHOR. Shiva9361.
       DATE-WRITTEN.  18/05/2025.
       DATE-COMPILED. 18/05/2025.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT COUNTER-FILE ASSIGN TO WS-FILENAME
               ORGANIZATION LINE SEQUENTIAL
               FILE STATUS IS FS-STATUS.

           SELECT NAME-FILE ASSIGN TO WS-NAMEPATH
               ORGANIZATION LINE SEQUENTIAL
               FILE STATUS IS FS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  COUNTER-FILE.
       01  FILE-RECORD PIC 9(12).

       FD  NAME-FILE.
       01  FILE-RECORD-2 PIC 9(12).
              
       WORKING-STORAGE SECTION.
       01  FS-STATUS   PIC XX.
       01  EOF-FLAG    PIC X VALUE 'N'.
           88 EOF       VALUE 'Y'.
           88 NOT-EOF   VALUE 'N'.

       LINKAGE SECTION.
       01  WS-FILENAME PIC X(100).

       01  WS-NAMEPATH PIC X(100).
       01  WS-EXISTS PIC 9(1).
       01  WS-VALUE PIC 9(12).



       PROCEDURE DIVISION USING WS-FILENAME WS-EXISTS WS-NAMEPATH
       WS-VALUE.
           
           OPEN INPUT COUNTER-FILE
      *    
           IF FS-STATUS = "00"
               READ COUNTER-FILE
                   NOT AT END
                       MOVE 1 TO WS-EXISTS
                       MOVE FILE-RECORD TO WS-VALUE
                   AT END
                       MOVE 0 TO WS-EXISTS
           ELSE 
               MOVE 0 TO WS-EXISTS
               
           END-IF

           MOVE SPACES TO WS-FILENAME
      *    
           CLOSE COUNTER-FILE

           OPEN INPUT NAME-FILE

           IF FS-STATUS = "00"
               READ NAME-FILE
                   NOT AT END
                       MOVE FILE-RECORD-2 TO WS-NAMEPATH
                   AT END
                       MOVE spaces TO WS-NAMEPATH
           ELSE 
               MOVE spaces TO WS-NAMEPATH
           end-if

           CLOSE NAME-FILE
                       
           EXIT PROGRAM.
       
           
       