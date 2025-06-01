       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01 WS-I PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.

       CALL 'measure_energy'.

        PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 50000
         CONTINUE
        END-PERFORM

       CALL 'measure_energy'.

       STOP RUN.
