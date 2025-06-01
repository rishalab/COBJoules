       IDENTIFICATION DIVISION.
       PROGRAM-ID. measure_energy.
       AUTHOR. Shiva9361.
       DATE-WRITTEN.  18/05/2025.
       DATE-COMPILED. 18/05/2025.

       DATA DIVISION.
           WORKING-STORAGE SECTION.

           01 ENERGY.
           05  ENERGY-DATA OCCURS 2 TIMES.
               10  DOMAIN-COUNT          PIC 9(2) COMP.
               10  DOMAIN-ENTRY OCCURS 20 TIMES
                                   INDEXED BY DOM-IDX.
                            
                   15  DOMAIN-NAME       PIC X(10).
                   15  DOMAIN-VALUE      PIC 9(12) COMP.
           01 FLAG PIC 9(1) VALUE 0.
           01 TEMP PIC 9(12) COMP.

       PROCEDURE DIVISION.

           IF FLAG = 0
               CALL "fetch_energy" USING ENERGY-DATA(1)
               MOVE 1 TO FLAG
           ELSE 
               CALL "fetch_energy" USING ENERGY-DATA(2)

               PERFORM VARYING DOM-IDX FROM 1 BY 1
                   UNTIL DOM-IDX > DOMAIN-COUNT(1)
                   MOVE DOMAIN-VALUE(2,DOM-IDX) TO TEMP
                   COMPUTE TEMP = TEMP - DOMAIN-VALUE(1,DOM-IDX)

                   DISPLAY "Domain: " DOMAIN-NAME(1,DOM-IDX) 
                       " Energy: " TEMP
               END-PERFORM
               MOVE 0 TO FLAG
           END-IF

           GOBACK.         
       
       EXIT PROGRAM.
       