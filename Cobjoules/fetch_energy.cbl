       IDENTIFICATION DIVISION.
       PROGRAM-ID. fetch_energy.
       AUTHOR. Shiva9361.
       DATE-WRITTEN.  18/05/2025.
       DATE-COMPILED. 18/05/2025.
       
       DATA DIVISION.

       WORKING-STORAGE SECTION.    
       01  WS-PATH PIC X(42) VALUE 
       "/sys/class/powercap/intel-rapl/intel-rapl:".
       
       01  WS-FILENAME PIC X(100) VALUE SPACES.
       01  WS-NAMEPATH PIC X(100) VALUE SPACES.
       01  WS-EXISTS PIC 9(1) VALUE 0.
       01  WS-VALUE PIC 9(12) VALUE 0.
       
       01  WS-CORE PIC 9(1) VALUE 0.
       01  WS-SUBMOD PIC 9(1) VALUE 0.
       
       LINKAGE SECTION.
       01  DOMAIN-DATA.
           05  DOMAIN-COUNT          PIC 9(2) COMP.
           05  DOMAIN-ENTRY OCCURS 20 TIMES 
                             INDEXED BY DOM-IDX.
               10  DOMAIN-NAME       PIC X(10).
               10  DOMAIN-VALUE      PIC 9(12) COMP.

       PROCEDURE DIVISION USING DOMAIN-DATA.
       
       MOVE 0 TO DOMAIN-COUNT.
       MOVE 0 TO WS-CORE.
       MOVE 0 TO WS-SUBMOD.
      
       SET DOM-IDX TO 1.
       FetchCore.
           
           STRING WS-PATH DELIMITED BY SIZE 
                  WS-CORE DELIMITED BY SIZE 
                  "/energy_uj" DELIMITED BY SIZE
               INTO WS-FILENAME
           
           STRING WS-PATH DELIMITED BY SIZE 
                  WS-CORE DELIMITED BY SIZE 
                  "/name" DELIMITED BY SIZE
               INTO WS-NAMEPATH
                             
           CALL "read_counter" USING WS-FILENAME WS-EXISTS WS-NAMEPATH
           WS-VALUE
           
           IF WS-EXISTS = 0
               MOVE spaces TO WS-NAMEPATH
               PERFORM Exitpath
           ELSE 
               MOVE WS-NAMEPATH TO DOMAIN-NAME (DOM-IDX)
               MOVE WS-VALUE TO DOMAIN-VALUE (DOM-IDX)
               
               SET DOM-IDX UP BY 1
               ADD 1 TO DOMAIN-COUNT
               
               MOVE spaces TO WS-NAMEPATH
               PERFORM FetchSubDomains
           END-IF.
       
       FetchSubDomains.

           STRING WS-PATH DELIMITED BY SIZE 
                  WS-CORE DELIMITED BY SIZE 
                  '/intel-rapl:' DELIMITED BY SIZE
                  WS-CORE DELIMITED BY SIZE
                  ':' DELIMITED BY SIZE
                  WS-SUBMOD DELIMITED BY SIZE
                  "/energy_uj" DELIMITED BY SIZE
               INTO WS-FILENAME
           
           STRING WS-PATH DELIMITED BY SIZE 
                  WS-CORE DELIMITED BY SIZE 
                  '/intel-rapl:' DELIMITED BY SIZE
                  WS-CORE DELIMITED BY SIZE
                  ':' DELIMITED BY SIZE
                  WS-SUBMOD DELIMITED BY SIZE
                  "/name" DELIMITED BY SIZE
               INTO WS-NAMEPATH
           
           CALL "read_counter" USING WS-FILENAME WS-EXISTS WS-NAMEPATH
           WS-VALUE
           
           IF WS-EXISTS = 0
               MOVE 0 TO WS-SUBMOD 
               MOVE spaces TO WS-NAMEPATH
               COMPUTE WS-CORE = WS-CORE + 1
               PERFORM FetchCore
           ELSE
               STRING WS-NAMEPATH DELIMITED BY space
                   '-' DELIMITED BY SIZE 
                   WS-CORE DELIMITED BY SIZE
                   INTO WS-NAMEPATH
               
               MOVE WS-NAMEPATH TO DOMAIN-NAME (DOM-IDX)
               MOVE WS-VALUE TO DOMAIN-VALUE (DOM-IDX)
               
               SET DOM-IDX UP BY 1
               ADD 1 TO DOMAIN-COUNT
               
               COMPUTE WS-SUBMOD = WS-SUBMOD + 1
               MOVE spaces TO WS-NAMEPATH
               PERFORM FetchSubDomains
           END-IF.

       Exitpath.
           
           CONTINUE

       EXIT PROGRAM.
           
