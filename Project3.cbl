      ******************************************************************
      * This program is to resolve Project 3. (Typical Print program)
      *
      * - Read "INVENT3.TXT" file and then create "INVREPRT.TXT" and
      *    "REORDER.TXT".
      *
      ******************************************************************
       IDENTIFICATION              DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                 PROJECT3.
       AUTHOR.                     Byung Seon Kim.
       DATE-WRITTEN.               November 3, 2016. 
       DATE-COMPILED.     
       
      ****************************************************************** 
       ENVIRONMENT                 DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION               SECTION.
       SOURCE-COMPUTER.            ASUS X751.
      *-----------------------------------------------------------------
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
           SELECT  SUPPLIERS-FILE-IN   
                   ASSIGN TO "D:\SUPPLIERS.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
                   
           SELECT  INVENT-FILE-IN   
                   ASSIGN TO "D:\INVENT3.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
                   
           SELECT  INVENT-REPORT-OUT   
                   ASSIGN TO "D:\INVREPRT.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
                   
           SELECT  INVENT-REORDER-OUT        
                   ASSIGN TO "D:\REORDER.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
                   
      ******************************************************************
       DATA                        DIVISION.
      *-----------------------------------------------------------------
       FILE                        SECTION.
       FD  SUPPLIERS-FILE-IN
           RECORD CONTAINS 17 CHARACTERS
           DATA RECORD IS SUPPLIERS-RECORD.
       01  SUPPLIERS-RECORD.
           05  SUPPLIER-ID-S       PIC X(02).
           05  SUPPLIER-NAME-S     PIC X(15).
           
       FD  INVENT-FILE-IN
           RECORD CONTAINS 39 CHARACTERS
           DATA RECORD IS INVENTORY-RECORD.
       01  INVENTORY-RECORD.
           05  PART-NUMBER-I       PIC 9(05).
           05  PART-NAME-I         PIC X(20).
           05  QUANTITY-I          PIC 9(03).
           05  UNIT-PRICE-I        PIC 9(04)V99.
           05  REORDER-POINT-I     PIC 9(03).
           05  SUPPLIER-ID-I       PIC X(02).
           
       FD  INVENT-REPORT-OUT
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS INVENTORY-OUT.
       01  INVENTORY-OUT               PIC X(53).
       
       FD  INVENT-REORDER-OUT
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS REORDER-OUT.
       01  REORDER-OUT                 PIC X(49).
      *-----------------------------------------------------------------     
       WORKING-STORAGE             SECTION.
      *-----------------------------------------------------------------
      *    This record is for getting the name of month.
       01  MONTH-RECORD.
           05  FILLER              PIC X(03) VALUE "JAN".
           05  FILLER              PIC X(03) VALUE "FEB".
           05  FILLER              PIC X(03) VALUE "MAR".
           05  FILLER              PIC X(03) VALUE "APR".
           05  FILLER              PIC X(03) VALUE "MAY".
           05  FILLER              PIC X(03) VALUE "JUN".
           05  FILLER              PIC X(03) VALUE "JUL".
           05  FILLER              PIC X(03) VALUE "AUG".
           05  FILLER              PIC X(03) VALUE "SEP".
           05  FILLER              PIC X(03) VALUE "OCT".
           05  FILLER              PIC X(03) VALUE "NOV".
           05  FILLER              PIC X(03) VALUE "DEC".
       01  MONTH-TABLE             REDEFINES MONTH-RECORD.
           05 MONTH                PIC X(03) OCCURS 12 TIMES.
      
      *    This table is for saving of Supplier Records     
       01  SUPPLIER-TABLE.
           05  SUPPLIER-TABLE-LEN   PIC 9(03).
           05  SUPPLIER-TAB        OCCURS 1 TO 999
                                   DEPENDING ON SUPPLIER-TABLE-LEN.
               10  SUPPLIER-ID     PIC X(02).
               10  SUPPLIER-NAME   PIC X(15).
      
      *    This record is for printing the title of inventory report.
       01  INVENT-TITLE.
           05  FILLER              PIC X(09) VALUE SPACES.
           05  FILLER              PIC X(17) VALUE "INVENTORY REPORT".
           05  MONTH-NAME          PIC X(04).
           05  YEAR                PIC 9(04).
           
      *    This record is for printing the header of inventory report.
       01  INVENT-HEADER.
           05  FILLER              PIC X(01) VALUE SPACES.
           05  FILLER              PIC X(08) VALUE "PART NO".
           05  FILLER              PIC X(22) VALUE "PART NAME".
           05  FILLER              PIC X(05) VALUE " OH".
           05  FILLER              PIC X(08) VALUE "PRICE".
           05  FILLER              PIC X(08) VALUE "   VALUE".
      
      *    This record is for printing the detail of inventory report.
       01  INVENT-DETAIL.
           05  FILLER              PIC X(01) VALUE SPACES.
           05  PART-NUMBER-O       PIC X(05).
           05  FILLER              PIC X(03) VALUE SPACES.
           05  PART-NAME-O         PIC X(20).
           05  FILLER              PIC X(01) VALUE SPACES.
           05  QUANTITY-O          PIC ZZZ9.
           05  FILLER              PIC X(01) VALUE SPACES.
           05  UNIT-PRICE-O        PIC ZZ9.99.
           05  FILLER              PIC X(02) VALUE SPACES.
           05  VALUE-O             PIC $$$,$$9.99.
           
      *    This record is for printing the total of inventory report.
       01  INVENT-TOTAL.
           05  FILLER              PIC X(02) VALUE SPACES.
           05  FILLER              PIC X(13) VALUE "TOTAL VALUE".
           05  TOTAL               PIC $$$$,$$9.99.
       
      *    This record is for print the footer of inventory report.
       01  INVENT-FOOTER.   
           05  FILLER              PIC X(02) VALUE SPACES.
           05  FOOTER-NAME         PIC X(15).
           05  FOOTER-COUNTER      PIC ZZZ9.
           
      *    This record is for print the title of reorder report.
       01  REORDER-TITLE.
           05  FILLER              PIC X(01) VALUE SPACES.
           05  FILLER              PIC X(20) VALUE "REORDER REPORT".
      
      *    This record is for printing the header of reorder report.
       01  REORDER-HEADER.
           05  FILLER              PIC X(01) VALUE SPACES.
           05  FILLER              PIC X(08) VALUE "PART NO".
           05  FILLER              PIC X(21) VALUE "PART NAME".
           05  FILLER              PIC X(04) VALUE "CSL".
           05  FILLER              PIC X(15) VALUE "SUPPLIER NAME".
      
      *    This record is for printing the detail of reorder report.
       01  REORDER-DETAIL.
           05  FILLER              PIC X(01) VALUE SPACES.
           05  PART-NUMBER-R       PIC X(05).
           05  FILLER              PIC X(03) VALUE SPACES.
           05  PART-NAME-R         PIC X(20).
           05  FILLER              PIC X(01) VALUE SPACES.
           05  REORDER-POINT-R     PIC ZZ9.
           05  FILLER              PIC X(01) VALUE SPACES.
           05  SUPPLIER-NAME-R     PIC X(15).

       01  SWITCHES-AND-COUNTERS.
           05  EOF-SW              PIC X(01) VALUE "N".
           05  FOUND-SW            PIC X(01) VALUE "N".
           05  RCNT                PIC 9(03) VALUE ZEROS.
           05  WCNT                PIC 9(03).
           05  LCNT                PIC 9(02).
           
       01  ACCUMULATORS.
           05  GRAND-TOTAL         PIC 9(09)V99 VALUE ZEROS.
       
       01  CURRENT-DATE.
           05  CUR-YEAR            PIC 9(04).
           05  CUR-MONTH           PIC 9(02).
           05  FILLER              PIC 9(02).
       
       01  MISCELLONIOUS.
           05  INVENT-VALUE        PIC 9(05)V99.
           05  IDX                 PIC 9(04).
           
      ******************************************************************
       PROCEDURE                   DIVISION.
      *-----------------------------------------------------------------
      * Main procedure
      *-----------------------------------------------------------------
       100-PRINT-INVENTORY-REPORTS.
           PERFORM 200-INITIATE-INVENTORY-REPORTS.
           PERFORM 200-PRINT-INVENTORY-REPORTS UNTIL EOF-SW = "Y".
           PERFORM 200-TERMINATE-INVENTORY-REPORTS.
           STOP RUN.
           
      ******************************************************************     
      * Open input & print file, initialize variables that used as 
      * switch and for sum, print report headers, and 
      * read the very first record from input file.
      *-----------------------------------------------------------------
       200-INITIATE-INVENTORY-REPORTS.
           PERFORM 300-BUILD-SUPPLIER-TABLE.
           PERFORM 300-OPEN-INVENTORY-FILES.
           PERFORM 300-INITIALIZE-SWITCHES-AND-COUNTERS.
           PERFORM 300-READ-INVENTORY-FILE.
           PERFORM 300-PRINT-REPORT-TITLES.
           PERFORM 300-PRINT-REPORT-HEADERS.
           
      *-----------------------------------------------------------------
      * Print one inventory record and reorder record as given format 
      *  and read next record. when reorder is printed, only prit if 
      *  quanty is less than or equal to re-order point
      *-----------------------------------------------------------------
       200-PRINT-INVENTORY-REPORTS.
           PERFORM 300-COMPUTE-INVENTORY-VALUE.
           IF  LCNT > 10
               PERFORM 300-INVENT-REPORT-PAGESKIP.
           PERFORM 300-PRINT-INVENTORY-DETAIL.
           PERFORM 300-COMPUTE-GRAND-TOTAL.
           IF QUANTITY-I NOT GREATER THAN REORDER-POINT-I
               PERFORM 300-INITIALIZE-BEFORE-SEARCH-SUPPLIER
               PERFORM 300-SEARCH-SUPPLIER
                   VARYING IDX FROM 1 BY 1 
                       UNTIL IDX > SUPPLIER-TABLE-LEN OR FOUND-SW = "Y"
               PERFORM 300-PRINT-REORDER-DETAIL.
           PERFORM 300-READ-INVENTORY-FILE.
           
      *-----------------------------------------------------------------
      * after printed all records, total and footer should be printed.
      * after that close all files.
      *-----------------------------------------------------------------
       200-TERMINATE-INVENTORY-REPORTS.
           PERFORM 300-PRINT-INVENT-REPORT-TOTAL.
           PERFORM 300-PRINT-INVENT-REPORT-FOOTER.
           PERFORM 300-CLOSE-INVENTORY-FILES.
           
      ******************************************************************
      * while reading SUPPLIERS.txt file, build suppliers table. 
      *-----------------------------------------------------------------
       300-BUILD-SUPPLIER-TABLE.
           PERFORM 400-OPEN-SUPPLIERS-FILE-IN.
           PERFORM 400-READ-SUPPLIERS-FILE-IN.
           PERFORM 400-BUILD-SUPPLIER-TABLE
                   UNTIL EOF-SW = "Y".
           PERFORM 400-CLOSE-SUPPLIERS-FILE-IN.
           
      *-----------------------------------------------------------------
      * open input file and output file to print                                          
      *-----------------------------------------------------------------
       300-OPEN-INVENTORY-FILES.
           OPEN    INPUT   INVENT-FILE-IN
                   OUTPUT  INVENT-REPORT-OUT
                   OUTPUT  INVENT-REORDER-OUT.
      
      *-----------------------------------------------------------------
      * initialize variables
      *-----------------------------------------------------------------
       300-INITIALIZE-SWITCHES-AND-COUNTERS.
           INITIALIZE SWITCHES-AND-COUNTERS.
      
      *----------------------------------------------------------------- 
      * read a input record. if eof then set EOF-SW as 'Yes'
      * if not, add 1 to read record count
      *-----------------------------------------------------------------
       300-READ-INVENTORY-FILE.
           READ INVENT-FILE-IN
                   AT END      MOVE "Y" TO EOF-SW
                   NOT AT END  ADD 1 TO RCNT
                               ADD 1 TO LCNT.

      *-----------------------------------------------------------------
      * print title of inventory report and reorder report. 
      *-----------------------------------------------------------------
       300-PRINT-REPORT-TITLES.
           PERFORM 400-PRINT-INVENT-REPORT-TITLE.
           PERFORM 400-PRINT-REORDER-REPORT-TITLE.
      
      *-----------------------------------------------------------------
      * print header of inventory report and reorder report. 
      *-----------------------------------------------------------------
       300-PRINT-REPORT-HEADERS.
           PERFORM 400-PRINT-INVENT-REPORT-HEADER.
           PERFORM 400-PRINT-REORDER-REPORT-HEADER.
                                                                        
      *-----------------------------------------------------------------
      * print header of inventory report and reorder report. 
      *-----------------------------------------------------------------
       300-COMPUTE-INVENTORY-VALUE.    
           COMPUTE INVENT-VALUE = QUANTITY-I * UNIT-PRICE-I.
        
      *-----------------------------------------------------------------
      * After printing every 10 records, skip a page.  
      *-----------------------------------------------------------------                                 
       300-INVENT-REPORT-PAGESKIP.
           PERFORM 400-PAGE-SKIP.
           PERFORM 400-PRINT-INVENT-REPORT-HEADER.
           MOVE    ZEROS       TO  LCNT.
           
      *-----------------------------------------------------------------
      * print inventory detail. After printed, add 1 to write counter
      * and add amount to total amount.
      *-----------------------------------------------------------------
       300-PRINT-INVENTORY-DETAIL.
           MOVE    PART-NUMBER-I       TO  PART-NUMBER-O.
           MOVE    PART-NAME-I         TO  PART-NAME-O.
           MOVE    QUANTITY-I          TO  QUANTITY-O.
           MOVE    UNIT-PRICE-I        TO  UNIT-PRICE-O.
           MOVE    INVENT-VALUE        TO  VALUE-O.
           WRITE   INVENTORY-OUT       FROM    INVENT-DETAIL.
           ADD     1                   TO  WCNT.
           
      *-----------------------------------------------------------------
      * print header of inventory report and reorder report. 
      *-----------------------------------------------------------------
       300-COMPUTE-GRAND-TOTAL.    
           ADD INVENT-VALUE TO GRAND-TOTAL.
      
      *----------------------------------------------------------------- 
      * print RE-order detail print  
      *-----------------------------------------------------------------
       300-PRINT-REORDER-DETAIL.
           MOVE    PART-NUMBER-I   TO  PART-NUMBER-R.
           MOVE    PART-NAME-I     TO  PART-NAME-R.
           MOVE    REORDER-POINT-I TO  REORDER-POINT-R.
           WRITE   REORDER-OUT     FROM    REORDER-DETAIL.

      *----------------------------------------------------------------- 
      * close files  
      *-----------------------------------------------------------------
       300-CLOSE-INVENTORY-FILES.
           CLOSE INVENT-FILE-IN
                 INVENT-REPORT-OUT
                 INVENT-REORDER-OUT.

      *----------------------------------------------------------------- 
      * initialize before searching supplier name using id.  
      *-----------------------------------------------------------------
       300-INITIALIZE-BEFORE-SEARCH-SUPPLIER.
           MOVE    SPACES  TO  SUPPLIER-NAME-R.
           MOVE    "N"     TO  FOUND-SW.
       
      *----------------------------------------------------------------- 
      * in order to search supplier. 
      *-----------------------------------------------------------------
       300-SEARCH-SUPPLIER.
           IF  SUPPLIER-ID-I = SUPPLIER-ID(IDX)
               MOVE    "Y"         TO  FOUND-SW
               MOVE    SUPPLIER-NAME(IDX)  TO  SUPPLIER-NAME-R.

      *----------------------------------------------------------------- 
      * print total of inventory report. 
      *-----------------------------------------------------------------
       300-PRINT-INVENT-REPORT-TOTAL.
           MOVE    GRAND-TOTAL     TO TOTAL.
           WRITE   INVENTORY-OUT   FROM    INVENT-TOTAL
                   AFTER ADVANCING 4   LINES.

      *----------------------------------------------------------------- 
      * print footers of inventory report 
      *-----------------------------------------------------------------
       300-PRINT-INVENT-REPORT-FOOTER.
           MOVE    "RECORDSREAD"       TO  FOOTER-NAME.
           MOVE    RCNT                TO  FOOTER-COUNTER.
           WRITE   INVENTORY-OUT   FROM    INVENT-FOOTER
                   AFTER ADVANCING 2   LINES.
           MOVE    "RECORDSWRITTEN"    TO  FOOTER-NAME.
           MOVE    WCNT                TO  FOOTER-COUNTER.
           WRITE   INVENTORY-OUT   FROM    INVENT-FOOTER.
           
      ******************************************************************
      * open SUPPLIERS-FILE-IN file. 
      *-----------------------------------------------------------------
       400-OPEN-SUPPLIERS-FILE-IN.
           OPEN  INPUT SUPPLIERS-FILE-IN.
      
      *-----------------------------------------------------------------
      * read SUPPLIERS-FILE-IN file. 
      *-----------------------------------------------------------------
       400-READ-SUPPLIERS-FILE-IN.
           READ  SUPPLIERS-FILE-IN 
                   AT END      MOVE "Y" TO EOF-SW
                   NOT AT END  ADD 1 TO RCNT.
           
      *-----------------------------------------------------------------
      * fill SUPPLIER-TABLE from SUPPLIER-RECORD. 
      *-----------------------------------------------------------------
       400-BUILD-SUPPLIER-TABLE.
           MOVE    RCNT            TO  SUPPLIER-TABLE-LEN.
           MOVE    SUPPLIER-ID-S   TO  SUPPLIER-ID(RCNT).               
           MOVE    SUPPLIER-NAME-S TO  SUPPLIER-NAME(RCNT).
           PERFORM 400-READ-SUPPLIERS-FILE-IN.
           
      *-----------------------------------------------------------------
      * close SUPPLIERS-FILE-IN. 
      *-----------------------------------------------------------------
       400-CLOSE-SUPPLIERS-FILE-IN.
           CLOSE SUPPLIERS-FILE-IN.
      
      *-----------------------------------------------------------------
      * print title of inventory report. 
      *-----------------------------------------------------------------
       400-PRINT-INVENT-REPORT-TITLE.  
           ACCEPT  CURRENT-DATE        FROM DATE YYYYMMDD.
           MOVE    CUR-YEAR            TO  YEAR.
           MOVE    MONTH(CUR-MONTH)    TO  MONTH-NAME.
           WRITE   INVENTORY-OUT       FROM    INVENT-TITLE
                   AFTER ADVANCING 1   LINES.
           
      *-----------------------------------------------------------------
      * print title of reorder report. 
      *-----------------------------------------------------------------
       400-PRINT-REORDER-REPORT-TITLE.
           WRITE   REORDER-OUT         FROM    REORDER-TITLE
                   AFTER ADVANCING 1   LINES.
      
      *-----------------------------------------------------------------    
      * print header of inventory report. 
      *-----------------------------------------------------------------
       400-PRINT-INVENT-REPORT-HEADER.
           WRITE   INVENTORY-OUT       FROM INVENT-HEADER
                   AFTER ADVANCING 3   LINES.
           MOVE    SPACES      TO INVENTORY-OUT.
           WRITE   INVENTORY-OUT.
           
      *-----------------------------------------------------------------
      * print header of RE-ORDER report.  
      *-----------------------------------------------------------------                                 
       400-PRINT-REORDER-REPORT-HEADER.
           WRITE   REORDER-OUT         FROM REORDER-HEADER
                   AFTER ADVANCING 2   LINES.
           MOVE    SPACES       TO REORDER-OUT.
           WRITE   REORDER-OUT.
        
      *-----------------------------------------------------------------
      * empty print after page.  
      *-----------------------------------------------------------------                                 
       400-PAGE-SKIP.
           MOVE    SPACES      TO  INVENTORY-OUT.
           WRITE   INVENTORY-OUT   AFTER ADVANCING PAGE.

           
