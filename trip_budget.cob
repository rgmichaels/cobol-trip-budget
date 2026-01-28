>>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRIPBUDGET.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EXPENSE-FILE
               ASSIGN TO DYNAMIC WS-INPUT-FILE
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-EXPENSE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  EXPENSE-FILE.
       01  EXPENSE-LINE               PIC X(200).

       WORKING-STORAGE SECTION.
       01 WS-DATE-TEXT                PIC X(10).
       01 WS-DATE                     PIC X(10).

       01 WS-MIN-DATE                 PIC X(10) VALUE HIGH-VALUES.
       01 WS-MAX-DATE                 PIC X(10) VALUE LOW-VALUES.

       01 WS-DATE-VALID               PIC X VALUE 'N'.
          88 DATE-OK                  VALUE 'Y'.
          88 DATE-BAD                 VALUE 'N'.

       01 WS-DISPLAY-MIN-DATE         PIC X(10) VALUE SPACES.
       01 WS-DISPLAY-MAX-DATE         PIC X(10) VALUE SPACES.

       01 TRIP-NAME                   PIC X(30) VALUE "NE Sprint Loop".
       01 RIDER-NAME                  PIC X(20) VALUE "Rob".

       01 WS-INPUT-FILE               PIC X(256) VALUE SPACES.
       01 WS-CMDLINE                  PIC X(256) VALUE SPACES.
       01 WS-EXPENSE-STATUS           PIC XX     VALUE "00".

       01 WS-EOF                      PIC X VALUE 'N'.
          88 EOF                      VALUE 'Y'.
          88 NOT-EOF                  VALUE 'N'.

       01 WS-LINE-NUM                 PIC 9(5) VALUE 0.
       01 WS-GOOD-COUNT               PIC 9(5) VALUE 0.
       01 WS-BAD-COUNT                PIC 9(5) VALUE 0.

       01 WS-CATEGORY                 PIC X(30).
       01 WS-AMOUNT-TEXT              PIC X(30).

       01 WS-AMOUNT                   PIC 9(6)V99 VALUE 0.
       01 TOTAL-AMOUNT                PIC 9(7)V99 VALUE 0.
       01 MAX-AMOUNT                  PIC 9(6)V99 VALUE 0.
       01 AVG-AMOUNT                  PIC 9(7)V99 VALUE 0.

       01 WS-AMOUNT-NUMVAL            PIC 9(7)V99 VALUE 0.

       01 WS-DISPLAY-AMT              PIC Z(6)9.99.
       01 WS-DISPLAY-TOTAL            PIC Z(7)9.99.
       01 WS-DISPLAY-AVG              PIC Z(7)9.99.
       01 WS-DISPLAY-MAX              PIC Z(6)9.99.

       01 CAT-MAX                     PIC 9(2) VALUE 20.
       01 CAT-COUNT                   PIC 9(2) VALUE 0.

       01 CAT-TABLE.
          05 CAT-ENTRY OCCURS 20 TIMES.
             10 CAT-NAME              PIC X(30) VALUE SPACES.
             10 CAT-SUM               PIC 9(7)V99 VALUE 0.

       01 WS-CAT-IDX                  PIC 9(2) VALUE 1.
       01 WS-FOUND-IDX                PIC 9(2) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           *> Command-line file input (defaults to data/expenses.csv)
           ACCEPT WS-CMDLINE FROM COMMAND-LINE

           IF FUNCTION TRIM(WS-CMDLINE) = SPACES
               MOVE "data/expenses.csv" TO WS-INPUT-FILE
           ELSE
               MOVE FUNCTION TRIM(WS-CMDLINE) TO WS-INPUT-FILE
           END-IF

           DISPLAY "===============================".
           DISPLAY " COBOL TRIP BUDGET REPORT (FILE)".
           DISPLAY "===============================".
           DISPLAY "Trip : " FUNCTION TRIM(TRIP-NAME).
           DISPLAY "Rider: " FUNCTION TRIM(RIDER-NAME).
           DISPLAY "File : " FUNCTION TRIM(WS-INPUT-FILE).
           DISPLAY " ".

           OPEN INPUT EXPENSE-FILE
           IF WS-EXPENSE-STATUS NOT = "00"
               DISPLAY "ERROR: Could not open file -> "
                       FUNCTION TRIM(WS-INPUT-FILE)
               DISPLAY "FILE STATUS: " WS-EXPENSE-STATUS
               STOP RUN
           END-IF

           PERFORM UNTIL EOF
               READ EXPENSE-FILE
                   AT END
                       SET EOF TO TRUE
                   NOT AT END
                       PERFORM PROCESS-LINE
               END-READ
           END-PERFORM
           CLOSE EXPENSE-FILE

           IF WS-GOOD-COUNT > 0
               COMPUTE AVG-AMOUNT = TOTAL-AMOUNT / WS-GOOD-COUNT
           END-IF

           MOVE TOTAL-AMOUNT TO WS-DISPLAY-TOTAL
           MOVE AVG-AMOUNT   TO WS-DISPLAY-AVG
           MOVE MAX-AMOUNT   TO WS-DISPLAY-MAX

           IF WS-GOOD-COUNT > 0
               MOVE WS-MIN-DATE TO WS-DISPLAY-MIN-DATE
               MOVE WS-MAX-DATE TO WS-DISPLAY-MAX-DATE
           END-IF

           DISPLAY " ".
           DISPLAY "-------------------------------".
           DISPLAY "Items processed: " WS-GOOD-COUNT.
           DISPLAY "Bad lines      : " WS-BAD-COUNT.

           IF WS-GOOD-COUNT > 0
               DISPLAY "Dates          : " WS-DISPLAY-MIN-DATE
                       " to " WS-DISPLAY-MAX-DATE
           ELSE
               DISPLAY "Dates          : (none)"
           END-IF

           DISPLAY "Total          : $" WS-DISPLAY-TOTAL.
           DISPLAY "Avg            : $" WS-DISPLAY-AVG.
           DISPLAY "Max            : $" WS-DISPLAY-MAX.
           DISPLAY "-------------------------------".

           PERFORM PRINT-CATEGORY-REPORT

           STOP RUN.

       PROCESS-LINE.
           ADD 1 TO WS-LINE-NUM

           *> Skip blank lines
           IF FUNCTION LENGTH(FUNCTION TRIM(EXPENSE-LINE)) = 0
               EXIT PARAGRAPH
           END-IF

           *> Split "YYYY-MM-DD,Category,Amount"
           MOVE SPACES TO WS-DATE-TEXT
           MOVE SPACES TO WS-CATEGORY
           MOVE SPACES TO WS-AMOUNT-TEXT

           UNSTRING EXPENSE-LINE
               DELIMITED BY ","
               INTO WS-DATE-TEXT WS-CATEGORY WS-AMOUNT-TEXT
           END-UNSTRING

           MOVE FUNCTION TRIM(WS-DATE-TEXT)   TO WS-DATE-TEXT
           MOVE FUNCTION TRIM(WS-CATEGORY)    TO WS-CATEGORY
           MOVE FUNCTION TRIM(WS-AMOUNT-TEXT) TO WS-AMOUNT-TEXT

           *> Basic validation: must have all 3 fields
           IF WS-DATE-TEXT = SPACES OR WS-CATEGORY = SPACES
              OR WS-AMOUNT-TEXT = SPACES
               ADD 1 TO WS-BAD-COUNT
               DISPLAY "WARN line " WS-LINE-NUM ": bad format -> "
                       FUNCTION TRIM(EXPENSE-LINE)
               EXIT PARAGRAPH
           END-IF

           *> Validate date (structure only: YYYY-MM-DD)
           PERFORM VALIDATE-DATE
           IF DATE-BAD
               ADD 1 TO WS-BAD-COUNT
               DISPLAY "WARN line " WS-LINE-NUM ": bad date -> "
                       FUNCTION TRIM(EXPENSE-LINE)
               EXIT PARAGRAPH
           END-IF

           MOVE WS-DATE-TEXT TO WS-DATE

           *> Convert text amount to numeric (handles "10.50")
           COMPUTE WS-AMOUNT-NUMVAL = FUNCTION NUMVAL(WS-AMOUNT-TEXT)

           *> Reject non-positive or suspicious values
           IF WS-AMOUNT-NUMVAL <= 0
               ADD 1 TO WS-BAD-COUNT
               DISPLAY "WARN line " WS-LINE-NUM ": bad amount -> "
                       FUNCTION TRIM(EXPENSE-LINE)
               EXIT PARAGRAPH
           END-IF

           MOVE WS-AMOUNT-NUMVAL TO WS-AMOUNT

           *> At this point, the record is accepted
           ADD 1 TO WS-GOOD-COUNT
           ADD WS-AMOUNT TO TOTAL-AMOUNT

           IF WS-AMOUNT > MAX-AMOUNT
               MOVE WS-AMOUNT TO MAX-AMOUNT
           END-IF

           *> Track min/max date (ISO string compare works)
           IF WS-DATE < WS-MIN-DATE
               MOVE WS-DATE TO WS-MIN-DATE
           END-IF
           IF WS-DATE > WS-MAX-DATE
               MOVE WS-DATE TO WS-MAX-DATE
           END-IF

           PERFORM UPDATE-CATEGORY-TOTAL

           *> Echo accepted line (with date)
           MOVE WS-AMOUNT TO WS-DISPLAY-AMT
           DISPLAY WS-DATE "  " FUNCTION TRIM(WS-CATEGORY)
                   ": $" WS-DISPLAY-AMT.

       VALIDATE-DATE.
           SET DATE-BAD TO TRUE

           *> Must be YYYY-MM-DD (length 10 after trim)
           IF FUNCTION LENGTH(FUNCTION TRIM(WS-DATE-TEXT)) NOT = 10
               EXIT PARAGRAPH
           END-IF

           *> Dashes in the right places
           IF WS-DATE-TEXT(5:1) NOT = "-" OR WS-DATE-TEXT(8:1) NOT = "-"
               EXIT PARAGRAPH
           END-IF

           *> All other characters must be numeric
           IF WS-DATE-TEXT(1:4) IS NOT NUMERIC
               EXIT PARAGRAPH
           END-IF
           IF WS-DATE-TEXT(6:2) IS NOT NUMERIC
               EXIT PARAGRAPH
           END-IF
           IF WS-DATE-TEXT(9:2) IS NOT NUMERIC
               EXIT PARAGRAPH
           END-IF

           SET DATE-OK TO TRUE.

       UPDATE-CATEGORY-TOTAL.
           MOVE 0 TO WS-FOUND-IDX

           *> Find existing category
           IF CAT-COUNT > 0
               MOVE 1 TO WS-CAT-IDX
               PERFORM UNTIL WS-CAT-IDX > CAT-COUNT OR WS-FOUND-IDX > 0
                   IF FUNCTION TRIM(CAT-NAME(WS-CAT-IDX))
                      = FUNCTION TRIM(WS-CATEGORY)
                       MOVE WS-CAT-IDX TO WS-FOUND-IDX
                   END-IF
                   ADD 1 TO WS-CAT-IDX
               END-PERFORM
           END-IF

           *> If not found, add new category
           IF WS-FOUND-IDX = 0
               IF CAT-COUNT < CAT-MAX
                   ADD 1 TO CAT-COUNT
                   MOVE WS-CATEGORY TO CAT-NAME(CAT-COUNT)
                   MOVE CAT-COUNT TO WS-FOUND-IDX
               ELSE
                   ADD 1 TO WS-BAD-COUNT
                   DISPLAY "WARN: category table full, skipping -> "
                           FUNCTION TRIM(WS-CATEGORY)
                   EXIT PARAGRAPH
               END-IF
           END-IF

           *> Add amount to category sum
           ADD WS-AMOUNT TO CAT-SUM(WS-FOUND-IDX).

       PRINT-CATEGORY-REPORT.
           DISPLAY " ".
           DISPLAY "Category breakdown:".
           DISPLAY "-------------------------------".

           IF CAT-COUNT = 0
               DISPLAY "(No categorized expenses)"
               EXIT PARAGRAPH
           END-IF

           MOVE 1 TO WS-CAT-IDX
           PERFORM UNTIL WS-CAT-IDX > CAT-COUNT
               MOVE CAT-SUM(WS-CAT-IDX) TO WS-DISPLAY-TOTAL
               DISPLAY FUNCTION TRIM(CAT-NAME(WS-CAT-IDX)) ": $"
                       WS-DISPLAY-TOTAL
               ADD 1 TO WS-CAT-IDX
           END-PERFORM

           DISPLAY "-------------------------------".
