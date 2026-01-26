       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRIPBUDGET.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EXPENSE-FILE ASSIGN TO "data/expenses.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  EXPENSE-FILE.
       01  EXPENSE-LINE               PIC X(200).

       WORKING-STORAGE SECTION.
       01 TRIP-NAME                   PIC X(30) VALUE "New England Sprint Loop".
       01 RIDER-NAME                  PIC X(20) VALUE "Rob".

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

       01 WS-DISPLAY-CAT              PIC X(30).
       01 WS-DISPLAY-AMT              PIC Z(6)9.99.
       01 WS-DISPLAY-TOTAL            PIC Z(7)9.99.
       01 WS-DISPLAY-AVG              PIC Z(7)9.99.
       01 WS-DISPLAY-MAX              PIC Z(6)9.99.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "===============================".
           DISPLAY " COBOL TRIP BUDGET REPORT (FILE)".
           DISPLAY "===============================".
           DISPLAY "Trip : " FUNCTION TRIM(TRIP-NAME).
           DISPLAY "Rider: " FUNCTION TRIM(RIDER-NAME).
           DISPLAY "File : data/expenses.csv".
           DISPLAY " ".

           OPEN INPUT EXPENSE-FILE
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

           DISPLAY " ".
           DISPLAY "-------------------------------".
           DISPLAY "Items processed: " WS-GOOD-COUNT.
           DISPLAY "Bad lines      : " WS-BAD-COUNT.
           DISPLAY "Total          : $" WS-DISPLAY-TOTAL.
           DISPLAY "Avg            : $" WS-DISPLAY-AVG.
           DISPLAY "Max            : $" WS-DISPLAY-MAX.
           DISPLAY "-------------------------------".
           STOP RUN.

       PROCESS-LINE.
           ADD 1 TO WS-LINE-NUM

           *> Skip blank lines
           IF FUNCTION LENGTH(FUNCTION TRIM(EXPENSE-LINE)) = 0
               EXIT PARAGRAPH
           END-IF

           *> Split "Category,Amount"
           MOVE SPACES TO WS-CATEGORY
           MOVE SPACES TO WS-AMOUNT-TEXT

           UNSTRING EXPENSE-LINE
               DELIMITED BY ","
               INTO WS-CATEGORY WS-AMOUNT-TEXT
           END-UNSTRING

           MOVE FUNCTION TRIM(WS-CATEGORY)    TO WS-CATEGORY
           MOVE FUNCTION TRIM(WS-AMOUNT-TEXT) TO WS-AMOUNT-TEXT

           *> Basic validation: must have both fields
           IF WS-CATEGORY = SPACES OR WS-AMOUNT-TEXT = SPACES
               ADD 1 TO WS-BAD-COUNT
               DISPLAY "WARN line " WS-LINE-NUM ": bad format -> " FUNCTION TRIM(EXPENSE-LINE)
               EXIT PARAGRAPH
           END-IF

           *> Convert text amount to numeric (handles "10.50")
           COMPUTE WS-AMOUNT-NUMVAL = FUNCTION NUMVAL(WS-AMOUNT-TEXT)

           *> Reject non-positive or suspicious values
           IF WS-AMOUNT-NUMVAL <= 0
               ADD 1 TO WS-BAD-COUNT
               DISPLAY "WARN line " WS-LINE-NUM ": bad amount -> " FUNCTION TRIM(EXPENSE-LINE)
               EXIT PARAGRAPH
           END-IF

           MOVE WS-AMOUNT-NUMVAL TO WS-AMOUNT

           ADD 1 TO WS-GOOD-COUNT
           ADD WS-AMOUNT TO TOTAL-AMOUNT

           IF WS-AMOUNT > MAX-AMOUNT
               MOVE WS-AMOUNT TO MAX-AMOUNT
           END-IF

           *> Optional: show each accepted line nicely
           MOVE WS-CATEGORY TO WS-DISPLAY-CAT
           MOVE WS-AMOUNT   TO WS-DISPLAY-AMT
           DISPLAY FUNCTION TRIM(WS-DISPLAY-CAT) ": $" WS-DISPLAY-AMT.
