       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRIPBUDGET.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EXPENSE-FILE ASSIGN TO DYNAMIC WS-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  EXPENSE-FILE.
       01  EXPENSE-LINE               PIC X(200).

       WORKING-STORAGE SECTION.
       01  WS-FILENAME                PIC X(256).
       01  WS-FILE-STATUS             PIC XX.

       01  TRIP-NAME                  PIC X(30) VALUE "NE Sprint Loop".
       01  RIDER-NAME                 PIC X(20) VALUE "Rob".

       01  WS-EOF                     PIC X VALUE 'N'.
           88 EOF                     VALUE 'Y'.
           88 NOT-EOF                 VALUE 'N'.

       01  WS-LINE-NUM                PIC 9(5) VALUE 0.
       01  WS-GOOD-COUNT              PIC 9(5) VALUE 0.
       01  WS-BAD-COUNT               PIC 9(5) VALUE 0.

       *> Signed so we can parse negatives and reject them explicitly
       01  WS-AMOUNT                  PIC S9(5)V99.
       01  WS-AMOUNT-DISPLAY          PIC Z,ZZ9.99.

       01  WS-TOTAL-AMOUNT            PIC 9(7)V99 VALUE 0.
       01  WS-TOTAL-DISPLAY           PIC Z,ZZZ,ZZ9.99.

       01  WS-DATE                    PIC X(10).
       01  WS-CATEGORY                PIC X(30).

       01  WS-REC-DATE                PIC X(10).
       01  WS-REC-CATEGORY            PIC X(30).
       01  WS-REC-AMOUNT              PIC S9(5)V99.

       01  WS-REPORT-LINE             PIC X(80).

       01  CAT-COUNT                  PIC 9(3) VALUE 0.
       01  CAT-TABLE.
           05 CAT-ENTRY OCCURS 20 TIMES.
               10 CAT-NAME            PIC X(30) VALUE SPACES.
               10 CAT-TOTAL           PIC 9(7)V99 VALUE 0.
       01  WS-CAT-FOUND               PIC X VALUE 'N'.
           88 CAT-FOUND               VALUE 'Y'.
           88 CAT-NOT-FOUND           VALUE 'N'.
       01  WS-INDEX                   PIC 9(3) VALUE 0.
       01  WS-CAT-TOTAL-DISPLAY       PIC Z,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       MAIN.
           ACCEPT WS-FILENAME FROM ARGUMENT-VALUE
           IF WS-FILENAME = SPACES
               MOVE "data/expenses.csv" TO WS-FILENAME
           END-IF

           OPEN INPUT EXPENSE-FILE
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "ERROR: Unable to open input file."
               STOP RUN
           END-IF

           PERFORM PRINT-HEADER

           PERFORM UNTIL EOF
               READ EXPENSE-FILE
                   AT END
                       SET EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-LINE-NUM
                       PERFORM PROCESS-LINE
               END-READ
           END-PERFORM

           PERFORM PRINT-TOTALS
           PERFORM PRINT-CATEGORY-SUMMARY
           PERFORM PRINT-FOOTER

           CLOSE EXPENSE-FILE
           STOP RUN
           .

       PROCESS-LINE.
           *> Ignore blank/whitespace-only lines (count as invalid)
           IF FUNCTION TRIM(EXPENSE-LINE) = SPACES
               ADD 1 TO WS-BAD-COUNT
               EXIT PARAGRAPH
           END-IF

           PERFORM POPULATE-RECORD-FROM-LINE
           IF WS-BAD-COUNT > 0
               EXIT PARAGRAPH
           END-IF

           *> Validation: reject zero or negative amounts
           IF WS-REC-AMOUNT <= 0
               ADD 1 TO WS-BAD-COUNT
               EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION TRIM(WS-REC-CATEGORY) TO WS-REC-CATEGORY

           PERFORM UPDATE-CATEGORY-TOTAL

           *> Print the line item
           MOVE WS-REC-AMOUNT TO WS-AMOUNT-DISPLAY
           DISPLAY WS-REC-DATE "  " WS-REC-CATEGORY "  $" WS-AMOUNT-DISPLAY

           ADD 1 TO WS-GOOD-COUNT
           ADD WS-REC-AMOUNT TO WS-TOTAL-AMOUNT
           .

       POPULATE-RECORD-FROM-LINE.
           MOVE SPACES TO WS-REC-DATE
           MOVE SPACES TO WS-REC-CATEGORY
           MOVE 0 TO WS-REC-AMOUNT

           UNSTRING EXPENSE-LINE
               DELIMITED BY ","
               INTO WS-DATE
                    WS-CATEGORY
                    WS-AMOUNT
               ON OVERFLOW
                   ADD 1 TO WS-BAD-COUNT
                   EXIT PARAGRAPH
           END-UNSTRING

           MOVE WS-DATE TO WS-REC-DATE
           MOVE WS-CATEGORY TO WS-REC-CATEGORY
           MOVE WS-AMOUNT TO WS-REC-AMOUNT
           .

       UPDATE-CATEGORY-TOTAL.
           SET CAT-NOT-FOUND TO TRUE
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > CAT-COUNT OR CAT-FOUND
               IF CAT-NAME(WS-INDEX) = WS-REC-CATEGORY
                   ADD WS-REC-AMOUNT TO CAT-TOTAL(WS-INDEX)
                   SET CAT-FOUND TO TRUE
               END-IF
           END-PERFORM

           IF CAT-NOT-FOUND
               IF CAT-COUNT < 20
                   ADD 1 TO CAT-COUNT
                   MOVE WS-REC-CATEGORY TO CAT-NAME(CAT-COUNT)
                   ADD WS-REC-AMOUNT TO CAT-TOTAL(CAT-COUNT)
               ELSE
                   DISPLAY "WARNING: Category table full, skipping "
                       WS-REC-CATEGORY
               END-IF
           END-IF
           .

       PRINT-HEADER.
           DISPLAY "==============================="
           DISPLAY " COBOL TRIP BUDGET REPORT (FILE)"
           DISPLAY "==============================="
           DISPLAY "Trip : " TRIP-NAME
           DISPLAY "Rider: " RIDER-NAME
           DISPLAY "File : " WS-FILENAME
           DISPLAY SPACE
           .

       PRINT-TOTALS.
           MOVE WS-TOTAL-AMOUNT TO WS-TOTAL-DISPLAY
           DISPLAY "-------------------------------"
           DISPLAY "Total Trip Cost: $" WS-TOTAL-DISPLAY
           DISPLAY "-------------------------------"
           DISPLAY SPACE
           .

       PRINT-CATEGORY-SUMMARY.
           DISPLAY "Category Totals:"
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > CAT-COUNT
               MOVE CAT-TOTAL(WS-INDEX) TO WS-CAT-TOTAL-DISPLAY
               DISPLAY CAT-NAME(WS-INDEX) "  $"
                   WS-CAT-TOTAL-DISPLAY
           END-PERFORM
           DISPLAY SPACE
           .

       PRINT-FOOTER.
           DISPLAY "Records processed: " WS-LINE-NUM
           DISPLAY "Valid records   : " WS-GOOD-COUNT
           DISPLAY "Invalid records : " WS-BAD-COUNT
           DISPLAY "==============================="
           .
