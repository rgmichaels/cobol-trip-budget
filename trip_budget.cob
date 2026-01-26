       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRIPBUDGET.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TRIP-NAME         PIC X(30) VALUE "New England Sprint Loop".
       01 RIDER-NAME        PIC X(20) VALUE "Rob".

       01 EXPENSE-COUNT     PIC 9(2) VALUE 5.

       01 EXPENSES.
          05 EXPENSE-AMOUNT OCCURS 10 TIMES
                            PIC 9(4)V99 VALUE 0.

       01 I                PIC 9(2) VALUE 1.
       01 TOTAL-AMOUNT     PIC 9(6)V99 VALUE 0.
       01 MAX-AMOUNT       PIC 9(4)V99 VALUE 0.
       01 AVG-AMOUNT       PIC 9(6)V99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM INIT-DATA.

           DISPLAY "===============================".
           DISPLAY " COBOL TRIP BUDGET REPORT".
           DISPLAY "===============================".
           DISPLAY "Trip : " FUNCTION TRIM(TRIP-NAME).
           DISPLAY "Rider: " FUNCTION TRIM(RIDER-NAME).
           DISPLAY " ".

           PERFORM CALC-TOTALS.

           IF EXPENSE-COUNT > 0
               COMPUTE AVG-AMOUNT = TOTAL-AMOUNT / EXPENSE-COUNT
           END-IF.

           DISPLAY "Items processed: " EXPENSE-COUNT.
           DISPLAY "Total: $" TOTAL-AMOUNT.
           DISPLAY "Avg  : $" AVG-AMOUNT.
           DISPLAY "Max  : $" MAX-AMOUNT.
           DISPLAY " ".

           STOP RUN.

       INIT-DATA.
           *> Amounts are dollars.cents (V99 means implied decimal)
           MOVE 10.50 TO EXPENSE-AMOUNT(1)
           MOVE 22.00 TO EXPENSE-AMOUNT(2)
           MOVE 7.25  TO EXPENSE-AMOUNT(3)
           MOVE 45.00 TO EXPENSE-AMOUNT(4)
           MOVE 60.00 TO EXPENSE-AMOUNT(5).

       CALC-TOTALS.
           MOVE 1 TO I
           PERFORM UNTIL I > EXPENSE-COUNT
               ADD EXPENSE-AMOUNT(I) TO TOTAL-AMOUNT

               IF EXPENSE-AMOUNT(I) > MAX-AMOUNT
                   MOVE EXPENSE-AMOUNT(I) TO MAX-AMOUNT
               END-IF

               ADD 1 TO I
           END-PERFORM.
