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

01  WS-AMOUNT                  PIC 9(5)V99.
01  WS-AMOUNT-DISPLAY          PIC Z,ZZ9.99.

01  WS-TOTAL-AMOUNT            PIC 9(7)V99 VALUE 0.
01  WS-TOTAL-DISPLAY           PIC Z,ZZZ,ZZ9.99.

01  WS-DATE                    PIC X(10).
01  WS-CATEGORY                PIC X(30).

01  WS-REPORT-LINE             PIC X(80).

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
    PERFORM PRINT-FOOTER

    CLOSE EXPENSE-FILE
    STOP RUN
    .

PROCESS-LINE.
    UNSTRING EXPENSE-LINE
        DELIMITED BY ","
        INTO WS-DATE
             WS-CATEGORY
             WS-AMOUNT
        ON OVERFLOW
            ADD 1 TO WS-BAD-COUNT
            EXIT PARAGRAPH
    END-UNSTRING

    ADD 1 TO WS-GOOD-COUNT
    ADD WS-AMOUNT TO WS-TOTAL-AMOUNT
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

PRINT-FOOTER.
    DISPLAY "Records processed: " WS-LINE-NUM
    DISPLAY "Valid records   : " WS-GOOD-COUNT
    DISPLAY "Invalid records : " WS-BAD-COUNT
    DISPLAY "==============================="
    .
