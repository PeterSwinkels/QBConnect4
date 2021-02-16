DEFINT A-Z
DECLARE SUB ComputerMakeMove ()
DECLARE SUB DisplayHelp ()
DECLARE SUB DrawDisk (Column, Row, ColorO)
DECLARE SUB DrawDisks ()
DECLARE SUB DropDisk (Column, ColorO)
DECLARE SUB FindMoves (ColorO, TriggerLength, AllowHelpingOpponent)
DECLARE SUB GreyOutDisks ()
DECLARE SUB InitializeGame ()
DECLARE SUB PlayGame ()
DECLARE FUNCTION CountDisks (StartColumn, StartRow, ColorO, XDirection, YDirection)
DECLARE FUNCTION Disks (Column, Row, NewDisk, ResetDisks)
DECLARE FUNCTION FoundMoves ()
DECLARE FUNCTION GameDone ()
DECLARE FUNCTION GetGameState ()
DECLARE FUNCTION MoveHelpsOpponent (OpponentColor, Column)
DECLARE FUNCTION StateText$ ()
DECLARE FUNCTION WinningPlayer ()

CONST FALSE = 0
CONST TRUE = -1

CONST DCOutsideField = 0
CONST DCNone = 1
CONST DCRed = 2
CONST DCYellow = 3

CONST GSNeitherPlaying = 0
CONST GSRedPlaying = 1
CONST GSRedWon = 2
CONST GSTied = 3
CONST GSYellowPlaying = 4
CONST GSYellowWon = 5

CONST DropDelay! = .1
CONST FirstColumn = 0
CONST FirstRow = 0
CONST LastColumn = 6
CONST LastRow = 5
CONST NoColumn = -1
CONST SlotSize = 50
CONST WinningLength = 4

DIM SHARED ComputerColor
DIM SHARED CurrentDisks(FirstColumn TO LastColumn, FirstRow TO LastRow)
DIM SHARED CurrentPlayer
DIM SHARED FirstColor
DIM SHARED HumanColor
DIM SHARED MovesFound(FirstColumn TO LastColumn)
DIM SHARED SelectedColumn

SCREEN 12: CLS : WIDTH 80, 30: WINDOW SCREEN (-145, -90)-(494, 429)
PlayGame

SUB ComputerMakeMove
DIM Column
DIM TriggerLength

   FindMoves ComputerColor, WinningLength, TRUE
   IF NOT FoundMoves THEN
      FindMoves HumanColor, WinningLength, FALSE
      IF NOT FoundMoves THEN
         FOR TriggerLength = WinningLength TO 0 STEP -1
            FindMoves ComputerColor, TriggerLength, FALSE
            IF FoundMoves THEN EXIT FOR
         NEXT TriggerLength
      
         IF NOT FoundMoves THEN
            FOR TriggerLength = WinningLength TO 0 STEP -1
               FindMoves HumanColor, TriggerLength, TRUE
               IF FoundMoves THEN EXIT FOR
            NEXT TriggerLength
         END IF
      END IF
   END IF

   IF FoundMoves THEN
      DO
         Column = INT(RND * (LastColumn + 1)) + FirstColumn
         IF MovesFound(Column) THEN
            DropDisk Column, CurrentPlayer
            EXIT DO
         END IF
      LOOP
   END IF
END SUB

FUNCTION CountDisks (StartColumn, StartRow, ColorO, XDirection, YDirection)
DIM CheckCount
DIM Column
DIM DiskCount
DIM Row

   Column = StartColumn
   CheckCount = 0
   DiskCount = 0
   Row = StartRow
   DO WHILE CheckCount < WinningLength
      SELECT CASE Disks(Column, Row, DCNone, FALSE)
         CASE ColorO
            DiskCount = DiskCount + 1
         CASE NOT DCNone
            EXIT DO
      END SELECT
   
      Column = Column + XDirection
      Row = Row + YDirection
      CheckCount = CheckCount + 1
   LOOP
   
   CountDisks = DiskCount
END FUNCTION

FUNCTION Disks (Column, Row, NewDisk, ResetDisks)
DIM Disk

   Disk = DCOutsideField
   
   IF ResetDisks THEN
      ERASE CurrentDisks
      Disk = DCNone
      
      FOR Column = FirstColumn TO LastColumn
         FOR Row = FirstRow TO LastRow
            CurrentDisks(Column, Row) = DCNone
         NEXT Row
      NEXT Column
   ELSE
      IF Column >= FirstColumn AND Column <= LastColumn THEN
         IF Row >= FirstRow AND Row <= LastRow THEN
            IF NOT NewDisk = DCNone THEN CurrentDisks(Column, Row) = NewDisk
            Disk = CurrentDisks(Column, Row)
         END IF
      END IF
   END IF

   Disks = Disk
END FUNCTION

SUB DisplayHelp
   CLS : COLOR 15
   PRINT
   PRINT SPC(3); "Connect Four - Help"
   PRINT
   PRINT SPC(3); "Keys:"
   PRINT SPC(3); "F1  = This help."
   PRINT SPC(3); "F2  = No computer player."
   PRINT SPC(3); "F3  = Computer plays as red."
   PRINT SPC(3); "F4  = Computer plays as yellow."
   PRINT SPC(3); "A   = Restart game."
   PRINT SPC(3); "Q   = Quit game."
   PRINT SPC(3); "R   = Red plays first."
   PRINT SPC(3); "Y   = Yellow plays first."
   PRINT SPC(3); "1-7 = Drop disk into column."
   DO WHILE INKEY$ = "": LOOP
END SUB

SUB DrawDisk (Column, Row, ColorO)
   LINE (Column * SlotSize, Row * SlotSize)-STEP(SlotSize, SlotSize), 0, B
   CIRCLE ((Column + .5) * SlotSize, (Row + .5) * SlotSize), SlotSize / 2.5, 0
   SELECT CASE ColorO
      CASE DCNone
         COLOR 11
      CASE DCRed
         COLOR 12
      CASE DCYellow
         COLOR 14
   END SELECT
   PAINT ((Column + .5) * SlotSize, (Row + .5) * SlotSize), , 0
END SUB

SUB DrawDisks
DIM Column
DIM Row

   CLS
   LINE (-1, -1)-(((ABS(LastColumn - FirstColumn) + 1) * SlotSize) + 1, ((ABS(LastRow - FirstRow) + 1) * SlotSize) + 1), 7, BF
   LINE (0, 0)-((ABS(LastColumn - FirstColumn) + 1) * SlotSize, (ABS(LastRow - FirstRow) + 1) * SlotSize), 1, BF
   FOR Column = FirstColumn TO LastColumn
      FOR Row = FirstRow TO LastRow
         DrawDisk Column, Row, Disks(Column, Row, DCNone, FALSE)
      NEXT Row
   NEXT Column
END SUB

SUB DropDisk (Column, ColorO)
DIM DelayStart!
DIM Disk
DIM KeyStroke$
DIM Row

   IF Disks(Column, FirstRow, DCNone, FALSE) = DCNone THEN
      Row = FirstRow
      DO
         DrawDisk Column, Row, ColorO
      
         IF Row = LastRow THEN
            EXIT DO
         ELSE
            IF NOT Disks(Column, Row + 1, DCNone, FALSE) = DCNone THEN EXIT DO
         END IF
           
         DelayStart! = TIMER
         DO WHILE TIMER < DelayStart! + DropDelay!
            IF TIMER < DelayStart! THEN DelayStart! = TIMER
         LOOP
       
         DrawDisk Column, Row, DCNone
         Row = Row + 1
         KeyStroke$ = INKEY$
      LOOP
         
      Disk = Disks(Column, Row, ColorO, FALSE)

      SELECT CASE CurrentPlayer
         CASE DCRed
            CurrentPlayer = DCYellow
         CASE DCYellow
            CurrentPlayer = DCRed
      END SELECT

   END IF
   SelectedColumn = NoColumn
END SUB

SUB FindMoves (ColorO, TriggerLength, AllowHelpingOpponent)
DIM Column
DIM CheckColumn
DIM CheckCount
DIM CheckRow
DIM DiskCount
DIM FoundMove
DIM Row
DIM XDirection
DIM YDirection

   ERASE MovesFound
   
   FOR Column = FirstColumn TO LastColumn
      FOR Row = FirstRow TO LastRow
         FOR XDirection = -1 TO 1
            FOR YDirection = -1 TO 1
               IF NOT (XDirection = 0 AND YDirection = 0) THEN
                  CheckColumn = Column
                  CheckRow = Row
                  CheckCount = 0
                  DiskCount = 0
                  FoundMove = NoColumn
               
                  DO UNTIL CheckCount = TriggerLength
                     SELECT CASE Disks(CheckColumn, CheckRow, DCNone, FALSE)
                        CASE ColorO
                           DiskCount = DiskCount + 1
                        CASE DCNone
                           SELECT CASE Disks(CheckColumn, CheckRow + 1, DCNone, FALSE)
                              CASE DCRed, DCYellow, DCOutsideField
                                 FoundMove = CheckColumn
                           END SELECT
                        CASE ELSE
                           EXIT DO
                     END SELECT
                     
                     CheckColumn = CheckColumn + XDirection
                     CheckRow = CheckRow + YDirection
                     CheckCount = CheckCount + 1
                  LOOP
               
                  IF DiskCount = TriggerLength - 1 THEN
                     IF NOT FoundMove = NoColumn THEN
                        IF AllowHelpingOpponent THEN
                           MovesFound(FoundMove) = TRUE
                        ELSE
                           IF NOT MoveHelpsOpponent(HumanColor, FoundMove) THEN MovesFound(FoundMove) = TRUE
                       END IF
                    END IF
                 END IF
              END IF
           NEXT YDirection
        NEXT XDirection
      NEXT Row
   NEXT Column
END SUB

FUNCTION FoundMoves
DIM Column
DIM Found

   Found = FALSE
   FOR Column = FirstColumn TO LastColumn
      IF MovesFound(Column) THEN
         Found = TRUE
         EXIT FOR
      END IF
   NEXT Column
   
   FoundMoves = Found
END FUNCTION

FUNCTION GameDone
DIM Column
DIM Done

   Done = TRUE
   FOR Column = FirstColumn TO LastColumn
      IF Disks(Column, FirstRow, DCNone, FALSE) = DCNone THEN
         Done = FALSE
         EXIT FOR
      END IF
   NEXT Column

   GameDone = Done
END FUNCTION

FUNCTION GetGameState
DIM GameState

   SELECT CASE WinningPlayer
      CASE DCNone
         IF GameDone THEN
            GameState = GSTied
         ELSE
            SELECT CASE CurrentPlayer
               CASE DCNone
                  GameState = GSNeitherPlaying
               CASE DCRed
                  GameState = GSRedPlaying
               CASE DCYellow
                  GameState = GSYellowPlaying
            END SELECT
         END IF
      CASE DCRed
        GameState = GSRedWon
      CASE DCYellow
        GameState = GSYellowWon
   END SELECT
   
   GetGameState = GameState
END FUNCTION

SUB GreyOutDisks
DIM Column
DIM Row

   FOR Column = FirstColumn TO LastColumn
      FOR Row = FirstRow TO LastRow
         CIRCLE ((Column + .5) * SlotSize, (Row + .5) * SlotSize), SlotSize / 2.5, 0
         SELECT CASE Disks(Column, Row, DCNone, FALSE)
            CASE DCRed
               COLOR 4
            CASE DCYellow
               COLOR 6
            CASE ELSE
               COLOR 8
         END SELECT
         PAINT ((Column + .5) * SlotSize, (Row + .5) * SlotSize), , 0
      NEXT Row
   NEXT Column
END SUB

SUB InitializeGame
STATIC CurrentFirstColor

   RANDOMIZE TIMER
   CurrentPlayer = DCNone
   tmp = Disks(FirstColumn, FirstRow, DCNone, TRUE)
   DrawDisks
   GreyOutDisks
   SelectedColumn = NoColumn
   
   COLOR 15: LOCATE 3, 3: PRINT "Connect Four, by: Peter Swinkels - press any key."
   DO WHILE INKEY$ = "": LOOP
   DrawDisks
   CurrentPlayer = FirstColor
END SUB

FUNCTION MoveHelpsOpponent (OpponentColor, Column)
DIM HelpsOpponent
DIM Row
DIM XDirection
DIM YDirection

   HelpsOpponent = FALSE
   Row = 0   
   DO UNTIL (Row = LastRow) OR (NOT Disks(Column, Row + 1, DCNone, FALSE) = DCNone) OR (NOT InterfaceWindow.Visible)
      Row = Row + 1
   LOOP
   
   IF Row > FirstRow THEN
      Row = Row - 1
   
      FOR XDirection = -1 TO 1
         FOR YDirection = -1 TO 1
            IF NOT (XDirection = 0 AND YDirection = 0) THEN
               IF CountDisks(Column, Row, OpponentColor, XDirection, YDirection) = WinningLength - 1 THEN
                  HelpsOpponent = TRUE
               END IF
            END IF
         NEXT YDirection
      NEXT XDirection
   END IF

   MoveHelpsOpponent = HelpsOpponent
END FUNCTION

SUB PlayGame
DIM KeyStroke$

   ComputerColor = DCYellow
   FirstColor = DCRed
   HumanColor = DCRed
   
   InitializeGame

   DO
      COLOR 15
      LOCATE 3, 3: PRINT "Connect Four - F1 = Help"
      LOCATE 5, 1: PRINT SPACE$(80)
      LOCATE 5, 20: PRINT StateText
  
      SELECT CASE GetGameState
         CASE GSRedPlaying, GSYellowPlaying
            IF NOT CurrentPlayer = ComputerColor THEN
            DO
               KeyStroke$ = INKEY$
            LOOP WHILE KeyStroke$ = ""
            SELECT CASE KeyStroke$
               CASE "A", "a"
                  InitializeGame
               CASE "Q", "q"
                  SCREEN 0: WIDTH 80, 25: COLOR 7, 0: CLS
                  END
               CASE "R", "r"
                  FirstColor = DCRed
                  InitializeGame
               CASE "Y", "y"
                  FirstColor = DCYellow

                  InitializeGame
               CASE CHR$(0) + ";"
                  DisplayHelp
                  DrawDisks
               CASE CHR$(0) + "<"
                  ComputerColor = DCNone

                  InitializeGame
               CASE CHR$(0) + "="
                  ComputerColor = DCRed
                  HumanColor = DCYellow

                  InitializeGame
               CASE CHR$(0) + ">"
                  ComputerColor = DCYellow
                  HumanColor = DCRed

                  InitializeGame
               CASE "1" TO "7"
                  IF NOT GetGameState = GSNeitherPlaying THEN SelectedColumn = VAL(KeyStroke$) - 1
            END SELECT
         END IF
      END SELECT

      SELECT CASE GetGameState
         CASE GSRedPlaying, GSYellowPlaying
            IF ComputerColor = DCNone THEN
               IF NOT SelectedColumn = NoColumn THEN DropDisk SelectedColumn, CurrentPlayer
            ELSE
               IF CurrentPlayer = ComputerColor THEN
                  ComputerMakeMove
               ELSE
                  IF NOT SelectedColumn = NoColumn THEN DropDisk SelectedColumn, CurrentPlayer
               END IF
            END IF
         CASE GSRedWon, GSYellowWon, GSTied
            GreyOutDisks
            DO WHILE INKEY$ = "": LOOP
            InitializeGame
      END SELECT
   LOOP
END SUB

FUNCTION StateText$
DIM Text$

   Text$ = ""
   SELECT CASE GetGameState
      CASE GSNeitherPlaying
         Text$ = "Inactive."
      CASE GSRedPlaying
         Text$ = "Red's turn."
      CASE GSYellowPlaying
         Text$ = "Yellow's turn."
      CASE GSRedWon
         Text$ = "Red won."
      CASE GSYellowWon
         Text$ = "Yellow won."
      CASE GSTied
         Text$ = "Game is tied."
   END SELECT
   
   StateText$ = Text$
END FUNCTION

FUNCTION WinningPlayer
DIM ColorO
DIM Column
DIM Row
DIM XDirection
DIM YDirection
DIM Winner

   Winner = DCNone

   FOR ColorO = DCRed TO DCYellow
      FOR Column = FirstColumn TO LastColumn
         FOR Row = FirstRow TO LastRow
            FOR XDirection = -1 TO 1
              FOR YDirection = -1 TO 1
                 IF NOT (XDirection = 0 AND YDirection = 0) THEN
                    IF CountDisks(Column, Row, ColorO, XDirection, YDirection) = WinningLength THEN
                       Winner = ColorO
                    END IF
                 END IF
              NEXT YDirection
            NEXT XDirection
         NEXT Row
      NEXT Column
   NEXT ColorO

   WinningPlayer = Winner
END FUNCTION

