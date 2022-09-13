    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; include required files with VCS register memory mapping and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare variables starting from memory address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    seg.u variables
    org $80

JetXPos         byte                    ; player 0 X position
JetYPos         byte                    ; player 0 Y position
BomberXPos      byte                    ; player 1 X position
BomberYPos      byte                    ; player 1 Y position
MissileXPos     byte                    ; missile X position
MissileYPos     byte                    ; missile Y position
Score           byte                    ; 2-digit score stored as BCD
Timer           byte                    ; 2-digit timer stored as BCD
Temp            byte                    ; aux variable to store tempoirary score values
OnesDigitOffset word                    ; Lookup table offset for scores 1':'s digit
TensDigitOffset word                    ; Lookup table offset for scores 10's digit
JetSpritePtr    word                    ; memory address of the jet sprite
JetColorPtr     word                    ; memory address of the jet color lookup tables
BomberSpritePtr word                    ; pointer to the player 1 sprite Lookup
BomberColorPtr  word                    ; pointer for bomber color lookup
JetAnimOffset   byte                    ; player0 sprite offset for animation
Random          byte                    ; random number generator seed
ScoreSprite     byte                    ; store the sprite bit pattern for the score
TimerSprite     byte                    ; store the sprite bit pattern for the timer
TerrainColour   byte                    ; stores the terrain colour
RiverColour     byte                    ; stores the river colour

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

JET_HEIGHT = 9                          ; player 0 sprite height
BOMBER_HEIGHT = 9                       ; player 1 sprite height
DIGITS_HEIGHT = 5                       ; scoreboard digit height

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code at memory address $f000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    seg code
    org $f000

Reset:
    CLEAN_START                         ; Call macro to reset memory and registers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialise RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda #10
    sta JetYPos                         ; JetYPos = 10
    lda #60
    sta JetXPos                         ; JetXPos = 60

    lda #83
    sta BomberYPos                      ; BomberYpos = 83
    lda #62
    sta BomberXPos                      ; BomberXPos = 62

    lda #%11010100
    sta Random

    lda #0
    sta Score                           ; Score = 0
    sta Timer                           ; Timer = 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare a macro to check if we should display missile 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    MAC DRAW_MISSILE
        lda #%00000000
        cpx MissileYPos      ; compare X (current scanline) with missile Y pos
        bne .SkipMissileDraw ; if (X != missile Y position), then skip draw
.DrawMissile:                ; else:
        lda #%00000010       ;     enable missile 0 display
        inc MissileYPos      ;     MissileYPos++
.SkipMissileDraw:
        sta ENAM0            ; store correct value in the TIA missile register
    ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialise pointers to correct lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda #<JetSprite
    sta JetSpritePtr                    ; low-byte pointer for jet sprite lookup tables
    lda #>JetSprite
    sta JetSpritePtr+1                  ; hi-byte pointer for jet sprite lookup table

    lda #<JetColor
    sta JetColorPtr                    ; low-byte pointer for jet color lookup tables
    lda #>JetColor
    sta JetColorPtr+1                  ; hi-byte pointer for jet color lookup table

    lda #<BomberSprite
    sta BomberSpritePtr                    ; low-byte pointer for bomber sprite lookup tables
    lda #>BomberSprite
    sta BomberSpritePtr+1                  ; hi-byte pointer for bomber sprite lookup table

    lda #<BomberColor
    sta BomberColorPtr                    ; low-byte pointer for bomber color lookup tables
    lda #>BomberColor
    sta BomberColorPtr+1                  ; hi-byte pointer for bomber color lookup table

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

StartFrame:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display Vsync and VBlank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda #2
    sta VBLANK                          ; turn on VBlank
    sta VSYNC                           ; Turn on Vsync
    REPEAT 3
        sta WSYNC                       ; display 3 lines of Vsync
    REPEND
    lda #0
    sta VSYNC                           ; turn off Vsync
    REPEAT 33
        sta WSYNC                       ; 33 lines of VBlank )4 wasted in next section
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed pre-VBlank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda JetXPos
    ldy #0
    jsr SetObjectXPos                       ; set jet x pos

    lda BomberXPos
    ldy #1
    jsr SetObjectXPos                       ; Set Bomber X pos

    lda MissileXPos
    ldy #2
    jsr SetObjectXPos                       ; Set missile x pos

    jsr CalculateDigitOffset                ; calculate the scoreboard digit lookup table offset

    sta WSYNC
    sta HMOVE                               ; apply the horizontal offsets applied in prior routines

    lda #0
    sta VBLANK                          ; turn off VBlank

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda #0                              ; clear TIA registers before each new StartFrame
    sta COLUBK                          ; set BK to black
    sta PF0
    sta PF1
    sta PF2
    sta GRP0                            ; reset p0 graphics
    sta GRP1
    sta CTRLPF

    lda #$1E
    sta COLUPF

    ldx #DIGITS_HEIGHT                  ; start the X counter at 5

.ScoreDigitLoop:
    ldy TensDigitOffset                 ; get the 10's digit offset for the score
    lda Digits,Y                        ;load the bit pattern from the lookup table
    and #$F0                            ; mask the graphics for the 1's digit
    sta ScoreSprite                     ; save the score tens digit pattern in a variable

    ldy OnesDigitOffset                 ; get tyhe OnesDigitOffset for the score
    lda Digits,Y                        ; load the bit pattern for the ones digit
    and #$0F                            ; mask the digits for the 10's

    ora ScoreSprite                     ; merge it with saved 10's digit sprite
    sta ScoreSprite                     ; save it

    sta WSYNC                           ; wait for the end of the scanline
    sta PF1                             ; display score on the playfield

    ldy TensDigitOffset+1               ; get the left digit offset for the timer
    lda Digits,Y                        ; load bitmap from lookup table
    and #$F0                            ; mask the Ones
    sta TimerSprite                     ; save to timersprite

    ldy OnesDigitOffset+1               ; get the ones digit offset
    lda Digits,Y                        ; load the bitmap for the ones digit
    and #$0F                            ; mask the 10's

    ora TimerSprite                     ; merge the two digits =
    sta TimerSprite                     ; save to timerSprite

    jsr Sleep12Cycles                   ; subroutine to waste 12 cycles

    sta PF1

    ldy ScoreSprite                     ; preload for the next scanline
    sta WSYNC                           ; wait for next scanline

    sty PF1                             ; update PF fro the score display
    inc TensDigitOffset
    inc TensDigitOffset+1
    inc OnesDigitOffset
    inc OnesDigitOffset+1               ; increment all digits for next line of data

    jsr Sleep12Cycles                   ; waste cycles

    dex                                 ; X--
    sta PF1                             ; display timer on the playfield
    bne .ScoreDigitLoop                 ; if not 0 branch back to start

    sta WSYNC                           ; wait for scanline

    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC
    sta WSYNC
    sta WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display 85 visible scanlines (2 line kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GameVisibleLine:
    lda RiverColour
    sta COLUBK                          ; set BG colour to river colour

    lda TerrainColour
    sta COLUPF                          ; set Terrain color to terrain Colour

    lda #%00000001
    sta CTRLPF                          ; enable playfield reflection

    lda #$F0
    sta PF0                             ; setting PF0 bit pattern
    lda #$FC
    sta PF1                             ; setting PF1 bit pattern
    lda #0
    sta PF2                             ; setting PF2 bit pattern

    ldx #85
.GameLineLoop:

    DRAW_MISSILE                        ; macro to check if we should draw the missile

.AreWeInsideJetSprite:
    txa                                 ; transfer x to a
    sec                                 ; set carry flag
    sbc JetYPos                         ; minus JetYPos
    cmp #JET_HEIGHT                     ; are we inside the sprite?
    bcc .DrawSpriteP0                   ; if result is less than sprite height, draw routine
    lda #0                              ; else, set lookup index to 0

.DrawSpriteP0
    clc
    adc JetAnimOffset                   ; Jump to correct sprite frame address in memory
    tay                                 ; transfer a to y (only y works with direct addressing)
    lda (JetSpritePtr),Y                ; load player0 bitmap data from the lookup table
    sta WSYNC                           ; wait for scanline
    sta GRP0                            ; set graphics for P0
    lda (JetColorPtr),Y                 ; load color from lookup tables
    sta COLUP0                          ; set colour of player 0

.AreWeInsideBomberSprite:
    txa                                 ; transfer x to a
    sec                                 ; set carry flag
    sbc BomberYPos                      ; minus BomberYPos
    cmp #BOMBER_HEIGHT                   ; are we inside the sprite?
    bcc .DrawSpriteP1                   ; if result is less than sprite height, draw routine
    lda #0                              ; else, set lookup index to 0

.DrawSpriteP1
    tay                                 ; transfer a to y (only y works with direct addressing)

    lda #%00000101
    sta NUSIZ1                          ; stretch payer 1 Sprite

    lda (BomberSpritePtr),Y             ; load player1 bitmap data from the lookup table
    sta WSYNC                           ; wait for scanline
    sta GRP1                            ; set graphics for P1
    lda (BomberColorPtr),Y              ; load color from lookup tables
    sta COLUP1                          ; set colour of player 1

    dex                                 ; x--
    bne .GameLineLoop                   ; repeat until 192 scanlines drawn

    lda 0                               ; reset the jet height in the anim offset
    sta JetAnimOffset

    sta WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda #2
    sta VBLANK                          ; turn on VBLANK
    REPEAT 30
        sta WSYNC                       ; Display 30 lines of Overscan
    REPEND
    lda #0
    sta VBLANK                          ; Turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process Joystick input for P0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CheckP0Up:
    lda #%00010000                      ; player0 joystick up
    bit SWCHA
    bne CheckP0Down                     ;if not equal, skip to next routine
    lda JetYPos
    cmp #70
    bpl CheckP0Down                     ; check against max height of 70
    inc JetYPos
    lda #0                               ; reset the jet height in the anim offset
    sta JetAnimOffset

CheckP0Down:
    lda #%00100000                      ; player0 joystick down
    bit SWCHA
    bne CheckP0Left                     ;if not equal, skip to next routine
    lda JetYPos
    cmp #5
    bmi CheckP0Left
    dec JetYPos
    lda #0                               ; reset the jet height in the anim offset
    sta JetAnimOffset

CheckP0Left:
    lda #%01000000                      ; player0 joystick left
    bit SWCHA
    bne CheckP0Right                     ;if not equal, skip to next routine
    lda JetXPos
    cmp #35
    bmi CheckP0Right
    dec JetXPos
    lda #JET_HEIGHT                      ; store the jet height in the anim offset
    sta JetAnimOffset

CheckP0Right:
    lda #%10000000                      ; player0 joystick right
    bit SWCHA
    bne CheckButtonPressed                   ;if not equal, skip to next routine
    lda JetXPos
    cmp #100
    bpl CheckButtonPressed
    inc JetXPos
    lda #JET_HEIGHT                      ; store the jet height in the anim offset
    sta JetAnimOffset

CheckButtonPressed:
    lda #%10000000           ; if button is pressed
    bit INPT4
    bne EndInputCheck
.ButtonPressed:
    lda JetXPos
    clc
    adc #5
    sta MissileXPos          ; set the missile X position equal to the player 0
    lda JetYPos
    clc
    adc #8
    sta MissileYPos          ; set the missile Y position equal to the player 0

EndInputCheck:                          ; fallback for when no input is detected

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations to update position for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

UpdateBomberPosition:
    lda BomberYPos
    clc
    cmp #0                              ; Compare Bomber Y pos to 0
    bmi .ResetBomberPosition            ; if minus jump to reset y routine
    dec BomberYPos                      ; else decrement BomboerYPos
    jmp EndPositionUpdate

.ResetBomberPosition:
    jsr GetRandomBomberPos              ; Call subroutine for next enemy position (x)

.SetScoreValues:
    sed                                 ; set decimal mode for score and timer values

    lda Timer                           ; BCD doesnt work with inc, so add 1 manually
    clc
    adc #1
    sta Timer

    cld                                 ; Disable decimal mode after score and timer

EndPositionUpdate:                      ; fallback for the position update code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for object collision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CheckCollisonP0P1:
    lda #%10000000                      ; CXPPMM bit 7 detects collision between P0 and P1
    bit CXPPMM
    bne .CollisionP0P1                  ; branch if collision happens

    jsr SetTerrainRiverColour           ; else set PF and BG colour

    jmp CheckCollisionM0P1              ; skip to the next col check


.CollisionP0P1:
    jsr GameOver                        ; Call the game over subroutine

CheckCollisionM0P1:
    lda #%10000000
    bit CXM0P                           ; detects M0 and P1 collision
    bne .M0P1Collided
    jmp EndCollisionCheck

.M0P1Collided
    sed
    lda Score
    clc
    adc #1
    sta Score                           ; Add 1 to the score via BCD
    cld                                 ; disable BCD
    lda #0
    sta MissileYPos                     ; reset the missile position

EndCollisionCheck:                      ; fallback
    sta CXCLR                           ; clear all collision flags before next step

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start a new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    jmp StartFrame                      ; continue from StartFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the colours for terrain and river
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SetTerrainRiverColour subroutine
    lda #$C2
    sta TerrainColour
    lda #$84
    sta RiverColour
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle object hoprizontal position with fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a is target x-co-ord position in pixels of our object
;; Y is the object type (0: player 0, 1: player 1, 2: missile0, 3:missile1, 4: ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SetObjectXPos subroutine
    sta WSYNC                           ; start a fresh scanline
    sec                                 ; set carry flag before subtraction
.Div15Loop
    sbc #15                             ; subtract 15 from A
    bcs .Div15Loop                      ; Loop until carry flag is clear
    eor #7                              ; handle offset of range -8 to 7
    asl
    asl
    asl
    asl                                 ; 4 shifts left to make number frontmost 4 bits
    sta HMP0,Y                          ; store the fine offset to the correct HMxx
    sta RESP0,Y                         ; fix object position in 15 step increments
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game over subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GameOver subroutine
    lda #$30
    sta TerrainColour                   ; set terrain colour to red
    sta RiverColour                     ; set river colour to red

    lda #0
    sta Score                           ; score = 0

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to generate a Linear-Feedback shift register random number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate a random number
;; Divide the random number by 4 to limit the size to the playfield size
;; Add 30 to compensate for left side of grass
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GetRandomBomberPos subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random                          ; perform a series of random shifts and bit operations

    lsr
    lsr                                 ; divide by 4 with 2 right shifts
    sta BomberXPos                      ; save value to BomberXPos
    lda #30
    adc BomberXPos                      ; add 30 to BomberXPos
    sta BomberXPos

    lda #96                             ; reload intitial Y pos
    sta BomberYPos                      ; Store in BomberYPos

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle scoreboard digits to be displayed on the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert high and low nibbles of the variable score and timer
;; into the offsets of digit lookup tables so the values can be displayed
;; each digit has a height of 5 bytes in the lookup table
;;
;; For the low nibble we need to multiply by 5
;;  -- we can use left shifts to perform multiplication by powers of 2
;;  -- for any number N. the value of N*5 = (N*2*2) + N
;;
;; For the upper nibble, since its already x16, we need to divide it by 16
;; then multiply by 5:
;; -- we can use right shifts to perform division by 2
;; -- for any number N, the value of (N/16)*5 = (n/2/2) + (N/2/2/2/2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CalculateDigitOffset subroutine
    ldx #1                              ; X register is the loop counter
.PrepareScoreLoop                       ; this will loop twice, X=1, X=0
    lda Score,X                         ; load accumulator with the timer (x=1) or score (x=0)
    and #$0F                            ; logical and will force first 4 digits to be 0000
    sta Temp                            ; save the value of A into Temp
    asl
    asl
    adc Temp                            ; Shift left twice then add once to achieve multiply by 5
    sta OnesDigitOffset,X               ; save A in OnesDigitOffset + 1 or OnesDigitOffset

    lda Score,X                         ; load A with timer (X=1) or Score (X=0)
    and #$F0                            ; logical and masks the first 4 bits
    lsr
    lsr                                 ; divide by 4
    sta Temp                            ; save values to temp
    lsr
    lsr                                 ; divide by 16
    adc Temp                            ; add the value in Temp to A (N/16 + N/4)
    sta TensDigitOffset,X               ; Store A in the appropriate byte

    dex
    bpl .PrepareScoreLoop               ; While X is positive, loop to pass a 2nd time
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subroutine to waste 12 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Sleep12Cycles subroutine
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM Lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jsr takes 6 cycles, trs takes 6 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Digits:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %00100010          ;  #   #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01100110          ; ##  ##
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #

JetSprite:
    .byte #%00000000
    .byte #%00010100                    ;   # #
    .byte #%01111111                    ; #######
    .byte #%00111110                    ;  #####
    .byte #%00011100                    ;   ###
    .byte #%00011100                    ;   ###
    .byte #%00001000                    ;    #
    .byte #%00001000                    ;    #
    .byte #%00001000                    ;    #

JetSpriteTurn:
    .byte #%00000000
    .byte #%00001000                    ;  # #
    .byte #%00111110                    ; #####
    .byte #%00011100                    ;  ###
    .byte #%00011100                    ;  ###
    .byte #%00011100                    ;  ###
    .byte #%00001000                    ;   #
    .byte #%00001000                    ;   #
    .byte #%00001000                    ;   #

BomberSprite:
    .byte #%00000000
    .byte #%00001000                    ;    #
    .byte #%00001000                    ;    #
    .byte #%00101010                    ;  # # #
    .byte #%00111110                    ;  #####
    .byte #%01111111                    ; #######
    .byte #%00101010                    ;  # # #
    .byte #%00001000                    ;    #
    .byte #%00011100                    ;   ###

JetColor:
    .byte #$00;
    .byte #$1E;
    .byte #$0A;
    .byte #$0E;
    .byte #$0E;
    .byte #$0E;
    .byte #$C8;
    .byte #$0E;
    .byte #$06;

JetTurnColor:
    .byte #$00;
    .byte #$1E;
    .byte #$0A;
    .byte #$0E;
    .byte #$0E;
    .byte #$0E;
    .byte #$C8;
    .byte #$0E;
    .byte #$06;

BomberColor:
    .byte #$00
    .byte #$34;
    .byte #$34;
    .byte #$0C;
    .byte #$32;
    .byte #$32;
    .byte #$32;
    .byte #$32;
    .byte #$32;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fill ROM to 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    org $FFFC
    word Reset                          ; write 2 bytes with program reset address
    word Reset                          ; write 2 bytes with interrupt address
