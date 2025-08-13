; 65C02 version of the 6502 assembly code
; To be compiled with ca65

; Set ACIA_BASE to the location of your 6551 ACIA (UART).
.define ACIA_BASE $5000  ; Base address of the 6551 ACIA

; Equates for serial I/O
.define SDR  ACIA_BASE   ; Data register
.define SSR  ACIA_BASE+1 ; Status register
.define SCMD ACIA_BASE+2 ; Command register
.define SCTL ACIA_BASE+3 ; Control register

; Control and command register values
.define SCTL_V 00011111 ; 1 stop, 8 bits, 19200 baud
.define SCMD_V 00001011 ; No parity, no echo, no tx or rx IRQ, DTR*
.define TX_RDY 00010000 ; Transmitter ready mask
.define RX_RDY 00001000 ; Receiver buffer full mask

; Zero-page storage
.define DPL      $00     ; Data pointer low
.define DPH      $01     ; Data pointer high
.define RECLEN   $02     ; Record length in bytes
.define START_LO $03     ; Start address low
.define START_HI $04     ; Start address high
.define RECTYPE  $05     ; Record type
.define CHKSUM   $06     ; Checksum accumulator
.define DLFAIL   $07     ; Download failure flag
.define TMP     $08     ; Temporary storage

; Shadow RAM vectors
.define NMIVEC   $7FFA   ; NMI vector
.define IRQVEC   $7FFE   ; IRQ vector

; Entry point for downloaded code
.define ENTRY_POINT $0380

; Start of the program
.segment "INTEL"
;.org $F800

START:
        sei                     ; Disable interrupts
        cld                     ; Binary mode arithmetic
        ldx     #$FF            ; Set up the stack pointer
        txs                     ; 
        lda     #>START         ; Initialize the interrupt vectors
        sta     NMIVEC+1        ; User program at ENTRY_POINT may change
        sta     IRQVEC+1        ; these vectors. Just do change before enabling
        lda     #<START         ; the interrupts, or you'll end up back in the d/l monitor.
        sta     NMIVEC
        sta     IRQVEC
        jsr     INITSER         ; Set up baud rate, parity, etc.

HEXDNLD:
        lda     #0
        sta     DLFAIL          ; Start by assuming no D/L failure
        jsr     PUTSTRI
        .byte   13,10,13,10
        .byte   "Send 6502 code in"
        .byte   " Intel Hex format"
        .byte   " at 19200,n,8,1 ->"
        .byte   13,10
        .byte   0               ; Null-terminate

HDWRECS:
        jsr     GETSER          ; Wait for start of record mark ':'
        cmp     #':'
        bne     HDWRECS         ; Not found yet

        ; Start of record marker has been found
        jsr     GETHEX          ; Get the record length
        sta     RECLEN          ; Save it
        sta     CHKSUM          ; And save first byte of checksum
        jsr     GETHEX          ; Get the high part of start address
        sta     START_HI
        clc
        adc     CHKSUM          ; Add in the checksum
        sta     CHKSUM
        jsr     GETHEX          ; Get the low part of the start address
        sta     START_LO
        clc
        adc     CHKSUM
        sta     CHKSUM
        jsr     GETHEX          ; Get the record type
        sta     RECTYPE         ; & save it
        clc
        adc     CHKSUM
        sta     CHKSUM
        lda     RECTYPE
        bne     HDER1           ; End-of-record

        ldx     RECLEN          ; Number of data bytes to write to memory
        ldy     #0              ; Start offset at 0

HDLP1:
        jsr     GETHEX          ; Get the first/next/last data byte
        sta     (START_LO),y    ; Save it to RAM
        clc
        adc     CHKSUM
        sta     CHKSUM
        iny                     ; Update data pointer
        dex                     ; Decrement count
        bne     HDLP1

        jsr     GETHEX          ; Get the checksum
        clc
        adc     CHKSUM
        bne     HDDLF1          ; If failed, report it

        ; Another successful record has been processed
        lda     #'#'            ; Character indicating record OK = '#'
        sta     SDR             ; Write it out but don't wait for output
        jmp     HDWRECS         ; Get next record

HDDLF1:
        lda     #'F'            ; Character indicating record failure = 'F'
        sta     DLFAIL          ; Download failed if non-zero
        sta     SDR             ; Write it to transmit buffer register
        jmp     HDWRECS         ; Wait for next record start

HDER1:
        cmp     #1              ; Check for end-of-record type
        beq     HDER2
        jsr     PUTSTRI         ; Warn user of unknown record type
        .byte   13,10,13,10
        .byte   "Unknown record type $"
        .byte   0               ; Null-terminate
        lda     RECTYPE         ; Get it
        sta     DLFAIL          ; Non-zero --> download has failed
        jsr     PUTHEX          ; Print it
        lda     #13             ; But we'll let it finish so as not to
        jsr     PUTSER          ; falsely start a new d/l from existing
        lda     #10             ; file that may still be coming in for
        jsr     PUTSER          ; quite some time yet.
        jmp     HDWRECS

HDER2:
        jsr     GETHEX          ; Get the checksum
        clc
        adc     CHKSUM          ; Add previous checksum accumulator value
        beq     HDER3           ; Checksum = 0 means we're OK!
        jsr     PUTSTRI         ; Warn user of bad checksum
        .byte   13,10,13,10
        .byte   "Bad record checksum!",13,10
        .byte   0               ; Null-terminate
        jmp     START

HDER3:
        lda     DLFAIL
        beq     HDEROK
        ; A download failure has occurred
        jsr     PUTSTRI
        .byte   13,10,13,10
        .byte   "Download Failed",13,10
        .byte   "Aborting!",13,10
        .byte   0               ; Null-terminate
        jmp     START

HDEROK:
        jsr     PUTSTRI
        .byte   13,10,13,10
        .byte   "Download Successful!",13,10
        .byte   "Jumping to location $"
        .byte   0               ; Null-terminate
        lda     #>ENTRY_POINT   ; Print the entry point in hex
        jsr     PUTHEX
        lda     #<ENTRY_POINT
        jsr     PUTHEX
        jsr     PUTSTRI
        .byte   13,10
        .byte   0               ; Stop lemming-like march of the program ctr. thru data
        jmp     ENTRY_POINT     ; Jump to canonical entry point

; Set up baud rate, parity, stop bits, interrupt control, etc. for the serial port.
INITSER:
        lda     #SCTL_V         ; Set baud rate 'n stuff
        sta     SCTL
        lda     #SCMD_V         ; Set parity, interrupt disable, n'stuff
        sta     SCMD
        rts

; SerRdy : Return
SERRDY:
        lda     SSR             ; Look at serial status
        and     #RX_RDY         ; Strip off "character waiting" bit
        rts                     ; If zero, nothing waiting.

; Warning: this routine busy-waits until a character is ready.
GETSER:
        lda $5000
        beq GETSER
        rts
;        lda     SSR             ; Look at serial status
;        and     #RX_RDY         ; See if anything is ready
;        beq     GETSER          ; Busy-wait until character comes in!
;        lda     SDR             ; Get the character
;        rts

; Get two hex digits and return as a byte
GETHEX:
        jsr     GETSER
        jsr     MKNIBL          ; Convert to 0..F numeric
        asl     a
        asl     a
        asl     a
        asl     a               ; This is the upper nibble
        and     #$F0
        sta     TMP
        jsr     GETSER
        jsr     MKNIBL
        ora     TMP
        rts                     ; Return with the nibble received

; Convert the ASCII nibble to numeric value from 0-F:
MKNIBL:
        cmp     #'9'+1          ; See if it's 0-9 or 'A'..'F' (no lowercase yet)
        bcc     MKNNH           ; If we borrowed, we lost the carry so 0..9
        sbc     #7+1            ; Subtract off extra 7 (sbc subtracts off one less)
MKNNH:
        sbc     #'0'-1          ; Subtract off '0' (if carry clear coming in)
        and     #$0F            ; No upper nibble no matter what
        rts                     ; And return the nibble

; Put byte in A as hexydecascii
PUTHEX:
        pha                     ; Save the byte
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        jsr     PRNIBL          ; Print the upper nibble
        pla                     ; Restore the byte
PRNIBL:
        and     #$0F            ; Strip off the low nibble
        cmp     #$0A
        bcc     NOTHX          ; If it's 0-9, add '0' else also add 7
        adc     #6              ; Add 7 (6+carry=1), result will be carry clear
NOTHX:
        adc     #'0'            ; If carry clear, we're 0-9
        ; Write the character in A as ASCII:
PUTSER:
        sta ACIA_BASE
;        sta     SDR             ; Write to transmit register
WRS1:
;        lda     SSR             ; Get status
;        and     #TX_RDY         ; See if transmitter is busy
;        beq     WRS1            ; If it is, wait
        rts

; Put the string following in-line until a NULL out to the console
PUTSTRI:
        pla                     ; Get the low part of "return" address (data start address)
        sta     DPL
        pla
        sta     DPH             ; Get the high part of "return" address
                                ; (data start address)
PSINB:
        ldy     #1
        lda     (DPL),y         ; Get the next string character
        inc     DPL             ; Update the pointer
        bne     PSICHO          ; If not, we're pointing to next character
        inc     DPH             ; Account for page crossing
PSICHO:
        ora     #0              ; Set flags according to contents of Accumulator
        beq     PSIX1           ; Don't print the final NULL
        jsr     PUTSER          ; Write it out
        jmp     PSINB           ; Back around
PSIX1:
        inc     DPL             ;
        bne     PSIX2           ;
        inc     DPH             ; Account for page crossing
PSIX2:
        jmp     (DPL)           ; Return to byte following final NULL

;; Vector table
;;.segment "VECTORS"
;.org $FFFA
;        .word   START           ; NMI vector
;        .word   START           ; Reset vector
;        .word   START           ; IRQ vector
