; ===================================================================
; 65c02 Homebrew Computer Code
; Description: This program runs on a custom-built 65c02 computer.
; Author: Donald Ray Moran
; ===================================================================
.setcpu "65c02"
.debuginfo

.include "libsd.asm"
.include "libfat32.asm"
.include "intel.s"
.include "Krusader_1.3_65C02.asm"
; -----------------------------------------------------------
; Zero Page Variables
; Description: Definitions of zero page variables used in the program.
; -----------------------------------------------------------
.zeropage
.org ZP_START0
READ_PTR:	        .res 1
WRITE_PTR:	      .res 1
;MSGL              = $FE
;MSGH              = $FF 
MSGL              = $B4
MSGH              = $B5
; -----------------------------------------------------------
; Data Segment
; Description: Contains static data used by the program.
; -----------------------------------------------------------
.segment "DATA"
;subdirname:  
;  .asciiz   	"BASIC      "
;filename:    
;  .asciiz   	"HELLOWRDBAS" 
; -----------------------------------------------------------
; Read-Only Data Segment
; Description: Contains read-only data and hardware definitions.
; -----------------------------------------------------------
.segment "RODATA"
ACIA_DATA   	    = $5000
ACIA_STATUS 	    = $5001
ACIA_CMD    	    = $5002
ACIA_CTRL   	    = $5003
;PORTB             = $6000
PORTA		          = $6001
;DDRB              = $6002
DDRA              = $6003
T1C_LO            = $6004
T1C_HI            = $6005
T2C_LO            = $6006
T2C_HI            = $6007
SR                = $6008
IRA               = $6009
IRB               = $600A
ACR               = $600B
PCR               = $600C
IFR               = $600D
IER               = $600E
PORTA_NO_HNDSHAKE = $600F
SD_CS   	        = %00010000
SD_SCK  	        = %00001000
SD_MOSI 	        = %00000100
SD_MISO 	        = %00000010
PORTA_OUTPUTPINS  = E | RW | RS | SD_CS | SD_SCK | SD_MOSI
input_prompt: .asciiz "Enter filename: "

;subdirname:  .res 12
;filename:    .res 12

.segment "BSS"
subdirname:  .res 12
filename:    .res 12
filename_temp	.res 12 
KRfilename_temp .res 12
SAREG .res 1 ; Storage Area for .A Register (Accumulator)
SXREG .res 1 ; Storage Area for .X Index Register
SYREG .res 1 ; Storage Area for .Y Index Register
SPREG .res 1 ; Storage Area for .P (Status) Register

;SAREG                   = $30C          ; Storage Area for .A Register (Accumulator)
;SXREG                   = $30D          ; Storage Area for .X Index Register
;SYREG                   = $30E          ; Storage Area for .Y Index Register
;SPREG                   = $30F          ; Storage Area for .P (Status) Register
; -----------------------------------------------------------
; Input Buffer
; Description: Defines an input buffer.
; -----------------------------------------------------------
.segment "INPUT_BUFFER"
INPUT_BUFFER:	.res $80
; -----------------------------------------------------------
; BIOS Segment
; Description: BIOS code segment for system initialization and management.
; -----------------------------------------------------------
.segment "BIOS"





; ===================================================================
; START Subroutine
; Description: This subroutine handles setting up the acia and jump
; to the reset vector which is defined in the Krusader source.
; ===================================================================
  .export STARTUP
STARTUP:
  lda #%00001011          ;No parity, no echo, no interrupt
  sta ACIA_CMD
  lda #%00011111          ;1 stop bit, 8 data bits, 19200 baud
  sta ACIA_CTRL  
; lda #%00001011          ; No parity, no echo, no interrupt
; sta ACIA_CMD
; lda #%01101111          ; 1 stop bit, 8 data bits, 115,200 baud
; sta ACIA_CTRL  
  jmp RESET 

.segment "RODATA"
subdirname_basic: .asciiz "BASIC      "
subdirname_aseembly: .asciiz "ASSEMBLY   "
;filename_basic:   .asciiz "HELLOWRDBAS"

.segment "CODE"
; ===================================================================
; LOAD Subroutine
; Description: This subroutine handles the loading process,
; initializing various components and loading a file from the SD card.
; ===================================================================
LOAD:
  jsr via_init                ; Initialise
  LDA #'.'
  JSR MONCOUT
  jsr sd_init
  LDA #'.'
  JSR MONCOUT
  jsr fat32_init
  LDA #'.'
  JSR MONCOUT
  jsr CRLF
  bcc initsuccessl
; ===================================================================
; Error during FAT32 initialization
; ===================================================================  
  jsr CRLF                    
  lda #<MSGFATERR
  sta MSGL
  lda #>MSGFATERR
  sta MSGH
  jsr SHWMSG
  jsr CRLF                    ; Message error stage
  lda #<MSGERRSTG
  sta MSGL
  lda #>MSGERRSTG
  sta MSGH
  jsr SHWMSG 
  lda fat32_errorstage
  jsr print_hex
  jsr CRLF
  RTS
; ===================================================================
; initsuccessl Label
; Description: Continues initialization if FAT32 setup was successful.
; ===================================================================
initsuccessl:
  jsr fat32_openroot		      ; Open root directory
  ldx #<subdirname			      ; Find subdirectory by name
  ldy #>subdirname
  jsr fat32_finddirent
  bcc foundsubdirl
; ===================================================================
; Error Subdirectory not found
; =================================================================== 
  jsr CRLF                    ; Message not found
  lda #<MSGSUBNOT
  sta MSGL
  lda #>MSGSUBNOT
  sta MSGH
  jsr SHWMSG
  jsr CRLF  
  RTS
; ===================================================================
; foundsubdirl Label
; Description: Proceeds if the subdirectory was found successfully.
; ===================================================================
foundsubdirl:
  jsr fat32_opendirent		    ; Open subdirectory
load_direct:  
  ldx #<filename			        ; Find file by name
  ldy #>filename
  jsr fat32_finddirent
  bcc foundfile
  jsr CRLF                    ; Message not found
  lda #<MSGFILNOT
  sta MSGL
  lda #>MSGFILNOT
  sta MSGH
  jsr SHWMSG
  jsr CRLF  
  RTS
; ===================================================================
; foundfile Label
; Description: This section is responsible for preparing the memory 
; locations to load the found file into the correct addresses.
; Specifically, it sets the low and high byte positions of the load 
; address (TXTTAB) and stores these values accordingly.
; ===================================================================
foundfile:
                              ; Set low byte of TXTTAB load address
  pha             			      ; PHA pushes Accumulator contents to Stack
  lda TXTTAB        			    ; Put a value of zero into the accumulator
  sta VARTAB								  ; Initialize VARTAB
  sta $00         			      ; Store 00 at location 00 
                              ; Set high byte of TXTTAB+1 load address
  lda TXTTAB+1        			  ; Put a value of 08 into the accumulator
  sta VARTAB+1								; Initialize VARTAB+1
  sta $01         			      ; Store Accumulator at 01 (0001 = 0080, little endian)
  pla  			  			          ; Restore our origional value in register A
  jsr fat32_opendirent        ; Open subdirectory
  ldx #0                      ; Reset X register to zero
  ldy #0                      ; Set index to 0
my_read_byte:
  phy                         ; Push Y register to the stack
  jsr fat32_file_readbyte     ; Returns byte in A and carry if at end of file
  ply                         ; Pull Y register from the stack
CNT:
  bcs done_readldr            ; Carry set when at end of file
  sta ($00),y                 ; Store in $00 (zero-page indirect), the value
  inc VARTAB                  ; Increment low byte of VARTAB to maintain for Basic
  iny                         ; Increment Y
  cpy #0
  bne my_read_byte            ; Skip increment high byte if no page boundary cross
  inc $01                     ; Increment high byte of pointer (our position in memory)
  inc VARTAB+1                ; Increment high byte of VARTAB to maintain for Basic
  jmp my_read_byte            ; Continue if not done
; ===================================================================
; done_readldr Label
; Description: Finalizes the loading process and fixes variable links.
; ===================================================================
done_readldr:                
  lda #<MSGLOADC              ; Message success
  sta MSGL
  lda #>MSGLOADC
  sta MSGH
  jsr SHWMSG
  jsr CRLF
  ;jsr FIX_LINKS               ; VARTAB maintained, call FIX_LINKS
  rts                         ; Never reached ?





SAVE_BASIC:
  jsr BASIC_SUBDIRNAME
  jsr gather_filename         ; Get the file name
  JMP SAVE
  
SAVE_KRUSADER_SRC:
  JSR SET_ASSEMBLY_SUBDIRNAME
  jsr KRkget_filename              ; Get the file name
  lda SRCSTL                       ; $FE
  sta TXTTAB
  lda SRCSTH                       ; $FF
  sta TXTTAB+1
  lda CURLNL                       ; $FC
  sta VARTAB
  lda CURLNH                       ; $FD
  sta VARTAB+1
  JMP SAVE

; ===================================================================
; SAVE Subroutine
; Description: This subroutine is responsible for saving a file to 
; the FAT32 filesystem on the SD card. It gathers the filename, 
; initializes necessary components, allocates space for the file, 
; writes the directory entry, and finally writes the file data.
; ===================================================================
SAVE:
  ;jsr BASIC_SUBDIRNAME
  ;jsr gather_filename         ; Get the file name
  jsr via_init                ; Initialise
  LDA #'.'
  JSR MONCOUT
  jsr sd_init
  LDA #'.'
  JSR MONCOUT
  jsr fat32_init
  LDA #'.'
  JSR MONCOUT
  bcc initsuccess1
; ===================================================================
; Error during FAT32 initialization
; ===================================================================
  jsr CRLF                
  lda #<MSGFATERR
  sta MSGL
  lda #>MSGFATERR
  sta MSGH
  jsr SHWMSG
  jsr CRLF                    ; Message error stage
  lda #<MSGERRSTG
  sta MSGL
  lda #>MSGERRSTG
  sta MSGH
  jsr SHWMSG 
  lda fat32_errorstage
  jsr print_hex
  jsr CRLF
  RTS
; ===================================================================
; initsuccess1 Label
; Description: Continues initialization if FAT32 setup was successful.
; ===================================================================
initsuccess1:
  ; Allocating
  lda #'.'
  jsr MONCOUT
  sec                         ; Allocate space for the file
  lda VARTAB
  sbc TXTTAB
  sta fat32_bytesremaining
  lda VARTAB+1
  sbc TXTTAB+1
  sta fat32_bytesremaining+1
  ; Round the size up to the next whole sector (256 bytes)
  lda fat32_bytesremaining
  clc                         ; Clear carry for addition
  adc #$FF                    ; Add 255 (0xFF) to round up
  and #$FF00                  ; Mask off to align with 256 bytes
  sta fat32_bytesremaining
  pha
  lda fat32_bytesremaining+1
  adc #0                      ; Add any carry from the previous addition
  sta fat32_bytesremaining+1  ; Store the high byte
  pha
  jsr fat32_allocatefile    ; Allocate space for the file
  lda #'.'                  ; Opening Directory
  jsr MONCOUT
  jsr fat32_openroot        ; Open root directory
  ldx #<subdirname          ; Find subdirectory by name
  ldy #>subdirname
  jsr fat32_finddirent
  bcc foundsubdir1
; ===================================================================
; Subdirectory not found error
; ===================================================================
  jsr CRLF                    ; Message not found
  lda #<MSGSUBNOT
  sta MSGL
  lda #>MSGSUBNOT
  sta MSGH
  jsr SHWMSG
  jsr CRLF
  pla
  pla  
  RTS
; ===================================================================
; foundsubdirl Label
; Description: Proceeds if the subdirectory was found successfully.
; ===================================================================
foundsubdir1:
  jsr fat32_opendirent        ; Open subdirectory
  lda #'.'                    ; Writing dirent
  jsr MONCOUT
  pla                         ; Restore filesize
  sta fat32_bytesremaining+1  
  pla
  sta fat32_bytesremaining
  lda #<filename              ; Store filename ponter
  sta fat32_filenamepointer
  lda #>filename
  sta fat32_filenamepointer+1
  jsr fat32_writedirent       ; Write the directory entry
  lda #'.'
  jsr MONCOUT
  lda TXTTAB                  ; Now write the file data
  sta fat32_address
  lda TXTTAB+1
  sta fat32_address+1
  jsr fat32_file_write
  jsr CRLF                    
  lda #<MSGSAVEC              ; Message success
  sta MSGL
  lda #>MSGSAVEC
  sta MSGH
  jsr SHWMSG
  jsr CRLF
  rts





; ===================================================================
; MONRDKEY and alias CHRIN Subroutine
; Description: Inputs a character from the serial interface.
; If no key is pressed, returns with carry clear. If a key is 
; pressed, echoes the character and returns with carry set.
; ===================================================================
.export MONRDKEY
MONRDKEY:
.export CHRIN
CHRIN:									      
  LDA ACIA_DATA	
  beq @no_keypressed	
  jsr CHROUT								  ;echo
  sec
  rts
@no_keypressed:
  clc
  rts
  
  
  
  
; ===================================================================
; MONCOUT and alias CHROUT Subroutine
; Description: Outputs a character from the Accumulator to the 
; serial interface.
; ===================================================================
.export MONCOUT
MONCOUT:
.export CHROUT
;CHROUT:
;  pha
;  cmp #$0D
;  bne normalchar
;  CMP #$0A
;  BEQ SKIP0A
;  jsr realputchar
;SKIP0A:
;  pla
;  rts
;
;normalchar:
;  jsr realputchar
;  pla
;  rts
;
;realputchar:
;  sta ACIA_DATA
;  rts
CHROUT:									     
  pha
  sta ACIA_DATA
  pla
  rts



 
  
; ===================================================================
; MON Subroutine
; Description: This subroutine transitions the system to the Wozmon 
; monitor. It outputs a carriage return and a line feed, and then 
; jumps to the Wozmon monitor's start address ($FE00). This supports
; a basic token.
; ===================================================================
.export MON
MON:									        ;Added not in Ben Eaters
  LDA #$0D
  JSR MONCOUT
  LDA #$0A
  JSR MONCOUT 
;  JMP $FE00
  ;JMP $FF1B ; GETLINE
  JMP $FF00





; ===================================================================
; print_hex Subroutine
; Description: Converts the high nibble of the Accumulator to its
; hexadecimal representation and prints it using the `print_nybble`
; subroutine.
; ===================================================================
.export print_hex
print_hex:
  pha
  ror
  ror
  ror
  ror
  jsr print_nybble
  pla
; ===================================================================
; print_nybble Subroutine
; Description: Converts the low nibble of the Accumulator to its
; hexadecimal representation and prints it. Handles letters A-F.
; ===================================================================
print_nybble:
  and #15
  cmp #10
  bmi skipletter
  adc #6
skipletter:
  adc #48
  jsr MONCOUT
  rts





; ===================================================================
; via_init Subroutine
; Description: Initializes the VIA by setting all pins on port B 
; to output and configuring specific pins on port A to output.
; ===================================================================
.export via_init
via_init:
  lda #%11111111          		; Set all pins on port B to output
  sta DDRB
  lda #PORTA_OUTPUTPINS   		; Set various pins on port A to output
  sta DDRA
  rts




;-----------------------------------------------------------------------
; gather_filename
;-----------------------------------------------------------------------
; Routine to gather the file name for saving. Evaluates the provided 
; formula, processes the result, and extracts the file name, storing it 
; in a specified memory location (filename) with a null terminator.
;
; - Assumes FRMEVL, FOUT, STRLIT, FREFAC, and MONCOUT subroutines are 
;   defined elsewhere. On a BASIC 1.1 installation, these should be
;   already present.
;-----------------------------------------------------------------------
gather_filename:
  jsr FRMEVL                  ; Evaluate formula
  bit VALTYP                  ; Check if it's a string
  bmi save_print              ; If so, continue
  jsr FOUT                    ; Convert floating-point to string
  jsr STRLIT                  ; Ensure it's a string literal
save_print:
  jsr FREFAC                  ; Free memory and get string length in A
  tax                         ; Transfer length from A to X
  ldy #0                      ; Initialize index
save_print_loop:
  lda (INDEX),y               ; Load character from string
  cmp #'.'                    ; Check if it's a '.'
  beq extension               ; If yes, handle extension
  sta filename,y              ; Store character in filename
  iny                         ; Increment index
  dex                         ; Decrement length counter
  bne save_print_loop         ; Continue loop if not done
end_filename:
  lda #0                      ; Null terminator
  sta filename,y              ; End filename with a null
  rts
extension:
  tya                         ; Save current index (position of '.')
  tax                         ; Transfer to X
fill_spaces:
  cpy #8                      ; Check if we've filled up to 8 characters
  beq restore_y               ; If yes, restore Y and handle extension
  lda #' '                    ; Load space character
  sta filename,y              ; Fill with space
  iny                         ; Increment index
  bra fill_spaces             ; Continue filling spaces
restore_y:
  txa                         ; Restore Y to the position of '.'
  tay
  iny                         ; Move past '.'
  lda (INDEX),y               ; Load first character of extension
  beq end_filename_ext        ; If null, end
  sta filename+8              ; Store in filename+8
  iny                         ; Increment index
  lda (INDEX),y               ; Load second character of extension
  beq end_filename_ext        ; If null, end
  sta filename+9              ; Store in filename+9
  iny                         ; Increment index
  lda (INDEX),y               ; Load third character of extension
  beq end_filename_ext        ; If null, end
  sta filename+10             ; Store in filename+10
end_filename_ext:
  lda #0                      ; Null terminator
  sta filename+8,x            ; End extension with a null
  rts




; ===================================================================
; SHWMSG Subroutine
; Description: Displays a message stored at the address pointed to 
; by the MSGL variable. The message is displayed one character at a 
; time until a null terminator is encountered.
; ===================================================================
SHWMSG:      
  LDY #$0
PRINTME:       
  LDA (MSGL),Y
  BEQ DONE
  JSR MONCOUT
  INY 
  BNE PRINTME
DONE:        
  RTS   
  
  
BASIC_SUBDIRNAME:
; Address of subdirname in RAM
  lda #<subdirname
  sta MSGL
  lda #>subdirname
  sta MSGH
update_basic_subdirname:
; Update subdirname to "ASSEMBLY"
  ldx #0
  ldy #0
copy_basic_subdirname:
  lda subdirname_basic, x
  sta (MSGL), y
  inx
  iny
  cpy #12
  bne copy_basic_subdirname
  LDA #0
  sta (MSGL), y
  rts 
  

LS_BASIC_SUBDIRNAME:
; Address of subdirname in RAM
  lda #<subdirname
  sta MSGL
  lda #>subdirname
  sta MSGH
update_ls_basic_subdirname:
; Update subdirname to "ASSEMBLY"
  ldx #0
  ldy #0
copy_ls_basic_subdirname:
  lda subdirname_basic, x
  sta (MSGL), y
  inx
  iny
  cpy #12
  bne copy_ls_basic_subdirname
  LDA #0
  sta (MSGL), y
  jmp LS





LS_ASSEMBLY_SUBDIRNAME:
  jsr SET_ASSEMBLY_SUBDIRNAME
  jmp LS
  rts






SET_ASSEMBLY_SUBDIRNAME:
; Address of subdirname in RAM
  lda #<subdirname
  sta MSGL
  lda #>subdirname
  sta MSGH
update_assembly_subdirname:
; Update subdirname to "ASSEMBLY"
  ldx #0
  ldy #0
copy_assembly_subdirname:
  lda subdirname_aseembly, x
  sta (MSGL), y
  inx
  iny
  cpy #12
  bne copy_assembly_subdirname
  LDA #0
  sta (MSGL), y
  rts


; ===================================================================
; LS Subroutine
; Description: This subroutine initializes various components, 
; including PORTA, PORTB, the SD card, and FAT32 filesystem. It then 
; attempts to open the root directory and read the contents of a 
; specified subdirectory, printing file names to the monitor.
; ===================================================================
indicator = $24 
.export LS
LS:
  ; Initialise
  stz indicator
  
  LDA #$0D
  JSR MONCOUT
  LDA #$0A
  JSR MONCOUT
  ;  
  LDA #<MSGINIT
  STA MSGL
  LDA #>MSGINIT
  STA MSGH
  JSR SHWMSG
  ;
  jsr via_init                ; Initialyze PORTA and PORTB            
  ;
  lda #'.'
  jsr MONCOUT
  ;
  jsr sd_init                 ; Initialyze SD card
  ;                           
  lda #'.'                    
  jsr MONCOUT                 
  ;                           
  jsr fat32_init              ; Initialyze FAT32
  ;                           
  lda #'.'                    
  jsr MONCOUT                 
  ;                           
  bcc initsuccess             
; ===================================================================
; Error during FAT32 initialization
; ===================================================================  
  jsr CRLF                    
  lda #<MSGFATERR
  sta MSGL
  lda #>MSGFATERR
  sta MSGH
  jsr SHWMSG
  jsr CRLF                    ; Message error stage
  lda #<MSGERRSTG
  sta MSGL
  lda #>MSGERRSTG
  sta MSGH
  jsr SHWMSG 
  lda fat32_errorstage
  jsr print_hex
  jsr CRLF
  RTS                    
initsuccess:                  
  LDA #$0D                    
  JSR MONCOUT                 
  LDA #$0A                    
  JSR MONCOUT                 
  LDA #<MSGMOUNT              
  STA MSGL                    
  LDA #>MSGMOUNT              
  STA MSGH                    
  JSR SHWMSG                  
  LDA #$0D                    
  JSR MONCOUT                 
  LDA #$0A                    
  JSR MONCOUT                 
  jsr fat32_openroot		      ; Open root directory
  ldx #<subdirname			      ; Find subdirectory by name
  ldy #>subdirname            
  jsr fat32_finddirent        
  bcc foundsubdir    
; ===================================================================
; Subdirectory not found error
; ===================================================================
  jsr CRLF                    ; Message not found
  lda #<MSGSUBNOT
  sta MSGL
  lda #>MSGSUBNOT
  sta MSGH
  jsr SHWMSG
  jsr CRLF  
  RTS                       
foundsubdir:  
; ===================================================================
; Subdirectory found
; ===================================================================                
  jsr fat32_opendirent	      ; Open subdirectory
  ;LDX #0                     
my_read_directory:            
  jsr fat32_readdirent 	      ; A is set to the file's attribute byte and zp_sd_address points at the returned directory entry
  bcs done_read 			        ; Carry set when at end of directory
  ; FIRST TIME                
  lda #1                      ; Just an indicator
  cmp indicator               
  BNE PRINT_HEADING           ; First cycle so have not printed heading yet
; ===================================================================
; First cycle we need to print the heading
; ===================================================================
  LDY #0                      
print_dirlist:  
; ===================================================================
; Print the list
; ===================================================================              
  ;INX                        
  ;CPX RM_KRUSADER:
  ;JSR SET_ASSEMBLY_SUBDIRNAME
  ;jsr KRkget_filename              ; Get the file name
  ;JMP RM#3                     
  ;BCC my_read_directory      ; Skip the first 2 entries which are . and ..
  lda (zp_sd_address),y       
  jsr MONCOUT                 
  iny                         
  cpy #8                      
  beq print_tabs              
cmp11:                        
  cpy #11                     
  bne print_dirlist           
  LDA #$0D				            ;Next line
  JSR MONCOUT                 
  LDA #$0A                    
  JSR MONCOUT                 
nextentry:                    
  bra my_read_directory       ; Branch till we don't need to branch no more
done_read:                    
;  jsr CRLF
  stz indicator
  clc
  rts
; ===================================================================
; print_tabs Subroutine
; Description: Outputs two tab characters to the monitor.
; ===================================================================
print_tabs:
  LDA #$09
  JSR MONCOUT
  LDA #$09
  JSR MONCOUT	
  bra cmp11
; ===================================================================
; PRINT_HEADING Subroutine
; Description: Prints the heading for the directory listing by 
; displaying a top bar, a title, and a bottom bar. Then it starts 
; printing the directory list.
; ===================================================================
PRINT_HEADING:
  LDA #1                      ; Just an indicator
  STA indicator  
  JSR CRLF
  ;
  LDA #<MSGBAR                ; Top Bar
  STA MSGL
  LDA #>MSGBAR
  STA MSGH
  JSR SHWMSG
  ;
  JSR CRLF
  ;
  LDA #<MSGDIRLST             ; Title Dir List
  STA MSGL
  LDA #>MSGDIRLST
  STA MSGH
  JSR SHWMSG
  ;
  JSR CRLF
  ;
  LDA #<MSGBAR                ; Bottom Bar
  STA MSGL
  LDA #>MSGBAR
  STA MSGH
  JSR SHWMSG
  ;
  JSR CRLF
  ;
  LDY #0                      ; Print the list
  jmp print_dirlist


RM_BASIC:
  jsr BASIC_SUBDIRNAME
  jsr gather_filename         ; Get the file name
  JMP RM

RM_KRUSADER:
  JSR SET_ASSEMBLY_SUBDIRNAME
  jsr KRkget_filename              ; Get the file name
  JMP RM

; ===================================================================
; RM Subroutine
; Description: Handles the deletion of a file in a specified subdirectory.
; This subroutine gathers the file name, initializes the system, 
; navigates to the subdirectory, and deletes the file.
; ===================================================================
RM:
;  jsr print_filename                                          
;  jsr CRLF
  jsr via_init                ; Initialise
  LDA #'.'
  JSR MONCOUT
  jsr sd_init
  LDA #'.'
  JSR MONCOUT
  jsr fat32_init
  LDA #'.'
  JSR MONCOUT
  jsr CRLF
  bcc initsuccessrm
  lda #'Z'                    ; Error during FAT32 initialization
  JSR MONCOUT
  lda fat32_errorstage
  jsr print_hex
  RTS
; ===================================================================
; initsuccessl Label
; Description: Continues initialization if FAT32 setup was successful.
; ===================================================================
initsuccessrm:
  jsr fat32_openroot		      ; Open root directory
  ldx #<subdirname			      ; Find subdirectory by name
  ldy #>subdirname
  jsr fat32_finddirent
  bcc foundsubdirrm
  lda #'X'					          ; Subdirectory not found
  JSR MONCOUT
  RTS
; ===================================================================
; foundsubdirl Label
; Description: Proceeds if the subdirectory was found successfully.
; ===================================================================
foundsubdirrm:
  jsr fat32_opendirent		    ; Open subdirectory
  ldx #<filename			        ; Find file by name
  ldy #>filename
  jsr fat32_finddirent
  bcc foundfilerm
  lda #'Y'				  	        ; File not found
  JSR MONCOUT
  RTS
; ===================================================================
; foundfile Label
; Description: This section is responsible for preparing the memory 
; locations to load the found file into the correct addresses.
; Specifically, it sets the low and high byte positions of the load 
; address (TXTTAB) and stores these values accordingly.
; ===================================================================
foundfilerm:
  jsr fat32_deletefile
;  JSR CRLF
  LDA #<MSGRMFL               ; MSG Remove success            
  STA MSGL
  LDA #>MSGRMFL
  STA MSGH
  JSR SHWMSG
  JSR CRLF
  rts
  
  
  
  
  
; ===================================================================
; Placeholder Subroutines
; Description: These placeholders are defined to ensure that the 
; token table remains valid for any BASIC programs created now. The 
; actual implementations will be added later.
; ===================================================================  
  SYS:
                jsr FRMNUM              ; Eval formula
                jsr GETADR              ; Convert to int. addr
                lda #>SYSRETURN         ; Push return address
                pha
                lda #<SYSRETURN
                pha
                lda SPREG               ; Status reg
                pha
                lda SAREG               ; Load 6502 regs
                ldx SXREG
                ldy SYREG
                plp                     ; Load 6502 status reg
                jmp (LINNUM)            ; Go do it
SYSRETURN=*-1                
                php                     ; Save status reg
                sta SAREG               ; Save 6502 regs
                stx SXREG
                sty SYREG
                pla                     ; Get status reg
                sta SPREG
                rts 




; ===================================================================
; CLS Label
; Description: This section is responsible to clear the screen and
; return the cursor to the home position via ansi escape codes. 
; ===================================================================  
CLS:
  LDA #$1B
  JSR MONCOUT
  ldx #$00
@L
  LDA CL,X
  BEQ @D
  JSR MONCOUT
  INX
  JMP @L
@D
  ;
  LDA #$1B
  JSR MONCOUT
  ldx #$00
@H
  LDA HME,X
  BEQ @E
  JSR MONCOUT
  INX
  JMP @H
@E
  RTS
CL:  .BYTE "[2J",0
HME: .BYTE "[H",0


BASIC_LOAD:
  jsr BASIC_SUBDIRNAME
  jsr gather_filename         ; Get the file name
  JSR LOAD
  jsr FIX_LINKS               ; VARTAB maintained, call FIX_LINKS
  RTS

KR_LOAD:
  jsr SET_ASSEMBLY_SUBDIRNAME
	jsr KRkget_filename              ; Get the file name
  lda SRCSTL
  sta TXTTAB
  lda SRCSTH
  sta TXTTAB+1
  
;  lda #0                           ; Clear first byte at TXTTAB
;  sta (TXTTAB)                     ; Ensure no stale data
  
  jsr LOAD
;	jsr PANIC
  RTS

; ===================================================================
; Get filename Subroutine
; Description: This subroutine is responsible for saving a file to 
; the FAT32 filesystem on the SD card. It gathers the filename, 
; initializes necessary components, allocates space for the file, 
; writes the directory entry, and finally writes the file data.
; ===================================================================
KRkget_filename:
	jsr CRLF
	ldx #0                   
	ldy #0                   
	lda #<input_prompt
	sta MSGL
	lda #>input_prompt
	sta MSGH
	ldy #0
lp_KRask_filename:
	lda (MSGL),y
	beq KRget_char
	jsr MONCOUT
	iny
	bra lp_KRask_filename
KRget_char:
	jsr KRk_get_input          
	cmp #$0D                 
	beq KRKRcheck_filename_init  
	sta KRfilename_temp,x      
	inx                      
	cpx #12                  
	bne KRget_char             
	lda #0                   
	sta KRfilename_temp,x      
KRKRcheck_filename_init:
  JSR CRLF
	ldx #0                   
KRcheck_filename:
	lda KRfilename_temp,x      
	cmp #'.'                 
	beq KRfill_extension       
	cpx #8
	beq KRfill_extension
	sta filename,x
	inx
	bra KRcheck_filename       
KRfill_extension:
	cpx #8
	beq KRcont
	txa
	tay
KRfiller:
	lda #' '
	sta filename,y
	iny
	cpy #8
	beq KRcont
	jmp KRfiller
KRcont:
	inx                      
	cpx #12
	beq KRdne
	lda KRfilename_temp,x
	sta filename,y
	iny
	cpy #11
	beq KRdne
	jmp KRcont
KRdne:
	lda #0
	sta filename,y
	rts
  
KRk_get_input:
	jsr MONRDKEY
	beq KRk_get_input
	rts                      ; Return from subroutine
; ===================================================================
; Message Constants
; Description: Defines various messages used throughout the program,
; including initialization, status updates, and directory listings.
; ===================================================================
MSGINIT:     .BYTE     "Initializing Storage",0
MSGMOUNT:    .BYTE     "SD Mounted.",0
MSGBAR:      .BYTE     "---------------------------------",0
MSGDIRLST:   .BYTE     "     DIRECTORY LIST             ",0
MSGSAVEC:    .BYTE     "Save complete.",0
MSGLOADC:    .BYTE     "Load complete.",$0A,$0D,"] ",0                  ; Hack prompt then fall into fix_links
MSGRMFL:     .BYTE     "File removed.",0
MSGFILNOT:   .BYTE     "File not found.",0
MSGSUBNOT:   .BYTE     "Subdirectory not found.",0
MSGFATERR:   .BYTE     "Error during FAT32 initialization.",0
MSGERRSTG:   .BYTE     "Error stage: ",0
