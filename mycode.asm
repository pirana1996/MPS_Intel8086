;0716:0007
; multi-segment executable file template.
;
;
;   PROEKTPROEKTPROEKTPROEKTPROEKTPROEKT
; PROEKT                              PROEKT
;   PROEKTPROEKTPROEKTPROEKTPROEKTPROEKT
;
;
;

; multi-segment executable file template.

data segment
    ; add your data here!     
    ;msg1 db " INSTRUKCIJA INSERT! $" 
    ;msg2 db " INSTRUKCIJA READ! $" 
    ;msg3 db " INSTRUKCIJA REMOVE! $"
    
    msg_empty db "Nema poraka$"
    valid_flag db ?  
    counter db ?
    tmp db ?  
    tmp_w dw ?
    tmp_message db 30 dup(?)    
    index db ?
    ;str1 db 300 dup(?)   
    m_str db ? ;mod_string: 1-vnes; 2-Citaj; 3-Brisi   
    i_cnt db ? ;insert_string
    r_cnt db ? ;read_string
    e_cnt db ? ;delete_string  
    str1 db 300 dup(?)
ends

stack segment
    dw   128  dup(0)
ends

code segment


vnes PROC
    REPEAT:
        MOV ah, 01h
        int 21h
        ;MOV [bx], al  
           
        i_check:
            CMP al, 'I'
            JNE r_check
                ;to do if 'i'
                MOV i_cnt, 1  
            JMP end_iteration   
        r_check:
            CMP al, 'R'
            JNE e_check
                ;to do if 'r'
                MOV r_cnt, 1  
            JMP second_check               
        e_check:               
            CMP al, 'E'
            JNE second_check
                 ;to do if 'e'
                 MOV e_cnt, 1  
            JMP end_iteration
        second_check:    
            i_cnt_check:
                CMP i_cnt, 1
                JNE r_cnt_check 
                    CMP al, 'N'    
                    JNE r_cnt_check
                    ;to do if i_cnt==1     
                    MOV i_cnt, 2
                JMP end_iteration
            r_cnt_check:        
                CMP r_cnt, 1
                JNE e_cnt_check
                    CMP al, 'D' 
                    JNE e_cnt_check
                    ;to do if r_cnt==1
                    MOV r_cnt, 2   
                JMP end_iteration
            e_cnt_check:
                CMP e_cnt, 1
                JNE third_check
                    CMP al, 'R' 
                    JNE third_check
                    MOV e_cnt, 2
                JMP end_iteration
        third_check:     
            i_s_check:
                CMP i_cnt, 2
                JNE check_final
                     CMP al, 'S'
                     JNE check_final
                     MOV i_cnt, 3 
                JMP end_iteration
            check_final:
            ;to do        
                  
            MOV tmp, al  
            MOV tmp_w, bx
            MOV index, al
            SUB index, 48d
            i_cnt_success:     
                CMP i_cnt, 3d
                JNE r_cnt_success
                    ;MOV dl, offset msg1
                    ;MOV ah, 09h
                    ;int 21h 
                    MOV i_cnt, 0 
                    CALL input_validation  
                    CMP valid_flag, 1
                    JNE cnt_success
                        CALL save_message
                JMP cnt_success
            r_cnt_success:                 
                CMP r_cnt, 2d
                JNE e_cnt_success   
                    ;MOV dl, offset msg2
                    ;MOV ah, 09h
                    ;int 21h     
                    MOV r_cnt, 0 
                    MOV ah, 01h
                    int 21h 
                    CMP al, '$'
                    JNE unknown_input
                    CALL read_message
                JMP cnt_success
            e_cnt_success:            
                CMP e_cnt, 2d
                JNE end_iteration 
                    ;MOV dl, offset msg3
                    ;MOV ah, 09h
                    ;int 21h     
                    MOV e_cnt, 0
                    MOV ah, 01h
                    int 21h 
                    CMP al, '$'
                    JNE unknown_input  
                    CALL remove_message
            
            unknown_input:  
                MOV bx, tmp_w   
                MOV i_cnt, 0
                MOV r_cnt, 0
                MOV e_cnt, 0
                JMP i_check
            
            cnt_success:  
                MOV al, tmp  
                MOV bx, tmp_w   
                MOV i_cnt, 0
                MOV r_cnt, 0
                MOV e_cnt, 0           
                
            
        end_iteration:     
            INC bx
    CMP al, 36d
    JE end
    JMP REPEAT 
  end:     
    RET
vnes ENDP

input_validation PROC   
    MOV bx, OFFSET tmp_message
    REPEAT_VALIDATION:
        MOV ah, 01h
        int 21h         
        
        MOV [bx], al   
        INC bx
        CMP al, '$'
        JE validate     ;end validation (Message is correct)
        
        CMP al, 'A'
        JB no_validate
        CMP al, 'Z'
        JA no_validate
        
    JMP REPEAT_VALIDATION 
    validate: 
        MOV valid_flag, 1d
        JMP end_validation
    no_validate:
        MOV valid_flag, 0d
    end_validation:       
     
    RET
input_validation ENDP             
 
save_message PROC   
    LEA si, tmp_message
    LEA di, str1
    MOV ax, 0   
    MOV al, index   
    MOV dl, 30
    MUL dl    
    ADD di, ax
    loop_message:
        MOVSB
    CMP [si-1], '$'
    JNE loop_message   
    RET
save_message ENDP 

read_message PROC
    MOV bx, OFFSET str1
    MOV ax, 0
    MOV al, index
    MOV dl, 30
    MUL dl    
    ADD bx, ax
                  
    MOV counter, 0                 
    REPEAT_READ:    
        MOV ah, 02h
        MOV dl, [bx]
        
        CMP [bx], '$'
        JE END_REPEAT_READ   
        
        int 21h  
        
        INC counter        
        INC bx
    JMP REPEAT_READ
    END_REPEAT_READ: 
    CMP counter, 0  
    JNE ne_prazna
        MOV dl, OFFSET msg_empty
        MOV ah, 09h
        INT 21h   
    ne_prazna:
    RET
read_message ENDP 

remove_message PROC
    MOV bx, OFFSET str1
    MOV ax, 0
    MOV al, index
    MOV dl, 30
    MUL dl    
    ADD bx, ax 
    
    MOV [bx], '$'
      
    RET
remove_message ENDP
             
start:
; set segment registers:
    mov ax, data
    mov ds, ax
    mov es, ax
                    
    
    LEA bx, str1
    MOV al, 0 ; al - counter[0, 9]
    set_empty_messages:
        MOV [bx], '$'
        ADD bx, 30d       
        INC al
    CMP al, 9
    JBE set_empty_messages
                    
    ; add your code here
    MOV i_cnt, 0d
    MOV r_cnt, 0d
    MOV e_cnt, 0d
    MOV bx, offset str1   
    CALL vnes
    
     
    
    mov ax, 4c00h ; exit to operating system.
    int 21h    
ends

end start ; set entry point and stop the assembler.
