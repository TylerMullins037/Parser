#lang racket

; Parser for a simple calculator-style programming language

; Define a structure to hold the current state of parsing
(struct parser-state (tokens line-num current-line) #:mutable)

; Function to tokenize the input - FIXED to handle delimiters properly
(define (tokenize input-string)
  (define lines (string-split input-string "\n"))
  
  ; First check for invalid characters in any line
  (define invalid-char-result
    (for/fold ([result #f])
              ([line lines]
               [line-num (in-naturals 1)])
      (if result
          result  ; We already found an invalid character
          (let ([invalid-char-match (regexp-match #rx"[^a-zA-Z0-9\\s\\$=;\\(\\)\\ \\\r\\+\\<>-]" line)])
            (if invalid-char-match
                (cons line-num (car invalid-char-match))  ; Found invalid char
                #f)))))  ; No invalid char in this line
  
  (if invalid-char-result
      ; Return error information if invalid character found
      (values (format "Scanning error at line ~a: Invalid character '~a'" 
                     (car invalid-char-result) (cdr invalid-char-result))
              (car invalid-char-result))
      
      ; Else continue with normal tokenization
      (let ([tokens '()]
            [line-info '()])
        (for ([line lines]
              [line-num (in-naturals 1)])
          (define line-tokens
            (filter (lambda (token) (not (equal? token "")))
                    (for/list ([token (regexp-match* #rx"\\$\\$|=|;|\\(|\\)|\\+|-|<|<=|>|>=|==|!=|[a-zA-Z]+|[0-9]+"
                                                     line
                                                     #:match-select car)])
                      token)))
          
          (for ([token line-tokens])
            (set! tokens (append tokens (list token)))
            (set! line-info (append line-info (list (cons line-num line))))))
        
        (values tokens line-info))))

; Helper function to peek at the next token without consuming it
(define (peek-token state)
  (if (null? (parser-state-tokens state))
      ""
      (car (parser-state-tokens state))))

; Helper function to consume the current token and advance
(define (consume-token state)
  (when (not (null? (parser-state-tokens state)))
    (set-parser-state-tokens! state (cdr (parser-state-tokens state)))
    (when (not (null? (parser-state-current-line state)))
      (set-parser-state-line-num! state (caar (parser-state-current-line state)))
      (set-parser-state-current-line! state (cdr (parser-state-current-line state))))))

; Helper function to match and consume a specific token
(define (match-token state expected)
  (let ([current (peek-token state)])
    (if (equal? current expected)
        (begin (consume-token state) #t)
        #f)))

; Helper function to check if a string is a valid identifier
(define (is-id? token)
  (and (not (null? token))
       (regexp-match? #rx"^[a-zA-Z]+$" token)
       (not (member token '("if" "endif" "read" "write")))))  ;; Prevents treating keywords as identifiers

; Helper function to check if a string is a valid number
(define (is-num? token)
  (regexp-match? #rx"^[0-9]+$" token))

; Helper function to check if a string is a comparison operator
(define (is-compare? token)
  (member token '("<" "<=" ">" ">=" "==" "!=")))

; Helper function to get the current line text
(define (get-current-line state)
  (if (null? (parser-state-current-line state))
      ""
      (cdar (parser-state-current-line state))))

; Modified: Parser function for the 'program' non-terminal - no curly braces required
(define (parse-program state)
  (let ([result (list 'program)])
    (let ([stmt-list (parse-stmt-list state)])
      (if (not (string? stmt-list)) ; If no error
          (if (match-token state "$$")
              (append result (list stmt-list))
              (format "Syntax error at line ~a: Expected '$$' but found '~a'~a"
                      (parser-state-line-num state)
                      (peek-token state)
                      (get-current-line state)))
          stmt-list)))) ; Return the error

; Parser function for the 'stmt_list' non-terminal
(define (parse-stmt-list state)
  (let ([token (peek-token state)])
    (if (or (is-id? token) 
            (equal? token "if") 
            (equal? token "read") 
            (equal? token "write"))
        (let ([stmt (parse-stmt state)])
          (if (not (string? stmt)) ; If no error
              (let ([rest-stmt-list (parse-stmt-list state)])
                (if (not (string? rest-stmt-list)) ; If no error
                    (list 'stmt_list stmt rest-stmt-list)
                    rest-stmt-list)) ; Return the error
              stmt)) ; Return the error
        ; epsilon production
        (list 'stmt_list))))

(define (parse-stmt state)
  (let ([token (peek-token state)])
    (cond
      ;; Handle 'if' statements
      [(equal? token "if")
       (consume-token state)
       (if (match-token state "(")
           (let ([expr (parse-expr state)])
             (if (not (string? expr))
                 (if (match-token state ")")
                     (let ([stmt-list (parse-stmt-list state)])
                       (if (not (string? stmt-list))
                           (if (match-token state "endif")
                               (if (match-token state ";")
                                   (list 'stmt 'if expr stmt-list)
                                   (format "Syntax error at line ~a: Expected ';' after 'endif' but found '~a' ~a"
                                           (parser-state-line-num state)
                                           (peek-token state)
                                           (get-current-line state)))
                               (format "Syntax error at line ~a: Expected 'endif' but found '~a' ~a"
                                       (parser-state-line-num state)
                                       (peek-token state)
                                       (get-current-line state)))
                           stmt-list))
                     (format "Syntax error at line ~a: Expected ')' but found '~a' ~a"
                             (parser-state-line-num state)
                             (peek-token state)
                             (get-current-line state)))
                 expr))
           (format "Syntax error at line ~a: Expected '(' after 'if' but found '~a' ~a"
                   (parser-state-line-num state)
                   (peek-token state)
                   (get-current-line state)))]
      
      ;; Handle 'write' before ID
      [(equal? token "write")
       (consume-token state)
       (let ([expr (parse-expr state)])
         (if (not (string? expr))
             (if (match-token state ";")
                 (list 'stmt 'write expr)
                 (format "Syntax error at line ~a: Expected ';' but found '~a' ~a"
                         (parser-state-line-num state)
                         (peek-token state)
                         (get-current-line state)))
             expr))] ;; Return error if expression parsing fails

      ;; Handle 'read'
      [(equal? token "read")
       (consume-token state)
       (let ([id-node (parse-id state)])
         (if (not (string? id-node))
             (if (match-token state ";")
                 (list 'stmt 'read id-node)
                 (format "Syntax error at line ~a: Expected ';' but found '~a' ~a"
                         (parser-state-line-num state)
                         (peek-token state)
                         (get-current-line state)))
             id-node))]

      ;; Handle assignment separately
      [(is-id? token)  
       (let ([id-node (parse-id state)])
         (if (not (string? id-node)) 
             (if (match-token state "=")
                 (let ([expr (parse-expr state)])
                   (if (not (string? expr))
                       (if (match-token state ";")
                           (list 'stmt id-node '= expr)
                           (format "Syntax error at line ~a: Expected ';' but found '~a' ~a"
                                   (parser-state-line-num state)
                                   (peek-token state)
                                   (get-current-line state)))
                       expr)) 
                 (format "Syntax error at line ~a: Expected '=' but found '~a' ~a"
                         (parser-state-line-num state)
                         (peek-token state)
                         (get-current-line state)))
             id-node))] 

      ;; Catch-all error
      [else
       (format "Syntax error at line ~a: Expected id, 'if', 'read', or 'write' but found '~a' ~a"
               (parser-state-line-num state)
               token
               (get-current-line state))])))

; Parser function for the 'expr' non-terminal
(define (parse-expr state)
  (let ([token (peek-token state)])
    (cond
      [(is-id? token)
       (let ([id-node (parse-id state)])
         (if (not (string? id-node)) ; If no error
             (let ([etail (parse-etail state)])
               (if (not (string? etail)) ; If no error
                   (list 'expr id-node etail)
                   etail)) ; Return the error
             id-node))] ; Return the error
      [(or (is-num? token) 
           (equal? token "+") 
           (equal? token "-"))
       (let ([num-node (parse-num state)])
         (if (not (string? num-node)) ; If no error
             (let ([etail (parse-etail state)])
               (if (not (string? etail)) ; If no error
                   (list 'expr num-node etail)
                   etail)) ; Return the error
             num-node))] ; Return the error
      [else
       (format "Syntax error at line ~a: Expected id or number but found '~a' ~a"
               (parser-state-line-num state)
               token
               (get-current-line state))])))

; Parser function for the 'etail' non-terminal
(define (parse-etail state)
  (let ([token (peek-token state)])
    (cond
      [(equal? token "+")
       (consume-token state)
       (let ([expr (parse-expr state)])
         (if (not (string? expr)) ; If no error
             (list 'etail '+ expr)
             expr))] ; Return the error
      [(equal? token "-")
       (consume-token state)
       (let ([expr (parse-expr state)])
         (if (not (string? expr)) ; If no error
             (list 'etail '- expr)
             expr))] ; Return the error
      [(is-compare? token)
       (let ([compare (parse-compare state)])
         (if (not (string? compare)) ; If no error
             (let ([expr (parse-expr state)])
               (if (not (string? expr)) ; If no error
                   (list 'etail compare expr)
                   expr)) ; Return the error
             compare))] ; Return the error
      [else
       ; epsilon production
       (list 'etail)])))

; Parser function for the 'id' non-terminal
(define (parse-id state)
  (let ([token (peek-token state)])
    (if (is-id? token)
        (begin (consume-token state) (list 'id token))
        (format "Syntax error at line ~a: Expected identifier but found '~a' ~a"
                (parser-state-line-num state)
                token
                (get-current-line state)))))

; Parser function for the 'num' non-terminal
(define (parse-num state)
  (let ([numsign (parse-numsign state)])
    (if (not (string? numsign)) ; If no error
        (let ([token (peek-token state)])
          (if (is-num? token)
              (begin (consume-token state) (list 'num numsign token))
              (format "Syntax error at line ~a: Expected digit but found '~a' ~a"
                      (parser-state-line-num state)
                      token
                      (get-current-line state))))
        numsign))) ; Return the error

; Parser function for the 'numsign' non-terminal
(define (parse-numsign state)
  (let ([token (peek-token state)])
    (cond
      [(equal? token "+")
       (consume-token state)
       (list 'numsign "+")]
      [(equal? token "-")
       (consume-token state)
       (list 'numsign "-")]
      [else
       ; epsilon production
       (list 'numsign)])))

; Parser function for the 'compare' non-terminal
(define (parse-compare state)
  (let ([token (peek-token state)])
    (if (is-compare? token)
        (begin (consume-token state) (list 'compare token))
        (format "Syntax error at line ~a: Expected comparison operator but found '~a' ~a"
                (parser-state-line-num state)
                token
                (get-current-line state)))))

; Main parse function
(define (parse filename)
  (define input (file->string filename))
  (define-values (result line-num) (tokenize input))
  
  ; Check if tokenization detected an error
  (if (string? result)
      result ; Return the error message
      (let ([state (parser-state result 
                                (if (null? line-num) 0 (car line-num))
                                line-num)])
        (let ([parse-result (parse-program state)])
          (if (string? parse-result)
              parse-result ; Return the error message
              (string-append "Accept:" (format "~a" parse-result)))))))

; Export the parse function
(provide parse)


(parse "C:\\Users\\Tyler\\Downloads\\ParserInput-1\\file1.txt")
(parse "C:\\Users\\Tyler\\Downloads\\ParserInput-1\\file2.txt")
(parse "C:\\Users\\Tyler\\Downloads\\ParserInput-1\\file3.txt")
(parse "C:\\Users\\Tyler\\Downloads\\ParserInput-1\\file4.txt")
(parse "C:\\Users\\Tyler\\Downloads\\ParserInput-1\\file5.txt")