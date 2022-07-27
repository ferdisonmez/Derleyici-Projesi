;; Liste Tanımlama
(defvar tokens_list '(nil))
(defvar val_list '(nil))

;; lexer kısmı *************************************************************

(defun is_in_alphabet(alpha)		; Harf Kontrol
	(if (not(or (and (> (char-int alpha) 96) (< (char-int alpha) 123))
			(and (> (char-int alpha) 64) (< (char-int alpha) 91))
		))
		nil
		t
	)
)
(defun digit_control(digit)		; Rakam Kontrol
	(if (not(and (> (char-int digit) 47) (< (char-int digit) 58)))
		nil
		t)
)

(defun my_lexer(whole_input size)
	
	(setq i 0)
	(setq j 0)
	(setq flag nil)
	(setq which_curly 1)
	(loop while (< i size) do 
		
		(setq one_char (char whole_input i))	

		(cond((or (is_in_alphabet one_char) (digit_control one_char))
			(setq temp (string ""))
			(setf j i)

			(loop while (or (digit_control one_char) (is_in_alphabet one_char)) do 	; Rakamları kaydet
				(setq temp (concatenate 'string temp (string one_char)))
				(setq j (+ j 1))
				(setq i (+ i 1))
				(setq one_char (char whole_input j))
			)
			(cond
				;;Keyword Kontrol
			 	((string-equal temp "exit")   (exit)(setq flag nil)(setf tokens_list (push "exit" tokens_list)))
			 	((string-equal temp "and")    (setq flag t) (setf tokens_list (push "KW_AND" tokens_list)))
			 	((string-equal temp "or")     (setq flag t) (setf tokens_list (push "KW_OR" tokens_list)))
			 	((string-equal temp "not")    (setq flag t) (setf tokens_list (push "KW_NOT" tokens_list)))
			 	((string-equal temp "equal")  (setq flag t) (setf tokens_list (push "KW_EQUAL" tokens_list)))
			 	((string-equal temp "less")   (setq flag t) (setf tokens_list (push "KW_LESS" tokens_list)))
			 	((string-equal temp "nil")    (setq flag t) (setf tokens_list (push "KW_NIL" tokens_list)))
			 	((string-equal temp "list")   (setq flag t) (setf tokens_list (push "KW_LIST" tokens_list)))
			 	((string-equal temp "set")    (setq flag t) (setf tokens_list (push "KW_SET" tokens_list)))
			 	((string-equal temp "append") (setq flag t) (setf tokens_list (push "KW_APPEND" tokens_list)))
			 	((string-equal temp "concat") (setq flag t) (setf tokens_list (push "KW_CONCAT" tokens_list)))
			 	((string-equal temp "set")    (setq flag t) (setf tokens_list (push "KW_SET" tokens_list)))
			 	((string-equal temp "deffun") (setq flag t) (setf tokens_list (push "KW_DEFFUN" tokens_list)))
			 	((string-equal temp "for")    (setq flag t) (setf tokens_list (push "KW_FOR" tokens_list)))
			 	((string-equal temp "if")     (setq flag t) (setf tokens_list (push "KW_IF" tokens_list)))
			 	((string-equal temp "laod")   (setq flag t) (setf tokens_list (push "KW_LOAD" tokens_list)))
			 	((string-equal temp "disp")   (setq flag t) (setf tokens_list (push "KW_DISP" tokens_list)))
			 	((string-equal temp "true")   (setq flag t) (setf tokens_list (push "KW_TRUE" tokens_list)))
			 	((string-equal temp "false")  (setq flag t) (setf tokens_list (push "KW_FALSE" tokens_list)))
			 	
			 	;;Degerin 0 Olup Olmadıgı Kontrol
			 	((string-equal temp "0") (setq flag t) (setf tokens_list (push "VALUE" tokens_list )) (setf val_list (push (parse-integer temp ) val_list)))
			 	((eq (char temp 0) #\0)
					(if (and ( > (length temp) 1)(digit_control (char temp 1)))
						(progn
							(print "SYNTAX ERROR Deger 0 Baslamaz!!!")
							(return-from my_lexer nil)
						)
					)
				)
			 	((digit_control (char temp 0)) (setq flag t) (setf tokens_list (push "VALUE" tokens_list)) 
			 	(setf val_list (push (parse-integer temp) val_list))
			 	);;Stringde Rakam Kontrolu
			 	((is_in_alphabet (char temp 0)) (setq flag t) (setf tokens_list (push "IDENTIFIER" tokens_list)));;Identifier Kontrol

			)
		))			 

		(cond
			((equal one_char #\+) (setq flag t) (setf tokens_list (push "OP_PLUS" tokens_list)))
			((equal one_char #\-) (setq flag t) (setf tokens_list (push "OP_MINUS" tokens_list)))
			((equal one_char #\/) (setq flag t)(setf tokens_list (push "OP_DIV" tokens_list)))
			((equal one_char #\*)
				(if(equal (char whole_input (+ i 1)) #\*)
					(progn
						(setf tokens_list (push "OP_DBLMULT" tokens_list))
						(setq flag t)
						(setq i (+ i 1))
					)
					(progn
						(setq flag t)
						(setf tokens_list (push "OP_MULT" tokens_list))
					)
				)
			)
			((equal one_char #\() (setq flag t) (setf tokens_list (push "OP_OP" tokens_list)))	
			((equal one_char #\)) (setq flag t) (setf tokens_list (push "OP_CP" tokens_list)))
			((equal one_char #\")
				(if(equal which_curly 1)
					(progn
						(setf tokens_list (push "OP_OP" tokens_list))
						(setq flag t)
					 	(setf which_curly 0)
					)
					(progn
						(setf tokens_list (push "OP_CP" tokens_list))
						(setq flag t)
					  	(setf which_curly 1)
					)
				)
			)	
		)
		(if(equal one_char #\;)
			(if(equal (char whole_input (+ i 1)) #\;)
				(progn
					(setf tokens_list (push "COMMENT" tokens_list))
					(setq flag t)
					(return-from my_lexer flag)
				)
		))
		(setf i (+ i 1))
	)
	(setf tokens_list (cdr(reverse tokens_list)))
	(setf val_list (cdr (reverse val_list)))
	(return-from my_lexer flag)
)
 ;; ************************************************************************

 ;; parser generator kismi ++++++++++++++++++++++++++++++++++++++++++++++++++

(defun parser()
	
	;;parse operator kontrol
	(if (equal "COMMENT" (nth 0 tokens_list))(progn(print " Syntax OK ")(return-from parser t))) ;;Yorum Kontrol
	(if (equal "OP_OP" (nth 0 tokens_list))
		(progn
			(if (equal "KW_SET" (nth 1 tokens_list))
				(progn
					(if(and (equal "IDENTIFIER" (nth 2 tokens_list)) (equal "VALUE" (nth 3 tokens_list)) (equal "OP_CP" (nth 4 tokens_list)))
						(progn (terpri)(format t "Syntax OK")(return-from parser t))
						(progn (print "SYNTAX_ERROR")(return-from parser nil))
					)
				)
			)
			(if (equal "OP_PLUS" (nth 1 tokens_list))
				(progn ;;if
					(if(and(equal "VALUE" (nth 2 tokens_list))(equal "VALUE" (nth 3 tokens_list)) (equal "OP_CP" (nth 4 tokens_list)))
						(progn (terpri)(format t "Syntax OK  Result: ~D" ( + (nth 0 val_list) (nth 1 val_list)))(return-from parser t))
						(progn (print "SYNTAX_ERROR ")(return-from parser nil))
					)
				)
			)
			(if (equal "OP_MINUS" (nth 1 tokens_list))
				(progn ;;if
					(if(and(equal "VALUE" (nth 2 tokens_list))(equal "VALUE" (nth 3 tokens_list)) (equal "OP_CP" (nth 4 tokens_list)))
						(progn (terpri)(format t "Syntax OK Result: ~D" ( - (nth 0 val_list) (nth 1 val_list)))(return-from parser t))
						(progn (print "SYNTAX_ERROR ")(return-from parser nil))
					)
				)
			)
			(if (equal "OP_MULT" (nth 1 tokens_list))
				(progn ;;if
					(if(and(equal "VALUE" (nth 2 tokens_list))(equal "VALUE" (nth 3 tokens_list)) (equal "OP_CP" (nth 4 tokens_list)))
						(progn (terpri)(format t "Syntax OK Result: ~D" ( * (nth 0 val_list) (nth 1 val_list)))(return-from parser t))
						(progn (print "SYNTAX_ERROR")(return-from parser nil))
					)
				)
			)
			(if (equal "OP_DIV" (nth 1 tokens_list))
				(progn ;;if
					(if(and(equal "VALUE" (nth 2 tokens_list))(equal "VALUE" (nth 3 tokens_list)) (equal "OP_CP" (nth 4 tokens_list)))
						(progn (terpri)(format t "Syntax OK Result: ~D" ( / (nth 0 val_list) (nth 1 val_list)))(return-from parser t))
						(progn (print "SYNTAX_ERROR")(return-from parser nil))
					)
				)
			)
			(if (equal "OP_DBLMULT" (nth 1 tokens_list))
				(progn ;;if
					(if(and(equal "VALUE" (nth 2 tokens_list))(equal "VALUE" (nth 3 tokens_list)) (equal "OP_CP" (nth 4 tokens_list)))
						(progn
							(setq res 1)
							(dotimes (i (nth 1 val_list))(setq res ( * (nth 0 val_list) res )))
							(terpri)(format t "Syntax OK Result: ~D" res)(return-from parser t)
						)
						(progn (print "SYNTAX_ERROR ")(return-from parser nil))
					)
				)
			)
			(if (equal "KW_AND" (nth 1 tokens_list))
				(progn ;;if
					(if(and(equal "VALUE" (nth 2 tokens_list))(equal "VALUE" (nth 3 tokens_list)) (equal "OP_CP" (nth 4 tokens_list)))
						(progn (terpri)(format t "Syntax OK Result: ~D" ( and (nth 0 val_list) (nth 1 val_list)))(return-from parser t))
						(progn (print "SYNTAX_ERROR ")(return-from parser nil))
					)
				)
			)
			(if (equal "KW_OR" (nth 1 tokens_list))
				(progn ;;if
					(if(and(equal "VALUE" (nth 2 tokens_list))(equal "VALUE" (nth 3 tokens_list)) (equal "OP_CP" (nth 4 tokens_list)))
						(progn (terpri)(format t "Syntax OK Result: ~D" ( or (nth 0 val_list) (nth 1 val_list)))(return-from parser t))
						(progn (print "SYNTAX_ERROR ")(return-from parser nil))
					)
				)
			)
			(if (equal "KW_NOT" (nth 1 tokens_list))
				(progn ;;if
					(if(and(equal "VALUE" (nth 2 tokens_list))(equal "OP_CP" (nth 3 tokens_list)))
						(progn (terpri)(format t "Syntax OK Result: ~D" ( not (nth 0 val_list))) (return-from parser t))
						(progn (print "SYNTAX_ERROR ")(return-from parser nil))
					)
				)
			)
			(if (equal "KW_EQUAL" (nth 1 tokens_list))
				(progn ;;if
					(if(and(equal "VALUE" (nth 2 tokens_list))(equal "VALUE" (nth 3 tokens_list)) (equal "OP_CP" (nth 4 tokens_list)))
						(progn (terpri)(format t "Syntax OK Result: ~D" ( equal (nth 0 val_list) (nth 1 val_list)))(return-from parser t))
						(progn (print "SYNTAX_ERROR")(return-from parser nil))
					)
				)
			)
			(if (equal "KW_LESS" (nth 1 tokens_list))
				(progn ;;if
					(if(and(equal "VALUE" (nth 2 tokens_list))(equal "VALUE" (nth 3 tokens_list)) (equal "OP_CP" (nth 4 tokens_list)))
						(progn (terpri)(format t "Syntax OK Result: ~D" ( < (nth 0 val_list) (nth 1 val_list)))(return-from parser t))
						(progn (print "SYNTAX_ERROR")(return-from parser nil))
					)
				)
			)
		)
		(progn (print "SYNTAX_ERROR")(return-from parser nil))
	)
	(return-from parser flag)
)
 ;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 
 ;;pars_tree to file.txt >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(defun parser_file (lst )

	(setf tabs "")
	(setf whichInput "")
	(setf tabs (concatenate 'string '(#\tab) '(#\tab) tabs ))
	(cond 

		((string-equal  (second ( car (cdr lst)) ) "OP_PLUS")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "OP_MINUS")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "OP_MULT")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "OP_DIV")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "KW_SET")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "OP_DBLMULT")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "KW_AND")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "KW_OR")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "KW_NOT")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "KW_EQUAL")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "KW_LESS")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "KW_NIL")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "KW_LIST")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "KW_APPEND")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "KW_CONCAT")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "KW_EXIT")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "KW_LOAD")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "KW_DISP")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "KW_TRUE")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "KW_FALSE")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "KW_DEFFUN")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "KW_DEFVAR")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "KW_IF")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "KW_WHILE")  (setf whichInput "EXPI"))
		((string-equal  (second ( car (cdr lst)) ) "KW_FOR")  (setf whichInput "EXPI"))
		(t  (setf whichInput "EXPLISTI"))
	)
	(with-open-file (stream "parse_tree.tree" 
					:direction :output
					:if-exists :supersede
					:if-does-not-exist :create)
	(format stream "~a~a~a~a~a~a~a~a~a~a~a" "; DIRECTIVE: parse tree"  #\newline  "START" #\newline #\tab "INPUT" #\newline  #\tab #\tab whichInput #\newline  )
	(format stream "~a" (helper lst tabs tabs 0))
	(close stream))
)
(defun helper (lst n nControl defControl) 

	(setq lst1 '())
	(setq lst2 '())
	(setq writeString "")
	(setq control 0)
	(setq explstıForNullEmptyLıst 0)
	(cond((not (equal nil lst))
			(setf lst1 (push (car lst ) lst1))
			(pop lst1)
			(setf lst2  (cdr lst)))
	)
	(if (string= (car lst1) "OP_OP")
		(setf n (concatenate 'string '(#\tab) n ))
	)
	( cond((null lst) nil)((atom lst) (list lst))
		(t 
			(cond ((string= (car lst1) "OP_CP")
		 			(setf n (substring n (length '(#\tab))  (length n)))
		  			(setf writeString (concatenate 'string  n '(#\tab) (pop lst1 ) '(#\newline)))

				  	(cond ((equal defControl 1)
						  (setf defControl 0)))

				  	(cond (
				  			( and (string= (car lst2) "OP_OP") (not  (string-equal  nControl n ) ) ) 
						  	(setf writeString (concatenate 'string  writeString n ' "EXPB" '(#\newline)))
						  )
					)
		  		  )

				(t  (cond((string= (car lst1) "KW_DEFFUN")  

					(setf nControl "")
					(setf nControl (concatenate 'string  n nControl))
					(setf control 1)
					(setf defControl 1)
					(setf writeString (concatenate 'string  n (pop lst1 ) '(#\newline) n "ID" '(#\newline) 
															n '(#\tab) (car lst2) '(#\newline)
															n "IDLIST" '(#\newline) 
														     ))
					(pop lst2)
					)
					( (string= (car lst1) "KW_IF")  
					
					 (setf writeString (concatenate 'string  n  (pop lst1 ) '(#\newline) n "EXPB" '(#\newline)))
					 (setf nControl "")
					 (setf nControl (concatenate 'string  n nControl))
					 (setf control 1) ; aynı tab oluyor ve altta if ten sonra EXPLISTI BASIYOR onu engellemek için var control 
					)
					( (string= (car lst1) "KW_FOR")  
					
					 (setf writeString (concatenate 'string  n  (pop lst1 ) '(#\newline)))
					 (setf nControl "")
					 (setf nControl (concatenate 'string  n nControl))
					 (setf control 1) ; aynı tab oluyor ve altta if ten sonra EXPLISTI BASIYOR onu engellemek için var control 
					)
					( (string= (car lst1) "KW_CONCAT")  
					
					 (setf writeString (concatenate 'string  n  (pop lst1 ) '(#\newline)))
					 (setf nControl "")
					 (setf nControl (concatenate 'string  n nControl))
					 (setf control 1) ; aynı tab oluyor ve altta if ten sonra EXPLISTI BASIYOR onu engellemek için var control 
					)
					( (string= (car lst1) "KW_APPEND")  
					
					 (setf writeString (concatenate 'string  n  (pop lst1 ) '(#\newline)))
					 (setf nControl "")
					 (setf nControl (concatenate 'string  n nControl))
					 (setf control 1) ; aynı tab oluyor ve altta if ten sonra EXPLISTI BASIYOR onu engellemek için var control 

					)
					( (string= (car lst1) "KW_WHILE")  
					
					 (setf writeString (concatenate 'string  n  (pop lst1 ) '(#\newline) 
					 										 n  '(#\tab) "OP_OP" '(#\newline) 
					 										 n  '(#\tab) "EXPB" '(#\newline)))
					 (pop lst2)
					 (setf nControl "")
					 (setf nControl (concatenate 'string  n nControl))
					 (setf control 1) ; aynı tab oluyor ve altta if ten sonra EXPLISTI BASIYOR onu engellemek için var control 

					 (setf n (concatenate 'string '(#\tab) n ))
					)
					( (string= (car lst1) "KW_AND")  
					
					 (setf writeString (concatenate 'string  n  (pop lst1 ) '(#\newline) n "EXPB" '(#\newline)))
					)
					( (string= (car lst1) "KW_OR")  
					
					 (setf writeString (concatenate 'string  n  (pop lst1 ) '(#\newline) n "EXPB" '(#\newline)))
					) 
					( (string= (car lst1) "KW_NOT")  
					
					 (setf writeString (concatenate 'string  n  (pop lst1 ) '(#\newline) n "EXPB" '(#\newline)))
					)
					( (string= (car lst1) "KW_EQUAL")  

					  (cond 
					  	(  (string= (car lst2) "OP_OP")   (setf writeString (concatenate 'string  n  (pop lst1 ) '(#\newline) n "EXPB" '(#\newline))) )
					  	(t (setf writeString (concatenate 'string  n  (pop lst1 ) '(#\newline))))
					  )					
					)
					( (and (string= (car (car lst) ) "IDENTIFIER") (equal defControl 1)) 
					
						(setf writeString (concatenate 'string n "IDLIST" '(#\newline) n '(#\tab) "ID" '(#\newline) n '(#\tab) '(#\tab) (pop lst1) '(#\newline) ))
					)
					( (and (string= (car (car lst) ) "IDENTIFIER") (equal defControl 0)) 
					
						(setf writeString (concatenate 'string n "EXPI" '(#\newline) n '(#\tab) "ID" '(#\newline) n '(#\tab) '(#\tab) (pop lst1) '(#\newline) ))
					)
					( (string= (car (car lst) ) "VALUE")
  						(setf writeString (concatenate 'string 
  														n "EXPI" '(#\newline) 
  											            n '(#\tab) "VALUES" '(#\newline) 
  											            n '(#\tab) '(#\tab) "IntegerValue" '(#\newline)
  											            n '(#\tab) '(#\tab) '(#\tab) (pop lst1) '(#\newline)))
  					)
					( (string= (car (car lst) ) "KW_BOOL")
  						(setf writeString (concatenate 'string 
  														n "EXPB" '(#\newline) 
  											            n '(#\tab) "BinaryValue" '(#\newline) 
  											            n '(#\tab) '(#\tab) (pop lst1) '(#\newline)))
  					)
					(t 
						
						(cond 
							( ( or (string= (car lst1) "null") (string= (car lst1) "'()") (string= (car lst1) "'("))
					
							  (setf writeString (concatenate 'string  n "EXPLISTI" '(#\newline) n '(#\tab) (pop lst1 )  '(#\newline)))
							  (setf explstıForNullEmptyLıst 1)

							)

							(t (setf writeString (concatenate 'string  n  (pop lst1 ) '(#\newline))))
						)
					)
				)
				)
		    )
			(cond ((and (string-equal  nControl n ) (equal control 0) (equal explstıForNullEmptyLıst 0) (not (string-equal (second (car lst2)) "OP_CP")) ) 
				  (setf writeString (concatenate 'string  writeString n "EXPLISTI" '(#\newline)))))
			(concatenate 'string writeString (helper lst2 n nControl defControl))
		)
	)
)
 ;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

 ;; interpreter part  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
(defun gppinterpreter() ;; gppinterpreter function to start interpreter

	(setq st(read-line))
	(if(> 5 (length st))
		(progn;;input  terminal
			(loop
			
				(print "saü_>")
				(setf st(read-line))
				(setq size (length st)); text uzunlugu.
				(setq test(my_lexer st size ))
				(setq all_tokens `())

				
				(if(equal test nil)
					(progn
						(print "Syntax Error !!! Dogru Sekilde İnput Giriniz :)")
						(return-from gppinterpreter)
					)
				)
				(setq test (parser)) ;; parser cagır 
				(if(equal test nil)
					(return-from gppinterpreter)
				)
				
				(dotimes (n (list-length tokens_list))
					
					(setq one_element `())
					(setf one_element (push (nth n tokens_list) one_element))
					(setf all_tokens (push one_element all_tokens))
				)
				(setf all_tokens (reverse all_tokens))
			
				(parser_file all_tokens)

				(setf tokens_list '(nil))
				(setf val_list '(nil))
			)
		)

		(progn;;input from filename.g++
			(print"saü_>")
			(setq filename(subseq st 5));;dosyadan input okuma
			(read_from_file filename )
		)
	)
)
 ;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

 ;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(gppinterpreter)
 ;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
