 
(setq nextI 0)
(setq parser_list '())
(setq liste '())

(defun getSymbol ()

	(if (eq nextI (list-length lexeme_list))
		(progn
            (setq nextToken "eof")
			nil
		)
	)

    (setq nextToken (string (car (nth nextI lexeme_list)))) ; To take token part
    (setq nextToken2 (string (car (cdr (nth nextI lexeme_list))))) ; To take lexeme part
    (setq nextI (+ nextI 1)) ; to take next one 
    t
)

(defun spaces(i)
	(if (> i 0)
		(progn	
            (format liste "    ")  
			(spaces (- i 1))
		)
	)
    t
)

(defun start(i)
    (format liste ";DIRECTİVE:parse tree~%")
    ;(format liste "START~%")
    (push (cons i (cons "START" (cons nextI nil))) parser_list)
    (input (+ i 1))
)


(defun input(i)
    ;(spaces (- i 1))
    ;(format liste "INPUT~%")
    (push (cons i (cons "INPUT" (cons nextI nil))) parser_list)
    (getSymbol)
    (expi (+ i 1))
)
                                                                                      
(defun expi(i)
    ;(spaces i)
    ;(format liste "EXPI~%")
    (push (cons i (cons "EXPI" (cons nextI nil))) parser_list)
    (if(eq t (id_ (+ i 1)))
        t
        (if (eq t (integer_ (+ i 1)))
          t 
          (if (eq t (openParen (+ i 1)))
                (if (and (dortislem (+ i 1)) (and (expi (+ i 1)) (and (expi (+ i 1)) (closeParen (+ i 1)) )))
                    t
                    (if (and (id_ (+ i 1)) (and (explisti (+ i 1)) (closeParen (+ i 1)) ))
                        t
                        (if (and (keyword_ (+ i 1) "deffun") (and (id_(+ i 1)) (and (idlist(+ i 1)) (and (explisti(+ i 1)) (closeParen(+ i 1))))))
                            t
                           (if (and (keyword_ (+ i 1) "defvar") (and (id_ (+ i 1)) (and (expi (+ i 1)) (closeParen (+ i 1)) )))
                                t
                                (if (and (keyword_ (+ i 1) "for") (and (openParen (+ i 1)) (and (id_(+ i 1)) (and (expi (+ i 1)) (and (expi (+ i 1)) (and (closeParen(+ i 1)) (and (explisti(+ i 1)) (closeParen(+ i 1))  )))))))                                                   
                                    t
                                    (if (and (keyword_ (+ i 1) "while") (and (openParen (+ i 1)) (and (expb (+ i 1)) (and (closeParen(+ i 1)) (and (explisti(+ i 1)) (closeParen(+ i 1))  )))) )                                                  
                                        t
                                        (if (and (keyword_ (+ i 1) "if") (and (expb (+ i 1)) (explisti (+ i 1)) ))
                                            (if (and (explisti (+ i 1)) (closeParen (+ i 1)))
                                                t
                                                (if (eq t (closeParen (+ i 1)))
                                                    t
                                                    ;nil
                                                )
                                            )
                                            (progn
                                                ;(print_list (parser_list))
                                                (error_check)
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                ) 
                (progn
                    ;(print_list (parser_list))
                    (error_check)
                )
            )
           
        )
    
    )
    
)


(defun dortislem(i)
    (if (or (or (string= nextToken2 "+") (string= nextToken2 "-")) (or (string= nextToken2 "*") (string= nextToken2 "/")) ) 
        (progn
            ;(spaces i)
            ;(format liste "~a~%" nextToken2)
            (push (cons i (cons nextToken2 (cons nextI nil))) parser_list)
            (getSymbol)
            t ;return true
        )
        nil ;return false
    )

)
(defun expb(i)
    ;(spaces i)
    ;(format liste "EXPB~%")

    (push (cons i (cons "EXPB" (cons nextI nil))) parser_list)
    
    (if (eq t (openParen(+ i 1)))
        (if (and (and_or (+ i 1)) (and (expb (+ i 1)) (and (expb (+ i 1)) (closeParen (+ i 1)))))
            t
            (if (and (keyword_ (+ i 1) "not") (and (expb (+ i 1)) (closeParen (+ i 1))))
                t
                (if (eq t (keyword_ (+ i 1) "equal"))
                    (if (and (expb (+ i 1)) (and (expb (+ i 1)) (closeParen (+ i 1))) )
                        t
                        (if (and (expi (+ i 1)) (and (expi (+ i 1)) (closeParen (+ i 1)) ))
                            t
                            nil
                        )
                    )

                )

            )
        )
        (if (eq t (binary_(+ i 1))  )
            t
            nil
        )
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Token_name = "binaryvalue"  olarak kullandım binaryvalue ilgili input örneği bulamadığımdan 
(defun binary_(i)

    (if  (and (string= nextToken "binaryvalue"))
         (if  (and (string= nextToken2 "true"))
            (progn
                ;(spaces i)
                ;(format liste "~a~%" nextToken2)
                (push (cons i (cons nextToken (cons nextI nil))) parser_list)
                (binary_terminal (+ i 1))
                (getSymbol)
                t ;return true
            )
        )
        (if  (and (string= nextToken2 "false"))
            (progn
                ;(spaces i)
                ;(format liste "~a~%" nextToken2)
                (push (cons i (cons nextToken (cons nextI nil))) parser_list)
                (binary_terminal (+ i 1))
                (getSymbol)
                t ;return true
            )
        ) 
    )
)

(defun binary_terminal (i)
    (push (cons i (cons nextToken2 (cons nextI nil))) parser_list)
)


(defun keyword_(i key)
       ; (format liste "~a ~%" key)
    (if  (and (string= nextToken "keyword") (string= nextToken2 key) ) 
        (progn
           ; (spaces i)
          ;  (format liste "~a~%" nextToken2)
            (push (cons i (cons key (cons nextI nil))) parser_list)
            (getSymbol)
            t ;return true
        )
        nil ;return false
    )
)


(defun and_or(i)
    (if  (and (string= nextToken "keyword") (or (string= nextToken2 "and") (string= nextToken2 "or") )) 
        (progn
            ;(spaces i)
            ;(format liste "~a~%" nextToken2)
            (push (cons i (cons nextToken2 (cons nextI nil))) parser_list)
            (getSymbol)
            t ;return true
        )
        nil ;return false
    )
    
)


(defun for_(i)
    (if  (and (string= nextToken "keyword") (string= nextToken2 "for") ) 
        (progn
            ;(spaces i)
            ;(format liste "~a~%" nextToken2)
            (push (cons i (cons nextToken2 (cons nextI nil))) parser_list)
            (getSymbol)
            t ;return true
        )
        nil ;return false
    ) 
)

(defun while_(i)
    (if  (and (string= nextToken "keyword") (string= nextToken2 "while") ) 
        (progn
            ;(spaces i)
            ;(format liste "~a~%" nextToken2)
            (push (cons i (cons nextToken2 (cons nextI nil))) parser_list)
            (getSymbol)
            t ;return true
        )
        nil ;return false
    )
    
)

(defun integer_(i)
    (if (string= nextToken "integer")  
        (progn
            ;(spaces i)
            ;(format liste "IntegerValue~%")
            (push (cons i (cons "IntegerValue" (cons nextI nil))) parser_list)
            (integer_terminal (+ i 1))
            (getSymbol)
            t ;return true
        )
        nil ;return false
    )
)

(defun integer_terminal (i)
    (push (cons i (cons nextToken2 (cons nextI nil))) parser_list)
)

(defun id_(i)
    (if (string= nextToken "identifier")  
        (progn
            ;(spaces i)
            ;(format liste "ID~%")
            (push (cons i (cons "ID" (cons nextI nil))) parser_list)
            (id_terminal (+ i 1))
            (getSymbol)
            t ;return true
        )
        nil ;return false
    )
)

(defun id_terminal (i)
    (push (cons i (cons nextToken2 (cons nextI nil))) parser_list)
)


(defun openParen(i)

    (operator_ i "(")
)



(defun closeParen (i)
    (operator_ i ")")
)

(defun idlist(i)

    ;(spaces i)
    ;(format liste "IDLIST~%")

    (push (cons i (cons "IDLIST" (cons nextI nil))) parser_list)

    (if (and (openParen(+ i 1)) (and (idlist(+ i 1)) (closeParen(+ i 1))))
        t
        (if (eq t (id_ (+ i 1)))
            (progn
                (idlist (+ i 1))
                t
            )
        )
    )

)

(defun operator_(i o)
    (if (and (string= nextToken "operator") (string= nextToken2 o))
        (progn 
            ;(spaces i)
            ;(format liste "~a~%" o)
            (push (cons i (cons o (cons nextI nil))) parser_list)
            (getSymbol)
            t
        )
        nil
    )

)

(defun explisti(i)

    ;(spaces i)
    ;(format liste "EXPLISTI~%") 
    (push (cons i (cons "EXPLISTI" (cons nextI nil))) parser_list)


        (if(eq t (openParen (+ i 1)) )
            (if (and (keyword_ (+ i 1) "concat")  (and  (explisti (+ i 1)) (and (explisti (+ i 1)) (closeParen (+ i 1))) ))
                t
                (if (and (keyword_ (+ i 1) "append")  (and  (expi (+ i 1)) (and (explisti (+ i 1)) (closeParen (+ i 1))) ))
                    t
                    nil
                )
            )
            (if (string= nextToken "null")
                (progn
                     (push (cons i (cons "null" (cons nextI nil))) parser_list)
                     (getSymbol)
                    t 
                )
                (if (and (operator_ (+ i 1) "'") (and (openParen(+ i 1)) (closeParen (+ i 1))) )
                    t
                    (if (and (operator_ (+ i 1) "'") (and (openParen(+ i 1)) (and (values_ (+ i 1)) (closeParen (+ i 1))) ))
                        t
                        (if (eq t (expi (+ i 1)))
                            t
                            nil
                        )
                    ) 
                )
            )
        )

)

(defun values_(i)

    (push (cons i (cons "VALUES" (cons nextI nil))) parser_list)

    (if (eq t (integer_(+ i 1)))  
        (progn
            (values (+ i 1))
            t ;return true
        )
        nil ;return false
    )

)


(defun exception()
    ;(format liste "exception")
    ;(quit)
)    


(defun error_check()
    
    (format liste "Error")
    ;(quit)
)

(defun print_list(list1)

    
    (setq space_num (car (car list1)))
    (spaces space_num)
    (format liste "~a~%" (nth 1 (car list1))) 
    ;(format liste "~a" space_num)

    (if (> (list-length list1) 1 )
        (progn 
            ;(format liste "~a~%" (car  )
            (print_list (cdr list1))
        )
    )


)

(defun parse(lexeme)
    (setq lexeme_list lexeme)
    (start 0)
    ;(format liste "~a" parser_list)
    (print_list (reverse parser_list))
)
(defun parser (liste_deneme)
    (with-open-file (str "161044103.tree"
    :direction :output
    :if-exists :supersede
    :if-does-not-exist :create)
    (setq liste str)
    (parse liste_deneme))
    
    )




