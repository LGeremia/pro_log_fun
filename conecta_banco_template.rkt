#lang racket
(require db)
(define pgc
  (mysql-connect
      #:user "your_user"
      #:database "your_data_base"
      #:server "localhost"
      #:password "your_password"
  )
)

(define (search-team team)
        (print (query-rows pgc (~a "SELECT * FROM teams WHERE id = " team) ))
    )
(define (insert-team team)
  (query pgc "INSERT INTO copa_mundo_racket.teams VALUES (DEFAULT, ?)" team)
  )

(define (update-team id team)
  (query pgc "UPDATE teams SET name=? WHERE id=?" team id) 
  )

(define (delete-all-teams)
  (query pgc "DELETE FROM teams") 
  )

(define (delete-one-team id)
  (query pgc "DELETE FROM teams WHERE id =" id)
  )

; (query-exec pgc "INSERT INTO users VALEUS (DEFAULT, 'Nome)")
; (query-rows pgc "SELECT * FROM users")
; (query-exec pgc "DELETE FROM users")web-server/insta
