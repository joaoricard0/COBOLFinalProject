      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAREFA_FINAL.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL FIC-CLIENTES ASSIGN "CLIENTES.TXT"
           ORGANISATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CLIENTE-COD
           ALTERNATE RECORD KEY IS NIF
           FILE STATUS IS FS.

           SELECT OPTIONAL FIC-TEMAS ASSIGN "TEMAS.TXT"
           ORGANISATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TEMA-COD
           FILE STATUS IS FS.

           SELECT OPTIONAL FIC-AUTORES ASSIGN "AUTORES.TXT"
           ORGANISATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS AUTOR-COD
           FILE STATUS IS FS.

           SELECT OPTIONAL FIC-LIVROS ASSIGN "LIVROS.TXT"
           ORGANISATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS LIVRO-COD
           FILE STATUS IS FS.

           SELECT OPTIONAL FIC-ALUGUERES ASSIGN "ALUGUERES.TXT"
           ORGANISATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS ALUGUER-COD
           FILE STATUS IS FS.

           SELECT OPTIONAL IND-ALUGADOS ASSIGN "ALUGADOS.TXT"
           ORGANISATION IS LINE SEQUENTIAL.

           SELECT OPTIONAL IND-TODOS ASSIGN "TODOS.TXT"
           ORGANISATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD FIC-CLIENTES.
       01 REGISTO-CLIENTES.
           05 CLIENTE-COD                  PIC 9(5) BLANK WHEN ZEROS.
           05 NIF                          PIC 9(9) BLANK WHEN ZEROS.
           05 NOME                         PIC X(30).
           05 DATA-ADMIT.
               10 ANO-ADMIT                PIC 9999.
               10 FILLER                   PIC X VALUE "/".
               10 MES-ADMIT                PIC 99.
               10 FILLER                   PIC X VALUE "/".
               10 DIA-ADMIT                PIC 99.
           05 EMAIL                        PIC X(30).

       FD FIC-LIVROS.
       01 REGISTO-LIVROS.
           05 LIVRO-COD                    PIC 9(5) BLANK WHEN ZEROS.
           05 TITULO                       PIC X(30).
           05 LIVRO-TEMA-COD               PIC 9(5) BLANK WHEN ZEROS.
           05 LIVRO-AUTOR-COD              PIC 9(5) BLANK WHEN ZEROS.

       FD FIC-TEMAS.
       01 REGISTO-TEMAS.
           05 TEMA-COD                     PIC 9(5) BLANK WHEN ZEROS.
           05 TEMA                         PIC X(30).

       FD FIC-AUTORES.
       01 REGISTO-AUTORES.
           05 AUTOR-COD                    PIC 9(5) BLANK WHEN ZEROS.
           05 AUTOR                        PIC X(30).

       FD FIC-ALUGUERES.
       01 REGISTO-ALUGUER.
           05 ALUGUER-COD                  PIC 9(5) BLANK WHEN ZEROS.
           05 DATA-ALUGUER.
               10 ANO-ALUGUER              PIC 9999.
               10 FILLER                   PIC X    VALUE "/".
               10 MES-ALUGUER              PIC 99.
               10 FILLER                   PIC X    VALUE "/".
               10 DIA-ALUGUER              PIC 99.
           05 DATA-ENTREGA.
               10 ANO-ENTREGA              PIC 9999.
               10 FILLER                   PIC X    VALUE "/".
               10 MES-ENTREGA              PIC 99.
               10 FILLER                   PIC X    VALUE "/".
               10 DIA-ENTREGA              PIC 99.
           05 ALUGUER-LIVRO-COD            PIC 9(5) BLANK WHEN ZEROS.
           05 ALUGUER-CLIENTE-COD          PIC 9(5) BLANK WHEN ZEROS.
           05 ESTADO                       PIC X.

       FD IND-TODOS.
       01 REGISTO-TODOS-CLIENTE.
           05 TODOS1                       PIC X(10).
           05 IND-TODOS-CLIENTE            PIC 9(5).
           05 TODOS2                       PIC X(10).
           05 IND-TODOS-NIF                PIC 9(9).
           05 TODOS3                       PIC X(10).
           05 IND-TODOS-NOME               PIC X(30).
           05 TODOS4                       PIC X(14).
           05 IND-TODOS-DATA-ADMIT.
               10 IND-TODOS-ANO-ADMIT      PIC 9999.
               10 DATA1                    PIC X.
               10 IND-TODOS-MES-ADMIT      PIC 99.
               10 DATA2                    PIC X.
               10 IND-TODOS-DIA-ADMIT      PIC 99.
           05 TODOS5                           PIC X(10).
           05 IND-TODOS-EMAIL              PIC X(30).

       01 REGISTO-TODOS-LIVROS.
           05 TODOS6                       PIC X(6).
           05 IND-TODOS-LIVRO              PIC 9(5).
           05 TODOS7                       PIC X(9).
           05 IND-TODOS-LIVRO-TEMA         PIC 9(5).
           05 TODOS8                       PIC X(9).
           05 IND-TODOS-LIVRO-AUTOR        PIC 9(5).
           05 TODOS9                       PIC X(11).
           05 IND-TODOS-TITULO             PIC X(30).

       01 REGISTO-TODOS-TEMA.
           05 TODOS10                      PIC X(9).
           05 IND-TODOS-TEMA-COD           PIC 9(5).
           05 TODOS11                      PIC X(9).
           05 IND-TODOS-TEMA               PIC X(30).

       01 REGISTO-TODOS-AUTOR.
           05 TODOS12                      PIC X(9).
           05 IND-TODOS-AUTOR-COD          PIC 9(5).
           05 TODOS13                      PIC X(10).
           05 IND-TODOS-AUTOR              PIC X(30).

       01 REGISTO-TODOS-ALUGUER.
           05 TODOS14                      PIC X(9).
           05 IND-TODOS-ALUGUER            PIC 9(5).
           05 TODOS15                      PIC X(9).
           05 IND-TODOS-ALUGUER-CLIENTE    PIC 9(5).
           05 TODOS16                      PIC X(9).
           05 IND-TODOS-ALUGUER-LIVRO      PIC 9(5).
           05 TODOS17                      PIC X(14).
           05 IND-TODOS-DATA-ALUGUER.
               10 IND-TODOS-ANO-ALUGUER    PIC 9999.
               10 DATA3                    PIC X.
               10 IND-TODOS-MES-ALUGUER    PIC 99.
               10 DATA4                    PIC X.
               10 IND-TODOS-DIA-ALUGUER    PIC 99.
           05 TODOS18                      PIC X(14).
           05 IND-TODOS-DATA-ENTREGA.
               10 IND-TODOS-ANO-ENTREGA    PIC 9999.
               10 DATA5                    PIC X.
               10 IND-TODOS-MES-ENTREGA    PIC 99.
               10 DATA6                    PIC X.
               10 IND-TODOS-DIA-ENTREGA    PIC 99.
           05 TODOS19                      PIC X(13).
           05 IND-TODOS-ESTADO             PIC X.

       FD IND-ALUGADOS.
       01 REGISTO-ALUGADOS.
           05 TODOS20                      PIC X(9).
           05 IND-ALUGADOS-ALUGUER         PIC 9(5).
           05 TODOS21                      PIC X(9).
           05 IND-ALUGADOS-CLIENTE         PIC 9(5).
           05 TODOS22                      PIC X(9).
           05 IND-ALUGADOS-LIVRO           PIC 9(5).
           05 TODOS23                      PIC X(14).
           05 IND-DATA-ALUGUER.
               10 IND-ALUGADOS-ANO-ALUGUER PIC 9999.
               10 DATA7                    PIC X.
               10 IND-ALUGADOS-MES-ALUGUER PIC 99.
               10 DATA8                    PIC X.
               10 IND-ALUGADOS-DIA-ALUGUER PIC 99.
           05 TODOS24                      PIC X(14).
           05 IND-DATA-ENTREGA.
               10 IND-ALUGADOS-ANO-ENTREGA PIC 9999.
               10 DATA9                    PIC X.
               10 IND-ALUGADOS-MES-ENTREGA PIC 99.
               10 DATA10                   PIC X.
               10 IND-ALUGADOS-DIA-ENTREGA PIC 99.
           05 TODOS25                      PIC X(13).
           05 IND-ALUGADOS-ESTADO          PIC X.

       WORKING-STORAGE SECTION.
       77 FS                               PIC XX.

       01 DATA-SIST.
           05 ANO-SIST                     PIC 9999.
           05 MES-SIST                     PIC 99.
           05 DIA-SIST                     PIC 99.
       01 DATA-ACTUAL.
           05 DIA-ACTUAL                   PIC 99.
           05 FILLER                       PIC X   VALUE "/".
           05 MES-ACTUAL                   PIC 99.
           05 FILLER                       PIC X   VALUE "/".
           05 ANO-ACTUAL                   PIC 9999.

       77 SAIR                             PIC X   VALUE "N".
       77 EXISTE                           PIC X   VALUE SPACES.
       77 CONTADOR                         PIC 99  VALUE 4.
       77 ESCOLHA                          PIC 9   VALUE 0.
       77 REPETIR-MENU                     PIC X   VALUE "S".
       77 ANO-BISSEXTO                     PIC 9   VALUE 0.
       77 TEMP                             PIC 999 VALUE 0.
       77 DIAS-MES                         PIC 99  VALUE 31.
       77 LINHA                            PIC 99  VALUE 4.
       77 PAGINA                           PIC 99  VALUE 1.
       77 NIF-TEMP                         PIC 9(9).

       SCREEN SECTION.
       01 CLS BLANK SCREEN.

       PROCEDURE DIVISION.

       INICIO.
           ACCEPT DATA-SIST FROM DATE YYYYMMDD.
           MOVE DIA-SIST TO DIA-ACTUAL.
           MOVE MES-SIST TO MES-ACTUAL.
           MOVE ANO-SIST TO ANO-ACTUAL.

           OPEN I-O FIC-CLIENTES.
           OPEN I-O FIC-LIVROS.
           OPEN I-O FIC-TEMAS.
           OPEN I-O FIC-AUTORES.
           OPEN I-O FIC-ALUGUERES.

           PERFORM MENU-INICIAL.

           CLOSE FIC-CLIENTES.
           CLOSE FIC-LIVROS.
           CLOSE FIC-TEMAS.
           CLOSE FIC-AUTORES.
           CLOSE FIC-ALUGUERES.

           STOP RUN.

       BASE.
           DISPLAY "*************************************************" &
           "**********************************************************"
           AT 0101 HIGHLIGHT.

           DISPLAY "*************************************************" &
           "**********************************************************"
           AT 0301 HIGHLIGHT.
           DISPLAY "*************************************************" &
           "**********************************************************"
           AT 2501 HIGHLIGHT.

           DISPLAY "VILABIBLIO." HIGHLIGHT AT 0203.
           DISPLAY "DATA:" HIGHLIGHT AT 0290.
           DISPLAY DATA-ACTUAL HIGHLIGHT AT 0296.

       MENU-INICIAL.
           PERFORM UNTIL SAIR = "S"
               DISPLAY CLS
               PERFORM BASE
               DISPLAY "MENU INICIAL" HIGHLIGHT AT 0217
               DISPLAY "GESTAO DE ALUGUER DE LIVROS" HIGHLIGHT AT 0541
               DISPLAY "***************************" HIGHLIGHT AT 0641
               DISPLAY "1          CLIENTES" HIGHLIGHT AT 0745
               DISPLAY "2            LIVROS" HIGHLIGHT AT 0845
               DISPLAY "3             TEMAS" HIGHLIGHT AT 0945
               DISPLAY "4           AUTORES" HIGHLIGHT AT 1045
               DISPLAY "5         ALUGUERES" HIGHLIGHT AT 1145
               DISPLAY "6          EXPORTAR" HIGHLIGHT AT 1245
               DISPLAY "0              SAIR" HIGHLIGHT AT 1345
               DISPLAY "[ ] INSIRA UMA OPCAO" HIGHLIGHT AT 1544
               MOVE "S" TO REPETIR-MENU
               PERFORM UNTIL REPETIR-MENU = "N"
                   ACCEPT ESCOLHA AT 1545 AUTO
                   EVALUATE ESCOLHA
                       WHEN 1 PERFORM MENU-CLIENTES
                       WHEN 2 PERFORM MENU-LIVROS
                       WHEN 3 PERFORM MENU-TEMAS
                       WHEN 4 PERFORM MENU-AUTORES
                       WHEN 5 PERFORM MENU-ALUGUERES
                       WHEN 6 PERFORM MENU-EXPORTAR
                       WHEN 0
                           MOVE "S" TO SAIR
                           MOVE "N" TO REPETIR-MENU
                       WHEN OTHER
                           DISPLAY "OPCAO INCORRETA!" AT 1846
                           FOREGROUND-COLOR 3 HIGHLIGHT
                           MOVE "S" TO REPETIR-MENU
                   END-EVALUATE
               END-PERFORM
           END-PERFORM.

       MENU-CLIENTES.
           PERFORM UNTIL SAIR = "S"
               DISPLAY CLS
               PERFORM BASE
               DISPLAY "MENU CLIENTES" FOREGROUND-COLOR 5
               HIGHLIGHT AT 0217
               DISPLAY "GESTAO DE CLIENTES "
               FOREGROUND-COLOR 5 HIGHLIGHT AT 0545
               DISPLAY "******************"
               FOREGROUND-COLOR 5 HIGHLIGHT AT 0645
               DISPLAY "1             NOVO"
               HIGHLIGHT AT 0745
               DISPLAY "2          ALTERAR"
               HIGHLIGHT AT 0845
               DISPLAY "3         ELIMINAR"
               HIGHLIGHT AT 0945
               DISPLAY "4   LISTAGEM GERAL"
               HIGHLIGHT AT 1045
               DISPLAY "0     MENU INICIAL"
               HIGHLIGHT AT 1145
               DISPLAY "[ ]INSIRA UMA OPCAO"
               HIGHLIGHT AT 1344
               MOVE "S" TO REPETIR-MENU
               PERFORM UNTIL REPETIR-MENU = "N"
                   ACCEPT ESCOLHA AT 1345 AUTO
                   EVALUATE ESCOLHA
                       WHEN 1 PERFORM CLIENTES-NOVO
      *                WHEN 2 PERFORM CLIENTES-CONSULTAR
                       WHEN 2 PERFORM CLIENTES-ALTERAR
                       WHEN 3 PERFORM CLIENTES-ELIMINAR
                       WHEN 4 PERFORM CLIENTES-LISTAGEM
                       WHEN 0
                           MOVE "S" TO SAIR
                           MOVE "N" TO REPETIR-MENU
                       WHEN OTHER
                           DISPLAY "OPCAO INCORRETA!" AT 1746
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           MOVE "S" TO REPETIR-MENU
                   END-EVALUATE
               END-PERFORM
           END-PERFORM.
           MOVE "N" TO SAIR.
           MOVE "N" TO REPETIR-MENU.

       CLIENTES-NOVO.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "NOVO CLIENTE" FOREGROUND-COLOR 5 HIGHLIGHT AT 0217.
           DISPLAY "PREENCHA O FORMULARIO:" HIGHLIGHT AT 0403.

           DISPLAY "CODIGO CLIENTE:"
           AT 0603 HIGHLIGHT.

           DISPLAY "DEIXAR EM BRANCO PARA RETROCEDER."
           AT 0803 HIGHLIGHT.

           ACCEPT CLIENTE-COD AUTO HIGHLIGHT AT 0618.
           DISPLAY CLIENTE-COD HIGHLIGHT AT 0618.
           DISPLAY "                                " AT 0803.

           READ FIC-CLIENTES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "S") THEN
               DISPLAY "CLIENTE JA EXISTE!"
               FOREGROUND-COLOR 4 HIGHLIGHT AT 0628
           ELSE
               IF CLIENTE-COD = SPACES THEN
                   DISPLAY " " HIGHLIGHT AT 0618
               ELSE
                   DISPLAY "PREENCHA O FORMULARIO:"
                   AT 0403 HIGHLIGHT
                   DISPLAY "  NIF:" AT 0803 HIGHLIGHT
                   DISPLAY " NOME:" AT 1003 HIGHLIGHT
                   DISPLAY " DATA:" AT 1203 HIGHLIGHT
                   DISPLAY "EMAIL:" AT 1403 HIGHLIGHT
                   DISPLAY CLIENTE-COD HIGHLIGHT AT 0618

                   PERFORM WITH TEST AFTER UNTIL
                   EXISTE = "N" AND NIF >= 100000000
                       ACCEPT NIF AUTO HIGHLIGHT AT 0809
                       READ FIC-CLIENTES KEY NIF
                           INVALID KEY
                               MOVE "N" TO EXISTE
                           NOT INVALID KEY
                               MOVE "S" TO EXISTE
                       END-READ
                       IF EXISTE = "S" THEN
                           DISPLAY "NIF JA EXISTE!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 0820
                       END-IF
                       IF NIF NOT >= 100000000 THEN
                           DISPLAY "NIF INCORRETO!  "
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 0820
                       END-IF
                   END-PERFORM
                   DISPLAY "               " AT 0820

                   PERFORM WITH TEST AFTER UNTIL
                   NOME > SPACES
                       ACCEPT NOME HIGHLIGHT AT 1009
                       IF NOME = SPACES THEN
                           DISPLAY "NOME INCORRETO!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1020
                       ELSE
                           DISPLAY "               " AT 1020
                       END-IF
                   END-PERFORM
                   DISPLAY "/" AT 1211 HIGHLIGHT
                   DISPLAY "/" AT 1214 HIGHLIGHT
                   PERFORM WITH TEST AFTER UNTIL
                   (DIA-ADMIT >= 0 AND DIA-ADMIT <= DIAS-MES)
                       ACCEPT DIA-ADMIT AUTO AT 1209 HIGHLIGHT
                       IF DIA-ADMIT = 0 THEN
                          MOVE DIA-SIST TO DIA-ADMIT
                       END-IF
                       IF NOT (DIA-ADMIT >= 0 AND
                       DIA-ADMIT <= DIAS-MES) THEN
                           DISPLAY "DIA INCORRETO!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1225
                       END-IF
                       IF ANO-ADMIT = ANO-SIST THEN
                           IF MES-ADMIT = MES-SIST THEN
                               IF DIA-ADMIT > DIA-SIST THEN
                                   DISPLAY "DIA INCORRETO!"
                                   FOREGROUND-COLOR 4 HIGHLIGHT AT 1225
                                   MOVE 32 TO DIA-ADMIT
                               END-IF
                           END-IF
                       END-IF
                   END-PERFORM
                   PERFORM WITH TEST AFTER UNTIL
                   (MES-ADMIT >= 0 AND <= 12)
                       ACCEPT MES-ADMIT AUTO AT 1212 HIGHLIGHT
                       IF NOT (MES-ADMIT >= 0 AND <= 12) THEN
                           DISPLAY "MES INCORRETO!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1225
                       END-IF
                       IF ANO-ADMIT = ANO-SIST THEN
                           IF MES-ADMIT > MES-SIST THEN
                               DISPLAY "MES INCORRETO!"
                               FOREGROUND-COLOR 4 HIGHLIGHT AT 1225
                               MOVE 13 TO MES-ADMIT
                           END-IF
                       END-IF
                   END-PERFORM

                   IF MES-ADMIT = 0 THEN
                       MOVE MES-SIST TO MES-ADMIT
                   END-IF
                   PERFORM WITH TEST AFTER UNTIL (ANO-ADMIT = 0) OR
                  (ANO-ADMIT >= 1968 AND ANO-ADMIT <= ANO-SIST)
                       ACCEPT ANO-ADMIT AUTO AT 1215 HIGHLIGHT
                       IF NOT (ANO-ADMIT >= 1968 AND
                       ANO-ADMIT <= ANO-SIST)
                           DISPLAY "ANO INCORRETO!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1225
                       END-IF
                   END-PERFORM

                   IF ANO-ADMIT = 0 THEN
                       MOVE ANO-SIST TO ANO-ADMIT
                   END-IF
                   DISPLAY DIA-ADMIT AT 1209 HIGHLIGHT
                   DISPLAY MES-ADMIT AT 1212 HIGHLIGHT
                   DISPLAY ANO-ADMIT AT 1215 HIGHLIGHT
                   DIVIDE ANO-ADMIT BY 5 GIVING TEMP
                   REMAINDER ANO-BISSEXTO
                   EVALUATE MES-ADMIT
                       WHEN 1
                       WHEN 3
                       WHEN 5
                       WHEN 7
                       WHEN 8
                       WHEN 10
                       WHEN 12
                           MOVE 31 TO DIAS-MES
                       WHEN 4
                       WHEN 6
                       WHEN 9
                       WHEN 11
                           MOVE 30 TO DIAS-MES
                       WHEN 2
                           IF ANO-BISSEXTO = 0 THEN
                               MOVE 29 TO DIAS-MES
                           ELSE
                               MOVE 28 TO DIAS-MES
                   END-EVALUATE

                   ACCEPT EMAIL AT 1409 HIGHLIGHT

                   WRITE REGISTO-CLIENTES
                       INVALID KEY
                           DISPLAY "ERRO AO CRIAR CLIENTE!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1603
                       NOT INVALID KEY
                           DISPLAY "CLIENTE CRIADO COM SUCESSO!"
                           FOREGROUND-COLOR 2 HIGHLIGHT AT 1603
                   END-WRITE
               END-IF
           END-IF.

           DISPLAY "ENTER - CONTINUAR"
           HIGHLIGHT AT 1803.
           ACCEPT OMITTED AT 1821.
           MOVE "N" TO REPETIR-MENU.

       CLIENTES-ALTERAR.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "ALTERAR CLIENTE"
           FOREGROUND-COLOR 5 HIGHLIGHT AT 0217.
           DISPLAY "INSIRA CODIGO DE CLIENTE:" AT 0403
           HIGHLIGHT.

           DISPLAY "CODIGO DE CLIENTE:"
           AT 0603 HIGHLIGHT.

           DISPLAY "DEIXAR EM BRANCO PARA RETROCEDER."
           AT 0803 HIGHLIGHT.

           ACCEPT CLIENTE-COD AUTO HIGHLIGHT AT 0621.
           DISPLAY CLIENTE-COD HIGHLIGHT AT 0621.
           DISPLAY "                                " AT 0803.

           READ FIC-CLIENTES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF CLIENTE-COD = SPACES THEN
                   DISPLAY " " HIGHLIGHT AT 0621
               ELSE
                   DISPLAY "CLIENTE NAO EXISTE!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0621
               END-IF
           ELSE
               DISPLAY "PREENCHA O FORMULARIO:     " AT 0403 HIGHLIGHT
               DISPLAY " NIF:" AT 0803 HIGHLIGHT
               DISPLAY "NOME:" AT 1003 HIGHLIGHT
               DISPLAY "DATA:" AT 1203 HIGHLIGHT
               DISPLAY "MAIL:" AT 1403 HIGHLIGHT
               DISPLAY CLIENTE-COD AT 0622

               PERFORM WITH TEST AFTER UNTIL
               EXISTE = "N" AND NIF >= 100000000
                   ACCEPT NIF AUTO HIGHLIGHT AT 0809
                   READ FIC-CLIENTES KEY NIF
                       INVALID KEY
                           MOVE "N" TO EXISTE
                       NOT INVALID KEY
                           MOVE "S" TO EXISTE
                   END-READ
                   IF EXISTE = "S" THEN
                       DISPLAY "NIF JA EXISTE!"
                       FOREGROUND-COLOR 3 HIGHLIGHT AT 0820
                   END-IF
                   IF NIF NOT >= 100000000 THEN
                       DISPLAY "NIF INCORRETO!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 0820
                   END-IF
               END-PERFORM
               DISPLAY "               " AT 0820

               PERFORM WITH TEST AFTER UNTIL
               NOME > SPACES
                   ACCEPT NOME HIGHLIGHT AT 1009
                   IF NOME = SPACES THEN
                       DISPLAY "NOME INCORRETO! "
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1020
                   ELSE
                       DISPLAY "               " AT 1020
                   END-IF
               END-PERFORM
               DISPLAY "/" AT 1211 HIGHLIGHT
                   DISPLAY "/" AT 1214 HIGHLIGHT
                   PERFORM WITH TEST AFTER UNTIL
                   (DIA-ADMIT >= 0 AND DIA-ADMIT <= DIAS-MES)
                       ACCEPT DIA-ADMIT AUTO AT 1209 HIGHLIGHT
                       IF DIA-ADMIT = 0 THEN
                          MOVE DIA-SIST TO DIA-ADMIT
                       END-IF
                       IF NOT (DIA-ADMIT >= 0 AND
                       DIA-ADMIT <= DIAS-MES) THEN
                           DISPLAY "DIA INCORRETO!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1225
                       END-IF
                       IF ANO-ADMIT = ANO-SIST THEN
                           IF MES-ADMIT = MES-SIST THEN
                               IF DIA-ADMIT > DIA-SIST THEN
                                   DISPLAY "DIA INCORRETO!"
                                   FOREGROUND-COLOR 4 HIGHLIGHT AT 1225
                                   MOVE 32 TO DIA-ADMIT
                               END-IF
                           END-IF
                       END-IF
                   END-PERFORM
                   PERFORM WITH TEST AFTER UNTIL
                   (MES-ADMIT >= 0 AND <= 12)
                       ACCEPT MES-ADMIT AUTO AT 1212 HIGHLIGHT
                       IF NOT (MES-ADMIT >= 0 AND <= 12) THEN
                           DISPLAY "MES INCORRETO!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1225
                       END-IF
                       IF ANO-ADMIT = ANO-SIST THEN
                           IF MES-ADMIT > MES-SIST THEN
                               DISPLAY "MES INCORRETO!"
                               FOREGROUND-COLOR 4 HIGHLIGHT AT 1225
                               MOVE 13 TO MES-ADMIT
                           END-IF
                       END-IF
                   END-PERFORM

                   IF MES-ADMIT = 0 THEN
                       MOVE MES-SIST TO MES-ADMIT
                   END-IF
                   PERFORM WITH TEST AFTER UNTIL (ANO-ADMIT = 0) OR
                  (ANO-ADMIT >= 1968 AND ANO-ADMIT <= ANO-SIST)
                       ACCEPT ANO-ADMIT AUTO AT 1215 HIGHLIGHT
                       IF NOT (ANO-ADMIT >= 1968 AND
                       ANO-ADMIT <= ANO-SIST)
                           DISPLAY "ANO INCORRETO!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1225
                       END-IF
                   END-PERFORM

                   IF ANO-ADMIT = 0 THEN
                       MOVE ANO-SIST TO ANO-ADMIT
                   END-IF
                   DISPLAY DIA-ADMIT AT 1209 HIGHLIGHT
                   DISPLAY MES-ADMIT AT 1212 HIGHLIGHT
                   DISPLAY ANO-ADMIT AT 1215 HIGHLIGHT
                   DIVIDE ANO-ADMIT BY 5 GIVING TEMP
                   REMAINDER ANO-BISSEXTO
                   EVALUATE MES-ADMIT
                       WHEN 1
                       WHEN 3
                       WHEN 5
                       WHEN 7
                       WHEN 8
                       WHEN 10
                       WHEN 12
                           MOVE 31 TO DIAS-MES
                       WHEN 4
                       WHEN 6
                       WHEN 9
                       WHEN 11
                           MOVE 30 TO DIAS-MES
                       WHEN 2
                           IF ANO-BISSEXTO = 0 THEN
                               MOVE 29 TO DIAS-MES
                           ELSE
                               MOVE 28 TO DIAS-MES
                   END-EVALUATE

               ACCEPT EMAIL AT 1410 HIGHLIGHT

               REWRITE REGISTO-CLIENTES
                   INVALID KEY
                       DISPLAY "ERRO AO ALTERAR CLIENTE!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1603
                       NOT INVALID KEY
                           DISPLAY "CLIENTE ALTERADO COM SUCESSO!"
                           FOREGROUND-COLOR 2 HIGHLIGHT AT 1603
               END-REWRITE
           END-IF.

           DISPLAY "ENTER - CONTINUAR"
           HIGHLIGHT AT 1803.
           ACCEPT OMITTED AT 1821.
           MOVE "N" TO REPETIR-MENU.

       CLIENTES-ELIMINAR.
           DISPLAY CLS.
           PERFORM BASE.

           DISPLAY "ELIMINAR CLIENTE" FOREGROUND-COLOR 5
           HIGHLIGHT AT 0217.

           DISPLAY "INSIRA CODIGO DE CLIENTE:" AT 0403
           HIGHLIGHT.
           DISPLAY "CODIGO DE CLIENTE:"
           AT 0603 HIGHLIGHT.

           DISPLAY "DEIXE EM BRANCO PARA RETROCEDER."
           AT 0803 HIGHLIGHT.

           ACCEPT CLIENTE-COD AUTO HIGHLIGHT AT 0621.
           DISPLAY CLIENTE-COD HIGHLIGHT AT 0621.
           DISPLAY "                                " AT 0803.

           READ FIC-CLIENTES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF CLIENTE-COD = SPACES THEN
                   DISPLAY " " AT 0621 HIGHLIGHT
               ELSE
                   DISPLAY "CLIENTE NAO EXISTE!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0621
               END-IF
           ELSE
               DELETE FIC-CLIENTES
               INVALID KEY
                   DISPLAY "ERRO AO ELIMINAR CLIENTE! "
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0621
               NOT INVALID KEY
                   DISPLAY "CLIENTE ELIMINADO COM SUCESSO!"
                   FOREGROUND-COLOR 2 HIGHLIGHT AT 0621
               END-DELETE
           END-IF.

           DISPLAY "ENTER - CONTINUAR"
           HIGHLIGHT AT 1803.
           ACCEPT OMITTED AT 1821.
           MOVE "N" TO REPETIR-MENU.

       CLIENTES-LISTAGEM.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "LISTAGEM DE CLIENTES" FOREGROUND-COLOR 5
           HIGHLIGHT AT 0217.

           DISPLAY "*************************************************" &
           "**********************************************************"
           AT 0301.

           DISPLAY "C.CLIENTE" FOREGROUND-COLOR 5 HIGHLIGHT AT 0403.
           DISPLAY "NIF" FOREGROUND-COLOR 5 HIGHLIGHT AT 0414.
           DISPLAY "NOME" FOREGROUND-COLOR 5 HIGHLIGHT AT 0425.
           DISPLAY "DATA ADMISSAO" FOREGROUND-COLOR 5 HIGHLIGHT AT 0456.
           DISPLAY "EMAIL" FOREGROUND-COLOR 5 HIGHLIGHT AT 0472.
           DISPLAY "PAG" FOREGROUND-COLOR 5 HIGHLIGHT AT 2002.
           DISPLAY "|" FOREGROUND-COLOR 5 AT 2010.

           DISPLAY "*************************************************" &
           "**********************************************************"
           AT 0501.
           MOVE 1 TO PAGINA.
           MOVE 0 TO CLIENTE-COD.
           START FIC-CLIENTES KEY > CLIENTE-COD
               INVALID KEY
                   DISPLAY "FICHEIRO VAZIO. ENTER - CONTINUAR."
                   HIGHLIGHT AT 2011
                   DISPLAY "00" HIGHLIGHT AT 2006
                   ACCEPT OMITTED AT 2054
               NOT INVALID KEY
                   MOVE 6 TO LINHA
                   IF FS <> "05" AND FS <> "23" THEN
                       PERFORM UNTIL FS = "10"
                           READ FIC-CLIENTES NEXT RECORD
                               NOT AT END
                                   DISPLAY PAGINA
                                   HIGHLIGHT AT 2006
                                   DISPLAY CLIENTE-COD
                                   HIGHLIGHT LINE LINHA COL 3
                                   DISPLAY NIF
                                   HIGHLIGHT LINE LINHA COL 14
                                   DISPLAY NOME
                                   HIGHLIGHT LINE LINHA COL 25
                                   DISPLAY DIA-ADMIT
                                   HIGHLIGHT LINE LINHA COL 56
                                   DISPLAY "/" LINE LINHA COL 58
                                   DISPLAY MES-ADMIT
                                   HIGHLIGHT LINE LINHA COL 59
                                   DISPLAY "/" LINE LINHA COL 61
                                   DISPLAY ANO-ADMIT
                                   HIGHLIGHT LINE LINHA COL 62
                                   DISPLAY EMAIL
                                   HIGHLIGHT LINE LINHA COL 72
                                   ADD 1 TO LINHA
                                   IF LINHA = 19 THEN
                                       MOVE 6 TO LINHA
                                       DISPLAY "ENTER - PROXIMA PAGINA"
                                       HIGHLIGHT AT 2011
                                       ACCEPT OMITTED AT 2053
                                       ADD 1 TO PAGINA
                                   END-IF
                           END-READ
                       END-PERFORM
                   END-IF
                   DISPLAY "*****************************************" &
           "*********************************************************" &
           "*********"
           LINE LINHA COL 1
                   DISPLAY "ENTER - CONTINUAR" HIGHLIGHT AT 2011
                   ACCEPT OMITTED AT 2030
           END-START.

           MOVE "N" TO REPETIR-MENU.

       MENU-LIVROS.
           PERFORM UNTIL SAIR = "S"
               DISPLAY CLS
               PERFORM BASE
               DISPLAY "MENU LIVROS" FOREGROUND-COLOR 3
               HIGHLIGHT AT 0217
               DISPLAY " GESTAO DE LIVROS  "
               FOREGROUND-COLOR 3 HIGHLIGHT AT 0545
               DISPLAY "******************"
               FOREGROUND-COLOR 3 HIGHLIGHT AT 0645
               DISPLAY "1             NOVO" HIGHLIGHT AT 0745
               DISPLAY "2          ALTERAR" HIGHLIGHT AT 0845
               DISPLAY "3         ELIMINAR" HIGHLIGHT AT 0945
               DISPLAY "4         LISTAGEM" HIGHLIGHT AT 1045
               DISPLAY "0     MENU INICIAL" HIGHLIGHT AT 1145
               DISPLAY "[ ]    INSIRA OPCAO" HIGHLIGHT AT 1344
               MOVE "S" TO REPETIR-MENU
               PERFORM UNTIL REPETIR-MENU = "N"
                   ACCEPT ESCOLHA AT 1345 AUTO
                   EVALUATE ESCOLHA
                       WHEN 1 PERFORM LIVROS-NOVO
                       WHEN 2 PERFORM LIVROS-ALTERAR
                       WHEN 3 PERFORM LIVROS-ELIMINAR
                       WHEN 4 PERFORM LIVROS-LISTAGEM
                       WHEN 0
                           MOVE "S" TO SAIR
                           MOVE "N" TO REPETIR-MENU
                       WHEN OTHER
                           DISPLAY "OPCAO INCORRETA!" AT 1745
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           MOVE "S" TO REPETIR-MENU
                   END-EVALUATE
               END-PERFORM
           END-PERFORM.
           MOVE "N" TO SAIR.
           MOVE "N" TO REPETIR-MENU.

       LIVROS-NOVO.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "NOVO LIVRO" FOREGROUND-COLOR 3 HIGHLIGHT AT 0217.
           DISPLAY "PREENCHA O FORMULARIO:" AT 0403
           HIGHLIGHT.
           DISPLAY "CODIGO LIVRO:" AT 0603 HIGHLIGHT.
           DISPLAY "DEIXE EM BRANCO PARA RETROCEDER."
           AT 0803 HIGHLIGHT.
           ACCEPT LIVRO-COD AUTO HIGHLIGHT AT 0618.
           DISPLAY LIVRO-COD HIGHLIGHT AT 0618.
           DISPLAY "                                " AT 0803.
           READ FIC-LIVROS
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.
           IF (EXISTE = "S") THEN
               DISPLAY "LIVRO JA EXISTE!"
               FOREGROUND-COLOR 4 HIGHLIGHT AT 0618
           ELSE
               IF LIVRO-COD = SPACES THEN
                   DISPLAY " " AT 0618 HIGHLIGHT
               ELSE
                   DISPLAY "PREENCHA O FORMULARIO:" AT 0403 HIGHLIGHT
                   DISPLAY "      TITULO:" AT 0803 HIGHLIGHT
                   DISPLAY " CODIGO TEMA:" AT 1003 HIGHLIGHT
                   DISPLAY "CODIGO AUTOR:" AT 1203 HIGHLIGHT
                   PERFORM WITH TEST AFTER UNTIL
                   TITULO > SPACES
                       ACCEPT TITULO HIGHLIGHT AT 0818
                       IF TITULO = SPACES THEN
                           DISPLAY "TITULO INCORRETO! "
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 0818
                       ELSE
                           DISPLAY "                 " AT 0818
                       END-IF
                   END-PERFORM
                   DISPLAY TITULO HIGHLIGHT AT 0818
                   PERFORM WITH TEST AFTER UNTIL
                   LIVRO-TEMA-COD > 0
                       ACCEPT LIVRO-TEMA-COD AUTO HIGHLIGHT AT 1018
                       IF LIVRO-TEMA-COD = SPACES THEN
                           DISPLAY "CODIGO INCORRETO! "
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1018
                       ELSE
                           DISPLAY "                 " AT 1018
                       END-IF
                   END-PERFORM
                   MOVE LIVRO-TEMA-COD TO TEMA-COD
                   READ FIC-TEMAS KEY TEMA-COD
                       INVALID KEY
                           MOVE "N" TO EXISTE
                       NOT INVALID KEY
                           MOVE "S" TO EXISTE
                   END-READ
                   IF (EXISTE = "N") THEN
                       DISPLAY "TEMA NAO EXISTE"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1018
                   END-IF
                   DISPLAY TEMA-COD HIGHLIGHT AT 1018
                   PERFORM WITH TEST AFTER UNTIL
                   LIVRO-AUTOR-COD > 0
                       ACCEPT LIVRO-AUTOR-COD AUTO HIGHLIGHT AT 1218
                       IF LIVRO-AUTOR-COD = SPACES THEN
                           DISPLAY "CODIGO INCORRETO! "
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1218
                       ELSE
                           DISPLAY "                 " AT 1218
                       END-IF
                   END-PERFORM
                   MOVE LIVRO-AUTOR-COD TO AUTOR-COD
                   READ FIC-AUTORES KEY AUTOR-COD
                       INVALID KEY
                           MOVE "N" TO EXISTE
                       NOT INVALID KEY
                           MOVE "S" TO EXISTE
                   END-READ
                   IF (EXISTE = "N") THEN
                       DISPLAY "AUTOR NAO EXISTE!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1218
                   END-IF
                   DISPLAY AUTOR-COD HIGHLIGHT AT 1218

                   WRITE REGISTO-LIVROS
                       INVALID KEY
                           DISPLAY "ERRO AO CRIAR LIVRO!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1403
                       NOT INVALID KEY
                           DISPLAY "LIVRO CRIADO COM SUCESSO!"
                           FOREGROUND-COLOR 2 HIGHLIGHT AT 1403
                   END-WRITE
               END-IF
           END-IF.
           DISPLAY "ENTER - CONTINUAR"
           HIGHLIGHT AT 1803.
           ACCEPT OMITTED AT 1821.
           MOVE "N" TO REPETIR-MENU.

       LIVROS-ALTERAR.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "ALTERAR LIVRO" FOREGROUND-COLOR 3 HIGHLIGHT AT 0217.

           DISPLAY "INSIRA CODIGO DO LIVRO:" AT 0403 HIGHLIGHT.
           DISPLAY "CODIGO LIVRO:" AT 0603 HIGHLIGHT.
           DISPLAY "DEIXE EM BRANCO PARA RETROCEDER."
           AT 0803 HIGHLIGHT.
           ACCEPT LIVRO-COD AUTO HIGHLIGHT AT 0618.
           DISPLAY LIVRO-COD HIGHLIGHT AT 0618.
           DISPLAY "                                " AT 0803.
           READ FIC-LIVROS
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.
           IF (EXISTE = "N") THEN
               IF LIVRO-COD = SPACES THEN
                   DISPLAY " " AT 0618 HIGHLIGHT
               ELSE
                   DISPLAY "LIVRO NAO EXISTE!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0618
               END-IF
           ELSE
               DISPLAY "PREENCHA O FORMULARIO:" AT 0403 HIGHLIGHT
               DISPLAY "      TITULO:"AT 0803 HIGHLIGHT
               DISPLAY " CODIGO TEMA:" AT 1003 HIGHLIGHT
               DISPLAY "CODIGO AUTOR:" AT 1203 HIGHLIGHT

               PERFORM WITH TEST AFTER UNTIL
               TITULO > SPACES
                   ACCEPT TITULO HIGHLIGHT AT 0818
                   IF TITULO = SPACES THEN
                       DISPLAY "TITULO INCORRETO! "
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 0818
                   ELSE
                       DISPLAY "                 " AT 0818
                   END-IF
               END-PERFORM
               DISPLAY TITULO HIGHLIGHT AT 0818
               PERFORM WITH TEST AFTER UNTIL
               LIVRO-TEMA-COD > 0
                   ACCEPT LIVRO-TEMA-COD AUTO HIGHLIGHT AT 1018
                   IF LIVRO-TEMA-COD = SPACES THEN
                       DISPLAY "CODIGO INCORRETO! "
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1018
                   ELSE
                       DISPLAY "                 " AT 1018
                   END-IF
               END-PERFORM
               MOVE LIVRO-TEMA-COD TO TEMA-COD
               READ FIC-TEMAS KEY TEMA-COD
                   INVALID KEY
                       MOVE "N" TO EXISTE
                   NOT INVALID KEY
                       MOVE "S" TO EXISTE
               END-READ
               IF (EXISTE = "N") THEN
                   DISPLAY "TEMA NAO EXISTE"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 1018
               END-IF
               DISPLAY LIVRO-TEMA-COD HIGHLIGHT AT 1018
               PERFORM WITH TEST AFTER UNTIL
               LIVRO-AUTOR-COD > 0
                   ACCEPT LIVRO-AUTOR-COD AUTO HIGHLIGHT AT 1220
                   IF LIVRO-AUTOR-COD = SPACES THEN
                       DISPLAY "CODIGO INCORRETO! "
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1220
                   ELSE
                       DISPLAY "                 " AT 1220
                   END-IF
               END-PERFORM
               MOVE LIVRO-AUTOR-COD TO AUTOR-COD
               READ FIC-AUTORES KEY AUTOR-COD
                   INVALID KEY
                       MOVE "N" TO EXISTE
                   NOT INVALID KEY
                       MOVE "S" TO EXISTE
               END-READ
               IF (EXISTE = "N") THEN
                   DISPLAY "AUTOR NAO EXISTE"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 1220
               END-IF

               REWRITE REGISTO-LIVROS
                   INVALID KEY
                       DISPLAY "ERRO AO ALTERAR LIVRO!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1403
                   NOT INVALID KEY
                       DISPLAY "LIVRO ALTERADO COM SUCESSO!"
                       FOREGROUND-COLOR 2 HIGHLIGHT AT 1403
               END-REWRITE
           END-IF.

           DISPLAY "ENTER - CONTINUAR"
           HIGHLIGHT AT 1803.
           ACCEPT OMITTED AT 1821.
           MOVE "N" TO REPETIR-MENU.

       LIVROS-ELIMINAR.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "ELIMINAR LIVRO" FOREGROUND-COLOR 3
           HIGHLIGHT AT 0217.

           DISPLAY "INSIRA CODIGO DO LIVRO:" AT 0403
           HIGHLIGHT.

           DISPLAY "CODIGO LIVRO:" AT 0603 HIGHLIGHT.

           DISPLAY "DEIXE EM BRANCO PARA RETROCEDER." AT 0803 HIGHLIGHT.

           ACCEPT LIVRO-COD AUTO HIGHLIGHT AT 0618.
           DISPLAY LIVRO-COD HIGHLIGHT AT 0618.
           DISPLAY "                                " AT 0803.

           READ FIC-LIVROS
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF LIVRO-COD = SPACES THEN
                   DISPLAY " " AT 0618 HIGHLIGHT
               ELSE
                   DISPLAY "LIVRO NAO EXISTE!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0618
               END-IF
           ELSE
               DELETE FIC-LIVROS
               INVALID KEY
                   DISPLAY "ERRO AO ELIMINAR LIVRO! "
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0803
               NOT INVALID KEY
                   DISPLAY "LIVRO APAGADO!"
                   FOREGROUND-COLOR 2 HIGHLIGHT AT 0803
               END-DELETE
           END-IF.

           DISPLAY "ENTER - CONTINUAR"
           HIGHLIGHT AT 1803.
           ACCEPT OMITTED AT 1821.
           MOVE "N" TO REPETIR-MENU.

       LIVROS-LISTAGEM.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "LISTAGEM DE LIVROS" FOREGROUND-COLOR 3
           HIGHLIGHT AT 0217.

           DISPLAY "*************************************************" &
           "**********************************************************"
           AT 0301.

           DISPLAY "COD. LIVRO" FOREGROUND-COLOR 3 HIGHLIGHT AT 0403.
           DISPLAY "COD. TEMA" FOREGROUND-COLOR 3 HIGHLIGHT AT 0416.
           DISPLAY "COD. AUTOR" FOREGROUND-COLOR 3 HIGHLIGHT AT 0428.
           DISPLAY "TITULO" FOREGROUND-COLOR 3 HIGHLIGHT AT 0440.
           DISPLAY "PAG." FOREGROUND-COLOR 3 HIGHLIGHT AT 2002.
           DISPLAY "|" FOREGROUND-COLOR 3 AT 2009.

           DISPLAY "*************************************************" &
           "**********************************************************"
           AT 0501.
           DISPLAY "*************************************************" &
           "**********************************************************"
           AT 1901.

           MOVE 1 TO PAGINA.
           MOVE 0 TO LIVRO-COD.
           START FIC-LIVROS KEY > LIVRO-COD
               INVALID KEY
                   DISPLAY "FICHEIRO VAZIO. ENTER - CONTINUAR."
                   HIGHLIGHT AT 2011
                   DISPLAY "00" HIGHLIGHT AT 2006
                   ACCEPT OMITTED AT 2050
               NOT INVALID KEY
                   MOVE 6 TO LINHA
                   IF FS <> "05" AND FS <> "23" THEN
                       PERFORM UNTIL FS = "10"
                           READ FIC-LIVROS NEXT RECORD
                               NOT AT END
                                   DISPLAY PAGINA
                                   HIGHLIGHT AT 2006

                                   READ FIC-LIVROS KEY LIVRO-COD
                                       INVALID KEY
                                           MOVE "N" TO EXISTE
                                       NOT INVALID KEY
                                           MOVE "S" TO EXISTE
                                   END-READ

                                   IF (EXISTE = "N") THEN
                                       DISPLAY LIVRO-COD
                                       FOREGROUND-COLOR 4
                                       HIGHLIGHT LINE LINHA COL 3
                                       DISPLAY "INSIRA RESTANTES DADOS!"
                                       FOREGROUND-COLOR 4
                                       HIGHLIGHT LINE LINHA COL 80
                                   ELSE
                                       DISPLAY LIVRO-COD
                                       HIGHLIGHT LINE LINHA COL 3
                                   END-IF


                                   MOVE LIVRO-TEMA-COD TO TEMA-COD
                                   READ FIC-TEMAS KEY TEMA-COD
                                       INVALID KEY
                                           MOVE "N" TO EXISTE
                                       NOT INVALID KEY
                                           MOVE "S" TO EXISTE
                                   END-READ

                                   IF (EXISTE = "N") THEN
                                       DISPLAY LIVRO-TEMA-COD
                                       FOREGROUND-COLOR 4
                                       HIGHLIGHT LINE LINHA COL 16
                                       DISPLAY "INSIRA RESTANTES DADOS!"
                                       FOREGROUND-COLOR 4
                                       HIGHLIGHT LINE LINHA COL 80
                                   ELSE
                                       DISPLAY LIVRO-TEMA-COD
                                       HIGHLIGHT LINE LINHA COL 16
                                   END-IF


                                   MOVE LIVRO-AUTOR-COD TO AUTOR-COD
                                   READ FIC-AUTORES KEY AUTOR-COD
                                       INVALID KEY
                                           MOVE "N" TO EXISTE
                                       NOT INVALID KEY
                                           MOVE "S" TO EXISTE
                                   END-READ

                                   IF (EXISTE = "N") THEN
                                       DISPLAY LIVRO-AUTOR-COD
                                       FOREGROUND-COLOR 4
                                       HIGHLIGHT LINE LINHA COL 28
                                       DISPLAY "INSIRA RESTANTES DADOS!"
                                       FOREGROUND-COLOR 4
                                       HIGHLIGHT LINE LINHA COL 80
                                   ELSE
                                       DISPLAY LIVRO-AUTOR-COD
                                       HIGHLIGHT LINE LINHA COL 28
                                   END-IF

                                   DISPLAY TITULO
                                   HIGHLIGHT LINE LINHA COL 40
                                   ADD 1 TO LINHA
                                   IF LINHA = 19 THEN
                                       MOVE 6 TO LINHA
                                       DISPLAY
                            "ENTER - PROXIMA PAGINA."
                                       HIGHLIGHT AT 2011
                                       ACCEPT OMITTED AT 2040
                                       ADD 1 TO PAGINA

                                   END-IF
                           END-READ
                       END-PERFORM
                   END-IF
                   DISPLAY "*****************************************" &
           "*********************************************************" &
           "*********"
           LINE LINHA COL 1
                   DISPLAY "ENTER - CONTINUAR"
                   HIGHLIGHT AT 2011
                   ACCEPT OMITTED AT 2030
           END-START.

           MOVE "N" TO REPETIR-MENU.

       MENU-TEMAS.
           PERFORM UNTIL SAIR = "S"
               DISPLAY CLS
               PERFORM BASE
               DISPLAY "MENU DE TEMAS" FOREGROUND-COLOR 1
               HIGHLIGHT AT 0217
               DISPLAY "  GESTAO DE TEMAS  "
               FOREGROUND-COLOR 1 HIGHLIGHT AT 0545
               DISPLAY "*******************"
               FOREGROUND-COLOR 1 HIGHLIGHT AT 0645
               DISPLAY "1              NOVO" HIGHLIGHT AT 0745
               DISPLAY "2           ALTERAR" HIGHLIGHT AT 0845
               DISPLAY "3          ELIMINAR" HIGHLIGHT AT 0945
               DISPLAY "4          LISTAGEM" HIGHLIGHT AT 1045
               DISPLAY "0      MENU INICIAL" HIGHLIGHT AT 1145
               DISPLAY "[ ] INSIRA UMA OPCAO" HIGHLIGHT AT 1344
               MOVE "S" TO REPETIR-MENU
               PERFORM UNTIL REPETIR-MENU = "N"
                   ACCEPT ESCOLHA AT 1345 AUTO
                   EVALUATE ESCOLHA
                       WHEN 1 PERFORM TEMAS-NOVO
                       WHEN 2 PERFORM TEMAS-ALTERAR
                       WHEN 3 PERFORM TEMAS-ELIMINAR
                       WHEN 4 PERFORM TEMAS-LISTAGEM
                       WHEN 0
                           MOVE "S" TO SAIR
                           MOVE "N" TO REPETIR-MENU
                       WHEN OTHER
                           DISPLAY "OPCAO INCORRETA!" AT 1745
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           MOVE "S" TO REPETIR-MENU
                   END-EVALUATE
               END-PERFORM
           END-PERFORM.
           MOVE "N" TO SAIR.
           MOVE "N" TO REPETIR-MENU.

       TEMAS-NOVO.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "NOVO TEMA" FOREGROUND-COLOR 1 HIGHLIGHT AT 0217.
           DISPLAY "PREENCHA O FORMULARIO:" AT 0403 HIGHLIGHT.
           DISPLAY "CODIGO TEMA:" AT 0603 HIGHLIGHT.
           DISPLAY "DEIXAR EM BRANCO PARA RETROCEDER."
           AT 0803 HIGHLIGHT.
           ACCEPT TEMA-COD AUTO HIGHLIGHT AT 0616.
           DISPLAY TEMA-COD HIGHLIGHT AT 0616.
           DISPLAY "                                " AT 0803.
           READ FIC-TEMAS
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.
           IF (EXISTE = "S") THEN
               DISPLAY "TEMA JA EXISTE!"
               FOREGROUND-COLOR 4 HIGHLIGHT AT 0616
           ELSE
               IF TEMA-COD = SPACES THEN
                   DISPLAY " " AT 0616 HIGHLIGHT
               ELSE
                   DISPLAY "PREENCHA O FORMULARIO:" AT 0403 HIGHLIGHT
                   DISPLAY "TEMA:" AT 0803 HIGHLIGHT

                   PERFORM WITH TEST AFTER UNTIL
                   TEMA > SPACES
                       ACCEPT TEMA HIGHLIGHT AT 0809
                       IF TEMA = SPACES THEN
                           DISPLAY "TEMA INCORRETO! "
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 0840
                       ELSE
                           DISPLAY "               " AT 0840
                       END-IF
                   END-PERFORM

                   WRITE REGISTO-TEMAS
                       INVALID KEY
                           DISPLAY "ERRO AO CRIAR TEMA!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1003
                       NOT INVALID KEY
                           DISPLAY "TEMA CRIADO COM SUCESSO!"
                           FOREGROUND-COLOR 2 HIGHLIGHT AT 1003
                   END-WRITE
               END-IF
           END-IF.

           DISPLAY "ENTER - CONTINUAR"
           HIGHLIGHT AT 1803.
           ACCEPT OMITTED AT 1821.
           MOVE "N" TO REPETIR-MENU.

       TEMAS-ALTERAR.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "ALTERAR TEMA" FOREGROUND-COLOR 1 HIGHLIGHT AT 0217.
           DISPLAY "PREENCHA O FORMULARIO:" AT 0403 HIGHLIGHT.
           DISPLAY "CODIGO TEMA:"AT 0603 HIGHLIGHT.
           DISPLAY "DEIXAR EM BRANCO PARA RETROCEDER."
           AT 0803 HIGHLIGHT.
           ACCEPT TEMA-COD AUTO HIGHLIGHT AT 0616.
           DISPLAY TEMA-COD HIGHLIGHT AT 0616.
           DISPLAY "                                " AT 0803.

           READ FIC-TEMAS
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF TEMA-COD = SPACES THEN
                   DISPLAY " " AT 0618 HIGHLIGHT
               ELSE
                   DISPLAY "TEMA NAO EXISTE!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0616
               END-IF
           ELSE
               DISPLAY "PREENCHA O FORMULARIO:"
               AT 0403 HIGHLIGHT
               DISPLAY "TEMA:" AT 0803 HIGHLIGHT

               PERFORM WITH TEST AFTER UNTIL
               TEMA > SPACES
                   ACCEPT TEMA HIGHLIGHT AT 0809
                   IF TEMA = SPACES THEN
                       DISPLAY "TEMA INCORRETO! "
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 0816
                   ELSE
                       DISPLAY "               " AT 0816
                   END-IF
               END-PERFORM

               REWRITE REGISTO-TEMAS
                   INVALID KEY
                       DISPLAY "ERRO AO ALTERAR TEMA!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1003
                   NOT INVALID KEY
                       DISPLAY "TEMA ALTERADO COM SUCESSO!"
                       FOREGROUND-COLOR 2 HIGHLIGHT AT 1003
               END-REWRITE
           END-IF.

           DISPLAY "ENTER - CONTINUAR"
           HIGHLIGHT AT 1803.
           ACCEPT OMITTED AT 1821.
           MOVE "N" TO REPETIR-MENU.

       TEMAS-ELIMINAR.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "ELIMINAR TEMAS" FOREGROUND-COLOR 1
           HIGHLIGHT AT 0217.
           DISPLAY "PREENCHA O FORMULARIO:" AT 0403 HIGHLIGHT.
           DISPLAY "CODIGO TEMA:" AT 0603 HIGHLIGHT.

           DISPLAY "DEIXAR EM BRANCO PARA RETROCEDER."
           AT 0803 HIGHLIGHT.
           ACCEPT TEMA-COD AUTO HIGHLIGHT AT 0616.
           DISPLAY TEMA-COD HIGHLIGHT AT 0616.
           DISPLAY "                                " AT 0803.

           READ FIC-TEMAS
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF TEMA-COD = SPACES THEN
                   DISPLAY " " HIGHLIGHT AT 0616
               ELSE
                   DISPLAY "TEMA NAO EXISTE!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0616
               END-IF
           ELSE
               DELETE FIC-TEMAS
               INVALID KEY
                   DISPLAY "ERRO AO ELIMINAR TEMA!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0803
               NOT INVALID KEY
                   DISPLAY "TEMA ELIMINADO COM SUCESSO!"
                   FOREGROUND-COLOR 2 HIGHLIGHT AT 0803
               END-DELETE
           END-IF.

           DISPLAY "ENTER - CONTINUAR"
           HIGHLIGHT AT 1803.
           ACCEPT OMITTED AT 1821.
           MOVE "N" TO REPETIR-MENU.

       TEMAS-LISTAGEM.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "LISTAGEM DE TEMAS"  FOREGROUND-COLOR 1
           HIGHLIGHT AT 0217.

           DISPLAY "*************************************************" &
           "**********************************************************"
           AT 0301.

           DISPLAY "COD. TEMA" FOREGROUND-COLOR 1 HIGHLIGHT AT 0403.
           DISPLAY "TEMA" FOREGROUND-COLOR 1 HIGHLIGHT AT 0414.
           DISPLAY "PAG." FOREGROUND-COLOR 1 HIGHLIGHT AT 2002.
           DISPLAY "|" FOREGROUND-COLOR 1 AT 2009.

           DISPLAY "*************************************************" &
           "**********************************************************"
           AT 0501.
           DISPLAY "*************************************************" &
           "**********************************************************"
           AT 1901.

           MOVE 1 TO PAGINA.
           MOVE 0 TO TEMA-COD.
           START FIC-TEMAS KEY > TEMA-COD
               INVALID KEY
                   DISPLAY "FICHEIRO VAZIO. ENTER - CONTINUAR."
                   HIGHLIGHT AT 2011
                   DISPLAY "00" HIGHLIGHT AT 2006
                   ACCEPT OMITTED AT 2050
               NOT INVALID KEY
                   MOVE 6 TO LINHA
                   IF FS <> "05" AND FS <> "23" THEN
                       PERFORM UNTIL FS = "10"
                           READ FIC-TEMAS NEXT RECORD
                               NOT AT END
                                   DISPLAY PAGINA
                                   HIGHLIGHT AT 2006
                                   DISPLAY TEMA-COD
                                   HIGHLIGHT LINE LINHA COL 3
                                   DISPLAY TEMA
                                   HIGHLIGHT LINE LINHA COL 14
                                   ADD 1 TO LINHA
                                   IF LINHA = 19 THEN
                                       MOVE 6 TO LINHA
                                       DISPLAY
                            "ENTER - PROXIMA PAGINA."
                                       HIGHLIGHT AT 2011
                                       ACCEPT OMITTED AT 2050
                                       ADD 1 TO PAGINA
                                   END-IF
                           END-READ
                       END-PERFORM
                   END-IF
                   DISPLAY "*****************************************" &
           "*********************************************************" &
           "*********"
           LINE LINHA COL 1
                   DISPLAY "ENTER - CONTINUAR.                     "
                   HIGHLIGHT AT 2011
                   ACCEPT OMITTED AT 2030
           END-START.
           MOVE "N" TO REPETIR-MENU.
       MENU-AUTORES.
           PERFORM UNTIL SAIR = "S"
               DISPLAY CLS
               PERFORM BASE
               DISPLAY "MENU AUTORES" FOREGROUND-COLOR 1 AT 0217
               DISPLAY " GESTAO DE AUTORES " FOREGROUND-COLOR 1 AT 0545
               DISPLAY "*******************" FOREGROUND-COLOR 1 AT 0645
               DISPLAY "1              NOVO" HIGHLIGHT AT 0745
               DISPLAY "2           ALTERAR" HIGHLIGHT AT 0845
               DISPLAY "3          ELIMINAR" HIGHLIGHT AT 0945
               DISPLAY "4          LISTAGEM" HIGHLIGHT AT 1045
               DISPLAY "0      MENU INICIAL" HIGHLIGHT AT 1145
               DISPLAY "[ ] INSIRA UMA OPCAO" HIGHLIGHT AT 1344
               MOVE "S" TO REPETIR-MENU
               PERFORM UNTIL REPETIR-MENU = "N"
                   ACCEPT ESCOLHA AT 1345 AUTO
                   EVALUATE ESCOLHA
                       WHEN 1 PERFORM AUTORES-NOVO
                       WHEN 2 PERFORM AUTORES-ALTERAR
                       WHEN 3 PERFORM AUTORES-ELIMINAR
                       WHEN 4 PERFORM AUTORES-LISTAGEM
                       WHEN 0
                           MOVE "S" TO SAIR
                           MOVE "N" TO REPETIR-MENU
                       WHEN OTHER
                           DISPLAY "OPCAO INCORRETA!" AT 1745
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           MOVE "S" TO REPETIR-MENU
                   END-EVALUATE
               END-PERFORM
           END-PERFORM.
           MOVE "N" TO SAIR.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       AUTORES-NOVO.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "NOVO AUTOR" FOREGROUND-COLOR 1 AT 0217.
           DISPLAY "INSIRA CODIGO DO AUTOR:" AT 0403 HIGHLIGHT.
           DISPLAY "CODIGO AUTOR:" AT 0603 HIGHLIGHT.
           DISPLAY "DEIXAR EM BRANCO PARA RETROCEDER."
           AT 0803 HIGHLIGHT.
           ACCEPT AUTOR-COD AUTO HIGHLIGHT AT 0618.
           DISPLAY AUTOR-COD HIGHLIGHT AT 0618.
           DISPLAY "                                " AT 0803.
           READ FIC-AUTORES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "S") THEN
               DISPLAY "AUTOR JA EXISTE!"
               FOREGROUND-COLOR 4 HIGHLIGHT AT 0618
           ELSE
               IF AUTOR-COD = SPACES THEN
                   DISPLAY "00000" AT 0620 HIGHLIGHT
               ELSE
                   DISPLAY "PREENCHA O FORMULARIO: " AT 0403 HIGHLIGHT
                   DISPLAY "AUTOR:" AT 0803 HIGHLIGHT
                   PERFORM WITH TEST AFTER UNTIL
                   AUTOR > SPACES
                       ACCEPT AUTOR HIGHLIGHT AT 0810
                       IF AUTOR = SPACES THEN
                           DISPLAY "AUTOR INCORRETO!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 0820
                       ELSE
                           DISPLAY "                " AT 0820
                       END-IF
                   END-PERFORM

                   WRITE REGISTO-AUTORES
                       INVALID KEY
                           DISPLAY "ERRO AO CRIAR AUTOR!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1003
                       NOT INVALID KEY
                           DISPLAY "AUTOR CRIADO COM SUCESSO!"
                           FOREGROUND-COLOR 2 HIGHLIGHT AT 1003
                   END-WRITE
               END-IF
           END-IF.

           DISPLAY "ENTER - CONTINUAR"
           HIGHLIGHT AT 1803.
           ACCEPT OMITTED AT 1821.
           MOVE "N" TO REPETIR-MENU.

       AUTORES-ALTERAR.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "ALTERAR AUTORES" FOREGROUND-COLOR 1 AT 0217.
           DISPLAY "INSIRA CODIGO DO AUTOR:" AT 0403 HIGHLIGHT.
           DISPLAY "CODIGO AUTOR:" AT 0603 HIGHLIGHT.
           DISPLAY "DEIXAR EM BRANCO PARA RETROCEDER."
           AT 0803 HIGHLIGHT.
           ACCEPT AUTOR-COD AUTO HIGHLIGHT AT 0618.
           DISPLAY AUTOR-COD HIGHLIGHT AT 0618.
           DISPLAY "                                " AT 0803.
           READ FIC-AUTORES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF AUTOR-COD = SPACES THEN
                   DISPLAY " " AT 0618 HIGHLIGHT
               ELSE
                   DISPLAY "AUTOR NAO EXISTE!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0618
               END-IF
           ELSE
               DISPLAY "INSIRA CODIGO DO AUTOR:" AT 0403 HIGHLIGHT
               DISPLAY "AUTOR:" AT 0803 HIGHLIGHT
               PERFORM WITH TEST AFTER UNTIL
               AUTOR > SPACES
                   ACCEPT AUTOR HIGHLIGHT AT 0810
                   IF AUTOR = SPACES THEN
                       DISPLAY "AUTOR INCORRETO! "
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 0820
                   ELSE
                       DISPLAY "                " AT 0820
                   END-IF
               END-PERFORM
               REWRITE REGISTO-AUTORES
                   INVALID KEY
                       DISPLAY "ERRO AO ALTERAR AUTOR!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1003
                   NOT INVALID KEY
                       DISPLAY "AUTOR ALTERADO COM SUCESSO!"
                       FOREGROUND-COLOR 2 HIGHLIGHT AT 1003
               END-REWRITE
           END-IF.
           DISPLAY "ENTER - CONTINUAR"
           HIGHLIGHT AT 1803.
           ACCEPT OMITTED AT 1821.
           MOVE "N" TO REPETIR-MENU.

       AUTORES-ELIMINAR.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "ELIMINAR AUTOR" FOREGROUND-COLOR 1 AT 0217.
           DISPLAY "INSIRA CODIGO DO AUTOR:" AT 0403 HIGHLIGHT.
           DISPLAY "CODIGO AUTOR:" AT 0603 HIGHLIGHT.
           DISPLAY "DEIXAR EM BRANCO PARA RETROCEDER."
           AT 0803 HIGHLIGHT.
           ACCEPT AUTOR-COD AUTO HIGHLIGHT AT 0618.
           DISPLAY AUTOR-COD HIGHLIGHT AT 0618.
           DISPLAY "                                " AT 0803.
           READ FIC-AUTORES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.
           IF (EXISTE = "N") THEN
               IF AUTOR-COD = SPACES THEN
                   DISPLAY " " AT 0618 HIGHLIGHT
               ELSE
                   DISPLAY "AUTOR NAO EXISTE!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0618
               END-IF
           ELSE
               DELETE FIC-AUTORES
               INVALID KEY
                   DISPLAY "ERRO AO ELIMINAR AUTOR!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0803
               NOT INVALID KEY
                   DISPLAY "AUTOR ELIMINADO COM SUCESSO!"
                   FOREGROUND-COLOR 2 HIGHLIGHT AT 0803
               END-DELETE
           END-IF.
           DISPLAY "ENTER - CONTINUAR"
           HIGHLIGHT AT 1803.
           ACCEPT OMITTED AT 1821.
           MOVE "N" TO REPETIR-MENU.

       AUTORES-LISTAGEM.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "LISTAGEM DE AUTORES" FOREGROUND-COLOR 1 AT 0217.

           DISPLAY "*************************************************" &
           "**********************************************************"
           AT 0301.

           DISPLAY "COD. AUTOR" FOREGROUND-COLOR 1 AT 0403.
           DISPLAY "AUTOR" FOREGROUND-COLOR 1 AT 0414.
           DISPLAY "PAG." FOREGROUND-COLOR 1 AT 2002.
           DISPLAY "|" FOREGROUND-COLOR 1 AT 2009.

           DISPLAY "*************************************************" &
           "**********************************************************"
           AT 0501.
           DISPLAY "*************************************************" &
           "**********************************************************"
           AT 1901.

           MOVE 1 TO PAGINA.
           MOVE 0 TO AUTOR-COD.
           START FIC-AUTORES KEY > AUTOR-COD
               INVALID KEY
                   DISPLAY "FICHEIRO VAZIO. ENTER - CONTINUAR."
                   HIGHLIGHT AT 2011
                   DISPLAY "00" HIGHLIGHT AT 2006
                   ACCEPT OMITTED AT 2050
               NOT INVALID KEY
                   MOVE 6 TO LINHA
                   IF FS <> "05" AND FS <> "23" THEN
                       PERFORM UNTIL FS = "10"
                           READ FIC-AUTORES NEXT RECORD
                               NOT AT END
                                   DISPLAY PAGINA
                                   HIGHLIGHT AT 2006
                                   DISPLAY AUTOR-COD
                                   HIGHLIGHT LINE LINHA COL 3
                                   DISPLAY AUTOR
                                   HIGHLIGHT LINE LINHA COL 14
                                   ADD 1 TO LINHA
                                   IF LINHA = 19 THEN
                                       MOVE 6 TO LINHA
                                       DISPLAY "ENTER - PROXIMA PAGINA."
                                       HIGHLIGHT AT 2011
                                       ACCEPT OMITTED AT 2053
                                       ADD 1 TO PAGINA
                                   END-IF
                           END-READ
                       END-PERFORM
                   END-IF
                   DISPLAY "*****************************************" &
           "*********************************************************" &
           "*********"
           LINE LINHA COL 1
                   DISPLAY "ENTER - CONTINUAR.                    "
                   HIGHLIGHT AT 2011
                   ACCEPT OMITTED AT 2030
           END-START.
           MOVE "N" TO REPETIR-MENU.

       MENU-ALUGUERES.
           PERFORM UNTIL SAIR = "S"
               DISPLAY CLS
               PERFORM BASE
               DISPLAY "MENU ALUGUERES" FOREGROUND-COLOR 6 AT 0217
               DISPLAY "GESTAO DE ALUGUERES" FOREGROUND-COLOR 6 AT 0545
               DISPLAY "*******************" FOREGROUND-COLOR 6 AT 0645
               DISPLAY "1              NOVO" AT 0745
               DISPLAY "2         CONSULTAR" AT 0845
               DISPLAY "3           ALTERAR" AT 0945
               DISPLAY "4          ELIMINAR" AT 1045
               DISPLAY "5            LISTAR" AT 1145
               DISPLAY "6          DEVOLVER" AT 1245
               DISPLAY "0      MENU INICIAL" AT 1345
               DISPLAY "[ ] INSIRA UMA OPCAO" AT 1544
               MOVE "S" TO REPETIR-MENU
               PERFORM UNTIL REPETIR-MENU = "N"
                   ACCEPT ESCOLHA AT 1545 AUTO
                   EVALUATE ESCOLHA
                       WHEN 1 PERFORM ALUGUERES-NOVO
                       WHEN 2 PERFORM ALUGUERES-CONSULTAR
                       WHEN 3 PERFORM ALUGUERES-ALTERAR
                       WHEN 4 PERFORM ALUGUERES-ELIMINAR
                       WHEN 5 PERFORM ALUGUERES-LISTAGEM
                       WHEN 6 PERFORM ALUGUERES-DEVOLVER
                       WHEN 0
                           MOVE "S" TO SAIR
                           MOVE "N" TO REPETIR-MENU
                       WHEN OTHER
                           DISPLAY "OPCAO INCORRETA!" AT 1845
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           MOVE "S" TO REPETIR-MENU
                   END-EVALUATE
               END-PERFORM
           END-PERFORM.
           MOVE "N" TO SAIR.
           MOVE "N" TO REPETIR-MENU.

       ALUGUERES-NOVO.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "NOVO ALUGUER" FOREGROUND-COLOR 6 AT 0217.

           DISPLAY "PREENCHA O FORMULARIO:" AT 0403 HIGHLIGHT.

           DISPLAY "CODIGO ALUGUER:" AT 0603 HIGHLIGHT.

           DISPLAY "DEIXAR EM BRANCO PARA RETROCEDER."
           AT 0803 HIGHLIGHT.

           ACCEPT ALUGUER-COD AUTO HIGHLIGHT AT 0618.
           DISPLAY ALUGUER-COD HIGHLIGHT AT 0618.
           DISPLAY "                                " AT 0803.

           READ FIC-ALUGUERES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "S") THEN
               DISPLAY "ALUGUER JA EXISTE!"
               FOREGROUND-COLOR 4 HIGHLIGHT AT 0618
           ELSE
               IF ALUGUER-COD = SPACES THEN
                   DISPLAY " " AT 0618 HIGHLIGHT
               ELSE
                   DISPLAY "PREENCHA O FORMULARIO:" AT 0403 HIGHLIGHT
                   DISPLAY "CODIGO CLIENTE:" AT 0803 HIGHLIGHT
                   DISPLAY "CODIGO LIVRO:" AT 1003 HIGHLIGHT
                   DISPLAY "DATA LEVANTAMENTO:" AT 1203 HIGHLIGHT

                   PERFORM WITH TEST AFTER UNTIL
                   ALUGUER-CLIENTE-COD > SPACES
                       ACCEPT ALUGUER-CLIENTE-COD AUTO HIGHLIGHT AT 0820
                       IF ALUGUER-CLIENTE-COD = SPACES THEN
                           DISPLAY "OPCAO DE CLIENTE INCORRETA! "
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 0820
                       ELSE
                           DISPLAY "                  " AT 0820
                       END-IF
                   END-PERFORM
                   MOVE ALUGUER-CLIENTE-COD TO CLIENTE-COD
                   READ FIC-CLIENTES KEY CLIENTE-COD
                       INVALID KEY
                           MOVE "N" TO EXISTE
                       NOT INVALID KEY
                           MOVE "S" TO EXISTE
                   END-READ
                   IF (EXISTE = "N") THEN
                       DISPLAY "CLIENTE NAO EXISTE"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 0828
                   END-IF

                   PERFORM WITH TEST AFTER UNTIL
                   ALUGUER-LIVRO-COD > SPACES
                       ACCEPT ALUGUER-LIVRO-COD AUTO HIGHLIGHT AT 1020
                       IF ALUGUER-LIVRO-COD = SPACES THEN
                           DISPLAY "LIVRO INCORRETO! "
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1020
                       ELSE
                           DISPLAY "                " AT 1020
                       END-IF
                   END-PERFORM
                   MOVE ALUGUER-LIVRO-COD TO LIVRO-COD
                   READ FIC-LIVROS KEY LIVRO-COD
                       INVALID KEY
                           MOVE "N" TO EXISTE
                       NOT INVALID KEY
                           MOVE "S" TO EXISTE
                   END-READ
                   IF (EXISTE = "N") THEN
                       DISPLAY "LIVRO NAO EXISTE"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1020
                   END-IF
                   DISPLAY "/" AT 1224 HIGHLIGHT
                   DISPLAY "/" AT 1227 HIGHLIGHT
                   PERFORM WITH TEST AFTER UNTIL
                   (DIA-ALUGUER >= 0 AND DIA-ALUGUER <= DIAS-MES)
                       ACCEPT DIA-ALUGUER AUTO AT 1222 HIGHLIGHT
                       IF DIA-ALUGUER = 0 THEN
                          MOVE DIA-SIST TO DIA-ALUGUER
                       END-IF
                       IF NOT (DIA-ALUGUER >= 0 AND
                       DIA-ALUGUER <= DIAS-MES) THEN
                           DISPLAY "DIA INCORRETO!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1240
                       END-IF
                       IF ANO-ALUGUER = ANO-SIST THEN
                           IF MES-ALUGUER = MES-SIST THEN
                               IF DIA-ALUGUER > DIA-SIST THEN
                                   DISPLAY "DIA INCORRETO!"
                                   FOREGROUND-COLOR 4 HIGHLIGHT AT 1240
                                   MOVE 32 TO DIA-ALUGUER
                               END-IF
                           END-IF
                       END-IF
                   END-PERFORM

                   DISPLAY DIA-ALUGUER AT 1222 HIGHLIGHT
                   PERFORM WITH TEST AFTER UNTIL
                   (MES-ALUGUER >= 0 AND <= 12)
                       ACCEPT MES-ALUGUER AUTO AT 1225 HIGHLIGHT
                       IF NOT (MES-ALUGUER >= 0 AND <= 12) THEN
                           DISPLAY "MES INCORRETO!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1240
                       END-IF
                       IF ANO-ALUGUER = ANO-SIST THEN
                           IF MES-ALUGUER > MES-SIST THEN
                               DISPLAY "MES INCORRETO!"
                               FOREGROUND-COLOR 4 HIGHLIGHT AT 1240
                               MOVE 13 TO MES-ALUGUER
                           END-IF
                       END-IF
                   END-PERFORM

                   IF MES-ALUGUER = 0 THEN
                       MOVE MES-SIST TO MES-ALUGUER
                   END-IF
                   DISPLAY MES-ALUGUER AT 1225 HIGHLIGHT
                   DIVIDE ANO-ALUGUER BY 4 GIVING TEMP
                   REMAINDER ANO-BISSEXTO
                   EVALUATE MES-ALUGUER
                       WHEN 1
                       WHEN 3
                       WHEN 5
                       WHEN 7
                       WHEN 8
                       WHEN 10
                       WHEN 12
                           MOVE 31 TO DIAS-MES
                       WHEN 4
                       WHEN 6
                       WHEN 9
                       WHEN 11
                           MOVE 30 TO DIAS-MES
                       WHEN 2
                           IF ANO-BISSEXTO = 0 THEN
                               MOVE 29 TO DIAS-MES
                           ELSE
                               MOVE 28 TO DIAS-MES
                   END-EVALUATE
                   PERFORM WITH TEST AFTER UNTIL (ANO-ALUGUER = 0) OR
                   (ANO-ALUGUER >= 1968 AND ANO-ALUGUER <= ANO-SIST)
                       ACCEPT ANO-ALUGUER AUTO AT 1228 HIGHLIGHT
                       IF NOT (ANO-ALUGUER >= 1968 AND
                       ANO-ALUGUER <= ANO-SIST)
                           DISPLAY "ANO INCORRETO!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1240
                       END-IF
                   END-PERFORM
                   IF ANO-ALUGUER = 0 THEN
                       MOVE ANO-SIST TO ANO-ALUGUER
                   END-IF
                   DISPLAY ANO-ALUGUER AT 1228 HIGHLIGHT

                   MOVE 0 TO ANO-ENTREGA
                   MOVE 0 TO MES-ENTREGA
                   MOVE 0 TO DIA-ENTREGA
                   MOVE "A" TO ESTADO

                   WRITE REGISTO-ALUGUER
                       INVALID KEY
                           DISPLAY "ERRO AO CRIAR O ALUGUER!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1403
                       NOT INVALID KEY
                           DISPLAY "ALUGUER CRIADO COM SUCESSO!"
                           FOREGROUND-COLOR 2 HIGHLIGHT AT 1403
                   END-WRITE
               END-IF
           END-IF.

           DISPLAY "ENTER - CONTINUAR"
           HIGHLIGHT AT 1803.
           ACCEPT OMITTED AT 1821.
           MOVE "N" TO REPETIR-MENU.

       ALUGUERES-CONSULTAR.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "CONSULTAR ALUGUERES" HIGHLIGHT AT 0217.

           DISPLAY "INSIRA O CODIGO DE ALUGUER:" HIGHLIGHT AT 0403.

           DISPLAY "CODIGO ALUGUER:" HIGHLIGHT AT 0603.

           DISPLAY "DEIXAR EM BRANCO PARA RETROCEDER."
           AT 0803 HIGHLIGHT.

           ACCEPT ALUGUER-COD AUTO HIGHLIGHT AT 0620.
           DISPLAY ALUGUER-COD HIGHLIGHT AT 0620.
           DISPLAY "                                " AT 0803.

           READ FIC-ALUGUERES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF ALUGUER-COD = SPACES THEN
                   DISPLAY " " AT 0620 HIGHLIGHT
               ELSE
                   DISPLAY "ALUGUER NAO EXISTE!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0620
               END-IF
           ELSE
               DISPLAY "CODIGO CLIENTE:" AT 0803 HIGHLIGHT
               MOVE ALUGUER-CLIENTE-COD TO CLIENTE-COD
               READ FIC-CLIENTES KEY CLIENTE-COD
                   INVALID KEY
                       MOVE "N" TO EXISTE
                   NOT INVALID KEY
                       MOVE "S" TO EXISTE
               END-READ
               DISPLAY CLIENTE-COD HIGHLIGHT AT 0820
               IF (EXISTE = "N") THEN
                   DISPLAY "CLIENTE NAO EXISTE"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0820
               END-IF
               DISPLAY "  CODIGO LIVRO:"AT 1003
               MOVE ALUGUER-LIVRO-COD TO LIVRO-COD
               READ FIC-LIVROS KEY LIVRO-COD
                   INVALID KEY
                       MOVE "N" TO EXISTE
                   NOT INVALID KEY
                       MOVE "S" TO EXISTE
               END-READ
               DISPLAY LIVRO-COD HIGHLIGHT AT 1020
               IF (EXISTE = "N") THEN
                   DISPLAY "LIVRO NAO EXISTE"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 1020
               END-IF

               DISPLAY "  DATA ALUGUER:" AT 1403
               DISPLAY FUNCTION CONCATENATE(ANO-ALUGUER, "/"
               MES-ALUGUER, "/" DIA-ALUGUER) AT 1420 HIGHLIGHT
               DISPLAY "        ESTADO:" AT 1203 HIGHLIGHT
               IF (ESTADO = "A") THEN
                   DISPLAY "ALUGADO" FOREGROUND-COLOR 4
                   HIGHLIGHT AT 1220
               ELSE
                   DISPLAY "DEVOLVIDO" FOREGROUND-COLOR 2
                   HIGHLIGHT AT 1220
                   DISPLAY "DATA DEVOLUCAO:" AT 1603 HIGHLIGHT
                   DISPLAY FUNCTION CONCATENATE(DIA-ENTREGA, "/"
                   MES-ENTREGA, "/" ANO-ENTREGA ) AT 1620 HIGHLIGHT
               END-IF
           END-IF.

           DISPLAY "ENTER - CONTINUAR" AT 1803.
           ACCEPT OMITTED AT 1821.
           MOVE "N" TO REPETIR-MENU.

       ALUGUERES-ALTERAR.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "ALTERAR ALUGUER" FOREGROUND-COLOR 6 AT 0217.
           DISPLAY "INSIRA CODIGO DE ALUGUER:" AT 0403 HIGHLIGHT.
           DISPLAY "CODIGO ALUGUER:" AT 0603 HIGHLIGHT.
           DISPLAY "DEIXAR EM BRANCO PARA RETROCEDER."
           AT 0803 HIGHLIGHT.
           ACCEPT ALUGUER-COD AUTO HIGHLIGHT AT 0620.
           DISPLAY ALUGUER-COD HIGHLIGHT AT 0620.
           DISPLAY "                                " AT 0803.

           READ FIC-ALUGUERES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF ALUGUER-COD = SPACES THEN
                   DISPLAY " " AT 0620 HIGHLIGHT
               ELSE
                   DISPLAY "ALUGUER NAO EXISTE!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0620
               END-IF
           ELSE
               DISPLAY "PREENCHA O FORMULARIO:" AT 0403 HIGHLIGHT
               DISPLAY "   CODIGO CLIENTE:" AT 0803 HIGHLIGHT
               DISPLAY "     CODIGO LIVRO:" AT 1003 HIGHLIGHT
               DISPLAY "DATA LEVANTAMENTO:" AT 1203 HIGHLIGHT
               PERFORM WITH TEST AFTER UNTIL
               ALUGUER-CLIENTE-COD > SPACES
                   ACCEPT ALUGUER-CLIENTE-COD AUTO HIGHLIGHT AT 0823
                   IF ALUGUER-CLIENTE-COD = SPACES THEN
                       DISPLAY "CLIENTE INCORRETO!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 0823
                   ELSE
                       DISPLAY "                  " AT 0823
                   END-IF
               END-PERFORM

               MOVE ALUGUER-CLIENTE-COD TO CLIENTE-COD
               READ FIC-CLIENTES KEY CLIENTE-COD
                   INVALID KEY
                       MOVE "N" TO EXISTE
                   NOT INVALID KEY
                       MOVE "S" TO EXISTE
               END-READ
               IF (EXISTE = "N") THEN
                   DISPLAY "CLIENTE NAO EXISTE"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0823
               END-IF
               PERFORM WITH TEST AFTER UNTIL
               ALUGUER-LIVRO-COD > SPACES
                   ACCEPT ALUGUER-LIVRO-COD AUTO HIGHLIGHT AT 1023
                   IF ALUGUER-LIVRO-COD = SPACES THEN
                       DISPLAY "LIVRO INCORRETO! "
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1023
                   ELSE
                       DISPLAY "                " AT 1023
                   END-IF
               END-PERFORM


               MOVE ALUGUER-LIVRO-COD TO LIVRO-COD
               READ FIC-LIVROS KEY LIVRO-COD
                   INVALID KEY
                       MOVE "N" TO EXISTE
                   NOT INVALID KEY
                       MOVE "S" TO EXISTE
               END-READ
               IF (EXISTE = "N") THEN
                   DISPLAY "LIVRO NAO EXISTE"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 1023
               END-IF
               DISPLAY "/" AT 1225 HIGHLIGHT
               DISPLAY "/" AT 1228 HIGHLIGHT
               PERFORM WITH TEST AFTER UNTIL
               (DIA-ALUGUER >= 0 AND DIA-ALUGUER <= DIAS-MES)
                   ACCEPT DIA-ALUGUER AUTO AT 1223 HIGHLIGHT
                   IF DIA-ALUGUER = 0 THEN
                       MOVE DIA-SIST TO DIA-ALUGUER
                   END-IF
                   IF NOT (DIA-ALUGUER >= 0 AND
                   DIA-ALUGUER <= DIAS-MES) THEN
                       DISPLAY "DIA INCORRETO!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1240
                   END-IF
                   PERFORM WITH TEST AFTER UNTIL
               (MES-ALUGUER >= 0 AND <= 12)
                   ACCEPT MES-ALUGUER AUTO AT 1226 HIGHLIGHT
                   IF NOT (MES-ALUGUER >= 0 AND <= 12) THEN
                       DISPLAY "MES INCORRETO!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1240
                   END-IF
                   IF ANO-ALUGUER = ANO-SIST THEN
                       IF MES-ALUGUER > MES-SIST THEN
                           DISPLAY "MES INCORRETO!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1240
                           MOVE 13 TO MES-ALUGUER
                       END-IF
                   END-IF
               END-PERFORM

               IF MES-ALUGUER = 0 THEN
                   MOVE MES-SIST TO MES-ALUGUER
               END-IF
               DIVIDE ANO-ALUGUER BY 4 GIVING TEMP
               REMAINDER ANO-BISSEXTO
               EVALUATE MES-ALUGUER
                   WHEN 1
                   WHEN 3
                   WHEN 5
                   WHEN 7
                   WHEN 8
                   WHEN 10
                   WHEN 12
                       MOVE 31 TO DIAS-MES
                   WHEN 4
                   WHEN 6
                   WHEN 9
                   WHEN 11
                       MOVE 30 TO DIAS-MES
                   WHEN 2
                       IF ANO-BISSEXTO = 0 THEN
                           MOVE 29 TO DIAS-MES
                       ELSE
                           MOVE 28 TO DIAS-MES
               END-EVALUATE
                   IF ANO-ALUGUER = ANO-SIST THEN
                       IF MES-ALUGUER = MES-SIST THEN
                           IF DIA-ALUGUER > DIA-SIST THEN
                               DISPLAY "DIA INCORRETO!"
                               FOREGROUND-COLOR 4 HIGHLIGHT AT 1240
                               MOVE 32 TO DIA-ALUGUER
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
               PERFORM WITH TEST AFTER UNTIL (ANO-ALUGUER = 0) OR
               (ANO-ALUGUER >= 1968 AND ANO-ALUGUER <= ANO-SIST)
                   ACCEPT ANO-ALUGUER AUTO AT 1229 HIGHLIGHT
                   IF NOT (ANO-ALUGUER >= 1968 AND
                   ANO-ALUGUER <= ANO-SIST)
                       DISPLAY "ANO INCORRETO!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1240
                   END-IF
               END-PERFORM

               IF ANO-ALUGUER = 0 THEN
                   MOVE ANO-SIST TO ANO-ALUGUER
               END-IF
               DISPLAY DIA-ALUGUER AT 1223 HIGHLIGHT
               DISPLAY MES-ALUGUER AT 1226 HIGHLIGHT
               DISPLAY ANO-ALUGUER AT 1229 HIGHLIGHT


               IF ESTADO = "D" THEN
                   MOVE 0 TO DIA-ENTREGA
                   MOVE 0 TO MES-ENTREGA
                   MOVE 0 TO ANO-ENTREGA
               END-IF

               MOVE "A" TO ESTADO

               REWRITE REGISTO-ALUGUER
                   INVALID KEY
                       DISPLAY "ERRO AO ALTERAR ALUGUER!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1403
                   NOT INVALID KEY
                       DISPLAY "ALUGUER ALTERADO COM SUCESSO!"
                       FOREGROUND-COLOR 2 HIGHLIGHT AT 1403
               END-REWRITE
           END-IF.

           DISPLAY "ENTER - CONFIRMAR"
           HIGHLIGHT AT 1803.
           ACCEPT OMITTED AT 1821.
           MOVE "N" TO REPETIR-MENU.

       ALUGUERES-ELIMINAR.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "ELIMINAR ALUGUER" FOREGROUND-COLOR 6 AT 0217.
           DISPLAY "PREENCHA O FORMULARIO:" AT 0403 HIGHLIGHT.
           DISPLAY "CODIGO ALUGUER:" AT 0603 HIGHLIGHT.
           DISPLAY "DEIXAR EM BRANCO PARA RETROCEDER."
           AT 0803 HIGHLIGHT.
           ACCEPT ALUGUER-COD AUTO HIGHLIGHT AT 0620.
           DISPLAY ALUGUER-COD HIGHLIGHT AT 0620.
           DISPLAY "                                " AT 0803.

           READ FIC-ALUGUERES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF ALUGUER-COD = SPACES THEN
                   DISPLAY " " AT 0620 HIGHLIGHT
               ELSE
                   DISPLAY "ALUGUER NAO EXISTE!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0620
               END-IF
           ELSE
               DELETE FIC-ALUGUERES
                   INVALID KEY
                       DISPLAY "ERRO A ELIMINAR ALUGUER!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 0803
                   NOT INVALID KEY
                       DISPLAY "ALUGUER ELIMINADO COM SUCESSO!"
                       FOREGROUND-COLOR 2 HIGHLIGHT AT 0803
               END-DELETE
           END-IF.

           DISPLAY "ENTER - CONTINUAR"
           HIGHLIGHT AT 1803.
           ACCEPT OMITTED AT 1821.
           MOVE "N" TO REPETIR-MENU.

       ALUGUERES-LISTAGEM.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "LISTA DE ALUGUERES" FOREGROUND-COLOR 6 AT 0217.

           DISPLAY "*************************************************" &
           "**********************************************************"
           AT 0301.

           DISPLAY "COD AlUGUER" FOREGROUND-COLOR 6 AT 0403.
           DISPLAY "COD ClIENTE" FOREGROUND-COLOR 6 AT 0416.
           DISPLAY "COD LEVANT" FOREGROUND-COLOR 6 AT 0430.
           DISPLAY "DATA ALUGUER" FOREGROUND-COLOR 6 AT 0442.
           DISPLAY "DATA DEVOL" FOREGROUND-COLOR 6 AT 0457.
           DISPLAY "ESTADO" FOREGROUND-COLOR 6 AT 0470.
           DISPLAY "PAG." FOREGROUND-COLOR 6 AT 2002.
           DISPLAY "|" FOREGROUND-COLOR 6 AT 2009.

           DISPLAY "*************************************************" &
           "**********************************************************"
           AT 0501.
           DISPLAY "*************************************************" &
           "**********************************************************"
           AT 1901.

           MOVE 1 TO PAGINA.
           MOVE 0 TO ALUGUER-COD.
           START FIC-ALUGUERES KEY > ALUGUER-COD
               INVALID KEY
                   DISPLAY "FICHEIRO VAZIO. ENTER - CONTINUAR."
                   HIGHLIGHT AT 2011
                   DISPLAY "00" HIGHLIGHT AT 2006
                   ACCEPT OMITTED AT 2050
               NOT INVALID KEY
                   MOVE 6 TO LINHA
                   IF FS <> "05" AND FS <> "23" THEN
                       PERFORM UNTIL FS = "10"
                           READ FIC-ALUGUERES NEXT RECORD
                               NOT AT END
                                   DISPLAY PAGINA
                                   HIGHLIGHT AT 2006

                                   DISPLAY ALUGUER-COD
                                   HIGHLIGHT LINE LINHA COL 3

                                   MOVE ALUGUER-CLIENTE-COD
                                   TO CLIENTE-COD

                                   READ FIC-CLIENTES KEY CLIENTE-COD
                                       INVALID KEY
                                           MOVE "N" TO EXISTE
                                       NOT INVALID KEY
                                           MOVE "S" TO EXISTE
                                   END-READ

                                   IF (EXISTE = "N") THEN
                                       DISPLAY ALUGUER-CLIENTE-COD
                                       HIGHLIGHT LINE LINHA COL 16
                                       DISPLAY "INSIRA RESTANTES DADOS!"
                                       FOREGROUND-COLOR 4
                                       HIGHLIGHT LINE LINHA COL 80
                                   ELSE
                                       DISPLAY ALUGUER-CLIENTE-COD
                                       HIGHLIGHT LINE LINHA COL 16
                                   END-IF

                                   MOVE ALUGUER-LIVRO-COD
                                   TO LIVRO-COD

                                   READ FIC-LIVROS KEY LIVRO-COD
                                       INVALID KEY
                                           MOVE "N" TO EXISTE
                                       NOT INVALID KEY
                                           MOVE "S" TO EXISTE
                                   END-READ

                                   IF (EXISTE = "N") THEN
                                       DISPLAY ALUGUER-LIVRO-COD
                                       HIGHLIGHT LINE LINHA COL 30
                                       DISPLAY "INSIRA RESTANTES DADOS!"
                                       FOREGROUND-COLOR 4
                                       HIGHLIGHT LINE LINHA COL 80
                                   ELSE
                                       DISPLAY ALUGUER-LIVRO-COD
                                       HIGHLIGHT LINE LINHA COL 30
                                   END-IF

                                   DISPLAY FUNCTION CONCATENATE
                                   (DIA-ALUGUER, "/" MES-ALUGUER,
                                   "/" ANO-ALUGUER) HIGHLIGHT
                                   LINE LINHA COL 42

                                   IF ANO-ENTREGA = 0 THEN
                                       DISPLAY "**********" HIGHLIGHT
                                       LINE LINHA COL 57
                                   ELSE
                                       DISPLAY FUNCTION CONCATENATE
                                       (DIA-ENTREGA, "/" MES-ENTREGA,
                                       "/" ANO-ENTREGA) HIGHLIGHT
                                       LINE LINHA COL 57
                                   END-IF

                                   IF ESTADO = "A" THEN
                                       DISPLAY "ALUGADO" HIGHLIGHT
                                       LINE LINHA COL 70
                                   ELSE
                                       DISPLAY "DEVOLVIDO" HIGHLIGHT
                                       LINE LINHA COL 70
                                   END-IF

                                   ADD 1 TO LINHA
                                   IF LINHA = 19 THEN
                                       MOVE 6 TO LINHA
                                       DISPLAY "ENTER - PROXIMA PAGINA."
                                       HIGHLIGHT AT 2011
                                       ACCEPT OMITTED AT 2053
                                       ADD 1 TO PAGINA
                                   END-IF
                           END-READ
                       END-PERFORM
                   END-IF
                   DISPLAY "*****************************************" &
           "*********************************************************" &
           "*********"
           LINE LINHA COL 1
                   DISPLAY "ENTER - CONTINUAR"
                   HIGHLIGHT AT 2011
                   ACCEPT OMITTED AT 2030
           END-START.

           MOVE "N" TO REPETIR-MENU.

       ALUGUERES-DEVOLVER.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "DEVOLVER ALUGUER" FOREGROUND-COLOR 6 AT 0217.

           DISPLAY "INSIRA CODIGO DE ALUGUER:" AT 0403 HIGHLIGHT.

           DISPLAY "CODIGO ALUGUER:" AT 0603 HIGHLIGHT.

           DISPLAY "DEIXAR EM BRANCO PARA RETROCEDER."
           AT 0803 HIGHLIGHT.

           ACCEPT ALUGUER-COD AUTO HIGHLIGHT AT 0620.
           DISPLAY ALUGUER-COD HIGHLIGHT AT 0620.
           DISPLAY "                                " AT 0803.

           READ FIC-ALUGUERES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF ALUGUER-COD = SPACES THEN
                   DISPLAY " " AT 0620 HIGHLIGHT
               ELSE
                   DISPLAY "CODIGO NAO EXISTE!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0620
               END-IF
           ELSE
               IF (ESTADO = "A") THEN
                   MOVE "D" TO ESTADO
                   DISPLAY "DATA DEVOLUCAO:" HIGHLIGHT AT 0803
                   DISPLAY "/" AT 0822 HIGHLIGHT
                   DISPLAY "/" AT 0825 HIGHLIGHT
                   PERFORM WITH TEST AFTER UNTIL
                   (DIA-ENTREGA >= 0 AND DIA-ENTREGA <= DIAS-MES)
                       ACCEPT DIA-ENTREGA AUTO AT 0820 HIGHLIGHT
                       IF DIA-ENTREGA = 0 THEN
                           MOVE DIA-SIST TO DIA-ENTREGA
                       END-IF
                       IF NOT (DIA-ENTREGA >= 0 AND
                       DIA-ENTREGA <= DIAS-MES) THEN
                           DISPLAY "DIA INCORRETO!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 0840
                       END-IF
                       IF ANO-ENTREGA = ANO-SIST THEN
                           IF MES-ENTREGA = MES-SIST THEN
                               IF DIA-ENTREGA > DIA-SIST THEN
                                   DISPLAY "DIA INCORRETO!"
                                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0840
                                   MOVE 32 TO DIA-ENTREGA
                               END-IF
                           END-IF
                       END-IF
                       IF ANO-ENTREGA = ANO-ALUGUER THEN
                           IF MES-ENTREGA = MES-ALUGUER THEN
                               IF DIA-ENTREGA < DIA-ALUGUER THEN
                                   DISPLAY "DIA INCORRETO!"
                                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0840
                                   MOVE 32 TO DIA-ENTREGA
                               END-IF
                           END-IF
                       END-IF
                   END-PERFORM
                   PERFORM WITH TEST AFTER UNTIL
                   (MES-ENTREGA >= 0 AND <= 12)
                   AND (MES-ENTREGA >= MES-ALUGUER)
                       ACCEPT MES-ENTREGA AUTO AT 0823 HIGHLIGHT
                       IF MES-ENTREGA = 0 THEN
                           MOVE MES-SIST TO MES-ENTREGA
                       END-IF
                       IF NOT (MES-ENTREGA >= 0 AND <= 12) THEN
                           DISPLAY "MES INCORRETO!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 0840
                       END-IF
                       IF ANO-ENTREGA = ANO-SIST THEN
                           IF MES-ENTREGA > MES-SIST THEN
                               DISPLAY "MES INCORRETO!"
                               FOREGROUND-COLOR 4 HIGHLIGHT AT 0840
                               MOVE 13 TO MES-ENTREGA
                           END-IF
                       END-IF
                       IF NOT (MES-ENTREGA >= MES-ALUGUER) THEN
                           DISPLAY "MES INCORRETO!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 0840
                       END-IF
                   END-PERFORM
                   DIVIDE ANO-ENTREGA BY 4 GIVING TEMP
                   REMAINDER ANO-BISSEXTO
                   EVALUATE MES-ENTREGA
                       WHEN 1
                       WHEN 3
                       WHEN 5
                       WHEN 7
                       WHEN 8
                       WHEN 10
                       WHEN 12
                           MOVE 31 TO DIAS-MES
                       WHEN 4
                       WHEN 6
                       WHEN 9
                       WHEN 11
                           MOVE 30 TO DIAS-MES
                       WHEN 2
                           IF ANO-BISSEXTO = 0 THEN
                               MOVE 29 TO DIAS-MES
                           ELSE
                               MOVE 28 TO DIAS-MES
                   END-EVALUATE
                   PERFORM WITH TEST AFTER UNTIL (ANO-ENTREGA = 0) OR
                   (ANO-ENTREGA >= 1968 AND ANO-ENTREGA <= ANO-SIST
                   AND ANO-ENTREGA >= ANO-ALUGUER)
                       ACCEPT ANO-ENTREGA AUTO AT 0826 HIGHLIGHT
                       IF NOT (ANO-ENTREGA >= 1968 AND
                       ANO-ENTREGA <= ANO-SIST
                       AND ANO-ENTREGA >= ANO-ALUGUER)
                           DISPLAY "ANO INCORRETO!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 0840
                       END-IF
                   END-PERFORM

                   IF ANO-ENTREGA = 0 THEN
                       MOVE ANO-SIST TO ANO-ENTREGA
                   END-IF
                   DISPLAY DIA-ENTREGA AT 0820 HIGHLIGHT
                   DISPLAY MES-ENTREGA AT 0823 HIGHLIGHT
                   DISPLAY ANO-ENTREGA AT 0826 HIGHLIGHT


                   REWRITE REGISTO-ALUGUER
                       INVALID KEY
                           DISPLAY "ERRO NA DEVOLUCAO!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1003
                       NOT INVALID KEY
                           DISPLAY "LIVRO DEVOLVIDO COM SUCESSO!"
                           FOREGROUND-COLOR 2 HIGHLIGHT AT 1003
                   END-REWRITE
               ELSE
                   DISPLAY "LIVRO JA DEVOLVIDO!"
                   FOREGROUND-COLOR 2 HIGHLIGHT AT 0803
               END-IF
           END-IF.
           DISPLAY "ENTER - CONTINUAR"
           HIGHLIGHT AT 1803.
           ACCEPT OMITTED AT 1821.
           MOVE "N" TO REPETIR-MENU.

       MENU-EXPORTAR.
           PERFORM UNTIL SAIR = "S"
               DISPLAY CLS
               PERFORM BASE
               DISPLAY "MENU EXPORTAR" FOREGROUND-COLOR 6
               HIGHLIGHT AT 0217
               DISPLAY "     EXPORTAR:     "
               FOREGROUND-COLOR 6 HIGHLIGHT AT 0545
               DISPLAY "*******************"
               FOREGROUND-COLOR 6 HIGHLIGHT AT 0645
               DISPLAY "1              TUDO" HIGHLIGHT AT 0745
               DISPLAY "2         ALUGUERES" HIGHLIGHT AT 0845
               DISPLAY "0      MENU INICIAL" HIGHLIGHT AT 0945
               DISPLAY "[ ] INSIRA UMA OPCAO"HIGHLIGHT AT 1144
               MOVE "S" TO REPETIR-MENU
               PERFORM UNTIL REPETIR-MENU = "N"
                   ACCEPT ESCOLHA AT 1145 AUTO
                   EVALUATE ESCOLHA
                       WHEN 1 PERFORM EXPORTAR-TUDO
                       WHEN 2 PERFORM EXPORTAR-ALUGUERES
                       WHEN 0
                           MOVE "S" TO SAIR
                           MOVE "N" TO REPETIR-MENU
                       WHEN OTHER
                           DISPLAY "OPCAO INCORRETA!" AT 1445
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           MOVE "S" TO REPETIR-MENU
                   END-EVALUATE
               END-PERFORM
           END-PERFORM.
           MOVE "N" TO SAIR.
           MOVE "N" TO REPETIR-MENU.

       EXPORTAR-TUDO.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "EXPORTAR FICHEIRO" FOREGROUND-COLOR 6
           HIGHLIGHT AT 0217.
           OPEN OUTPUT IND-TODOS.
           MOVE 6 TO LINHA.
           MOVE 0 TO CLIENTE-COD.
           START FIC-CLIENTES KEY > CLIENTE-COD
               INVALID KEY
                   DISPLAY "FICHEIRO CLIENTES VAZIO"
                   HIGHLIGHT LINE LINHA COL 3
               NOT INVALID KEY
                   IF FS <> "05" AND FS <> "23" THEN
                       PERFORM UNTIL FS = "10"
                           READ FIC-CLIENTES NEXT RECORD
                               NOT AT END
                                   MOVE "C CLIENT:"    TO TODOS1
                                   MOVE CLIENTE-COD TO IND-TODOS-CLIENTE
                                   MOVE "     NIF:" TO TODOS2
                                   MOVE NIF TO IND-TODOS-NIF
                                   MOVE "    NOME:" TO TODOS3
                                   MOVE NOME TO IND-TODOS-NOME
                                   MOVE "    DATA:" TO TODOS4
                                   MOVE DIA-ADMIT
                                   TO IND-TODOS-DIA-ADMIT
                                   MOVE "/"         TO DATA1
                                   MOVE MES-ADMIT
                                   TO IND-TODOS-MES-ADMIT
                                   MOVE "/"         TO DATA2
                                   MOVE ANO-ADMIT
                                   TO IND-TODOS-ANO-ADMIT
                                   MOVE "  MAIL:"   TO TODOS5
                                   MOVE EMAIL TO IND-TODOS-EMAIL
                                   WRITE REGISTO-TODOS-CLIENTE
                           END-READ
                       END-PERFORM
                       DISPLAY "CLIENTES EXPORTADOS COM SUCESSO!"
                       FOREGROUND-COLOR 2 HIGHLIGHT LINE LINHA COL 3
                   ELSE
                       DISPLAY "ERRO AO EXPORTAR CLIENTE!" HIGHLIGHT
                       FOREGROUND-COLOR 4 LINE LINHA COL 3
                   END-IF
           END-START.
           ADD 1 TO LINHA
           MOVE 0 TO LIVRO-COD.
           START FIC-LIVROS KEY > LIVRO-COD
               INVALID KEY
                  DISPLAY "FICHEIRO DE LIVROS VAZIO!"
                   HIGHLIGHT LINE LINHA COL 3
               NOT INVALID KEY
                   IF FS <> "05" AND FS <> "23" THEN
                       PERFORM UNTIL FS = "10"
                           READ FIC-LIVROS NEXT RECORD
                               NOT AT END
                                   MOVE "C LEV:" TO TODOS6
                                   MOVE LIVRO-COD
                                   TO IND-TODOS-LIVRO
                                   MOVE "COD TEMA:" TO TODOS7
                                   MOVE LIVRO-TEMA-COD
                                   TO IND-TODOS-LIVRO-TEMA
                                   MOVE "C AUTOR:" TO TODOS8
                                   MOVE LIVRO-AUTOR-COD
                                   TO IND-TODOS-LIVRO-AUTOR
                                   MOVE "    TITULO:" TO TODOS9
                                   MOVE TITULO
                                   TO IND-TODOS-TITULO
                                   WRITE REGISTO-TODOS-LIVROS
                           END-READ
                       END-PERFORM
                       DISPLAY "LIVROS EXPORTADOS COM SUCESSO!"
                       HIGHLIGHT FOREGROUND-COLOR 2 LINE LINHA COL 3
                   ELSE
                       DISPLAY "ERRO A EXPORTAR LIVROS" HIGHLIGHT
                       FOREGROUND-COLOR 4 LINE LINHA COL 3
                   END-IF
           END-START.
           ADD 1 TO LINHA.
           MOVE 0 TO TEMA-COD.
           START FIC-TEMAS KEY > TEMA-COD
               INVALID KEY
                   DISPLAY "FICHEIRO DE TEMAS VAZIO!"
                   HIGHLIGHT LINE LINHA COL 3
               NOT INVALID KEY
                   IF FS <> "05" AND FS <> "23" THEN
                       PERFORM UNTIL FS = "10"
                           READ FIC-TEMAS NEXT RECORD
                               NOT AT END
                                   MOVE "COD TEMA:" TO TODOS10
                                   MOVE TEMA-COD TO IND-TODOS-TEMA-COD
                                   MOVE "    TEMA:" TO TODOS11
                                   MOVE TEMA TO IND-TODOS-TEMA
                                   WRITE REGISTO-TODOS-TEMA
                           END-READ
                       END-PERFORM
                       DISPLAY "TEMAS EXPORTADOS COM SUCESSO!"
                       FOREGROUND-COLOR 2 HIGHLIGHT LINE LINHA COL 3
                   ELSE
                       DISPLAY "ERRO AO EXPORTAR TEMAS!" HIGHLIGHT
                       FOREGROUND-COLOR 4 LINE LINHA COL 3
                   END-IF
           END-START.
           ADD 1 TO LINHA.
           MOVE 0 TO AUTOR-COD.
           START FIC-AUTORES KEY > AUTOR-COD
               INVALID KEY
                   DISPLAY "FICHEIRO DE AUTORES VAZIO!"
                   HIGHLIGHT LINE LINHA COL 3
               NOT INVALID KEY
                   IF FS <> "05" AND FS <> "23" THEN
                       PERFORM UNTIL FS = "10"
                           READ FIC-AUTORES NEXT RECORD
                               NOT AT END
                                   MOVE "C AUTOR:" TO TODOS12
                                   MOVE AUTOR-COD TO IND-TODOS-AUTOR-COD
                                   MOVE "  AUTOR:" TO TODOS13
                                   MOVE AUTOR TO IND-TODOS-AUTOR
                                   WRITE REGISTO-TODOS-AUTOR
                           END-READ
                       END-PERFORM
                       DISPLAY "AUTORES EXPORTADOS COM SUCESSO!"
                       FOREGROUND-COLOR 2 HIGHLIGHT LINE LINHA COL 3
                   ELSE
                       DISPLAY "ERRO A EXPORTAR AUTORES!" HIGHLIGHT
                       FOREGROUND-COLOR 4 LINE LINHA COL 3
                   END-IF
           END-START.
           ADD 1 TO LINHA.
           MOVE 0 TO ALUGUER-COD.
           START FIC-ALUGUERES KEY > ALUGUER-COD
               INVALID KEY
                   DISPLAY "FICHEIRO DE ALUGUERES VAZIO!"
                   FOREGROUND-COLOR 4 HIGHLIGHT LINE LINHA COL 3
               NOT INVALID KEY
                   IF FS <> "05" AND FS <> "23" THEN
                       PERFORM UNTIL FS = "10"
                           READ FIC-ALUGUERES NEXT RECORD
                               NOT AT END
                                   MOVE "COD AlUG:" TO TODOS14
                                   MOVE ALUGUER-COD
                                   TO IND-TODOS-ALUGUER
                                   MOVE "C CLIENT:" TO TODOS15
                                   MOVE ALUGUER-CLIENTE-COD
                                   TO IND-TODOS-ALUGUER-CLIENTE
                                   MOVE " COD LEV:" TO TODOS16
                                   MOVE ALUGUER-LIVRO-COD
                                   TO IND-TODOS-ALUGUER-LIVRO
                                   MOVE "DATA ALUG:" TO TODOS17
                                   MOVE DIA-ALUGUER
                                   TO IND-TODOS-DIA-ALUGUER
                                   MOVE "/" TO DATA3
                                   MOVE MES-ALUGUER
                                   TO IND-TODOS-MES-ALUGUER
                                   MOVE "/" TO DATA4
                                   MOVE ANO-ALUGUER
                                   TO IND-TODOS-ANO-ALUGUER
                                   MOVE "DATA DEVOL:" TO TODOS18
                                   MOVE DIA-ENTREGA
                                   TO IND-TODOS-DIA-ENTREGA
                                   MOVE "/" TO DATA5
                                   MOVE MES-ENTREGA
                                   TO IND-TODOS-MES-ENTREGA
                                   MOVE "/" TO DATA6
                                   MOVE ANO-ENTREGA
                                   TO IND-TODOS-ANO-ENTREGA
                                   MOVE "   ESTADO: " TO TODOS19
                                   MOVE ESTADO
                                   TO IND-TODOS-ESTADO
                                   WRITE REGISTO-TODOS-ALUGUER
                           END-READ
                       END-PERFORM
                       DISPLAY "ALUGUERES EXPORTADOS COM SUCESSO!"
                       FOREGROUND-COLOR 2 HIGHLIGHT LINE LINHA COL 3
                   ELSE
                       DISPLAY "ERRO AO EXPORTAR ALUGUERES!" HIGHLIGHT
                       FOREGROUND-COLOR 4 LINE LINHA COL 3
                   END-IF
           END-START.

           CLOSE IND-TODOS.
           DISPLAY "ENTER - CONTINUAR "
           HIGHLIGHT AT 1803.
           ACCEPT OMITTED AT 1821.
           MOVE "N" TO REPETIR-MENU.
       EXPORTAR-ALUGUERES.
           DISPLAY CLS.
           PERFORM BASE.
           DISPLAY "EXPORTAR ALUGUERES" HIGHLIGHT AT 0217.
           OPEN OUTPUT IND-ALUGADOS.
           MOVE 6 TO LINHA.
           MOVE 0 TO ALUGUER-COD.
           START FIC-ALUGUERES KEY > ALUGUER-COD
               INVALID KEY
                   DISPLAY "FICHEIRO DE ALUGUERES VAZIO!"
                   FOREGROUND-COLOR 4 HIGHLIGHT LINE LINHA COL 3
               NOT INVALID KEY
                   IF FS <> "05" AND FS <> "23" THEN
                       PERFORM UNTIL FS = "10"
                           READ FIC-ALUGUERES NEXT RECORD
                               NOT AT END
                                   MOVE "COD AlUG:" TO TODOS20
                                   MOVE ALUGUER-COD
                                   TO IND-ALUGADOS-ALUGUER
                                   MOVE "C ClIENT:" TO TODOS21
                                   MOVE ALUGUER-CLIENTE-COD
                                   TO IND-ALUGADOS-CLIENTE
                                   MOVE " COD LEV:" TO TODOS22
                                   MOVE ALUGUER-LIVRO-COD
                                   TO IND-ALUGADOS-LIVRO
                                   MOVE "DATA ALUG:" TO TODOS23
                                   MOVE DIA-ALUGUER
                                   TO IND-ALUGADOS-DIA-ALUGUER
                                   MOVE "/" TO DATA7
                                   MOVE MES-ALUGUER
                                   TO IND-ALUGADOS-MES-ALUGUER
                                   MOVE "/" TO DATA8
                                   MOVE ANO-ALUGUER
                                   TO IND-ALUGADOS-ANO-ALUGUER
                                   MOVE " DATA DEVOL:" TO TODOS24
                                   MOVE DIA-ENTREGA
                                   TO IND-ALUGADOS-DIA-ENTREGA
                                   MOVE "-" TO DATA9
                                   MOVE MES-ENTREGA
                                   TO IND-ALUGADOS-MES-ENTREGA
                                   MOVE "-" TO DATA10
                                   MOVE ANO-ENTREGA
                                   TO IND-ALUGADOS-ANO-ENTREGA
                                   MOVE "    ESTADO:" TO TODOS25
                                   MOVE ESTADO
                                   TO IND-ALUGADOS-ESTADO
                                   WRITE REGISTO-ALUGADOS
                           END-READ
                       END-PERFORM
                       DISPLAY "ALUGUERES EXPORTADOS COM SUCESSO!"
                       FOREGROUND-COLOR 2 HIGHLIGHT LINE LINHA COL 3
                   ELSE
                       DISPLAY "ERRO AO EXPORTAR ALUGUERES!" HIGHLIGHT
                       FOREGROUND-COLOR 4 LINE LINHA COL 3
                   END-IF
           END-START.
           CLOSE IND-ALUGADOS.
           DISPLAY "ENTER - CONTINUAR"
           HIGHLIGHT AT 1803.
           ACCEPT OMITTED AT 1821.
           MOVE "N" TO REPETIR-MENU.
       END PROGRAM TAREFA_FINAL.
