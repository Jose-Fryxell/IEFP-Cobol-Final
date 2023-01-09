      ******************************************************************
      * Author: JOSE SERRA
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAREFAFINAL.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL FIC-CLIENTES ASSIGN "CLIENTES.TXT"
           ORGANISATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CLIENTE-COD
           ALTERNATE RECORD KEY IS NIF
           FILE STATUS IS FS.

           SELECT OPTIONAL FIC-LIVROS ASSIGN "LIVROS.TXT"
           ORGANISATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS LIVRO-COD
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

           SELECT OPTIONAL FIC-ALUGUERES ASSIGN "ALUGUERES.TXT"
           ORGANISATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS ALUGUER-COD
           FILE STATUS IS FS.

           SELECT OPTIONAL IND-ALUGADOS ASSIGN "ALUGADOS.TXT"
           ORGANISATION IS LINE SEQUENTIAL.

           SELECT OPTIONAL IND-TODOS ASSIGN "TUDO.TXT"
           ORGANISATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
       FD FIC-CLIENTES.
       01 REGISTO-CLIENTES.
           05 CLIENTE-COD                     PIC 9(5) BLANK WHEN ZEROS.
           05 NIF                             PIC 9(9) BLANK WHEN ZEROS.
           05 NOME                            PIC X(30).
           05 DATA-ADMISSAO.
               10 ANO-ADMISSAO                PIC 9999.
               10 FILLER                      PIC X VALUE "-".
               10 MES-ADMISSAO                PIC 99.
               10 FILLER                      PIC X VALUE "-".
               10 DIA-ADMISSAO                PIC 99.
           05 EMAIL                           PIC X(30).
      ******************************************************************
       FD FIC-LIVROS.
       01 REGISTO-LIVROS.
           05 LIVRO-COD                       PIC 9(5) BLANK WHEN ZEROS.
           05 TITULO                          PIC X(30).
           05 LIVRO-TEMA-COD                  PIC 9(5) BLANK WHEN ZEROS.
           05 LIVRO-AUTOR-COD                 PIC 9(5) BLANK WHEN ZEROS.
      ******************************************************************
       FD FIC-TEMAS.
       01 REGISTO-TEMAS.
           05 TEMA-COD                        PIC 9(5) BLANK WHEN ZEROS.
           05 TEMA                            PIC X(30).
      ******************************************************************
       FD FIC-AUTORES.
       01 REGISTO-AUTORES.
           05 AUTOR-COD                       PIC 9(5) BLANK WHEN ZEROS.
           05 AUTOR                           PIC X(30).
      ******************************************************************
       FD FIC-ALUGUERES.
       01 REGISTO-ALUGUER.
           05 ALUGUER-COD                     PIC 9(5) BLANK WHEN ZEROS.
           05 DATA-ALUGUER.
               10 ANO-ALUGUER                 PIC 9999.
               10 FILLER                      PIC X    VALUE "-".
               10 MES-ALUGUER                 PIC 99.
               10 FILLER                      PIC X    VALUE "-".
               10 DIA-ALUGUER                 PIC 99.
           05 DATA-ENTREGA.
               10 ANO-ENTREGA                 PIC 9999.
               10 FILLER                      PIC X    VALUE "-".
               10 MES-ENTREGA                 PIC 99.
               10 FILLER                      PIC X    VALUE "-".
               10 DIA-ENTREGA                 PIC 99.
           05 ALUGUER-LIVRO-COD               PIC 9(5) BLANK WHEN ZEROS.
           05 ALUGUER-CLIENTE-COD             PIC 9(5) BLANK WHEN ZEROS.
           05 SITUACAO                        PIC X.
      ******************************************************************
       FD IND-TODOS.
       01 REGISTO-TODOS-CLIENTE.
           05 T1                              PIC X(6).
           05 IND-TODOS-CLIENTE               PIC 9(5).
           05 T2                              PIC X(8).
           05 IND-TODOS-NIF                   PIC 9(9).
           05 T3                              PIC X(9).
           05 IND-TODOS-NOME                  PIC X(30).
           05 T4                              PIC X(14).
           05 IND-TODOS-DATA-ADMISSAO.
               10 IND-TODOS-ANO-ADMISSAO      PIC 9999.
               10 D1                          PIC X.
               10 IND-TODOS-MES-ADMISSAO      PIC 99.
               10 D2                          PIC X.
               10 IND-TODOS-DIA-ADMISSAO      PIC 99.
           05 T5                              PIC X(10).
           05 IND-TODOS-EMAIL                 PIC X(30).
      ******
       01 REGISTO-TODOS-LIVROS.
           05 T6                              PIC X(6).
           05 IND-TODOS-LIVRO                 PIC 9(5).
           05 T7                              PIC X(9).
           05 IND-TODOS-LIVRO-TEMA            PIC 9(5).
           05 T8                              PIC X(9).
           05 IND-TODOS-LIVRO-AUTOR           PIC 9(5).
           05 T9                              PIC X(11).
           05 IND-TODOS-TITULO                PIC X(30).
      ******
       01 REGISTO-TODOS-TEMA.
           05 T10                             PIC X(6).
           05 IND-TODOS-TEMA-COD              PIC 9(5).
           05 T11                             PIC X(9).
           05 IND-TODOS-TEMA                  PIC X(30).
      ******
       01 REGISTO-TODOS-AUTOR.
           05 T12                             PIC X(6).
           05 IND-TODOS-AUTOR-COD             PIC 9(5).
           05 T13                             PIC X(10).
           05 IND-TODOS-AUTOR                 PIC X(30).
      ******
       01 REGISTO-TODOS-ALUGUER.
           05 T14                             PIC X(6).
           05 IND-TODOS-ALUGUER               PIC 9(5).
           05 T15                             PIC X(9).
           05 IND-TODOS-ALUGUER-CLIENTE       PIC 9(5).
           05 T16                             PIC X(9).
           05 IND-TODOS-ALUGUER-LIVRO         PIC 9(5).
           05 T17                             PIC X(14).
           05 IND-TODOS-DATA-ALUGUER.
               10 IND-TODOS-ANO-ALUGUER       PIC 9999.
               10 D3                          PIC X.
               10 IND-TODOS-MES-ALUGUER       PIC 99.
               10 D4                          PIC X.
               10 IND-TODOS-DIA-ALUGUER       PIC 99.
           05 T18                             PIC X(14).
           05 IND-TODOS-DATA-ENTREGA.
               10 IND-TODOS-ANO-ENTREGA       PIC 9999.
               10 D5                          PIC X.
               10 IND-TODOS-MES-ENTREGA       PIC 99.
               10 D6                          PIC X.
               10 IND-TODOS-DIA-ENTREGA       PIC 99.
           05 T19                             PIC X(13).
           05 IND-TODOS-SITUACAO              PIC X.
      ******************************************************************
       FD IND-ALUGADOS.
       01 REGISTO-ALUGADOS.
           05 T20                             PIC X(6).
           05 IND-ALUGADOS-ALUGUER            PIC 9(5).
           05 T21                             PIC X(9).
           05 IND-ALUGADOS-CLIENTE            PIC 9(5).
           05 T22                             PIC X(9).
           05 IND-ALUGADOS-LIVRO              PIC 9(5).
           05 T23                             PIC X(14).
           05 IND-DATA-ALUGUER.
               10 IND-ALUGADOS-ANO-ALUGUER    PIC 9999.
               10 D7                          PIC X.
               10 IND-ALUGADOS-MES-ALUGUER    PIC 99.
               10 D8                          PIC X.
               10 IND-ALUGADOS-DIA-ALUGUER    PIC 99.
           05 T24                             PIC X(14).
           05 IND-DATA-ENTREGA.
               10 IND-ALUGADOS-ANO-ENTREGA    PIC 9999.
               10 D9                          PIC X.
               10 IND-ALUGADOS-MES-ENTREGA    PIC 99.
               10 D10                         PIC X.
               10 IND-ALUGADOS-DIA-ENTREGA    PIC 99.
           05 T25                             PIC X(13).
           05 IND-ALUGADOS-SITUACAO           PIC X.
      ******************************************************************
       WORKING-STORAGE SECTION.
       77 FS                                  PIC XX.
      ******************************************************************
       01 DATA-SISTEMA.
           05 ANO-SISTEMA                     PIC 9999.
           05 MES-SISTEMA                     PIC 99.
           05 DIA-SISTEMA                     PIC 99.
       01 DATA-FORMATADA.
           05 DIA-FORMATADO                   PIC 99.
           05 FILLER                          PIC X   VALUE "-".
           05 MES-FORMATADO                   PIC 99.
           05 FILLER                          PIC X   VALUE "-".
           05 ANO-FORMATADO                   PIC 9999.
      ******************************************************************
       77 SAIR                                PIC X   VALUE "N".
       77 EXISTE                              PIC X   VALUE SPACES.
       77 CONTADOR                            PIC 99  VALUE 4.
       77 ESCOLHA                             PIC 9   VALUE 0.
       77 REPETIR-MENU                        PIC X   VALUE "S".
       77 ANO-BISSEXTO                        PIC 9   VALUE 0.
       77 TEMP                                PIC 999 VALUE 0.
       77 DIAS-MES                            PIC 99  VALUE 31.
       77 LINHA                               PIC 99  VALUE 4.
       77 PAGINA                              PIC 99  VALUE 1.
       77 NIF-TEMP                            PIC 9(9).
      ******************************************************************
       PROCEDURE DIVISION.
       INICIO.
      ****** OBTER DATA ************************************************
           ACCEPT DATA-SISTEMA FROM DATE YYYYMMDD.
           MOVE DIA-SISTEMA TO DIA-FORMATADO.
           MOVE MES-SISTEMA TO MES-FORMATADO.
           MOVE ANO-SISTEMA TO ANO-FORMATADO.
      ****** ABRIR FICHEIROS E INICIAR PROGRAMA ************************
           OPEN I-O FIC-CLIENTES.
           OPEN I-O FIC-LIVROS.
           OPEN I-O FIC-TEMAS.
           OPEN I-O FIC-AUTORES.
           OPEN I-O FIC-ALUGUERES.

           PERFORM MENU-INICIAL.
      ****** FECHAR FICHEIROS E FECHAR PROGRAMA ************************
           CLOSE FIC-CLIENTES.
           CLOSE FIC-LIVROS.
           CLOSE FIC-TEMAS.
           CLOSE FIC-AUTORES.
           CLOSE FIC-ALUGUERES.

           STOP RUN.
      ****** MENUS *****************************************************
       BASE.
           DISPLAY "o-------------o----------------------------------" &
           "--------------------------------------o------------------o"
           AT 0101 FOREGROUND-COLOR 3.
           DISPLAY "|             |                                  " &
           "                                      |                  |"
           AT 0201 FOREGROUND-COLOR 3.
           DISPLAY "o-------------o----------------------------------" &
           "--------------------------------------o------------------o"
           AT 0301 FOREGROUND-COLOR 3.

           PERFORM VARYING CONTADOR FROM 4 BY 1 UNTIL CONTADOR > 20
           DISPLAY "|                                                " &
           "                                                         |"
           LINE CONTADOR FOREGROUND-COLOR 3
           END-PERFORM.

           DISPLAY "o------------------------------------------------" &
           "---------------------------------------------------------o"
           AT 2101 FOREGROUND-COLOR 3.

           DISPLAY "VilaBiblio." FOREGROUND-COLOR 3 HIGHLIGHT AT 0203.
           DISPLAY "Data:" FOREGROUND-COLOR 3 HIGHLIGHT AT 0290.
           DISPLAY DATA-FORMATADA HIGHLIGHT AT 0296.
      ******************************************************************
       MENU-INICIAL.
           PERFORM UNTIL SAIR = "S"
               PERFORM BASE
               DISPLAY "Menu Inicial" HIGHLIGHT AT 0217
               DISPLAY "   Menu Inicial    "
               FOREGROUND-COLOR 3 HIGHLIGHT AT 0545
               DISPLAY "o-----------------------o"
               FOREGROUND-COLOR 3 AT 0642
               DISPLAY "Clientes ---------- 1" HIGHLIGHT AT 0744
               DISPLAY "Livros ------------ 2" HIGHLIGHT AT 0844
               DISPLAY "Temas ------------- 3" HIGHLIGHT AT 0944
               DISPLAY "Autores ----------- 4" HIGHLIGHT AT 1044
               DISPLAY "Alugueres --------- 5" HIGHLIGHT AT 1144
               DISPLAY "Exportar ---------- 6" HIGHLIGHT AT 1244
               DISPLAY "Sair -------------- 9" HIGHLIGHT AT 1444
               DISPLAY "[ ]"                   HIGHLIGHT AT 1653
               MOVE "S" TO REPETIR-MENU
               PERFORM UNTIL REPETIR-MENU = "N"
                   ACCEPT ESCOLHA AT 1654 AUTO
                   EVALUATE ESCOLHA
                       WHEN 1 PERFORM MENU-CLIENTES
                       WHEN 2 PERFORM MENU-LIVROS
                       WHEN 3 PERFORM MENU-TEMAS
                       WHEN 4 PERFORM MENU-AUTORES
                       WHEN 5 PERFORM MENU-ALUGUERES
                       WHEN 6 PERFORM MENU-EXPORTAR
                       WHEN 9
                           MOVE "S" TO SAIR
                           MOVE "N" TO REPETIR-MENU
                       WHEN OTHER
                           DISPLAY "Escolha invalida!" AT 1846
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           MOVE "S" TO REPETIR-MENU
                   END-EVALUATE
               END-PERFORM
           END-PERFORM.
      ******************************************************************
      ******************************************************************
      ******************************************************************
       MENU-CLIENTES.
           PERFORM UNTIL SAIR = "S"
               PERFORM BASE
               DISPLAY "Menu Clientes" HIGHLIGHT AT 0217
               DISPLAY "Gestao de Clientes "
               FOREGROUND-COLOR 3 HIGHLIGHT AT 0545
               DISPLAY "o-----------------------o"
               FOREGROUND-COLOR 3 AT 0642
               DISPLAY "Novo -------------- 1" HIGHLIGHT AT 0744
               DISPLAY "Consultar --------- 2" HIGHLIGHT AT 0844
               DISPLAY "Alterar ----------- 3" HIGHLIGHT AT 0944
               DISPLAY "Eliminar ---------- 4" HIGHLIGHT AT 1044
               DISPLAY "Listagem ---------- 5" HIGHLIGHT AT 1144
               DISPLAY "Menu Inicial ------ 9" HIGHLIGHT AT 1344
               DISPLAY "[ ]"                   HIGHLIGHT AT 1553
               MOVE "S" TO REPETIR-MENU
               PERFORM UNTIL REPETIR-MENU = "N"
                   ACCEPT ESCOLHA AT 1554 AUTO
                   EVALUATE ESCOLHA
                       WHEN 1 PERFORM CLIENTES-NOVO
                       WHEN 2 PERFORM CLIENTES-CONSULTAR
                       WHEN 3 PERFORM CLIENTES-ALTERAR
                       WHEN 4 PERFORM CLIENTES-ELIMINAR
                       WHEN 5 PERFORM CLIENTES-LISTAGEM
                       WHEN 9
                           MOVE "S" TO SAIR
                           MOVE "N" TO REPETIR-MENU
                       WHEN OTHER
                           DISPLAY "Escolha invalida!" AT 1746
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           MOVE "S" TO REPETIR-MENU
                   END-EVALUATE
               END-PERFORM
           END-PERFORM.
           MOVE "N" TO SAIR.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       CLIENTES-NOVO.
           PERFORM BASE.
           DISPLAY "Novo Cliente" HIGHLIGHT AT 0217.
           DISPLAY "Por favor preencha o seguinte campo:" AT 0403
           HIGHLIGHT.

           DISPLAY "Codigo de Cliente:"
           AT 0603 FOREGROUND-COLOR 3 HIGHLIGHT.

           DISPLAY "Deixe em branco para retroceder."
           AT 0803 HIGHLIGHT.

           ACCEPT CLIENTE-COD AUTO HIGHLIGHT AT 0622.
           DISPLAY CLIENTE-COD HIGHLIGHT AT 0622.
           DISPLAY "                                " AT 0803.

           READ FIC-CLIENTES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "S") THEN
               DISPLAY "Cliente ja existe!"
               FOREGROUND-COLOR 4 HIGHLIGHT AT 0628
           ELSE
               IF CLIENTE-COD = SPACES THEN
                   DISPLAY "00000" HIGHLIGHT AT 0622
               ELSE
                   DISPLAY "Por favor preencha os seguintes campos:"
                   AT 0403 HIGHLIGHT
                   DISPLAY "NIF:" AT 0803 FOREGROUND-COLOR 3 HIGHLIGHT
                   DISPLAY "Nome:" AT 1003 FOREGROUND-COLOR 3 HIGHLIGHT
                   DISPLAY "Data de Admissao:"
                   AT 1203 FOREGROUND-COLOR 3 HIGHLIGHT
                   DISPLAY "Email:" AT 1403 FOREGROUND-COLOR 3 HIGHLIGHT
                   DISPLAY CLIENTE-COD HIGHLIGHT AT 0622

                   PERFORM WITH TEST AFTER UNTIL
                   EXISTE = "N" AND NIF >= 100000000
                       ACCEPT NIF AUTO HIGHLIGHT AT 0808
                       READ FIC-CLIENTES KEY NIF
                           INVALID KEY
                               MOVE "N" TO EXISTE
                           NOT INVALID KEY
                               MOVE "S" TO EXISTE
                       END-READ
                       IF EXISTE = "S" THEN
                           DISPLAY "NIF ja existe!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 0818
                       END-IF
                       IF NIF NOT >= 100000000 THEN
                           DISPLAY "NIF invalido!  "
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 0818
                       END-IF
                   END-PERFORM
                   DISPLAY "               " AT 0818

                   PERFORM WITH TEST AFTER UNTIL
                   NOME > SPACES
                       ACCEPT NOME HIGHLIGHT AT 1009
                       IF NOME = SPACES THEN
                           DISPLAY "Nome invalido! "
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1040
                       ELSE
                           DISPLAY "               " AT 1040
                       END-IF
                   END-PERFORM

                   DISPLAY "(Ano)" AT 1226 FOREGROUND-COLOR 3 HIGHLIGHT
                   DISPLAY "Deixe Zero para inserir o Ano atual."
                   HIGHLIGHT AT 1232

                   PERFORM WITH TEST AFTER UNTIL (ANO-ADMISSAO = 0) OR
                  (ANO-ADMISSAO >= 1950 AND ANO-ADMISSAO <= ANO-SISTEMA)
                       ACCEPT ANO-ADMISSAO AUTO AT 1221 HIGHLIGHT
                       IF NOT (ANO-ADMISSAO >= 1950 AND
                       ANO-ADMISSAO <= ANO-SISTEMA)
                           DISPLAY "Ano invalido!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1269
                       END-IF
                   END-PERFORM

                   IF ANO-ADMISSAO = 0 THEN
                       MOVE ANO-SISTEMA TO ANO-ADMISSAO
                   END-IF

               DISPLAY "                                               "
                   & "            " AT 1226
                   DISPLAY ANO-ADMISSAO AT 1221 HIGHLIGHT

                   DISPLAY "-" AT 1225 HIGHLIGHT

                   DISPLAY "(Mes)" AT 1229 FOREGROUND-COLOR 3 HIGHLIGHT
                   DISPLAY "Deixe Zero para inserir o Mes atual."
                   HIGHLIGHT AT 1235

                   PERFORM WITH TEST AFTER UNTIL
                   (MES-ADMISSAO >= 0 AND <= 12)
                       ACCEPT MES-ADMISSAO AUTO AT 1226 HIGHLIGHT
                       IF NOT (MES-ADMISSAO >= 0 AND <= 12) THEN
                           DISPLAY "Mes invalido!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1272
                       END-IF
                       IF ANO-ADMISSAO = ANO-SISTEMA THEN
                           IF MES-ADMISSAO > MES-SISTEMA THEN
                               DISPLAY "Mes invalido!"
                               FOREGROUND-COLOR 4 HIGHLIGHT AT 1272
                               MOVE 13 TO MES-ADMISSAO
                           END-IF
                       END-IF
                   END-PERFORM

                   IF MES-ADMISSAO = 0 THEN
                       MOVE MES-SISTEMA TO MES-ADMISSAO
                   END-IF
                  DISPLAY "                                            "
                   & "            " AT 1229
                   DISPLAY MES-ADMISSAO AT 1226 HIGHLIGHT

                   DIVIDE ANO-ADMISSAO BY 4 GIVING TEMP
                   REMAINDER ANO-BISSEXTO
                   EVALUATE MES-ADMISSAO
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

                   DISPLAY "-" AT 1228 HIGHLIGHT

                   DISPLAY "(Dia)" AT 1232 FOREGROUND-COLOR 3 HIGHLIGHT
                   DISPLAY "Deixe Zero para inserir o Dia atual."
                   HIGHLIGHT AT 1238

                   PERFORM WITH TEST AFTER UNTIL
                   (DIA-ADMISSAO >= 0 AND DIA-ADMISSAO <= DIAS-MES)
                       ACCEPT DIA-ADMISSAO AUTO AT 1229 HIGHLIGHT
                       IF DIA-ADMISSAO = 0 THEN
                          MOVE DIA-SISTEMA TO DIA-ADMISSAO
                       END-IF
                       IF NOT (DIA-ADMISSAO >= 0 AND
                       DIA-ADMISSAO <= DIAS-MES) THEN
                           DISPLAY "Dia invalido!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1275
                       END-IF
                       IF ANO-ADMISSAO = ANO-SISTEMA THEN
                           IF MES-ADMISSAO = MES-SISTEMA THEN
                               IF DIA-ADMISSAO > DIA-SISTEMA THEN
                                   DISPLAY "Dia invalido!"
                                   FOREGROUND-COLOR 4 HIGHLIGHT AT 1275
                                   MOVE 32 TO DIA-ADMISSAO
                               END-IF
                           END-IF
                       END-IF
                   END-PERFORM

                  DISPLAY "                                            "
                   & "            " AT 1232
                   DISPLAY DIA-ADMISSAO AT 1229 HIGHLIGHT

                   ACCEPT EMAIL AT 1410 HIGHLIGHT

                   WRITE REGISTO-CLIENTES
                       INVALID KEY
                           DISPLAY "Erro ao criar cliente!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1603
                       NOT INVALID KEY
                           DISPLAY "Cliente criado com sucesso!"
                           HIGHLIGHT AT 1603
                   END-WRITE
               END-IF
           END-IF.

           DISPLAY "Prima ENTER para continuar.            "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       CLIENTES-CONSULTAR.
           PERFORM BASE.
           DISPLAY "Consultar Cliente" HIGHLIGHT AT 0217.
           DISPLAY "Por favor preencha o seguinte campo:" AT 0403
           HIGHLIGHT.
           DISPLAY "Codigo de Cliente:"
           AT 0603 FOREGROUND-COLOR 3 HIGHLIGHT.

           DISPLAY "Deixe em branco para retroceder."
           AT 0803 HIGHLIGHT.

           ACCEPT CLIENTE-COD AUTO HIGHLIGHT AT 0622.
           DISPLAY CLIENTE-COD HIGHLIGHT AT 0622.
           DISPLAY "                                " AT 0803.

           READ FIC-CLIENTES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF CLIENTE-COD = SPACES THEN
                   DISPLAY "00000" AT 0622 HIGHLIGHT
               ELSE
                   DISPLAY "Cliente nao existe!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0628
               END-IF
           ELSE
               DISPLAY "NIF:" AT 0803 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY "Nome:" AT 1003 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY "Data de Admissao:"
               AT 1203 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY "Email:" AT 1403 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY CLIENTE-COD HIGHLIGHT AT 0622
               DISPLAY NIF HIGHLIGHT AT 0808
               DISPLAY NOME HIGHLIGHT AT 1009
               DISPLAY FUNCTION CONCATENATE(ANO-ADMISSAO, "-"
               MES-ADMISSAO, "-" DIA-ADMISSAO) AT 1221 HIGHLIGHT
               DISPLAY EMAIL AT 1410 HIGHLIGHT
           END-IF.

           DISPLAY "Prima ENTER para continuar.         "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       CLIENTES-ALTERAR.
           PERFORM BASE.
           DISPLAY "Alterar Cliente" HIGHLIGHT AT 0217.
           DISPLAY "Por favor preencha o seguinte campo:" AT 0403
           HIGHLIGHT.

           DISPLAY "Codigo de Cliente:"
           AT 0603 FOREGROUND-COLOR 3 HIGHLIGHT.

           DISPLAY "Deixe em branco para retroceder."
           AT 0803 HIGHLIGHT.

           ACCEPT CLIENTE-COD AUTO HIGHLIGHT AT 0622.
           DISPLAY CLIENTE-COD HIGHLIGHT AT 0622.
           DISPLAY "                                " AT 0803.

           READ FIC-CLIENTES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF CLIENTE-COD = SPACES THEN
                   DISPLAY "00000" HIGHLIGHT AT 0622
               ELSE
                   DISPLAY "Cliente nao existe!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0628
               END-IF
           ELSE
               DISPLAY "Por favor preencha os seguintes campos:"
               AT 0403 HIGHLIGHT
               DISPLAY "NIF:" AT 0803 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY "Nome:" AT 1003 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY "Data de Admissao:"
               AT 1203 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY "Email:" AT 1403 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY CLIENTE-COD HIGHLIGHT AT 0622

               PERFORM WITH TEST AFTER UNTIL
               EXISTE = "N" AND NIF >= 100000000
                   ACCEPT NIF AUTO HIGHLIGHT AT 0808
                   READ FIC-CLIENTES KEY NIF
                       INVALID KEY
                           MOVE "N" TO EXISTE
                       NOT INVALID KEY
                           MOVE "S" TO EXISTE
                   END-READ
                   IF EXISTE = "S" THEN
                       DISPLAY "NIF ja existe!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 0818
                   END-IF
                   IF NIF NOT >= 100000000 THEN
                       DISPLAY "NIF invalido!  "
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 0818
                   END-IF
               END-PERFORM
               DISPLAY "               " AT 0818

               PERFORM WITH TEST AFTER UNTIL
               NOME > SPACES
                   ACCEPT NOME HIGHLIGHT AT 1009
                   IF NOME = SPACES THEN
                       DISPLAY "Nome invalido! "
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1040
                   ELSE
                       DISPLAY "               " AT 1040
                   END-IF
               END-PERFORM

               DISPLAY "(Ano)" AT 1226 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY "Deixe Zero para inserir o Ano atual."
               HIGHLIGHT AT 1232

               PERFORM WITH TEST AFTER UNTIL (ANO-ADMISSAO = 0) OR
               (ANO-ADMISSAO >= 1950 AND ANO-ADMISSAO <= ANO-SISTEMA)
                   ACCEPT ANO-ADMISSAO AUTO AT 1221 HIGHLIGHT
                   IF NOT (ANO-ADMISSAO >= 1950 AND
                   ANO-ADMISSAO <= ANO-SISTEMA)
                       DISPLAY "Ano invalido!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1269
                   END-IF
               END-PERFORM

               IF ANO-ADMISSAO = 0 THEN
                   MOVE ANO-SISTEMA TO ANO-ADMISSAO
               END-IF

           DISPLAY "                                               "
               & "            " AT 1226
               DISPLAY ANO-ADMISSAO AT 1221 HIGHLIGHT

               DISPLAY "-" AT 1225 HIGHLIGHT

               DISPLAY "(Mes)" AT 1229 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY "Deixe Zero para inserir o Mes atual."
               HIGHLIGHT AT 1235

               PERFORM WITH TEST AFTER UNTIL
               (MES-ADMISSAO >= 0 AND <= 12)
                   ACCEPT MES-ADMISSAO AUTO AT 1226 HIGHLIGHT
                   IF NOT (MES-ADMISSAO >= 0 AND <= 12) THEN
                       DISPLAY "Mes invalido!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1272
                   END-IF
                   IF ANO-ADMISSAO = ANO-SISTEMA THEN
                       IF MES-ADMISSAO > MES-SISTEMA THEN
                           DISPLAY "Mes invalido!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1272
                           MOVE 13 TO MES-ADMISSAO
                       END-IF
                   END-IF
               END-PERFORM

               IF MES-ADMISSAO = 0 THEN
                   MOVE MES-SISTEMA TO MES-ADMISSAO
               END-IF
               DISPLAY "                                            "
               & "            " AT 1229
               DISPLAY MES-ADMISSAO AT 1226 HIGHLIGHT

               DIVIDE ANO-ADMISSAO BY 4 GIVING TEMP
               REMAINDER ANO-BISSEXTO
               EVALUATE MES-ADMISSAO
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

               DISPLAY "-" AT 1228 HIGHLIGHT

               DISPLAY "(Dia)" AT 1232 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY "Deixe Zero para inserir o Dia atual."
               HIGHLIGHT AT 1238

               PERFORM WITH TEST AFTER UNTIL
               (DIA-ADMISSAO >= 0 AND DIA-ADMISSAO <= DIAS-MES)
                   ACCEPT DIA-ADMISSAO AUTO AT 1229 HIGHLIGHT
                   IF DIA-ADMISSAO = 0 THEN
                       MOVE DIA-SISTEMA TO DIA-ADMISSAO
                   END-IF
                   IF NOT (DIA-ADMISSAO >= 0 AND
                   DIA-ADMISSAO <= DIAS-MES) THEN
                       DISPLAY "Dia invalido!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1275
                   END-IF
                   IF ANO-ADMISSAO = ANO-SISTEMA THEN
                       IF MES-ADMISSAO = MES-SISTEMA THEN
                           IF DIA-ADMISSAO > DIA-SISTEMA THEN
                               DISPLAY "Dia invalido!"
                               FOREGROUND-COLOR 4 HIGHLIGHT AT 1275
                               MOVE 32 TO DIA-ADMISSAO
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM

               DISPLAY "                                            "
               & "            " AT 1232
               DISPLAY DIA-ADMISSAO AT 1229 HIGHLIGHT

               ACCEPT EMAIL AT 1410 HIGHLIGHT

               REWRITE REGISTO-CLIENTES
                   INVALID KEY
                       DISPLAY "Erro ao alterar cliente!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1603
                       NOT INVALID KEY
                           DISPLAY "Cliente alterado com sucesso!"
                           HIGHLIGHT AT 1603
               END-REWRITE
           END-IF.

           DISPLAY "Prima ENTER para continuar.            "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       CLIENTES-ELIMINAR.
           PERFORM BASE.
           DISPLAY "Eliminar Cliente" HIGHLIGHT AT 0217.

           DISPLAY "Por favor preencha o seguinte campo:" AT 0403
           HIGHLIGHT.
           DISPLAY "Codigo de Cliente:"
           AT 0603 FOREGROUND-COLOR 3 HIGHLIGHT.

           DISPLAY "Deixe em branco para retroceder."
           AT 0803 HIGHLIGHT.

           ACCEPT CLIENTE-COD AUTO HIGHLIGHT AT 0622.
           DISPLAY CLIENTE-COD HIGHLIGHT AT 0622.
           DISPLAY "                                " AT 0803.

           READ FIC-CLIENTES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF CLIENTE-COD = SPACES THEN
                   DISPLAY "00000" AT 0622 HIGHLIGHT
               ELSE
                   DISPLAY "Cliente nao existe!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0628
               END-IF
           ELSE
               DELETE FIC-CLIENTES
               INVALID KEY
                   DISPLAY "Erro ao eliminar cliente! "
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0628
               NOT INVALID KEY
                   DISPLAY "Cliente eliminado com sucesso!"
                   HIGHLIGHT AT 0803
               END-DELETE
           END-IF.

           DISPLAY "Prima ENTER para continuar.         "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       CLIENTES-LISTAGEM.
           PERFORM BASE.
           DISPLAY "Listagem de Clientes" HIGHLIGHT AT 0217.

           DISPLAY "o-------o-----o-----o----------------------------" &
           "----o------------o--------------------o------------------o"
           AT 0301 FOREGROUND-COLOR 3.

           PERFORM VARYING CONTADOR FROM 4 BY 1 UNTIL CONTADOR > 18
           DISPLAY "|       |           |                            " &
           "    |            |                                       |"
           LINE CONTADOR FOREGROUND-COLOR 3
           END-PERFORM.

           DISPLAY "C.Cl." FOREGROUND-COLOR 3 HIGHLIGHT AT 0403.
           DISPLAY "NIF" FOREGROUND-COLOR 3 HIGHLIGHT AT 0411.
           DISPLAY "Nome" FOREGROUND-COLOR 3 HIGHLIGHT AT 0423.
           DISPLAY "Data Adms." FOREGROUND-COLOR 3 HIGHLIGHT AT 0456.
           DISPLAY "Email" FOREGROUND-COLOR 3 HIGHLIGHT AT 0469.
           DISPLAY "P." FOREGROUND-COLOR 3 HIGHLIGHT AT 2003.
           DISPLAY "|" FOREGROUND-COLOR 3 AT 2009.
           DISPLAY "o" FOREGROUND-COLOR 3 AT 2109.

           DISPLAY "o-------o-----------o----------------------------" &
           "----o------------o---------------------------------------o"
           AT 0501 FOREGROUND-COLOR 3.
           DISPLAY "o-------o-----------o----------------------------" &
           "----o------------o---------------------------------------o"
           AT 1901 FOREGROUND-COLOR 3.

           MOVE 1 TO PAGINA.
           MOVE 0 TO CLIENTE-COD.
           START FIC-CLIENTES KEY > CLIENTE-COD
               INVALID KEY
                   DISPLAY "Ficheiro vazio! Prima ENTER para continuar."
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
                                   HIGHLIGHT LINE LINHA COL 11
                                   DISPLAY NOME
                                   HIGHLIGHT LINE LINHA COL 23
                                   DISPLAY DIA-ADMISSAO
                                   HIGHLIGHT LINE LINHA COL 56
                                   DISPLAY "-" LINE LINHA COL 58
                                   DISPLAY MES-ADMISSAO
                                   HIGHLIGHT LINE LINHA COL 59
                                   DISPLAY "-" LINE LINHA COL 61
                                   DISPLAY ANO-ADMISSAO
                                   HIGHLIGHT LINE LINHA COL 62
                                   DISPLAY EMAIL
                                   HIGHLIGHT LINE LINHA COL 69
                                   ADD 1 TO LINHA
                                   IF LINHA = 19 THEN
                                       MOVE 6 TO LINHA
                                       DISPLAY
                            "Prima ENTER para mostrar a proxima pagina."
                                       HIGHLIGHT AT 2011
                                       ACCEPT OMITTED AT 2053
                                       ADD 1 TO PAGINA
      *********************************^
           PERFORM VARYING CONTADOR FROM 6 BY 1 UNTIL CONTADOR > 18
           DISPLAY "|       |           |                            " &
           "    |            |                                       |"
           LINE CONTADOR FOREGROUND-COLOR 3
           END-PERFORM
      ******************************************************************
                                   END-IF
                           END-READ
                       END-PERFORM
                   END-IF
                   DISPLAY "-----" HIGHLIGHT
                   FOREGROUND-COLOR 3 LINE LINHA COL 3
                   DISPLAY "---------" HIGHLIGHT
                   FOREGROUND-COLOR 3 LINE LINHA COL 11
                   DISPLAY "------------------------------" HIGHLIGHT
                   FOREGROUND-COLOR 3 LINE LINHA COL 23
                   DISPLAY "----------" HIGHLIGHT
                   FOREGROUND-COLOR 3 LINE LINHA COL 56
                   DISPLAY "------------------------ Fim da Lista"
                   HIGHLIGHT FOREGROUND-COLOR 3 LINE LINHA COL 69
                   DISPLAY "Prima ENTER para continuar.               "
                   HIGHLIGHT AT 2011
                   ACCEPT OMITTED AT 2038
           END-START.

           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
      ******************************************************************
      ******************************************************************
       MENU-LIVROS.
           PERFORM UNTIL SAIR = "S"
               PERFORM BASE
               DISPLAY "Menu Livros" HIGHLIGHT AT 0217
               DISPLAY " Gestao de Livros  "
               FOREGROUND-COLOR 3 HIGHLIGHT AT 0545
               DISPLAY "o-----------------------o"
               FOREGROUND-COLOR 3 AT 0642
               DISPLAY "Novo -------------- 1" HIGHLIGHT AT 0744
               DISPLAY "Consultar --------- 2" HIGHLIGHT AT 0844
               DISPLAY "Alterar ----------- 3" HIGHLIGHT AT 0944
               DISPLAY "Eliminar ---------- 4" HIGHLIGHT AT 1044
               DISPLAY "Listagem ---------- 5" HIGHLIGHT AT 1144
               DISPLAY "Menu Inicial ------ 9" HIGHLIGHT AT 1344
               DISPLAY "[ ]"                   HIGHLIGHT AT 1553
               MOVE "S" TO REPETIR-MENU
               PERFORM UNTIL REPETIR-MENU = "N"
                   ACCEPT ESCOLHA AT 1554 AUTO
                   EVALUATE ESCOLHA
                       WHEN 1 PERFORM LIVROS-NOVO
                       WHEN 2 PERFORM LIVROS-CONSULTAR
                       WHEN 3 PERFORM LIVROS-ALTERAR
                       WHEN 4 PERFORM LIVROS-ELIMINAR
                       WHEN 5 PERFORM LIVROS-LISTAGEM
                       WHEN 9
                           MOVE "S" TO SAIR
                           MOVE "N" TO REPETIR-MENU
                       WHEN OTHER
                           DISPLAY "Escolha invalida!" AT 1746
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           MOVE "S" TO REPETIR-MENU
                   END-EVALUATE
               END-PERFORM
           END-PERFORM.
           MOVE "N" TO SAIR.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       LIVROS-NOVO.
           PERFORM BASE.
           DISPLAY "Novo Livro" HIGHLIGHT AT 0217.

           DISPLAY "Por favor preencha o seguinte campo:" AT 0403
           HIGHLIGHT.

           DISPLAY "Codigo do Livro:"
           AT 0603 FOREGROUND-COLOR 3 HIGHLIGHT.

           DISPLAY "Deixe em branco para retroceder."
           AT 0803 HIGHLIGHT.

           ACCEPT LIVRO-COD AUTO HIGHLIGHT AT 0620.
           DISPLAY LIVRO-COD HIGHLIGHT AT 0620.
           DISPLAY "                                " AT 0803.

           READ FIC-LIVROS
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "S") THEN
               DISPLAY "Livro ja existe!"
               FOREGROUND-COLOR 4 HIGHLIGHT AT 0626
           ELSE
               IF LIVRO-COD = SPACES THEN
                   DISPLAY "00000" AT 0620 HIGHLIGHT
               ELSE
                   DISPLAY "Por favor preencha os seguintes campos:"
                   AT 0403 HIGHLIGHT
                   DISPLAY "Titulo:"
                   AT 0803 FOREGROUND-COLOR 3 HIGHLIGHT
                   DISPLAY "Codigo do Tema:"
                   AT 1003 FOREGROUND-COLOR 3 HIGHLIGHT
                   DISPLAY "Codigo do Autor:"
                   AT 1203 FOREGROUND-COLOR 3 HIGHLIGHT

                   PERFORM WITH TEST AFTER UNTIL
                   TITULO > SPACES
                       ACCEPT TITULO HIGHLIGHT AT 0811
                       IF TITULO = SPACES THEN
                           DISPLAY "Titulo invalido! "
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 0842
                       ELSE
                           DISPLAY "                 " AT 0842
                       END-IF
                   END-PERFORM

                   PERFORM WITH TEST AFTER UNTIL
                   LIVRO-TEMA-COD > 0
                       ACCEPT LIVRO-TEMA-COD AUTO HIGHLIGHT AT 1019
                       IF LIVRO-TEMA-COD = SPACES THEN
                           DISPLAY "Codigo invalido! "
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1025
                       ELSE
                           DISPLAY "                 " AT 1025
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
                       DISPLAY "Tema nao existe! Por favor" &
                       " atualize a base de dados!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1025
                   END-IF
                   DISPLAY LIVRO-TEMA-COD HIGHLIGHT AT 1019

                   PERFORM WITH TEST AFTER UNTIL
                   LIVRO-AUTOR-COD > 0
                       ACCEPT LIVRO-AUTOR-COD AUTO HIGHLIGHT AT 1220
                       IF LIVRO-AUTOR-COD = SPACES THEN
                           DISPLAY "Codigo invalido! "
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1226
                       ELSE
                           DISPLAY "                 " AT 1226
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
                       DISPLAY "Autor nao existe! Por favor" &
                       " atualize a base de dados!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1226
                   END-IF
                   DISPLAY LIVRO-AUTOR-COD HIGHLIGHT AT 1220

                   WRITE REGISTO-LIVROS
                       INVALID KEY
                           DISPLAY "Erro ao criar livro!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1403
                       NOT INVALID KEY
                           DISPLAY "Livro criado com sucesso!"
                           HIGHLIGHT AT 1403
                   END-WRITE
               END-IF
           END-IF.

           DISPLAY "Prima ENTER para continuar.            "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       LIVROS-CONSULTAR.
           PERFORM BASE.
           DISPLAY "Consultar Livro" HIGHLIGHT AT 0217.

           DISPLAY "Por favor preencha o seguinte campo:" AT 0403
           HIGHLIGHT.

           DISPLAY "Codigo do Livro:"
           AT 0603 FOREGROUND-COLOR 3 HIGHLIGHT.

           DISPLAY "Deixe em branco para retroceder."
           AT 0803 HIGHLIGHT.

           ACCEPT LIVRO-COD AUTO HIGHLIGHT AT 0620.
           DISPLAY LIVRO-COD HIGHLIGHT AT 0620.
           DISPLAY "                                " AT 0803.

           READ FIC-LIVROS
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF LIVRO-COD = SPACES THEN
                   DISPLAY "00000" AT 0620 HIGHLIGHT
               ELSE
                   DISPLAY "Livro nao existe!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0626
               END-IF
           ELSE
               DISPLAY "Titulo:"
               AT 0803 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY "Codigo do Tema:"
               AT 1003 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY "Codigo do Autor:"
               AT 1203 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY TITULO HIGHLIGHT AT 0811
               DISPLAY LIVRO-TEMA-COD HIGHLIGHT AT 1019
               DISPLAY LIVRO-AUTOR-COD HIGHLIGHT AT 1220
           END-IF.

           DISPLAY "Prima ENTER para continuar.         "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       LIVROS-ALTERAR.
           PERFORM BASE.
           DISPLAY "Alterar Livro" HIGHLIGHT AT 0217.

           DISPLAY "Por favor preencha o seguinte campo:" AT 0403
           HIGHLIGHT.

           DISPLAY "Codigo do Livro:"
           AT 0603 FOREGROUND-COLOR 3 HIGHLIGHT.

           DISPLAY "Deixe em branco para retroceder."
           AT 0803 HIGHLIGHT.

           ACCEPT LIVRO-COD AUTO HIGHLIGHT AT 0620.
           DISPLAY LIVRO-COD HIGHLIGHT AT 0620.
           DISPLAY "                                " AT 0803.

           READ FIC-LIVROS
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF LIVRO-COD = SPACES THEN
                   DISPLAY "00000" AT 0620 HIGHLIGHT
               ELSE
                   DISPLAY "Livro nao existe!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0626
               END-IF
           ELSE
               DISPLAY "Por favor preencha os seguintes campos:"
               AT 0403 HIGHLIGHT
               DISPLAY "Titulo:"
               AT 0803 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY "Codigo do Tema:"
               AT 1003 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY "Codigo do Autor:"
               AT 1203 FOREGROUND-COLOR 3 HIGHLIGHT

               PERFORM WITH TEST AFTER UNTIL
               TITULO > SPACES
                   ACCEPT TITULO HIGHLIGHT AT 0811
                   IF TITULO = SPACES THEN
                       DISPLAY "Titulo invalido! "
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 0842
                   ELSE
                       DISPLAY "                 " AT 0842
                   END-IF
               END-PERFORM

               PERFORM WITH TEST AFTER UNTIL
               LIVRO-TEMA-COD > 0
                   ACCEPT LIVRO-TEMA-COD AUTO HIGHLIGHT AT 1019
                   IF LIVRO-TEMA-COD = SPACES THEN
                       DISPLAY "Codigo invalido! "
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1025
                   ELSE
                       DISPLAY "                 " AT 1025
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
                   DISPLAY "Tema nao existe! Por favor" &
                   " atualize a base de dados!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 1025
               END-IF
               DISPLAY LIVRO-TEMA-COD HIGHLIGHT AT 1019

               PERFORM WITH TEST AFTER UNTIL
               LIVRO-AUTOR-COD > 0
                   ACCEPT LIVRO-AUTOR-COD AUTO HIGHLIGHT AT 1220
                   IF LIVRO-AUTOR-COD = SPACES THEN
                       DISPLAY "Codigo invalido! "
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1226
                   ELSE
                       DISPLAY "                 " AT 1226
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
                   DISPLAY "Autor nao existe! Por favor" &
                   " atualize a base de dados!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 1226
               END-IF
               DISPLAY LIVRO-AUTOR-COD HIGHLIGHT AT 1220

               REWRITE REGISTO-LIVROS
                   INVALID KEY
                       DISPLAY "Erro ao alterar livro!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1403
                   NOT INVALID KEY
                       DISPLAY "Livro alterado com sucesso!"
                       HIGHLIGHT AT 1403
               END-REWRITE
           END-IF.

           DISPLAY "Prima ENTER para continuar.            "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       LIVROS-ELIMINAR.
           PERFORM BASE.
           DISPLAY "Eliminar Livro" HIGHLIGHT AT 0217.

           DISPLAY "Por favor preencha o seguinte campo:" AT 0403
           HIGHLIGHT.

           DISPLAY "Codigo do Livro:"
           AT 0603 FOREGROUND-COLOR 3 HIGHLIGHT.

           DISPLAY "Deixe em branco para retroceder."
           AT 0803 HIGHLIGHT.

           ACCEPT LIVRO-COD AUTO HIGHLIGHT AT 0620.
           DISPLAY LIVRO-COD HIGHLIGHT AT 0620.
           DISPLAY "                                " AT 0803.

           READ FIC-LIVROS
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF LIVRO-COD = SPACES THEN
                   DISPLAY "00000" AT 0620 HIGHLIGHT
               ELSE
                   DISPLAY "Livro nao existe!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0626
               END-IF
           ELSE
               DELETE FIC-LIVROS
               INVALID KEY
                   DISPLAY "Erro ao eliminar livro! "
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0803
               NOT INVALID KEY
                   DISPLAY "Livro eliminado com sucesso!"
                   HIGHLIGHT AT 0803
               END-DELETE
           END-IF.

           DISPLAY "Prima ENTER para continuar.         "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       LIVROS-LISTAGEM.
           PERFORM BASE.
           DISPLAY "Listagem de Livros" HIGHLIGHT AT 0217.

           DISPLAY "o-------o-----o-o-------o------------------------" &
           "--------------------------------------o------------------o"
           AT 0301 FOREGROUND-COLOR 3.

           PERFORM VARYING CONTADOR FROM 4 BY 1 UNTIL CONTADOR > 18
           DISPLAY "|       |       |       |                        " &
           "                                                         |"
           LINE CONTADOR FOREGROUND-COLOR 3
           END-PERFORM.

           DISPLAY "C.Lv." FOREGROUND-COLOR 3 HIGHLIGHT AT 0403.
           DISPLAY "C.Tm." FOREGROUND-COLOR 3 HIGHLIGHT AT 0411.
           DISPLAY "C.At." FOREGROUND-COLOR 3 HIGHLIGHT AT 0419.
           DISPLAY "Titulo" FOREGROUND-COLOR 3 HIGHLIGHT AT 0427.
           DISPLAY "P." FOREGROUND-COLOR 3 HIGHLIGHT AT 2003.
           DISPLAY "|" FOREGROUND-COLOR 3 AT 2009.
           DISPLAY "o" FOREGROUND-COLOR 3 AT 2109.

           DISPLAY "o-------o-------o-------o------------------------" &
           "---------------------------------------------------------o"
           AT 0501 FOREGROUND-COLOR 3.
           DISPLAY "o-------o-------o-------o------------------------" &
           "---------------------------------------------------------o"
           AT 1901 FOREGROUND-COLOR 3.

           MOVE 1 TO PAGINA.
           MOVE 0 TO LIVRO-COD.
           START FIC-LIVROS KEY > LIVRO-COD
               INVALID KEY
                   DISPLAY "Ficheiro vazio! Prima ENTER para continuar."
                   HIGHLIGHT AT 2011
                   DISPLAY "00" HIGHLIGHT AT 2006
                   ACCEPT OMITTED AT 2054
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
                                       DISPLAY "Dados em falta!"
                                       FOREGROUND-COLOR 4
                                       HIGHLIGHT LINE LINHA COL 91
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
                                       HIGHLIGHT LINE LINHA COL 11
                                       DISPLAY "Dados em falta!"
                                       FOREGROUND-COLOR 4
                                       HIGHLIGHT LINE LINHA COL 91
                                   ELSE
                                       DISPLAY LIVRO-TEMA-COD
                                       HIGHLIGHT LINE LINHA COL 11
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
                                       HIGHLIGHT LINE LINHA COL 19
                                       DISPLAY "Dados em falta!"
                                       FOREGROUND-COLOR 4
                                       HIGHLIGHT LINE LINHA COL 91
                                   ELSE
                                       DISPLAY LIVRO-AUTOR-COD
                                       HIGHLIGHT LINE LINHA COL 19
                                   END-IF


                                   DISPLAY TITULO
                                   HIGHLIGHT LINE LINHA COL 27
                                   ADD 1 TO LINHA
                                   IF LINHA = 19 THEN
                                       MOVE 6 TO LINHA
                                       DISPLAY
                            "Prima ENTER para mostrar a proxima pagina."
                                       HIGHLIGHT AT 2011
                                       ACCEPT OMITTED AT 2053
                                       ADD 1 TO PAGINA
      *********************************^
           PERFORM VARYING CONTADOR FROM 6 BY 1 UNTIL CONTADOR > 18
           DISPLAY "|       |       |       |                        " &
           "                                                         |"
           LINE CONTADOR FOREGROUND-COLOR 3
           END-PERFORM
      ******************************************************************
                                   END-IF
                           END-READ
                       END-PERFORM
                   END-IF
                   DISPLAY "-----" HIGHLIGHT
                   FOREGROUND-COLOR 3 LINE LINHA COL 3
                   DISPLAY "-----" HIGHLIGHT
                   FOREGROUND-COLOR 3 LINE LINHA COL 11
                   DISPLAY "-----" HIGHLIGHT
                   FOREGROUND-COLOR 3 LINE LINHA COL 19
                   DISPLAY "-----------------------------------------" &
                   "------------------------- Fim da Lista" HIGHLIGHT
                   FOREGROUND-COLOR 3 LINE LINHA COL 27
                   DISPLAY "Prima ENTER para continuar.               "
                   HIGHLIGHT AT 2011
                   ACCEPT OMITTED AT 2038
           END-START.

           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
      ******************************************************************
      ******************************************************************
       MENU-TEMAS.
           PERFORM UNTIL SAIR = "S"
               PERFORM BASE
               DISPLAY "Menu Temas" HIGHLIGHT AT 0217
               DISPLAY "  Gestao de Temas  "
               FOREGROUND-COLOR 3 HIGHLIGHT AT 0545
               DISPLAY "o-----------------------o"
               FOREGROUND-COLOR 3 AT 0642
               DISPLAY "Novo -------------- 1" HIGHLIGHT AT 0744
               DISPLAY "Consultar --------- 2" HIGHLIGHT AT 0844
               DISPLAY "Alterar ----------- 3" HIGHLIGHT AT 0944
               DISPLAY "Eliminar ---------- 4" HIGHLIGHT AT 1044
               DISPLAY "Listagem ---------- 5" HIGHLIGHT AT 1144
               DISPLAY "Menu Inicial ------ 9" HIGHLIGHT AT 1344
               DISPLAY "[ ]"                   HIGHLIGHT AT 1553
               MOVE "S" TO REPETIR-MENU
               PERFORM UNTIL REPETIR-MENU = "N"
                   ACCEPT ESCOLHA AT 1554 AUTO
                   EVALUATE ESCOLHA
                       WHEN 1 PERFORM TEMAS-NOVO
                       WHEN 2 PERFORM TEMAS-CONSULTAR
                       WHEN 3 PERFORM TEMAS-ALTERAR
                       WHEN 4 PERFORM TEMAS-ELIMINAR
                       WHEN 5 PERFORM TEMAS-LISTAGEM
                       WHEN 9
                           MOVE "S" TO SAIR
                           MOVE "N" TO REPETIR-MENU
                       WHEN OTHER
                           DISPLAY "Escolha invalida!" AT 1746
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           MOVE "S" TO REPETIR-MENU
                   END-EVALUATE
               END-PERFORM
           END-PERFORM.
           MOVE "N" TO SAIR.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       TEMAS-NOVO.
           PERFORM BASE.
           DISPLAY "Novo Tema" HIGHLIGHT AT 0217.

           DISPLAY "Por favor preencha o seguinte campo:" AT 0403
           HIGHLIGHT.

           DISPLAY "Codigo do Tema:"
           AT 0603 FOREGROUND-COLOR 3 HIGHLIGHT.

           DISPLAY "Deixe em branco para retroceder."
           AT 0803 HIGHLIGHT.

           ACCEPT TEMA-COD AUTO HIGHLIGHT AT 0619.
           DISPLAY TEMA-COD HIGHLIGHT AT 0619.
           DISPLAY "                                " AT 0803.

           READ FIC-TEMAS
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "S") THEN
               DISPLAY "Tema ja existe!"
               FOREGROUND-COLOR 4 HIGHLIGHT AT 0625
           ELSE
               IF TEMA-COD = SPACES THEN
                   DISPLAY "00000" AT 0619 HIGHLIGHT
               ELSE
                   DISPLAY "Por favor preencha os seguintes campos:"
                   AT 0403 HIGHLIGHT
                   DISPLAY "Tema:"
                   AT 0803 FOREGROUND-COLOR 3 HIGHLIGHT

                   PERFORM WITH TEST AFTER UNTIL
                   TEMA > SPACES
                       ACCEPT TEMA HIGHLIGHT AT 0809
                       IF TEMA = SPACES THEN
                           DISPLAY "Tema invalido! "
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 0840
                       ELSE
                           DISPLAY "               " AT 0840
                       END-IF
                   END-PERFORM

                   WRITE REGISTO-TEMAS
                       INVALID KEY
                           DISPLAY "Erro ao criar tema!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1003
                       NOT INVALID KEY
                           DISPLAY "Tema criado com sucesso!"
                           HIGHLIGHT AT 1003
                   END-WRITE
               END-IF
           END-IF.

           DISPLAY "Prima ENTER para continuar.            "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       TEMAS-CONSULTAR.
           PERFORM BASE.
           DISPLAY "Consultar Tema" HIGHLIGHT AT 0217.

           DISPLAY "Por favor preencha o seguinte campo:" AT 0403
           HIGHLIGHT.

           DISPLAY "Codigo do Tema:"
           AT 0603 FOREGROUND-COLOR 3 HIGHLIGHT.

           DISPLAY "Deixe em branco para retroceder."
           AT 0803 HIGHLIGHT.

           ACCEPT TEMA-COD AUTO HIGHLIGHT AT 0619.
           DISPLAY TEMA-COD HIGHLIGHT AT 0619.
           DISPLAY "                                " AT 0803.

           READ FIC-TEMAS
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF TEMA-COD = SPACES THEN
                   DISPLAY "00000" AT 0619 HIGHLIGHT
               ELSE
                   DISPLAY "Tema nao existe!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0625
               END-IF
           ELSE
               DISPLAY "Tema:"
               AT 0803 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY TEMA HIGHLIGHT AT 0809
           END-IF.

           DISPLAY "Prima ENTER para continuar.         "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       TEMAS-ALTERAR.
           PERFORM BASE.
           DISPLAY "Alterar Tema" HIGHLIGHT AT 0217.

           DISPLAY "Por favor preencha o seguinte campo:" AT 0403
           HIGHLIGHT.

           DISPLAY "Codigo do Tema:"
           AT 0603 FOREGROUND-COLOR 3 HIGHLIGHT.

           DISPLAY "Deixe em branco para retroceder."
           AT 0803 HIGHLIGHT.

           ACCEPT TEMA-COD AUTO HIGHLIGHT AT 0619.
           DISPLAY TEMA-COD HIGHLIGHT AT 0619.
           DISPLAY "                                " AT 0803.

           READ FIC-TEMAS
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF TEMA-COD = SPACES THEN
                   DISPLAY "00000" AT 0619 HIGHLIGHT
               ELSE
                   DISPLAY "Tema nao existe!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0625
               END-IF
           ELSE
               DISPLAY "Por favor preencha os seguintes campos:"
               AT 0403 HIGHLIGHT
               DISPLAY "Tema:" AT 0803 FOREGROUND-COLOR 3 HIGHLIGHT

               PERFORM WITH TEST AFTER UNTIL
               TEMA > SPACES
                   ACCEPT TEMA HIGHLIGHT AT 0809
                   IF TEMA = SPACES THEN
                       DISPLAY "Tema invalido! "
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 0840
                   ELSE
                       DISPLAY "               " AT 0840
                   END-IF
               END-PERFORM

               REWRITE REGISTO-TEMAS
                   INVALID KEY
                       DISPLAY "Erro ao alterar tema!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1003
                   NOT INVALID KEY
                       DISPLAY "Tema alterado com sucesso!"
                       HIGHLIGHT AT 1003
               END-REWRITE
           END-IF.

           DISPLAY "Prima ENTER para continuar.            "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       TEMAS-ELIMINAR.
           PERFORM BASE.
           DISPLAY "Eliminar Tema" HIGHLIGHT AT 0217.

           DISPLAY "Por favor preencha o seguinte campo:" AT 0403
           HIGHLIGHT.
           DISPLAY "Codigo do Tema:"
           AT 0603 FOREGROUND-COLOR 3 HIGHLIGHT.

           DISPLAY "Deixe em branco para retroceder."
           AT 0803 HIGHLIGHT.

           ACCEPT TEMA-COD AUTO HIGHLIGHT AT 0619.
           DISPLAY TEMA-COD HIGHLIGHT AT 0619.
           DISPLAY "                                " AT 0803.

           READ FIC-TEMAS
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF TEMA-COD = SPACES THEN
                   DISPLAY "00000" HIGHLIGHT AT 0619
               ELSE
                   DISPLAY "Tema nao existe!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0625
               END-IF
           ELSE
               DELETE FIC-TEMAS
               INVALID KEY
                   DISPLAY "Erro ao eliminar tema!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0803
               NOT INVALID KEY
                   DISPLAY "Tema eliminado com sucesso!"
                   HIGHLIGHT AT 0803
               END-DELETE
           END-IF.

           DISPLAY "Prima ENTER para continuar.         "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       TEMAS-LISTAGEM.
           PERFORM BASE.
           DISPLAY "Listagem de Temas" HIGHLIGHT AT 0217.

           DISPLAY "o-------o-----o----------------------------------" &
           "--------------------------------------o------------------o"
           AT 0301 FOREGROUND-COLOR 3.

           PERFORM VARYING CONTADOR FROM 4 BY 1 UNTIL CONTADOR > 18
           DISPLAY "|       |                                        " &
           "                                                         |"
           LINE CONTADOR FOREGROUND-COLOR 3
           END-PERFORM.

           DISPLAY "C.Tm." FOREGROUND-COLOR 3 HIGHLIGHT AT 0403.
           DISPLAY "Tema" FOREGROUND-COLOR 3 HIGHLIGHT AT 0411.
           DISPLAY "P." FOREGROUND-COLOR 3 HIGHLIGHT AT 2003.
           DISPLAY "|" FOREGROUND-COLOR 3 AT 2009.
           DISPLAY "o" FOREGROUND-COLOR 3 AT 2109.

           DISPLAY "o-------o----------------------------------------" &
           "---------------------------------------------------------o"
           AT 0501 FOREGROUND-COLOR 3.
           DISPLAY "o-------o----------------------------------------" &
           "---------------------------------------------------------o"
           AT 1901 FOREGROUND-COLOR 3.

           MOVE 1 TO PAGINA.
           MOVE 0 TO TEMA-COD.
           START FIC-TEMAS KEY > TEMA-COD
               INVALID KEY
                   DISPLAY "Ficheiro vazio! Prima ENTER para continuar."
                   HIGHLIGHT AT 2011
                   DISPLAY "00" HIGHLIGHT AT 2006
                   ACCEPT OMITTED AT 2054
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
                                   HIGHLIGHT LINE LINHA COL 11
                                   ADD 1 TO LINHA
                                   IF LINHA = 19 THEN
                                       MOVE 6 TO LINHA
                                       DISPLAY
                            "Prima ENTER para mostrar a proxima pagina."
                                       HIGHLIGHT AT 2011
                                       ACCEPT OMITTED AT 2053
                                       ADD 1 TO PAGINA
      *********************************^
           PERFORM VARYING CONTADOR FROM 6 BY 1 UNTIL CONTADOR > 18
           DISPLAY "|       |                                        " &
           "                                                         |"
           LINE CONTADOR FOREGROUND-COLOR 3
           END-PERFORM
      ******************************************************************
                                   END-IF
                           END-READ
                       END-PERFORM
                   END-IF
                   DISPLAY "-----" HIGHLIGHT
                   FOREGROUND-COLOR 3 LINE LINHA COL 3
                   DISPLAY "-----------------------------------------" &
                   "-----------------------------------------" &
                   " Fim da Lista" HIGHLIGHT
                   FOREGROUND-COLOR 3 LINE LINHA COL 11
                   DISPLAY "Prima ENTER para continuar.               "
                   HIGHLIGHT AT 2011
                   ACCEPT OMITTED AT 2038
           END-START.

           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
      ******************************************************************
      ******************************************************************
       MENU-AUTORES.
           PERFORM UNTIL SAIR = "S"
               PERFORM BASE
               DISPLAY "Menu Autores" HIGHLIGHT AT 0217
               DISPLAY " Gestao de Autores "
               FOREGROUND-COLOR 3 HIGHLIGHT AT 0545
               DISPLAY "o-----------------------o"
               FOREGROUND-COLOR 3 AT 0642
               DISPLAY "Novo -------------- 1" HIGHLIGHT AT 0744
               DISPLAY "Consultar --------- 2" HIGHLIGHT AT 0844
               DISPLAY "Alterar ----------- 3" HIGHLIGHT AT 0944
               DISPLAY "Eliminar ---------- 4" HIGHLIGHT AT 1044
               DISPLAY "Listagem ---------- 5" HIGHLIGHT AT 1144
               DISPLAY "Menu Inicial ------ 9" HIGHLIGHT AT 1344
               DISPLAY "[ ]"                   HIGHLIGHT AT 1553
               MOVE "S" TO REPETIR-MENU
               PERFORM UNTIL REPETIR-MENU = "N"
                   ACCEPT ESCOLHA AT 1554 AUTO
                   EVALUATE ESCOLHA
                       WHEN 1 PERFORM AUTORES-NOVO
                       WHEN 2 PERFORM AUTORES-CONSULTAR
                       WHEN 3 PERFORM AUTORES-ALTERAR
                       WHEN 4 PERFORM AUTORES-ELIMINAR
                       WHEN 5 PERFORM AUTORES-LISTAGEM
                       WHEN 9
                           MOVE "S" TO SAIR
                           MOVE "N" TO REPETIR-MENU
                       WHEN OTHER
                           DISPLAY "Escolha invalida!" AT 1746
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           MOVE "S" TO REPETIR-MENU
                   END-EVALUATE
               END-PERFORM
           END-PERFORM.
           MOVE "N" TO SAIR.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       AUTORES-NOVO.
           PERFORM BASE.
           DISPLAY "Novo Autor" HIGHLIGHT AT 0217.

           DISPLAY "Por favor preencha o seguinte campo:" AT 0403
           HIGHLIGHT.

           DISPLAY "Codigo do Autor:"
           AT 0603 FOREGROUND-COLOR 3 HIGHLIGHT.

           DISPLAY "Deixe em branco para retroceder."
           AT 0803 HIGHLIGHT.

           ACCEPT AUTOR-COD AUTO HIGHLIGHT AT 0620.
           DISPLAY AUTOR-COD HIGHLIGHT AT 0620.
           DISPLAY "                                " AT 0803.

           READ FIC-AUTORES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "S") THEN
               DISPLAY "Autor ja existe!"
               FOREGROUND-COLOR 4 HIGHLIGHT AT 0626
           ELSE
               IF AUTOR-COD = SPACES THEN
                   DISPLAY "00000" AT 0620 HIGHLIGHT
               ELSE
                   DISPLAY "Por favor preencha os seguintes campos:"
                   AT 0403 HIGHLIGHT
                   DISPLAY "Autor:"
                   AT 0803 FOREGROUND-COLOR 3 HIGHLIGHT

                   PERFORM WITH TEST AFTER UNTIL
                   AUTOR > SPACES
                       ACCEPT AUTOR HIGHLIGHT AT 0810
                       IF AUTOR = SPACES THEN
                           DISPLAY "Autor invalido! "
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 0841
                       ELSE
                           DISPLAY "                " AT 0841
                       END-IF
                   END-PERFORM

                   WRITE REGISTO-AUTORES
                       INVALID KEY
                           DISPLAY "Erro ao criar autor!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1003
                       NOT INVALID KEY
                           DISPLAY "Autor criado com sucesso!"
                           HIGHLIGHT AT 1003
                   END-WRITE
               END-IF
           END-IF.

           DISPLAY "Prima ENTER para continuar.            "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       AUTORES-CONSULTAR.
           PERFORM BASE.
           DISPLAY "Consultar Autor" HIGHLIGHT AT 0217.

           DISPLAY "Por favor preencha o seguinte campo:" AT 0403
           HIGHLIGHT.

           DISPLAY "Codigo do Autor:"
           AT 0603 FOREGROUND-COLOR 3 HIGHLIGHT.

           DISPLAY "Deixe em branco para retroceder."
           AT 0803 HIGHLIGHT.

           ACCEPT AUTOR-COD AUTO HIGHLIGHT AT 0620.
           DISPLAY AUTOR-COD HIGHLIGHT AT 0620.
           DISPLAY "                                " AT 0803.

           READ FIC-AUTORES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF AUTOR-COD = SPACES THEN
                   DISPLAY "00000" AT 0620 HIGHLIGHT
               ELSE
                   DISPLAY "Autor nao existe!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0626
               END-IF
           ELSE
               DISPLAY "Autor:"
               AT 0803 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY AUTOR HIGHLIGHT AT 0810
           END-IF.

           DISPLAY "Prima ENTER para continuar.         "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       AUTORES-ALTERAR.
           PERFORM BASE.
           DISPLAY "Alterar Autor" HIGHLIGHT AT 0217.

           DISPLAY "Por favor preencha o seguinte campo:" AT 0403
           HIGHLIGHT.

           DISPLAY "Codigo do Autor:"
           AT 0603 FOREGROUND-COLOR 3 HIGHLIGHT.

           DISPLAY "Deixe em branco para retroceder."
           AT 0803 HIGHLIGHT.

           ACCEPT AUTOR-COD AUTO HIGHLIGHT AT 0620.
           DISPLAY AUTOR-COD HIGHLIGHT AT 0620.
           DISPLAY "                                " AT 0803.

           READ FIC-AUTORES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF AUTOR-COD = SPACES THEN
                   DISPLAY "00000" AT 0620 HIGHLIGHT
               ELSE
                   DISPLAY "Autor nao existe!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0626
               END-IF
           ELSE
               DISPLAY "Por favor preencha os seguintes campos:"
               AT 0403 HIGHLIGHT
               DISPLAY "Autor:"
               AT 0803 FOREGROUND-COLOR 3 HIGHLIGHT

               PERFORM WITH TEST AFTER UNTIL
               AUTOR > SPACES
                   ACCEPT AUTOR HIGHLIGHT AT 0810
                   IF AUTOR = SPACES THEN
                       DISPLAY "Autor invalido! "
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 0841
                   ELSE
                       DISPLAY "                " AT 0841
                   END-IF
               END-PERFORM

               REWRITE REGISTO-AUTORES
                   INVALID KEY
                       DISPLAY "Erro ao alterar autor!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1003
                   NOT INVALID KEY
                       DISPLAY "Autor alterado com sucesso!"
                       HIGHLIGHT AT 1003
               END-REWRITE
           END-IF.

           DISPLAY "Prima ENTER para continuar.            "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       AUTORES-ELIMINAR.
           PERFORM BASE.
           DISPLAY "Eliminar Autor" HIGHLIGHT AT 0217.

           DISPLAY "Por favor preencha o seguinte campo:" AT 0403
           HIGHLIGHT.

           DISPLAY "Codigo do Autor:"
           AT 0603 FOREGROUND-COLOR 3 HIGHLIGHT.

           DISPLAY "Deixe em branco para retroceder."
           AT 0803 HIGHLIGHT.

           ACCEPT AUTOR-COD AUTO HIGHLIGHT AT 0620.
           DISPLAY AUTOR-COD HIGHLIGHT AT 0620.
           DISPLAY "                                " AT 0803.

           READ FIC-AUTORES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF AUTOR-COD = SPACES THEN
                   DISPLAY "00000" AT 0620 HIGHLIGHT
               ELSE
                   DISPLAY "Autor nao existe!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0626
               END-IF
           ELSE
               DELETE FIC-AUTORES
               INVALID KEY
                   DISPLAY "Erro ao eliminar autor!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0803
               NOT INVALID KEY
                   DISPLAY "Autor eliminado com sucesso!"
                   HIGHLIGHT AT 0803
               END-DELETE
           END-IF.

           DISPLAY "Prima ENTER para continuar.         "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       AUTORES-LISTAGEM.
           PERFORM BASE.
           DISPLAY "Listagem de Autores" HIGHLIGHT AT 0217.

           DISPLAY "o-------o-----o----------------------------------" &
           "--------------------------------------o------------------o"
           AT 0301 FOREGROUND-COLOR 3.

           PERFORM VARYING CONTADOR FROM 4 BY 1 UNTIL CONTADOR > 18
           DISPLAY "|       |                                        " &
           "                                                         |"
           LINE CONTADOR FOREGROUND-COLOR 3
           END-PERFORM.

           DISPLAY "C.At." FOREGROUND-COLOR 3 HIGHLIGHT AT 0403.
           DISPLAY "Autor" FOREGROUND-COLOR 3 HIGHLIGHT AT 0411.
           DISPLAY "P." FOREGROUND-COLOR 3 HIGHLIGHT AT 2003.
           DISPLAY "|" FOREGROUND-COLOR 3 AT 2009.
           DISPLAY "o" FOREGROUND-COLOR 3 AT 2109.

           DISPLAY "o-------o----------------------------------------" &
           "---------------------------------------------------------o"
           AT 0501 FOREGROUND-COLOR 3.
           DISPLAY "o-------o----------------------------------------" &
           "---------------------------------------------------------o"
           AT 1901 FOREGROUND-COLOR 3.

           MOVE 1 TO PAGINA.
           MOVE 0 TO AUTOR-COD.
           START FIC-AUTORES KEY > AUTOR-COD
               INVALID KEY
                   DISPLAY "Ficheiro vazio! Prima ENTER para continuar."
                   HIGHLIGHT AT 2011
                   DISPLAY "00" HIGHLIGHT AT 2006
                   ACCEPT OMITTED AT 2054
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
                                   HIGHLIGHT LINE LINHA COL 11
                                   ADD 1 TO LINHA
                                   IF LINHA = 19 THEN
                                       MOVE 6 TO LINHA
                                       DISPLAY
                            "Prima ENTER para mostrar a proxima pagina."
                                       HIGHLIGHT AT 2011
                                       ACCEPT OMITTED AT 2053
                                       ADD 1 TO PAGINA
      *********************************^
           PERFORM VARYING CONTADOR FROM 6 BY 1 UNTIL CONTADOR > 18
           DISPLAY "|       |                                        " &
           "                                                         |"
           LINE CONTADOR FOREGROUND-COLOR 3
           END-PERFORM
      ******************************************************************
                                   END-IF
                           END-READ
                       END-PERFORM
                   END-IF
                   DISPLAY "-----" HIGHLIGHT
                   FOREGROUND-COLOR 3 LINE LINHA COL 3
                   DISPLAY "-----------------------------------------" &
                   "-----------------------------------------" &
                   " Fim da Lista" HIGHLIGHT
                   FOREGROUND-COLOR 3 LINE LINHA COL 11
                   DISPLAY "Prima ENTER para continuar.               "
                   HIGHLIGHT AT 2011
                   ACCEPT OMITTED AT 2038
           END-START.

           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
      ******************************************************************
      ******************************************************************
       MENU-ALUGUERES.
           PERFORM UNTIL SAIR = "S"
               PERFORM BASE
               DISPLAY "Menu Alugueres" HIGHLIGHT AT 0217
               DISPLAY "Gestao de Alugueres"
               FOREGROUND-COLOR 3 HIGHLIGHT AT 0545
               DISPLAY "o-----------------------o"
               FOREGROUND-COLOR 3 AT 0642
               DISPLAY "Novo -------------- 1" HIGHLIGHT AT 0744
               DISPLAY "Consultar --------- 2" HIGHLIGHT AT 0844
               DISPLAY "Alterar ----------- 3" HIGHLIGHT AT 0944
               DISPLAY "Eliminar ---------- 4" HIGHLIGHT AT 1044
               DISPLAY "Listagem ---------- 5" HIGHLIGHT AT 1144
               DISPLAY "Devolver ---------- 6" HIGHLIGHT AT 1244
               DISPLAY "Menu Inicial ------ 9" HIGHLIGHT AT 1444
               DISPLAY "[ ]"                   HIGHLIGHT AT 1653
               MOVE "S" TO REPETIR-MENU
               PERFORM UNTIL REPETIR-MENU = "N"
                   ACCEPT ESCOLHA AT 1654 AUTO
                   EVALUATE ESCOLHA
                       WHEN 1 PERFORM ALUGUERES-NOVO
                       WHEN 2 PERFORM ALUGUERES-CONSULTAR
                       WHEN 3 PERFORM ALUGUERES-ALTERAR
                       WHEN 4 PERFORM ALUGUERES-ELIMINAR
                       WHEN 5 PERFORM ALUGUERES-LISTAGEM
                       WHEN 6 PERFORM ALUGUERES-DEVOLVER
                       WHEN 9
                           MOVE "S" TO SAIR
                           MOVE "N" TO REPETIR-MENU
                       WHEN OTHER
                           DISPLAY "Escolha invalida!" AT 1846
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           MOVE "S" TO REPETIR-MENU
                   END-EVALUATE
               END-PERFORM
           END-PERFORM.
           MOVE "N" TO SAIR.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       ALUGUERES-NOVO.
           PERFORM BASE.
           DISPLAY "Novo Aluguer" HIGHLIGHT AT 0217.

           DISPLAY "Por favor preencha o seguinte campo:" AT 0403
           HIGHLIGHT.

           DISPLAY "Codigo do Aluguer:"
           AT 0603 FOREGROUND-COLOR 3 HIGHLIGHT.

           DISPLAY "Deixe em branco para retroceder."
           AT 0803 HIGHLIGHT.

           ACCEPT ALUGUER-COD AUTO HIGHLIGHT AT 0622.
           DISPLAY ALUGUER-COD HIGHLIGHT AT 0622.
           DISPLAY "                                " AT 0803.

           READ FIC-ALUGUERES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "S") THEN
               DISPLAY "Aluguer ja existe!"
               FOREGROUND-COLOR 4 HIGHLIGHT AT 0628
           ELSE
               IF ALUGUER-COD = SPACES THEN
                   DISPLAY "00000" AT 0622 HIGHLIGHT
               ELSE
                   DISPLAY "Por favor preencha os seguintes campos:"
                   AT 0403 HIGHLIGHT
                   DISPLAY "Codigo de Cliente:"
                   AT 0803 FOREGROUND-COLOR 3 HIGHLIGHT
                   DISPLAY "Codigo do Livro:"
                   AT 1003 FOREGROUND-COLOR 3 HIGHLIGHT
                   DISPLAY "Data de Levantamento:"
                   AT 1203 FOREGROUND-COLOR 3 HIGHLIGHT

                   PERFORM WITH TEST AFTER UNTIL
                   ALUGUER-CLIENTE-COD > SPACES
                       ACCEPT ALUGUER-CLIENTE-COD AUTO HIGHLIGHT AT 0822
                       IF ALUGUER-CLIENTE-COD = SPACES THEN
                           DISPLAY "Cliente invalido! "
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 0828
                       ELSE
                           DISPLAY "                  " AT 0828
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
                       DISPLAY "Cliente nao existe! Por favor" &
                       " atualize a base de dados!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 0828
                   END-IF
                   DISPLAY ALUGUER-CLIENTE-COD HIGHLIGHT AT 0822

                   PERFORM WITH TEST AFTER UNTIL
                   ALUGUER-LIVRO-COD > SPACES
                       ACCEPT ALUGUER-LIVRO-COD AUTO HIGHLIGHT AT 1020
                       IF ALUGUER-LIVRO-COD = SPACES THEN
                           DISPLAY "Livro invalido! "
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1026
                       ELSE
                           DISPLAY "                " AT 1026
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
                       DISPLAY "Livro nao existe! Por favor" &
                       " atualize a base de dados!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1026
                   END-IF
                   DISPLAY ALUGUER-LIVRO-COD HIGHLIGHT AT 1020

                   DISPLAY "(Ano)" AT 1230 FOREGROUND-COLOR 3 HIGHLIGHT
                   DISPLAY "Deixe Zero para inserir o Ano atual."
                   HIGHLIGHT AT 1236

                   PERFORM WITH TEST AFTER UNTIL (ANO-ALUGUER = 0) OR
                   (ANO-ALUGUER >= 1950 AND ANO-ALUGUER <= ANO-SISTEMA)
                       ACCEPT ANO-ALUGUER AUTO AT 1225 HIGHLIGHT
                       IF NOT (ANO-ALUGUER >= 1950 AND
                       ANO-ALUGUER <= ANO-SISTEMA)
                           DISPLAY "Ano invalido!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1273
                       END-IF
                   END-PERFORM

                   IF ANO-ALUGUER = 0 THEN
                       MOVE ANO-SISTEMA TO ANO-ALUGUER
                   END-IF

                   DISPLAY "                                           "
                   & "                " AT 1230
                   DISPLAY ANO-ALUGUER AT 1225 HIGHLIGHT

                   DISPLAY "-" AT 1229 HIGHLIGHT

                   DISPLAY "(Mes)" AT 1233 FOREGROUND-COLOR 3 HIGHLIGHT
                   DISPLAY "Deixe Zero para inserir o Mes atual."
                   HIGHLIGHT AT 1239

                   PERFORM WITH TEST AFTER UNTIL
                   (MES-ALUGUER >= 0 AND <= 12)
                       ACCEPT MES-ALUGUER AUTO AT 1230 HIGHLIGHT
                       IF NOT (MES-ALUGUER >= 0 AND <= 12) THEN
                           DISPLAY "Mes invalido!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1276
                       END-IF
                       IF ANO-ALUGUER = ANO-SISTEMA THEN
                           IF MES-ALUGUER > MES-SISTEMA THEN
                               DISPLAY "Mes invalido!"
                               FOREGROUND-COLOR 4 HIGHLIGHT AT 1276
                               MOVE 13 TO MES-ALUGUER
                           END-IF
                       END-IF
                   END-PERFORM

                   IF MES-ALUGUER = 0 THEN
                       MOVE MES-SISTEMA TO MES-ALUGUER
                   END-IF
                   DISPLAY "                                           "
                   & "             " AT 1233
                   DISPLAY MES-ALUGUER AT 1230 HIGHLIGHT

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

                   DISPLAY "-" AT 1232 HIGHLIGHT

                   DISPLAY "(Dia)" AT 1236 FOREGROUND-COLOR 3 HIGHLIGHT
                   DISPLAY "Deixe Zero para inserir o Dia atual."
                   HIGHLIGHT AT 1242

                   PERFORM WITH TEST AFTER UNTIL
                   (DIA-ALUGUER >= 0 AND DIA-ALUGUER <= DIAS-MES)
                       ACCEPT DIA-ALUGUER AUTO AT 1233 HIGHLIGHT
                       IF DIA-ALUGUER = 0 THEN
                          MOVE DIA-SISTEMA TO DIA-ALUGUER
                       END-IF
                       IF NOT (DIA-ALUGUER >= 0 AND
                       DIA-ALUGUER <= DIAS-MES) THEN
                           DISPLAY "Dia invalido!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1279
                       END-IF
                       IF ANO-ALUGUER = ANO-SISTEMA THEN
                           IF MES-ALUGUER = MES-SISTEMA THEN
                               IF DIA-ALUGUER > DIA-SISTEMA THEN
                                   DISPLAY "Dia invalido!"
                                   FOREGROUND-COLOR 4 HIGHLIGHT AT 1279
                                   MOVE 32 TO DIA-ALUGUER
                               END-IF
                           END-IF
                       END-IF
                   END-PERFORM

                   DISPLAY "                                           "
                   & "             " AT 1236
                   DISPLAY DIA-ALUGUER AT 1233 HIGHLIGHT

                   MOVE 0 TO ANO-ENTREGA
                   MOVE 0 TO MES-ENTREGA
                   MOVE 0 TO DIA-ENTREGA
                   MOVE "A" TO SITUACAO

                   WRITE REGISTO-ALUGUER
                       INVALID KEY
                           DISPLAY "Erro ao criar aluguer!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1403
                       NOT INVALID KEY
                           DISPLAY "Aluguer criado com sucesso!"
                           HIGHLIGHT AT 1403
                   END-WRITE
               END-IF
           END-IF.

           DISPLAY "Prima ENTER para continuar.            "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       ALUGUERES-CONSULTAR.
           PERFORM BASE.
           DISPLAY "Consultar Aluguer" HIGHLIGHT AT 0217.

           DISPLAY "Por favor preencha o seguinte campo:" AT 0403
           HIGHLIGHT.

           DISPLAY "Codigo do Aluguer:"
           AT 0603 FOREGROUND-COLOR 3 HIGHLIGHT.

           DISPLAY "Deixe em branco para retroceder."
           AT 0803 HIGHLIGHT.

           ACCEPT ALUGUER-COD AUTO HIGHLIGHT AT 0622.
           DISPLAY ALUGUER-COD HIGHLIGHT AT 0622.
           DISPLAY "                                " AT 0803.

           READ FIC-ALUGUERES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF ALUGUER-COD = SPACES THEN
                   DISPLAY "00000" AT 0622 HIGHLIGHT
               ELSE
                   DISPLAY "Aluguer nao existe!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0628
               END-IF
           ELSE
               DISPLAY "Codigo de Cliente:"
               AT 0803 FOREGROUND-COLOR 3 HIGHLIGHT
               MOVE ALUGUER-CLIENTE-COD TO CLIENTE-COD
               READ FIC-CLIENTES KEY CLIENTE-COD
                   INVALID KEY
                       MOVE "N" TO EXISTE
                   NOT INVALID KEY
                       MOVE "S" TO EXISTE
               END-READ
               DISPLAY ALUGUER-CLIENTE-COD HIGHLIGHT AT 0822
               IF (EXISTE = "N") THEN
                   DISPLAY "Cliente nao existe! Por favor" &
                   " atualize a base de dados!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0828
               END-IF

               DISPLAY "Codigo do Livro:"
               AT 1003 FOREGROUND-COLOR 3 HIGHLIGHT
               MOVE ALUGUER-LIVRO-COD TO LIVRO-COD
               READ FIC-LIVROS KEY LIVRO-COD
                   INVALID KEY
                       MOVE "N" TO EXISTE
                   NOT INVALID KEY
                       MOVE "S" TO EXISTE
               END-READ
               DISPLAY ALUGUER-LIVRO-COD HIGHLIGHT AT 1020
               IF (EXISTE = "N") THEN
                   DISPLAY "Livro nao existe! Por favor" &
                   " atualize a base de dados!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 1026
               END-IF

               DISPLAY "Data de Aluguer:"
               AT 1403 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY FUNCTION CONCATENATE(ANO-ALUGUER, "-"
               MES-ALUGUER, "-" DIA-ALUGUER) AT 1420 HIGHLIGHT

               DISPLAY "Situacao: "
               AT 1203 FOREGROUND-COLOR 3 HIGHLIGHT

               IF (SITUACAO = "A") THEN
                   DISPLAY "Alugado" HIGHLIGHT AT 1213
               ELSE
                   DISPLAY "Devolvido" HIGHLIGHT AT 1213
                   DISPLAY "Data de Devolucao:"
                   AT 1603 FOREGROUND-COLOR 3 HIGHLIGHT
                   DISPLAY FUNCTION CONCATENATE(ANO-ENTREGA, "-"
                   MES-ENTREGA, "-" DIA-ENTREGA) AT 1622 HIGHLIGHT
               END-IF
           END-IF.

           DISPLAY "Prima ENTER para continuar.         "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       ALUGUERES-ALTERAR.
           PERFORM BASE.
           DISPLAY "Alterar Aluguer" HIGHLIGHT AT 0217.

           DISPLAY "Por favor preencha o seguinte campo:" AT 0403
           HIGHLIGHT.

           DISPLAY "Codigo do Aluguer:"
           AT 0603 FOREGROUND-COLOR 3 HIGHLIGHT.

           DISPLAY "Deixe em branco para retroceder."
           AT 0803 HIGHLIGHT.

           ACCEPT ALUGUER-COD AUTO HIGHLIGHT AT 0622.
           DISPLAY ALUGUER-COD HIGHLIGHT AT 0622.
           DISPLAY "                                " AT 0803.

           READ FIC-ALUGUERES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF ALUGUER-COD = SPACES THEN
                   DISPLAY "00000" AT 0622 HIGHLIGHT
               ELSE
                   DISPLAY "Aluguer nao existe!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0628
               END-IF
           ELSE
               DISPLAY "Por favor preencha os seguintes campos:"
               AT 0403 HIGHLIGHT
               DISPLAY "Codigo de Cliente:"
               AT 0803 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY "Codigo do Livro:"
               AT 1003 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY "Data de Levantamento:"
               AT 1203 FOREGROUND-COLOR 3 HIGHLIGHT


               PERFORM WITH TEST AFTER UNTIL
               ALUGUER-CLIENTE-COD > SPACES
                   ACCEPT ALUGUER-CLIENTE-COD AUTO HIGHLIGHT AT 0822
                   IF ALUGUER-CLIENTE-COD = SPACES THEN
                       DISPLAY "Cliente invalido! "
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 0828
                   ELSE
                       DISPLAY "                  " AT 0828
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
                   DISPLAY "Cliente nao existe! Por favor" &
                   " atualize a base de dados!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0828
               END-IF
               DISPLAY ALUGUER-CLIENTE-COD HIGHLIGHT AT 0822


               PERFORM WITH TEST AFTER UNTIL
               ALUGUER-LIVRO-COD > SPACES
                   ACCEPT ALUGUER-LIVRO-COD AUTO HIGHLIGHT AT 1020
                   IF ALUGUER-LIVRO-COD = SPACES THEN
                       DISPLAY "Livro invalido! "
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1026
                   ELSE
                       DISPLAY "                " AT 1026
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
                   DISPLAY "Livro nao existe! Por favor" &
                   " atualize a base de dados!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 1026
               END-IF
               DISPLAY ALUGUER-LIVRO-COD HIGHLIGHT AT 1020

               DISPLAY "(Ano)" AT 1230 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY "Deixe Zero para inserir o Ano atual."
               HIGHLIGHT AT 1236

               PERFORM WITH TEST AFTER UNTIL (ANO-ALUGUER = 0) OR
               (ANO-ALUGUER >= 1950 AND ANO-ALUGUER <= ANO-SISTEMA)
                   ACCEPT ANO-ALUGUER AUTO AT 1225 HIGHLIGHT
                   IF NOT (ANO-ALUGUER >= 1950 AND
                   ANO-ALUGUER <= ANO-SISTEMA)
                       DISPLAY "Ano invalido!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1273
                   END-IF
               END-PERFORM

               IF ANO-ALUGUER = 0 THEN
                   MOVE ANO-SISTEMA TO ANO-ALUGUER
               END-IF

               DISPLAY "                                           "
               & "                " AT 1230
               DISPLAY ANO-ALUGUER AT 1225 HIGHLIGHT

               DISPLAY "-" AT 1229 HIGHLIGHT

               DISPLAY "(Mes)" AT 1233 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY "Deixe Zero para inserir o Mes atual."
               HIGHLIGHT AT 1239

               PERFORM WITH TEST AFTER UNTIL
               (MES-ALUGUER >= 0 AND <= 12)
                   ACCEPT MES-ALUGUER AUTO AT 1230 HIGHLIGHT
                   IF NOT (MES-ALUGUER >= 0 AND <= 12) THEN
                       DISPLAY "Mes invalido!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1276
                   END-IF
                   IF ANO-ALUGUER = ANO-SISTEMA THEN
                       IF MES-ALUGUER > MES-SISTEMA THEN
                           DISPLAY "Mes invalido!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1276
                           MOVE 13 TO MES-ALUGUER
                       END-IF
                   END-IF
               END-PERFORM

               IF MES-ALUGUER = 0 THEN
                   MOVE MES-SISTEMA TO MES-ALUGUER
               END-IF
               DISPLAY "                                           "
               & "             " AT 1233
               DISPLAY MES-ALUGUER AT 1230 HIGHLIGHT

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

               DISPLAY "-" AT 1232 HIGHLIGHT

               DISPLAY "(Dia)" AT 1236 FOREGROUND-COLOR 3 HIGHLIGHT
               DISPLAY "Deixe Zero para inserir o Dia atual."
               HIGHLIGHT AT 1242

               PERFORM WITH TEST AFTER UNTIL
               (DIA-ALUGUER >= 0 AND DIA-ALUGUER <= DIAS-MES)
                   ACCEPT DIA-ALUGUER AUTO AT 1233 HIGHLIGHT
                   IF DIA-ALUGUER = 0 THEN
                       MOVE DIA-SISTEMA TO DIA-ALUGUER
                   END-IF
                   IF NOT (DIA-ALUGUER >= 0 AND
                   DIA-ALUGUER <= DIAS-MES) THEN
                       DISPLAY "Dia invalido!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1279
                   END-IF
                   IF ANO-ALUGUER = ANO-SISTEMA THEN
                       IF MES-ALUGUER = MES-SISTEMA THEN
                           IF DIA-ALUGUER > DIA-SISTEMA THEN
                               DISPLAY "Dia invalido!"
                               FOREGROUND-COLOR 4 HIGHLIGHT AT 1279
                               MOVE 32 TO DIA-ALUGUER
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM

               DISPLAY "                                           "
               & "             " AT 1236
               DISPLAY DIA-ALUGUER AT 1233 HIGHLIGHT

               IF SITUACAO = "D" THEN
                   MOVE 0 TO ANO-ENTREGA
                   MOVE 0 TO MES-ENTREGA
                   MOVE 0 TO DIA-ENTREGA
               END-IF

               MOVE "A" TO SITUACAO

               REWRITE REGISTO-ALUGUER
                   INVALID KEY
                       DISPLAY "Erro ao alterar aluguer!"
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1403
                   NOT INVALID KEY
                       DISPLAY "Aluguer alterado com sucesso!"
                       HIGHLIGHT AT 1403
               END-REWRITE
           END-IF.

           DISPLAY "Prima ENTER para continuar.            "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       ALUGUERES-ELIMINAR.
           PERFORM BASE.
           DISPLAY "Eliminar Aluguer" HIGHLIGHT AT 0217.

           DISPLAY "Por favor preencha o seguinte campo:" AT 0403
           HIGHLIGHT.

           DISPLAY "Codigo do Aluguer:"
           AT 0603 FOREGROUND-COLOR 3 HIGHLIGHT.

           DISPLAY "Deixe em branco para retroceder."
           AT 0803 HIGHLIGHT.

           ACCEPT ALUGUER-COD AUTO HIGHLIGHT AT 0622.
           DISPLAY ALUGUER-COD HIGHLIGHT AT 0622.
           DISPLAY "                                " AT 0803.

           READ FIC-ALUGUERES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF ALUGUER-COD = SPACES THEN
                   DISPLAY "00000" AT 0622 HIGHLIGHT
               ELSE
                   DISPLAY "Aluguer nao existe!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0628
               END-IF
           ELSE
               DELETE FIC-ALUGUERES
                   INVALID KEY
                       DISPLAY "Erro ao eliminar aluguer! "
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 0803
                   NOT INVALID KEY
                       DISPLAY "Aluguer eliminado com sucesso!"
                       HIGHLIGHT AT 0803
               END-DELETE
           END-IF.

           DISPLAY "Prima ENTER para continuar.         "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       ALUGUERES-LISTAGEM.
           PERFORM BASE.
           DISPLAY "Listagem de Alugueres" HIGHLIGHT AT 0217.

           DISPLAY "o-------o-----o-o-------o------------o-----------" &
           "-o------------------------------------o------------------o"
           AT 0301 FOREGROUND-COLOR 3.

           PERFORM VARYING CONTADOR FROM 4 BY 1 UNTIL CONTADOR > 18
           DISPLAY "|       |       |       |            |           " &
           " |                                                       |"
           LINE CONTADOR FOREGROUND-COLOR 3
           END-PERFORM.

           DISPLAY "C.Al." FOREGROUND-COLOR 3 HIGHLIGHT AT 0403.
           DISPLAY "C.Cl." FOREGROUND-COLOR 3 HIGHLIGHT AT 0411.
           DISPLAY "C.Lv." FOREGROUND-COLOR 3 HIGHLIGHT AT 0419.
           DISPLAY "Data Alug." FOREGROUND-COLOR 3 HIGHLIGHT AT 0427.
           DISPLAY "Data Dvol." FOREGROUND-COLOR 3 HIGHLIGHT AT 0440.
           DISPLAY "Situacao" FOREGROUND-COLOR 3 HIGHLIGHT AT 0453.
           DISPLAY "P." FOREGROUND-COLOR 3 HIGHLIGHT AT 2003.
           DISPLAY "|" FOREGROUND-COLOR 3 AT 2009.
           DISPLAY "o" FOREGROUND-COLOR 3 AT 2109.

           DISPLAY "o-------o-------o-------o------------o-----------" &
           "-o-------------------------------------------------------o"
           AT 0501 FOREGROUND-COLOR 3.
           DISPLAY "o-------o-------o-------o------------o-----------" &
           "-o-------------------------------------------------------o"
           AT 1901 FOREGROUND-COLOR 3.

           MOVE 1 TO PAGINA.
           MOVE 0 TO ALUGUER-COD.
           START FIC-ALUGUERES KEY > ALUGUER-COD
               INVALID KEY
                   DISPLAY "Ficheiro vazio! Prima ENTER para continuar."
                   HIGHLIGHT AT 2011
                   DISPLAY "00" HIGHLIGHT AT 2006
                   ACCEPT OMITTED AT 2054
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
                                       FOREGROUND-COLOR 4
                                       HIGHLIGHT LINE LINHA COL 11
                                       DISPLAY "Dados em falta!"
                                       FOREGROUND-COLOR 4
                                       HIGHLIGHT LINE LINHA COL 91
                                   ELSE
                                       DISPLAY ALUGUER-CLIENTE-COD
                                       HIGHLIGHT LINE LINHA COL 11
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
                                       FOREGROUND-COLOR 4
                                       HIGHLIGHT LINE LINHA COL 19
                                       DISPLAY "Dados em falta!"
                                       FOREGROUND-COLOR 4
                                       HIGHLIGHT LINE LINHA COL 91
                                   ELSE
                                       DISPLAY ALUGUER-LIVRO-COD
                                       HIGHLIGHT LINE LINHA COL 19
                                   END-IF

                                   DISPLAY FUNCTION CONCATENATE
                                   (DIA-ALUGUER, "-" MES-ALUGUER,
                                   "-" ANO-ALUGUER) HIGHLIGHT
                                   LINE LINHA COL 27

                                   IF ANO-ENTREGA = 0 THEN
                                       DISPLAY "----------" HIGHLIGHT
                                       LINE LINHA COL 40
                                   ELSE
                                       DISPLAY FUNCTION CONCATENATE
                                       (DIA-ENTREGA, "-" MES-ENTREGA,
                                       "-" ANO-ENTREGA) HIGHLIGHT
                                       LINE LINHA COL 40
                                   END-IF

                                   IF SITUACAO = "A" THEN
                                       DISPLAY "Alugado" HIGHLIGHT
                                       LINE LINHA COL 53
                                   ELSE
                                       DISPLAY "Devolvido" HIGHLIGHT
                                       LINE LINHA COL 53
                                   END-IF

                                   ADD 1 TO LINHA
                                   IF LINHA = 19 THEN
                                       MOVE 6 TO LINHA
                                       DISPLAY
                            "Prima ENTER para mostrar a proxima pagina."
                                       HIGHLIGHT AT 2011
                                       ACCEPT OMITTED AT 2053
                                       ADD 1 TO PAGINA
      *********************************^
           PERFORM VARYING CONTADOR FROM 6 BY 1 UNTIL CONTADOR > 18
           DISPLAY "|       |       |       |            |           " &
           " |                                                       |"
           LINE CONTADOR FOREGROUND-COLOR 3
           END-PERFORM
      ******************************************************************
                                   END-IF
                           END-READ
                       END-PERFORM
                   END-IF
                   DISPLAY "-----" HIGHLIGHT
                   FOREGROUND-COLOR 3 LINE LINHA COL 3
                   DISPLAY "-----" HIGHLIGHT
                   FOREGROUND-COLOR 3 LINE LINHA COL 11
                   DISPLAY "-----" HIGHLIGHT
                   FOREGROUND-COLOR 3 LINE LINHA COL 19
                   DISPLAY "----------" HIGHLIGHT
                   FOREGROUND-COLOR 3 LINE LINHA COL 27
                   DISPLAY "----------" HIGHLIGHT
                   FOREGROUND-COLOR 3 LINE LINHA COL 40
                   DISPLAY "---------------------------------------- " &
                   "Fim da Lista" HIGHLIGHT
                   FOREGROUND-COLOR 3 LINE LINHA COL 53
                   DISPLAY "Prima ENTER para continuar.               "
                   HIGHLIGHT AT 2011
                   ACCEPT OMITTED AT 2038
           END-START.

           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       ALUGUERES-DEVOLVER.
           PERFORM BASE.
           DISPLAY "Devolver Aluguer" HIGHLIGHT AT 0217.

           DISPLAY "Por favor preencha o seguinte campo:" AT 0403
           HIGHLIGHT.

           DISPLAY "Codigo do Aluguer:"
           AT 0603 FOREGROUND-COLOR 3 HIGHLIGHT.

           DISPLAY "Deixe em branco para retroceder."
           AT 0803 HIGHLIGHT.

           ACCEPT ALUGUER-COD AUTO HIGHLIGHT AT 0622.
           DISPLAY ALUGUER-COD HIGHLIGHT AT 0622.
           DISPLAY "                                " AT 0803.

           READ FIC-ALUGUERES
               INVALID KEY
                   MOVE "N" TO EXISTE
               NOT INVALID KEY
                   MOVE "S" TO EXISTE
           END-READ.

           IF (EXISTE = "N") THEN
               IF ALUGUER-COD = SPACES THEN
                   DISPLAY "00000" AT 0622 HIGHLIGHT
               ELSE
                   DISPLAY "Codigo nao existe!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0628
               END-IF
           ELSE
               IF (SITUACAO = "A") THEN
                   MOVE "D" TO SITUACAO

                   DISPLAY "Data de Devolucao:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0803
                   DISPLAY "(Ano)" AT 0827 FOREGROUND-COLOR 3 HIGHLIGHT
                   DISPLAY "Deixe Zero para inserir o Ano atual."
                   HIGHLIGHT AT 0833

                   PERFORM WITH TEST AFTER UNTIL (ANO-ENTREGA = 0) OR
                   (ANO-ENTREGA >= 1950 AND ANO-ENTREGA <= ANO-SISTEMA
                   AND ANO-ENTREGA >= ANO-ALUGUER)
                       ACCEPT ANO-ENTREGA AUTO AT 0822 HIGHLIGHT
                       IF NOT (ANO-ENTREGA >= 1950 AND
                       ANO-ENTREGA <= ANO-SISTEMA
                       AND ANO-ENTREGA >= ANO-ALUGUER)
                           DISPLAY "Ano invalido!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 0870
                       END-IF
                   END-PERFORM

                   IF ANO-ENTREGA = 0 THEN
                       MOVE ANO-SISTEMA TO ANO-ENTREGA
                   END-IF

                   DISPLAY "                                           "
                   & "                " AT 0827
                   DISPLAY ANO-ENTREGA AT 0822 HIGHLIGHT

                   DISPLAY "-" AT 0826 HIGHLIGHT

                   DISPLAY "(Mes)" AT 0830 FOREGROUND-COLOR 3 HIGHLIGHT
                   DISPLAY "Deixe Zero para inserir o Mes atual."
                   HIGHLIGHT AT 0836

                   PERFORM WITH TEST AFTER UNTIL
                   (MES-ENTREGA >= 0 AND <= 12)
                   AND (MES-ENTREGA >= MES-ALUGUER)
                       ACCEPT MES-ENTREGA AUTO AT 0827 HIGHLIGHT
                       IF MES-ENTREGA = 0 THEN
                           MOVE MES-SISTEMA TO MES-ENTREGA
                       END-IF
                       IF NOT (MES-ENTREGA >= 0 AND <= 12) THEN
                           DISPLAY "Mes invalido!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 0873
                       END-IF
                       IF ANO-ENTREGA = ANO-SISTEMA THEN
                           IF MES-ENTREGA > MES-SISTEMA THEN
                               DISPLAY "Mes invalido!"
                               FOREGROUND-COLOR 4 HIGHLIGHT AT 0873
                               MOVE 13 TO MES-ENTREGA
                           END-IF
                       END-IF
                       IF NOT (MES-ENTREGA >= MES-ALUGUER) THEN
                           DISPLAY "Mes invalido!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 0873
                       END-IF
                   END-PERFORM


                   DISPLAY "                                           "
                   & "             " AT 0830
                   DISPLAY MES-ENTREGA AT 0827 HIGHLIGHT

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

                   DISPLAY "-" AT 0829 HIGHLIGHT

                   DISPLAY "(Dia)" AT 0833 FOREGROUND-COLOR 3 HIGHLIGHT
                   DISPLAY "Deixe Zero para inserir o Dia atual."
                   HIGHLIGHT AT 0839

                   PERFORM WITH TEST AFTER UNTIL
                   (DIA-ENTREGA >= 0 AND DIA-ENTREGA <= DIAS-MES)
                       ACCEPT DIA-ENTREGA AUTO AT 0830 HIGHLIGHT
                       IF DIA-ENTREGA = 0 THEN
                           MOVE DIA-SISTEMA TO DIA-ENTREGA
                       END-IF
                       IF NOT (DIA-ENTREGA >= 0 AND
                       DIA-ENTREGA <= DIAS-MES) THEN
                           DISPLAY "Dia invalido!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 0876
                       END-IF
                       IF ANO-ENTREGA = ANO-SISTEMA THEN
                           IF MES-ENTREGA = MES-SISTEMA THEN
                               IF DIA-ENTREGA > DIA-SISTEMA THEN
                                   DISPLAY "Dia invalido!"
                                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0876
                                   MOVE 32 TO DIA-ENTREGA
                               END-IF
                           END-IF
                       END-IF
                       IF ANO-ENTREGA = ANO-ALUGUER THEN
                           IF MES-ENTREGA = MES-ALUGUER THEN
                               IF DIA-ENTREGA < DIA-ALUGUER THEN
                                   DISPLAY "Dia invalido!"
                                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0876
                                   MOVE 32 TO DIA-ENTREGA
                               END-IF
                           END-IF
                       END-IF
                   END-PERFORM

                   DISPLAY "                                           "
                   & "             " AT 0833
                   DISPLAY DIA-ENTREGA AT 0830 HIGHLIGHT

                   REWRITE REGISTO-ALUGUER
                       INVALID KEY
                           DISPLAY "Erro ao devolver!"
                           FOREGROUND-COLOR 4 HIGHLIGHT AT 1003
                       NOT INVALID KEY
                           DISPLAY "Livro devolvido com sucesso!"
                           HIGHLIGHT AT 1003
                   END-REWRITE
               ELSE
                   DISPLAY "Livro ja foi devolvido!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0803
               END-IF
           END-IF.



           DISPLAY "Prima ENTER para continuar.         "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
      ******************************************************************
      ******************************************************************
       MENU-EXPORTAR.
           PERFORM UNTIL SAIR = "S"
               PERFORM BASE
               DISPLAY "Menu Exportar" HIGHLIGHT AT 0217
               DISPLAY "     Exportar:     "
               FOREGROUND-COLOR 3 HIGHLIGHT AT 0545
               DISPLAY "o-----------------------o"
               FOREGROUND-COLOR 3 AT 0642
               DISPLAY "Tudo -------------- 1" HIGHLIGHT AT 0744
               DISPLAY "Alugueres --------- 2" HIGHLIGHT AT 0844
               DISPLAY "Menu Inicial ------ 9" HIGHLIGHT AT 1044
               DISPLAY "[ ]"                   HIGHLIGHT AT 1253
               MOVE "S" TO REPETIR-MENU
               PERFORM UNTIL REPETIR-MENU = "N"
                   ACCEPT ESCOLHA AT 1254 AUTO
                   EVALUATE ESCOLHA
                       WHEN 1 PERFORM EXPORTAR-TUDO
                       WHEN 2 PERFORM EXPORTAR-ALUGUERES
                       WHEN 9
                           MOVE "S" TO SAIR
                           MOVE "N" TO REPETIR-MENU
                       WHEN OTHER
                           DISPLAY "Escolha invalida!" AT 1446
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           MOVE "S" TO REPETIR-MENU
                   END-EVALUATE
               END-PERFORM
           END-PERFORM.
           MOVE "N" TO SAIR.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       EXPORTAR-TUDO.
           PERFORM BASE.
           DISPLAY "Exportar Tudo" HIGHLIGHT AT 0217.
           OPEN OUTPUT IND-TODOS.
      *********
           MOVE 6 TO LINHA.
      *********
           MOVE 0 TO CLIENTE-COD.
           START FIC-CLIENTES KEY > CLIENTE-COD
               INVALID KEY
                   DISPLAY "Ficheiro de clientes vazio!"
                   HIGHLIGHT LINE LINHA COL 3
               NOT INVALID KEY
                   IF FS <> "05" AND FS <> "23" THEN
                       PERFORM UNTIL FS = "10"
                           READ FIC-CLIENTES NEXT RECORD
                               NOT AT END
                                   MOVE "C.Cl: " TO T1
                                   MOVE CLIENTE-COD TO IND-TODOS-CLIENTE
                                   MOVE " | NIF: " TO T2
                                   MOVE NIF TO IND-TODOS-NIF
                                   MOVE " | Nome: " TO T3
                                   MOVE NOME TO IND-TODOS-NOME
                                   MOVE " | Data Adms: " TO T4
                                   MOVE DIA-ADMISSAO
                                   TO IND-TODOS-DIA-ADMISSAO
                                   MOVE "-" TO D1
                                   MOVE MES-ADMISSAO
                                   TO IND-TODOS-MES-ADMISSAO
                                   MOVE "-" TO D2
                                   MOVE ANO-ADMISSAO
                                   TO IND-TODOS-ANO-ADMISSAO
                                   MOVE " | Email: " TO T5
                                   MOVE EMAIL TO IND-TODOS-EMAIL
                                   WRITE REGISTO-TODOS-CLIENTE
                           END-READ
                       END-PERFORM
                       DISPLAY "Clientes exportados com sucesso!"
                       HIGHLIGHT LINE LINHA COL 3
                   ELSE
                       DISPLAY "Erro ao exportar clientes!" HIGHLIGHT
                       FOREGROUND-COLOR 4 LINE LINHA COL 3
                   END-IF
           END-START.
      *********
           ADD 1 TO LINHA
           MOVE 0 TO LIVRO-COD.
           START FIC-LIVROS KEY > LIVRO-COD
               INVALID KEY
                  DISPLAY "Ficheiro de livros vazio!"
                   HIGHLIGHT LINE LINHA COL 3
               NOT INVALID KEY
                   IF FS <> "05" AND FS <> "23" THEN
                       PERFORM UNTIL FS = "10"
                           READ FIC-LIVROS NEXT RECORD
                               NOT AT END
                                   MOVE "C.Lv: " TO T6
                                   MOVE LIVRO-COD
                                   TO IND-TODOS-LIVRO
                                   MOVE " | C.Tm: " TO T7
                                   MOVE LIVRO-TEMA-COD
                                   TO IND-TODOS-LIVRO-TEMA
                                   MOVE " | C.At: " TO T8
                                   MOVE LIVRO-AUTOR-COD
                                   TO IND-TODOS-LIVRO-AUTOR
                                   MOVE " | Titulo: " TO T9
                                   MOVE TITULO
                                   TO IND-TODOS-TITULO
                                   WRITE REGISTO-TODOS-LIVROS
                           END-READ
                       END-PERFORM
                       DISPLAY "Livros exportados com sucesso!"
                       HIGHLIGHT LINE LINHA COL 3
                   ELSE
                       DISPLAY "Erro ao exportar livros!" HIGHLIGHT
                       FOREGROUND-COLOR 4 LINE LINHA COL 3
                   END-IF
           END-START.
      *********
           ADD 1 TO LINHA.
           MOVE 0 TO TEMA-COD.
           START FIC-TEMAS KEY > TEMA-COD
               INVALID KEY
                   DISPLAY "Ficheiro de temas vazio!"
                   HIGHLIGHT LINE LINHA COL 3
               NOT INVALID KEY
                   IF FS <> "05" AND FS <> "23" THEN
                       PERFORM UNTIL FS = "10"
                           READ FIC-TEMAS NEXT RECORD
                               NOT AT END
                                   MOVE "C.Tm: " TO T10
                                   MOVE TEMA-COD TO IND-TODOS-TEMA-COD
                                   MOVE " | Tema: " TO T11
                                   MOVE TEMA TO IND-TODOS-TEMA
                                   WRITE REGISTO-TODOS-TEMA
                           END-READ
                       END-PERFORM
                       DISPLAY "Temas exportados com sucesso!"
                       HIGHLIGHT LINE LINHA COL 3
                   ELSE
                       DISPLAY "Erro ao exportar temas!" HIGHLIGHT
                       FOREGROUND-COLOR 4 LINE LINHA COL 3
                   END-IF
           END-START.
      *********
           ADD 1 TO LINHA.
           MOVE 0 TO AUTOR-COD.
           START FIC-AUTORES KEY > AUTOR-COD
               INVALID KEY
                   DISPLAY "Ficheiro de autores vazio!"
                   HIGHLIGHT LINE LINHA COL 3
               NOT INVALID KEY
                   IF FS <> "05" AND FS <> "23" THEN
                       PERFORM UNTIL FS = "10"
                           READ FIC-AUTORES NEXT RECORD
                               NOT AT END
                                   MOVE "C.At: " TO T12
                                   MOVE AUTOR-COD TO IND-TODOS-AUTOR-COD
                                   MOVE " | Autor: " TO T13
                                   MOVE AUTOR TO IND-TODOS-AUTOR
                                   WRITE REGISTO-TODOS-AUTOR
                           END-READ
                       END-PERFORM
                       DISPLAY "Autores exportados com sucesso!"
                       HIGHLIGHT LINE LINHA COL 3
                   ELSE
                       DISPLAY "Erro ao exportar autores!" HIGHLIGHT
                       FOREGROUND-COLOR 4 LINE LINHA COL 3
                   END-IF
           END-START.
      *********
           ADD 1 TO LINHA.
           MOVE 0 TO ALUGUER-COD.
           START FIC-ALUGUERES KEY > ALUGUER-COD
               INVALID KEY
                   DISPLAY "Ficheiro de alugueres vazio!"
                   HIGHLIGHT LINE LINHA COL 3
               NOT INVALID KEY
                   IF FS <> "05" AND FS <> "23" THEN
                       PERFORM UNTIL FS = "10"
                           READ FIC-ALUGUERES NEXT RECORD
                               NOT AT END
                                   MOVE "C.Al: " TO T14
                                   MOVE ALUGUER-COD
                                   TO IND-TODOS-ALUGUER
                                   MOVE " | C.Cl: " TO T15
                                   MOVE ALUGUER-CLIENTE-COD
                                   TO IND-TODOS-ALUGUER-CLIENTE
                                   MOVE " | C.Lv: " TO T16
                                   MOVE ALUGUER-LIVRO-COD
                                   TO IND-TODOS-ALUGUER-LIVRO
                                   MOVE " | Data Alug: " TO T17
                                   MOVE DIA-ALUGUER
                                   TO IND-TODOS-DIA-ALUGUER
                                   MOVE "-" TO D3
                                   MOVE MES-ALUGUER
                                   TO IND-TODOS-MES-ALUGUER
                                   MOVE "-" TO D4
                                   MOVE ANO-ALUGUER
                                   TO IND-TODOS-ANO-ALUGUER
                                   MOVE " | Data Dvol: " TO T18
                                   MOVE DIA-ENTREGA
                                   TO IND-TODOS-DIA-ENTREGA
                                   MOVE "-" TO D5
                                   MOVE MES-ENTREGA
                                   TO IND-TODOS-MES-ENTREGA
                                   MOVE "-" TO D6
                                   MOVE ANO-ENTREGA
                                   TO IND-TODOS-ANO-ENTREGA
                                   MOVE " | Situacao: " TO T19
                                   MOVE SITUACAO
                                   TO IND-TODOS-SITUACAO
                                   WRITE REGISTO-TODOS-ALUGUER
                           END-READ
                       END-PERFORM
                       DISPLAY "Alugueres exportados com sucesso!"
                       HIGHLIGHT LINE LINHA COL 3
                   ELSE
                       DISPLAY "Erro ao exportar alugueres!" HIGHLIGHT
                       FOREGROUND-COLOR 4 LINE LINHA COL 3
                   END-IF
           END-START.

           CLOSE IND-TODOS.
           DISPLAY "Prima ENTER para continuar.         "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       EXPORTAR-ALUGUERES.
           PERFORM BASE.
           DISPLAY "Exportar Alugueres" HIGHLIGHT AT 0217.
           OPEN OUTPUT IND-ALUGADOS.
           MOVE 6 TO LINHA.
           MOVE 0 TO ALUGUER-COD.
           START FIC-ALUGUERES KEY > ALUGUER-COD
               INVALID KEY
                   DISPLAY "Ficheiro de alugueres vazio!"
                   HIGHLIGHT LINE LINHA COL 3
               NOT INVALID KEY
                   IF FS <> "05" AND FS <> "23" THEN
                       PERFORM UNTIL FS = "10"
                           READ FIC-ALUGUERES NEXT RECORD
                               NOT AT END
                                   MOVE "C.Al: " TO T20
                                   MOVE ALUGUER-COD
                                   TO IND-ALUGADOS-ALUGUER
                                   MOVE " | C.Cl: " TO T21
                                   MOVE ALUGUER-CLIENTE-COD
                                   TO IND-ALUGADOS-CLIENTE
                                   MOVE " | C.Lv: " TO T22
                                   MOVE ALUGUER-LIVRO-COD
                                   TO IND-ALUGADOS-LIVRO
                                   MOVE " | Data Alug: " TO T23
                                   MOVE DIA-ALUGUER
                                   TO IND-ALUGADOS-DIA-ALUGUER
                                   MOVE "-" TO D7
                                   MOVE MES-ALUGUER
                                   TO IND-ALUGADOS-MES-ALUGUER
                                   MOVE "-" TO D8
                                   MOVE ANO-ALUGUER
                                   TO IND-ALUGADOS-ANO-ALUGUER
                                   MOVE " | Data Dvol: " TO T24
                                   MOVE DIA-ENTREGA
                                   TO IND-ALUGADOS-DIA-ENTREGA
                                   MOVE "-" TO D9
                                   MOVE MES-ENTREGA
                                   TO IND-ALUGADOS-MES-ENTREGA
                                   MOVE "-" TO D10
                                   MOVE ANO-ENTREGA
                                   TO IND-ALUGADOS-ANO-ENTREGA
                                   MOVE " | Situacao: " TO T25
                                   MOVE SITUACAO
                                   TO IND-ALUGADOS-SITUACAO
                                   WRITE REGISTO-ALUGADOS
                           END-READ
                       END-PERFORM
                       DISPLAY "Alugueres exportados com sucesso!"
                       HIGHLIGHT LINE LINHA COL 3
                   ELSE
                       DISPLAY "Erro ao exportar alugueres!" HIGHLIGHT
                       FOREGROUND-COLOR 4 LINE LINHA COL 3
                   END-IF
           END-START.

           CLOSE IND-ALUGADOS.
           DISPLAY "Prima ENTER para continuar.         "
           HIGHLIGHT AT 0403.
           ACCEPT OMITTED AT 0430.
           MOVE "N" TO REPETIR-MENU.
      ******************************************************************
       END PROGRAM TAREFAFINAL.
