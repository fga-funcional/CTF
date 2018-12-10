# CTF
CTF

Nome | Matrícula
:-------: | ------:
Harrison Pedro     | 14/0142801
Gustavo Lopes    | 14/0142657

## Para compilar todo o projeto

```
bash run.sh
```

## Para compilar o front-end apenas

```
cd front
bash script.sh
elm reactor
```

## Para compilar o back-end apenas

```
sudo apt-get install sqlite3 libsqlite3-dev
stack install HDBC
stack install haskelldb
stack install haskelldb-hdbc
stack install HDBC-sqlite3
stack ghci --no-package-hiding
```

```
cd backend
stack build
stack ghci
```

Dentro do ghci

```
:load Main
main
```
