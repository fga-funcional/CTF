# CTF
CTF


## Para compilar o front-end

```
cd front
bash script.sh
elm reactor
```

## Para o back

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