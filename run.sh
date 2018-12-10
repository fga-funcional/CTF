#!/usr/bin/env bash
cd backend/ctf/
echo "main" | stack ghci --no-package-hiding &

cd ../../front/
bash script.sh &
elm reactor &

cd ..
devd -X /api/=http://localhost:3000 /=http://localhost:8000 -p 8080
