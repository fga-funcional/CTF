#!/usr/bin/env bash
devd -X /api/=http://localhost:3000 /=http://localhost:8000 -p 8080
