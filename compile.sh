python3 main.py
if [ -e output.ll ]
then
  gcc -Wno-override-module output.ll -o output
  rm output.ll
fi
