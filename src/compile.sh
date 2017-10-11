./minic $1.c
gcc -o $1.exe -g $1.s
$1.exe
echo ""
echo "----"
cat $1.out
echo ""