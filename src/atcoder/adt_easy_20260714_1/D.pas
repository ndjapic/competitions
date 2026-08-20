program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #grid #rotation
const
	NN = 100;
var
	n, i, j: int8;
	ch: char;
	a: array [1 .. NN] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do readln(a[i]);

	ch := a[1][1];
	for i := 2 to n do a[i-1][1] := a[i][1];
	for j := 2 to n do a[n][j-1] := a[n][j];
	for i := n-1 downto 1 do a[i+1][n] := a[i][n];
	for j := n-1 downto 2 do a[1][j+1] := a[1][j];
	a[1][2] := ch;

	for i := 1 to n do writeln(a[i]);
end.
