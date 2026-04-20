program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, i, j: int8;
	ch: char;
	a: array [1 .. nn] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do readln(a[i]);

	ch := a[1][1];
	for i := 1 to n-1 do a[i][1] := a[i+1][1];
	for j := 1 to n-1 do a[n][j] := a[n][j+1];
	for i := n downto 2 do a[i][n] := a[i-1][n];
	for j := n downto 3 do a[1][j] := a[1][j-1];
	a[1][2] := ch;

	for i := 1 to n do writeln(a[i]);
end.
