program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, m, i, j: int8;
	s: array [1 .. nn] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function tak(a, b, c, d: int8): boolean;
var
	i, j: int8;
begin
	result := true;

	for i := a to a+2 do begin
		for j := b to b+2 do
			result := result and (s[i][j] = '#');
		result := result and (s[i][b+3] = '.');
	end;

	for j := b to b+3 do
		result := result and (s[a+3][j] = '.');

	for j := d-3 to d do
		result := result and (s[c-3][j] = '.');

	for i := c-2 to c do begin
		result := result and (s[i][d-3] = '.');
		for j := d-2 to d do
			result := result and (s[i][j] = '#');
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do readln(s[i]);

	for i := 9 to n do
		for j := 9 to m do
			if tak(i-8, j-8, i, j) then writeln(i-8, ' ', j-8);
end.
