program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 100;
var
	n, i, e: int8;
	ans: int32;
	s, t, z: array [1 .. nn] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function dist(): int32;
var
	i, j: int8;
begin
	result := 0;
	for i := 1 to n do
		for j := 1 to n do
			if s[i][j] <> t[i][j] then inc(result);
end;

procedure rotate();
var
	i, j: int8;
begin
	for i := 1 to n do
		for j := 1 to n do
			z[j][n+1-i] := s[i][j];
	for i := 1 to n do
		s[i] := z[i];
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do readln(s[i]);
	for i := 1 to n do readln(t[i]);
	for i := 1 to n do setlength(z[i], n);

	ans := dist();
	for e := 1 to 3 do begin
		rotate();
		ans := min(ans, dist() + e);
	end;
	writeln(ans);
end.
