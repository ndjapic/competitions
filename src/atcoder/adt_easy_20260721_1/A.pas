program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	sysutils, strutils;
const
	NN = 100;
var
	n, i, i0: int8;
	line: string;
	sa: TStringArray;
	s: array [1 .. NN] of string;
	a: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	i0 := 1;
	for i := 1 to n do begin
		readln(line);
		sa := SplitString(line, ' ');
		s[i] := sa[0];
		a[i] := StrToInt(sa[1]);
		if a[i0] > a[i] then i0 := i;
	end;

	for i := i0 to n do writeln(s[i]);
	for i := 1 to i0-1 do writeln(s[i]);
end.
