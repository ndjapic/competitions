program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 100;
var
	n, a, b, i, j, e: int8;
	s: array [0 .. 1] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, a, b);

	for e := 0 to 1 do begin
		setlength(s[e], b*n);
		for j := 1 to b*n do
			if (j-1) mod b > 0 then
				s[e][j] := s[e][j-1]
			else if odd((j-1) div b + e) then
				s[e][j] := '#'
			else
				s[e][j] := '.';
	end;

	for i := 1 to a*n do begin
		e := (i-1) div a mod 2;
		writeln(s[e]);
	end;
end.
