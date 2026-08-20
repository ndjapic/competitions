program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 300 * 1000;
var
	notc, tci, n, i: int32;
	p: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do begin
			p[3*i-2] := i;
			p[3*i-1] := n-1+2*i;
			p[3*i-0] := n-0+2*i;
		end;

		for i := 1 to 3*n-1 do write(p[i], ' ');
		writeln(p[3*n]);

	end;
end.
