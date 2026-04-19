# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 500;
var
	notc, tci, n, i, j: int32;
	p: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do begin
			read(p[i]);
			if p[i] = n then j := i;
		end;
		readln;

		p[j] := p[1];
		p[1] := n;

		for i := 1 to n-1 do write(p[i], ' ');
		writeln(p[n]);

	end;
end.

```
