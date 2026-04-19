# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 5000;
var
	notc, tci, n, i, top: int32;
	s: string;
	stack: array [1 .. nn] of char;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(s);

		top := 0;
		for i := 1 to n do begin
			inc(top);
			stack[top] := s[i];
			if (top >= 2) and (stack[top-1] = stack[top]) then dec(top, 2);
		end;

		if top > 0 then
			writeln('NO')
		else
			writeln('YES');

	end;
end.

```
