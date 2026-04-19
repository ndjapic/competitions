# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, h, m, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	read(m);
	ans := 1;

	for i := 2 to n do begin
		read(h);
		if h > m then begin
			inc(ans);
			m := h;
		end;
	end;
	readln;

	writeln(ans);
end.

```
