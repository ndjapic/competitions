# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, m, s, t, x, i, p, v: int32;
	ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);
	readln(s, t);

	if t < s then begin
		x := s;
		s := t;
		t := x;
	end;

	ans := 0;
	for i := 1 to m do begin
		readln(p, v);
		if (s <= p) and (p <= t) then inc(ans, v);
	end;
	writeln(ans);
end.

```
