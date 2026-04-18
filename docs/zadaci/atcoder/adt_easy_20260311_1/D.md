# Задатак: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 5000;
var
	n, i, l, k: int16;
	s: string;
	found: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	for i := 1 to n-1 do begin
		l := n-i;
		found := false;
		while (l >= 0) and not found do begin

			k := 1;
			while (k <= l) and (s[k] <> s[k+i]) do inc(k);

			if k <= l then
				dec(l)
			else
				found := true;

		end;
		writeln(l);
	end;
end.

```
