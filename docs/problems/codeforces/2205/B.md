# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 500;
var
	notc, tci, n, k, p: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		k := 1;
		p := 2;
		while p*p <= n do begin
			if n mod p = 0 then k := k * p;
			while n mod p = 0 do n := n div p;
			inc(p);
		end;

		if n > 1 then k := k * n;

		writeln(k);

	end;
end.

```
