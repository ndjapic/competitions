# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults;
const
	nn = 200 * 1000;
var
	notc, tci, n, i, j, x: int32;
	a: array [1 .. nn] of int32;
	s: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do read(a[i]); readln;

		s := TList<int32>.Create;
		for i := 1 to n div 2 do
			if odd(i) then begin
				s.Clear;
				j := i;
				while (j <= n) do begin
					s.Add(a[j]);
					inc(j, j);
				end;

				s.Sort;
				j := i;
				for x in s do begin
					a[j] := x;
					inc(j, j);
				end;
			end;
		s.Free;

		i := 1;
		while (i < n) and (a[i] < a[i+1]) do inc(i);

		if i = n then
			writeln('YES')
		else
			writeln('NO');

	end;
end.

```
