# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 50;
var
	notc, tci, n, i, j, ans: int32;
	s: string;
	seen: array [1 .. nn] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(s);

		for i := 1 to n do seen[i] := false;

		i := 1;
		seen[i] := true;
		for j := 1 to n do begin
			case s[i] of
				'L': dec(i);
				'R': inc(i);
			end;
			seen[i] := true;
		end;

		ans := 0;
		for i := 1 to n do
			if seen[i] then inc(ans);

		writeln(ans);

	end;
end.

```
