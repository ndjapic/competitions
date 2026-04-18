# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	n = 9;
var
	i, j, r, c: int8;
	seen: int16;
	ans: boolean;
	a: array [1 .. n, 1 .. n] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for i := 1 to n do begin
		for j := 1 to n do read(a[i, j]);
		readln;
	end;

	ans := true;

	for i := 1 to n do begin
		seen := 0;
		for j := 1 to n do inc(seen, 1 shl a[i, j]);
		ans := ans and (seen = 1022);
	end;

	for j := 1 to n do begin
		seen := 0;
		for i := 1 to n do inc(seen, 1 shl a[i, j]);
		ans := ans and (seen = 1022);
	end;

	for r := 1 to 3 do
		for c := 1 to 3 do begin
			seen := 0;
			for i := 3*r-2 to 3*r do
				for j := 3*c-2 to 3*c do inc(seen, 1 shl a[i, j]);
			ans := ans and (seen = 1022);
		end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.

```
