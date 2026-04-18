# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, i, j, l, r: int8;
	found: boolean;
	s: array [1 .. nn] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function getc(i, j, k: int8): char;
begin
	if k <= length(s[i]) then
		getc := s[i][k]
	else
		getc := s[j][k - length(s[i])];
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do readln(s[i]);

	found := false;
	for i := 1 to n do
		if not found then
			for j := 1 to n do
				if not found and (i <> j) then begin
					l := 1;
					r := length(s[i]) + length(s[j]);

					while (l < r) and (getc(i, j, l) = getc(i, j, r)) do begin
						inc(l);
						dec(r);
					end;

					found := l >= r;
				end;

	if found then
		writeln('Yes')
	else
		writeln('No');
end.

```
