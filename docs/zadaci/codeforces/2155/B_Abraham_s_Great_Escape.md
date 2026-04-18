# Задатак: B_Abraham_s_Great_Escape.pas

```pascal
program B_Abraham_s_Great_Escape;
{$MODE DELPHI}
const
	nn = 100;
var
	notc, tci, n, k, i, j, x: int32;
	s: array [1 .. nn] of string;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, k);

		x := 0;
		for i := 1 to n do begin
			setlength(s[i], n);
			for j := 1 to n do
				if x < k then begin
					s[i][j] := 'U';
					inc(x);
				end else if i < n then
					s[i][j] := 'D'
				else if j < n then
					s[i][j] := 'R'
				else
					s[i][j] := 'L';
		end;

		if k = n*n-1 then
			writeln('NO')
		else begin
			writeln('YES');
			for i := 1 to n do writeln(s[i]);
		end;

	end;
end.

```
