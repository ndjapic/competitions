# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, j, k, e: int8;
	s, a, b: string;
	seen: int64;
	ans: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	seen := 0;

	a := 'HDCS';
	b := 'A23456789TJQK';
	ans := true;

	for i := 1 to n do begin
		readln(s);

		if ans then begin
			j := 1;
			while (j <= 4) and (s[1] <> a[j]) do inc(j);

			k := 1;
			while (k <= 13) and (s[2] <> b[k]) do inc(k);

			if (j <= 4) and (k <= 13) then begin
				e := (j-1) * 13 + k-1;
				if not odd(seen shr e) then
					inc(seen, int64(1) shl e)
				else
					ans := false;
			end else
				ans := false;
		end;
	end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.

```
