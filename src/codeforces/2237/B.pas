program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 2000;
var
	notc, tci, n, i, j, x, ans: int32;
	a, b: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do read(a[i]);
		readln;

		for i := 1 to n do read(b[i]);
		readln;

		ans := 0;
		for i := 1 to n do
			if ans > -1 then begin
				j := i;
				while (j <= n) and (a[j] > b[i]) do inc(j);

				if j > n then
					ans := -1
				else
					while j > i do begin
						x := a[j];
						a[j] := a[j-1];
						a[j-1] := x;
						inc(ans);
						dec(j);
					end;
			end;

		writeln(ans);

	end;
end.
